
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.4
 *  @date    Sat Sep  8 13:53:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @see eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Naive_Bayes_Classifier_Explained.pdf
 */

package scalation.analytics.classifier.par

import java.util.concurrent.ForkJoinPool

import scala.collection.parallel.ForkJoinTaskSupport
import scala.math.min

import scalation.linalgebra.{MatriI, VectoI, VectorD, VectorI}
import scalation.linalgebra.gen.HMatrix3
import scalation.columnar_db.Relation
import scalation.util.banner

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayes` class implements an Integer-Based Naive Bayes Classifier,
 *  which is a commonly used such classifier for discrete input data.  The
 *  classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier is naive, because it assumes feature independence and therefore
 *  simply multiplies the conditional probabilities.
 *
 *  This classifier uses the standard cross-validation technique.
 *  -----------------------------------------------------------------------------
 *
 *  @param x            the integer-valued data vectors stored as rows of a matrix
 *  @param y            the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn           the names for all features/variables
 *  @param k            the number of classes
 *  @param cn           the names for all classes
 *  @param vc           the value count (number of distinct values) for each feature
 *  @param me           use m-estimates (me == 0 => regular MLE estimates)
 *  @param PARALLELISM  the level of parallelism, the number of threads to use
 */
class NaiveBayes0 (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                  private var vc: VectoI = null, me: Double = me_default, private val PARALLELISM: Int = Runtime.getRuntime().availableProcessors())
      extends BayesClassifier (x, y, fn, k, cn, PARALLELISM)
{
    private val DEBUG = false                                // debug flag

    if (vc == null) {
        shiftToZero; vc = vc_fromData                        // set value counts from data
    } // if

    protected val vca = vc.toArray

    f_CX  = new HMatrix3 [Int] (k, n, vca)                   // frequency counts for class yi & feature xj
    protected val p_X_C = new HMatrix3 [Double] (k, n, vca)  // conditional probabilities for feature xj given class yi

    if (DEBUG) println ("distinct value count vc = " + vc)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int) = train (testStart until testEnd)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j. This is the quick version that uses
     *  the "subtraction" method to achieve efficiency.
     *  @param itest  indices of the instances considered testing data
     */
    override def train (itest: IndexedSeq [Int])
    {
        val idx = if (additive) 0 until m diff itest
                  else          itest
        frequencies (idx)
        if (DEBUG) banner ("train (itest)")
        train2 ()
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     */
    private def train2 ()
    {
        p_C = f_C.toDouble / md                               // prior probability for class yi
        for (i <- (0 until k).par; j <- (0 until n).par if fset(j)) {    // for each class yi & feature xj
            val me_vc = me / vc(j).toDouble
            for (xj <- (0 until vc(j)).par) {                 // for each value for feature j: xj
                p_X_C(i, j, xj) = (f_CX(i, j, xj) + me_vc) / (f_C(i) + me)
            } // for
        } // for

        if (DEBUG) {
            println ("p_C   = " + p_C)                        // P(C = yi)
            println ("p_X_C = " + p_X_C)                      // P(X_j = x | C = yi)
        } // if
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  This is the quick/decremental version.
     *  @param indices  indices of the instances considered training data
     */
    private def frequencies (indices: IndexedSeq [Int])
    {
        reset ()
        val idxA = split (indices, PARALLELISM)

        val f_Cw = Array.ofDim [VectorI] (PARALLELISM)
        val f_CXw = Array.ofDim [HMatrix3[Int]] (PARALLELISM)

        for(w <- (0 until PARALLELISM).par) {
            f_Cw(w) = new VectorI (k)
            f_CXw(w) = new HMatrix3 [Int] (k, n, vca)
        } // for

        val paraRange = (0 until PARALLELISM).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (w <- paraRange; i <- idxA(w)) updateFreq (i, f_Cw(w), f_CXw(w))

        for (w <- 0 until PARALLELISM){
            f_C += f_Cw (w)
            f_CX += f_CXw (w)
        } // for

        if (DEBUG) {
            println ("l_f_C = " + f_C)                  // #(C = i)
            println ("l_f_CX = " + f_CX)                // #(X_j = x & C = i)
        } // if
    } // frequencies


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i        the index for current data row
     *  @param f_C      frequency table of class C
     *  @param f_CX     joint frequency table of C and X
     */
    protected def updateFreq (i: Int, f_C: VectoI, f_CX: HMatrix3[Int])
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) += 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) f_CX (yi, j, x(i, j)) += 1
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        if (DEBUG) banner ("classify (z)")
        val prob = new VectorD (p_C)
        for (i <- 0 until k; j <- 0 until n if fset(j)) prob(i) *= p_X_C(i, j, z(j))   // P(X_j = z_j | C = i)
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()                    // class with the highest relative posterior probability
        (best, cn(best), prob(best))                 // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the frequency tables to 0.
     */
    def reset ()
    {
        f_C.set (0)
        f_CX.set (0)
    } // reset

} // NaiveBayes0 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `NaiveBayes0` is the companion object for the `NaiveBayes0` class.
 */
object NaiveBayes0
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NaiveBayes0` object, passing 'x' and 'y' together in one matrix.
     *
     *  @param xy           the data vectors along with their classifications stored as rows of a matrix
     *  @param fn           the names for all features/variables
     *  @param k            the number of classes
     *  @param cn           the names for all classes
     *  @param vc           the value count (number of distinct values) for each feature
     *  @param me           use m-estimates (me == 0 => regular MLE estimates)
     *  @param PARALLELISM  the level of parallelism, the number of threads to use
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI = null, me: Double = me_default, PARALLELISM: Int = Runtime.getRuntime().availableProcessors()) =
    {
        new NaiveBayes0 (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn, vc, me, PARALLELISM)
    } // apply

} // NaiveBayes0 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The same classifier but uses an optimized cross-validation technique.
 *  -----------------------------------------------------------------------------
 *
 *  @param x            the integer-valued data vectors stored as rows of a matrix
 *  @param y            the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn           the names for all features/variables
 *  @param k            the number of classes
 *  @param cn           the names for all classes
 *  @param vc           the value count (number of distinct values) for each feature
 *  @param me           use m-estimates (me == 0 => regular MLE estimates)
 *  @param PARALLELISM  the level of parallelism, the number of threads to use
 */
class NaiveBayes (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                   private var vc: VectoI = null, me: Double = me_default, private val PARALLELISM: Int = Runtime.getRuntime().availableProcessors())
        extends NaiveBayes0 (x, y, fn, k, cn, vc, me, PARALLELISM)
{
    private val DEBUG = false                              // debug flag

    private val g_f_C  = new VectorI (k)                   // global frequency counts (using entire dataset) for class yi
    private var g_f_CX = new HMatrix3 [Int] (k, n, vca)    // global frequency counts (using entire dataset) for class yi & feature xj

    additive = false

    if (vc == null) vc = new VectorI (vca.length, vca)

    if (DEBUG) println ("distinct value count vc = " + vc)

    frequenciesAll()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute frequency counts using the entire data matrix
     */
    def frequenciesAll ()
    {
        val size = m / PARALLELISM + 1
        val g_f_Cw = Array.ofDim [VectorI] (PARALLELISM)
        val g_f_CXw = Array.ofDim [HMatrix3[Int]] (PARALLELISM)

        for(w <- (0 until PARALLELISM).par) {
            g_f_Cw(w) = new VectorI (k)
            g_f_CXw(w) = new HMatrix3 [Int] (k, n, vca)
        } // for

        val paraRange = (0 until PARALLELISM).par
        paraRange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (PARALLELISM))

        for (w <- paraRange; i <- w * size until min((w+1)*size, m)) {
            val yi = y(i)
            g_f_Cw(w)(yi) += 1
            for (j <- 0 until n if fset(j)) {
                g_f_CXw(w)(yi, j, x(i, j)) += 1
            } // for
        } // for

        for (w <- 0 until PARALLELISM){
            g_f_C += g_f_Cw (w)
            g_f_CX += g_f_CXw (w)
        } // for

        if (DEBUG) {
            println ("g_f_C  = " + g_f_C)                         // #(C = yi)
            println ("g_f_CX = " + g_f_CX)                        // #(C = yi & X_j = x)
        } // if

    } // frequenciesAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decrement frequency counters based on the 'i'th row of the data matrix.
     *  @param i  the index for current data row
     */
    protected override def updateFreq (i: Int, f_C: VectoI, f_CX: HMatrix3[Int])
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) -= 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) f_CX (yi, j, x(i, j)) -= 1
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the frequency tables from global frequencies.
     */
    override def reset ()
    {
        for (i <- (0 until k).par) {
            f_C(i) = g_f_C(i)
            for (j <- x.range2.par if fset(j); xj <- (0 until vc(j)).par) f_CX(i, j, xj) = g_f_CX(i, j, xj)
        } // for
    } // reset

} // NaiveBayes class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `NaiveBayes` is the companion object for the `NaiveBayes` class.
 */
object NaiveBayes
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NaiveBayes` object, passing 'x' and 'y' together in one matrix.
     *
     *  @param xy           the data vectors along with their classifications stored as rows of a matrix
     *  @param fn           the names for all features/variables
     *  @param k            the number of classes
     *  @param cn           the names for all classes
     *  @param vc           the value count (number of distinct values) for each feature
     *  @param me           use m-estimates (me == 0 => regular MLE estimates)
     *  @param PARALLELISM  the level of parallelism, the number of threads to use
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectoI = null, me: Double = me_default, PARALLELISM: Int = Runtime.getRuntime().availableProcessors()) =
    {
        new NaiveBayes (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn, vc, me, PARALLELISM)
    } // apply

} // NaiveBayes object


import scalation.linalgebra.MatrixI

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest` object is used to test the `NaiveBayes` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > runMain scalation.analytics.classifier.par.NaiveBayesTest
 */
object NaiveBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // x3: Mpg:     High (1), Low (0)
    // features:                 x0 x1 x2 x3
    val x = new MatrixI ((10, 4), 1, 0, 1, 1,               // data matrix
                                  1, 0, 1, 0,
                                  1, 0, 1, 1,
                                  0, 0, 1, 1,
                                  0, 0, 0, 1,
                                  0, 1, 0, 0,
                                  0, 1, 0, 0,
                                  0, 1, 1, 1,
                                  1, 1, 0, 0,
                                  1, 0, 0, 0)

    val y  = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)         // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin", "Mpg")       // feature/variable names
    val cn = Array ("No", "Yes")                            // class names

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val nb0 = new NaiveBayes0 (x, y, fn, 2, cn)             // create the classifier
    val nb  = new NaiveBayes  (x, y, fn, 2, cn)             // create the classifier
    nb0.train ()                                            // train the classifier
    nb.train ()                                             // train the classifier

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1, 1)                           // existing data vector to classify
    val z2 = VectorI (1, 1, 1, 0)                           // new data vector to classify
    println ("Use nb0 to classify (" + z1 + ") = " + nb0.classify (z1))
    println ("Use nb  to classify (" + z1 + ") = " + nb.classify (z1))
    println ("Use nb0 to classify (" + z2 + ") = " + nb0.classify (z2))
    println ("Use nb  to classify (" + z2 + ") = " + nb.classify (z2))

} // NaiveBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest2` object is used to test the 'NaiveBayes' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.par.NaiveBayesTest2
 */
object NaiveBayesTest2 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                 x0 x1  y
    val xy = new MatrixI ((10, 3), 1, 1, 1,
                                   1, 1, 1,
                                   1, 0, 1,
                                   1, 0, 1,
                                   1, 0, 0,
                                   0, 1, 0,
                                   0, 1, 0,
                                   0, 1, 1,
                                   0, 0, 0,
                                   0, 0, 0)

    val fn = Array ("Fast", "Strong")                     // feature names
    val cn = Array ("No", "Yes")                          // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val nb0 = NaiveBayes0 (xy, fn, 2, cn, null)           // create the classifier
    val nb  = NaiveBayes  (xy, fn, 2, cn, null)           // create the classifier
    nb0.train ()                                          // train the classifier
    nb.train ()                                           // train the classifier

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                // new data vector to classify
    println ("Use nb0 to classify (" + z + ") = " + nb0.classify (z))
    println ("Use nb  to classify (" + z + ") = " + nb.classify (z))

    println ("nb0 cv accu = " + nb0.crossValidateRand ()) // cross validate the classifier
    println ("nb  cv accu = " + nb.crossValidateRand ())  // cross validate the classifier

} // NaiveBayesTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest3` object is used to test the 'NaiveBayes' class.
 *  > runMain scalation.analytics.classifier.par.NaiveBayesTest3
 */
object NaiveBayesTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray.slice (0, xy.dim2 - 1)
    val cn = Array ("0", "1")                              // class names

    val nb0   = NaiveBayes0 (xy, fn, 2, cn, null, 0)       // create the classifier
    val nb    = NaiveBayes  (xy, fn, 2, cn, null, 0)       // create the classifier

    println ("nb0 cv accu = " + nb0.crossValidateRand ())
    println ("nb  cv accu = " + nb.crossValidateRand ())

    nb0.featureSelection ()
    nb.featureSelection ()

    println ("After feature selection")
    println ("nb0 cv accu = " + nb0.crossValidateRand ())
    println ("nb  cv accu = " + nb.crossValidateRand ())

} // NaiveBayesTest3 object
