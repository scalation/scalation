
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.5
 *  @date    Sat Sep  8 13:53:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @see eric.univ-lyon2.fr/~ricco/tanagra/fichiers/en_Tanagra_Naive_Bayes_Classifier_Explained.pdf
 */

package scalation.analytics.classifier

import scala.math.log

import scalation.columnar_db.Relation
import scalation.linalgebra.{MatriI, MatrixI, VectoD, VectorD, VectoI, VectorI}
import scalation.linalgebra.gen.HMatrix3
import scalation.util.banner

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayes0` class implements an Integer-Based Naive Bayes Classifier,
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
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param vc  the value count (number of distinct values) for each feature
 *  @param me  use m-estimates (me == 0 => regular MLE estimates)
 */
class NaiveBayes0 (x: MatriI, y: VectoI, fn: Array [String] = null,
                   k: Int = 2, cn: Array [String] = Array ("no", "yes"),
                   protected var vc: Array [Int] = null, me: Double = me_default)
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG = false                                // debug flag

    if (cn.length != k) flaw ("constructor", "# class names != # classes")

    if (vc == null) {
        shiftToZero; vc = vc_fromData                        // set value counts from data
    } // if

    f_CX = new HMatrix3 [Int] (k, n, vc)                     // frequency counts for class yi & feature xj
    protected val p_X_C = new HMatrix3 [Double] (k, n, vc)   // conditional probabilities for feature xj given class yi

    if (DEBUG) println ("distinct value count vc = " + vc.deep)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for y, and the
     *  conditional probabilities for x_j.
     *  @param itest  indices of the instances considered as testing data
     */
    def train (itest: IndexedSeq [Int]): NaiveBayes0 =
    {
        val idx = if (additive) 0 until m diff itest else itest
        frequencies (idx)
        train2 ()
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for y, and the
     *  conditional probabilities for x_j.
     */
    private def train2 ()
    {
        p_C = nu_y.toDouble / md                               // probability for class yi
        for (i <- 0 until k; j <- 0 until n if fset(j)) {      // for each class yi & feature xj
            val me_vc = me / vc(j).toDouble
            for (xj <- 0 until vc(j)) {                        // for each value for feature j: xj
                p_X_C(i, j, xj) = (f_CX(i, j, xj) + me_vc) / (nu_y(i) + me)
            } // for
        } // for

        if (DEBUG) {
            println ("p_C   = " + p_C)                         // P(y = l)
            println ("p_X_C = " + p_X_C)                       // P(x_j = h | y = l)
        } // if
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  This is the quick/decremental version.
     *  @param idx  indices of the instances considered training data
     */
    protected def frequencies (idx: IndexedSeq [Int])
    {
        reset ()
        for (i <- idx) updateFreq (i)

        if (DEBUG) {
            println ("nu_y = " + nu_y)                         // #(y = l)
            println ("f_CX = " + f_CX)                         // #(x_j = h & y = l)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters based on the 'i'th row of the data matrix.
     *  @param i  the index for current data row
     */
    protected override def updateFreq (i: Int)
    {
        val yi    = y(i)                                       // get the class for ith row
        nu_y(yi) += 1                                          // increment frequency for class yi
        for (j <- x.range2 if fset(j)) f_CX (yi, j, x(i, j)) += 1
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
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
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative log-probability.
     *  This version adds log-probabilities.
     *  @param z  the data vector to classify
     */
    def lclassify (z: VectoI): (Int, String, Double) =
    {
        if (DEBUG) banner ("lclassify (z)")
        val prob = vlog (new VectorD (p_C))
        for (i <- 0 until k; j <- 0 until n if fset(j)) prob(i) += log (p_X_C(i, j, z(j)))   // P(X_j = z_j | C = i)
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()                    // class with the highest relative posterior probability
        (best, cn(best), prob(best))                 // return the best class and its name
    } // lclassify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the log of the given probability vector.
     *  @param p  the given probability vector
     */
    protected def vlog (p: VectoD): VectoD = p.map (log (_))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize all the population and probability vectors and
     *  hypermatrices to 0.
     */
    def reset ()
    {
        nu_y.set (0)
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
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: Array [Int] = null, me: Double = me_default) =
    {
        new NaiveBayes0 (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn, vc, me)
    } // apply

} // NaiveBayes0 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The same classifier but uses an optimized cross-validation technique.
 *  -----------------------------------------------------------------------------
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param vc  the value count (number of distinct values) for each feature
 *  @param me  use m-estimates (me == 0 => regular MLE estimates)
 */
class NaiveBayes (x: MatriI, y: VectoI, fn: Array [String] = null,
                  k: Int = 2, cn: Array [String] = Array ("no", "yes"),
                  vc_ : Array [Int] = null, me: Float = me_default)
        extends NaiveBayes0 (x, y, fn, k, cn, vc_, me)
{
    private val DEBUG = true                               // debug flag

    println ("vc = " + vc.deep)
    private val g_nu_y  = new VectorI (k)                  // global frequency counts (using entire dataset) for class yi
    private val g_f_CX = new HMatrix3 [Int] (k, n, vc)     // global frequency counts (using entire dataset) for class yi & feature xj

    additive = false

    if (DEBUG) println ("distinct value count vc = " + vc.deep)

    frequenciesAll ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute frequency counts using the entire data matrix
     */
    def frequenciesAll ()
    {
        for (i <- 0 until m) {
            val yi = y(i)
            g_nu_y(yi) += 1
            for (j <- 0 until n if fset(j)) g_f_CX (yi, j, x(i, j)) += 1
        } // for
    } // frequenciesAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decrement frequency counters based on the 'i'th row of the data matrix.
     *  @param i  the index for current data row
     */
    protected override def updateFreq (i: Int)
    {
        val yi   = y(i)                                        // get the class for ith row
        nu_y(yi) -= 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) f_CX (yi, j, x(i, j)) -= 1
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables from the global frequencies.
     */
    override def reset ()
    {
        for (i <- 0 until k) {
            nu_y(i) = g_nu_y(i)
            for (j <- x.range2 if fset(j); xj <- 0 until vc(j)) f_CX(i, j, xj) = g_f_CX(i, j, xj)
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
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               vc: Array [Int] = null, me: Float = me_default) =
    {
        new NaiveBayes (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn, vc, me)
    } // apply

} // NaiveBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest` object is used to test the 'NaiveBayes' class.
 *  > runMain scalation.analytics.classifier.NaiveBayesTest
 */
object NaiveBayesTest extends App
{
    import ExampleTennis._

    banner ("Tennis Example")
    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val nb0 = NaiveBayes0 (xy, fn, k, cn, null, 0)                      // create a classifier
    val nb  = NaiveBayes  (xy, fn, k, cn, null, 0)                      // create a classifier
    nb0.train ()                                                        // train the classifier
    nb.train ()                                                         // train the classifier

    val z = VectorI (2, 2, 1, 1)                                        // new data vector to classify
    println ("Use nb0 to classify (" + z + ") = " + nb0.classify (z))
    println ("Use nb  to classify (" + z + ") = " + nb.classify (z))

    println ("nb0 cv accu = " + nb0.crossValidateRand (10, true))
    println ("nb  cv accu = " + nb.crossValidateRand (10, true))

} // NaiveBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest2` object is used to test the `NaiveBayes` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 *  > runMain scalation.analytics.classifier.NaiveBayesTest2
 */
object NaiveBayesTest2 extends App
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

    val yp = nb.classify (x)
    println (nb.fitLabel)
    println (nb.fit (y, yp))

} // NaiveBayesTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest3` object is used to test the 'NaiveBayes' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.NaiveBayesTest3
 */
object NaiveBayesTest3 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                  x0 x1  y
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

} // NaiveBayesTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest4` object is used to test the 'NaiveBayes' class.
 *  @see archive.ics.uci.edu/ml/datasets/Lenses
 *  @see docs.roguewave.com/imsl/java/7.3/manual/api/com/imsl/datamining/NaiveBayesClassifierEx2.html
 *  > runMain scalation.analytics.classifier.NaiveBayesTest4
 */
object NaiveBayesTest4 extends App
{
    // training-set -----------------------------------------------------------
    // y:  Classification (1): hard contact lenses, (2) soft contact lenses, (3) no contact lenses
    // x0. Age of the patient: (1) young, (2) pre-presbyopic, (3) presbyopic
    // x1. Spectacle prescription:  (1) myope, (2) hypermetrope
    // x2. Astigmatic:     (1) no, (2) yes
    // x3. Tear production rate:  (1) reduced, (2) normal
    // features:                  x0  x1  x2  x3   y
    val xy = new MatrixI ((24, 5), 1,  1,  1,  1,  3,           // 1
                                   1,  1,  1,  2,  2,           // 2
                                   1,  1,  2,  1,  3,           // 3
                                   1,  1,  2,  2,  1,           // 4
                                   1,  2,  1,  1,  3,           // 5
                                   1,  2,  1,  2,  2,           // 6
                                   1,  2,  2,  1,  3,           // 7
                                   1,  2,  2,  2,  1,           // 8
                                   2,  1,  1,  1,  3,           // 9
                                   2,  1,  1,  2,  2,           // 10
                                   2,  1,  2,  1,  3,           // 11
                                   2,  1,  2,  2,  1,           // 12
                                   2,  2,  1,  1,  3,           // 13
                                   2,  2,  1,  2,  2,           // 14
                                   2,  2,  2,  1,  3,           // 15
                                   2,  2,  2,  2,  3,           // 16
                                   3,  1,  1,  1,  3,           // 17
                                   3,  1,  1,  2,  3,           // 18
                                   3,  1,  2,  1,  3,           // 19
                                   3,  1,  2,  2,  1,           // 20
                                   3,  2,  1,  1,  3,           // 21
                                   3,  2,  1,  2,  2,           // 22
                                   3,  2,  2,  1,  3,           // 23
                                   3,  2,  2,  2,  3)           // 24

    xy -= 1                                                     // shift values to start at 0

    val fn = Array ("Age", "Spectacle", "Astigmatic", "Tear")   // feature names
    val cn = Array ("Hard", "Soft", "Neither")                  // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val nb0 = NaiveBayes0 (xy, fn, 3, cn, null, 0)              // create the classifier
    val nb  = NaiveBayes  (xy, fn, 3, cn, null, 0)              // create the classifier
    nb0.train ()                                                // train the classifier
    nb.train ()                                                 // train the classifier

    // test all rows ------------------------------------------------------------
    for (i <- xy.range1) {
        val z   = xy(i).slice (0, 4)                            // x-values
        val y   = xy(i, 4)                                      // y-value
        val yp0 = nb0.classify (z)                              // y predicted
        val yp  = nb.classify (z)                               // y predicted
        println (s"Use nb0: yp = classify ($z) = $yp0,\t y = $y,\t ${cn(y)}")
        println (s"Use nb : yp = classify ($z) = $yp,\t y = $y,\t ${cn(y)}")
    } // for

} // NaiveBayesTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest5` object is used to test the 'NaiveBayes' class.
 *  > runMain scalation.analytics.classifier.NaiveBayesTest5
 */
object NaiveBayesTest5 extends App
{
    val fname = BASE_DIR + "breast-cancer.arff"
    var data  = Relation (fname, -1, null)
    val xy    = data.toMatriI2 (null)
    val fn    = data.colName.toArray.slice (0, xy.dim2 - 1)
    val cn    = Array ("0", "1")                               // class names
    val nb0   = NaiveBayes0 (xy, fn, 2, cn, null, 0)           // create the classifier
    val nb    = NaiveBayes  (xy, fn, 2, cn, null, 0)           // create the classifier

    println ("nb0 cv accu = " + nb0.crossValidateRand ())
    println ("nb  cv accu = " + nb.crossValidateRand ())

    nb0.featureSelection ()
    nb.featureSelection ()

    println ("After feature selection")
    println ("nb0 cv accu = " + nb0.crossValidateRand ())
    println ("nb  cv accu = " + nb.crossValidateRand ())

} // NaiveBayesTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest6` object is used to test the 'NaiveBayes' class.
 *  > runMain scalation.analytics.classifier.NaiveBayesTest6
 */
object NaiveBayesTest6 extends App
{
    // training-set -----------------------------------------------------------
    // x0: 
    // x1: 
    // y:  Classification (No/0, Yes/1)
    // features:                  x0 x1  y
    val xy = new MatrixI ((9, 3), 0, 0, 0,
                                  0, 1, 0,
                                  0, 2, 1,
                                  1, 0, 0,
                                  1, 1, 0,
                                  1, 2, 1,
                                  2, 0, 1,
                                  2, 1, 1,
                                  2, 2, 1)
    val x = xy.sliceCol (0, 2)

    val fn = Array ("Fast", "Strong")                     // feature names
    val cn = Array ("No", "Yes")                          // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")
    println ("x = " + x)
    println ("---------------------------------------------------------------")

    val nb  = NaiveBayes  (xy, fn, 2, cn, null)           // create the classifier
    nb.train ()                                           // train the classifier

    // test samples -----------------------------------------------------------
    for (i <- x.range1) {
        println ("Use nb to classify (" + x(i) + ") = " + nb.classify (x(i)))
    } // for

    println ("nb  cv accu = " + nb.crossValidateRand ())  // cross validate the classifier

} // NaiveBayesTest6 object

