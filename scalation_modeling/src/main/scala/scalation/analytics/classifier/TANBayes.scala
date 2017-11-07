
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng, Zhe Jin
 *  @version 1.4
 *  @date    Mon Jul 27 01:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.collection.mutable.{Set => SET, Map}

import scalation.columnar_db.Relation
import scalation.graph_db.{MGraph, MinSpanningTree, Pair}
import scalation.linalgebra.{MatriI, MatrixI, VectorD, VectoI, VectorI}
import scalation.linalgebra.gen.{HMatrix2, HMatrix3, HMatrix4, HMatrix5}

import BayesClassifier.me_default

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes0` class implements an Integer-Based Tree Augmented Naive Bayes
 *  Classifier,  which is a commonly used such classifier for discrete input data.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier supports limited dependency between features/variables.
 *
 *  This classifier uses the standard cross-validation technique.
 *  -----------------------------------------------------------------------------
 *
 *  @param x      the integer-valued data vectors stored as rows of a matrix
 *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn     the names for all features/variables
 *  @param k      the number of classes
 *  @param cn     the names for all classes
 *  @param vc     the value count (number of distinct values) for each feature
 *  @param me     use m-estimates (me == 0 => regular MLE estimates)
 */
class TANBayes0 (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                me: Double = me_default, private var vc: VectoI = null)
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG  = false                            // debug flag

    protected var parent = new VectorI (n)                // vector holding the parent for each feature/variable
    protected val vcp    = new VectorI (n)                // value count for the parent

    protected val f_CXP  = new HMatrix4 [Int] (k, n)      // conditional frequency counts for variable/feature j: xj
    protected val p_X_CP = new HMatrix4 [Double] (k, n)   // conditional probabilities for variable/feature j: xj

    if (vc == null) {
        shiftToZero; vc = vc_fromData                     // set value counts from data
    } // if

    protected val vca = vc.toArray

    f_CXZ = new HMatrix5 [Int] (k, n, n, vca, vca)      // local joint frequency (using partial dataset, i.e. when using cross validation) of C, X, and Z, where X, Z are features/columns
    f_CX  = new HMatrix3 [Int] (k, n, vca)              // local joint frequency of C and X
    f_X   = new HMatrix2 [Int] (n, vca)                 // local frequency of X

    if (DEBUG) {
        println ("value count vc      = " + vc)
        println ("value count vcp     = " + vcp)
        println ("parent features par = " + parent)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int) = train (testStart until testEnd)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itest  indices of the instances considered as testing data
     */
    override def train (itest: IndexedSeq [Int])
    {
        val idx = if (additive) 0 until m diff itest
                  else          itest
        computeParent (idx)                             // frequency computations are also done here
        computeVcp ()
        f_CXP.alloc (vc, vcp)
        p_X_CP.alloc (vc, vcp)
        copyFreqCXP ()
        train2 ()
        if (smooth) smoothParam (itest.size)
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     */
    private def train2 ()
    {
        p_C = f_C.toDouble / md                               // prior probability for class yi
        for (i <- 0 until k; j <- 0 until n if fset(j)) {                // for each class yi & feature xj
            val me_vc = me / vc(j).toDouble
            for (xj <- 0 until vc(j); xp <- 0 until vcp(j)) {
                val d = if (parent(j) > -1) f_CX(i, parent(j), xp)
                        else                f_C(i)
                // for each value for feature j: xj, par(j): xp
                p_X_CP(i, j, xj, xp) = (f_CXP(i, j, xj, xp) + me_vc) / (d + me)
            } // for
        } // for

        if (DEBUG) {
            println ("p_C = " + p_C)                 // P(C = i)
            println ("p_X_CP = " + p_X_CP)           // P(X_j = x | C = i)
        } // if
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature x_i is only a possible candidate for parent of feature x_j if
     *  i < j
     *  @param idx   indicies of either training or testing region
     */
    def computeParent (idx: IndexedSeq [Int])
    {
        val cmiMx = calcCMI (idx, vca)
        for (j1 <- 0 until n if fset(j1); j2 <- 0 until j1 if fset(j2)) cmiMx(j1, j2) = cmiMx(j2, j1)

        val ch       = Array.fill [SET[Int]] (n)(SET())
        val elabel   = Map [Pair, Double] ()

        for (i <- 0 until n if fset(i); j <- i + 1 until n if fset(j)) { ch(i) += j; elabel += new Pair(i, j) -> cmiMx(i, j) }

        parent = VectorI (maxSpanningTree (ch, elabel).makeITree ())
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone/copy the values from global freq variables into local ones.
     *  Only the joint frequencies of Class, X-feature, and its Parent needs
     *  to be copied for parameter learning purposes.
     */
    private def copyFreqCXP ()
    {
        for (i <- 0 until k; j <- x.range2 if fset(j); xj <- 0 until vc(j); xp <- 0 until vcp(j)) {
            f_CXP(i, j, xj, xp) = if (parent(j) > -1) f_CXZ(i, j, parent(j), xj, xp)
                                  else                f_CX(i, j, xj)
        } // for
    } // copyFreqCXP

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i  the index for current data row
     */
    protected override def updateFreq (i: Int)
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) += 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) {
            f_X(j, x(i, j)) += 1
            f_CX (yi, j, x(i, j)) += 1
            for (j2 <- j+1 until n if fset(j2)) {
                f_CXZ (yi, j, j2, x(i, j), x(i, j2)) += 1
                f_CXZ (yi, j2, j, x(i, j2), x(i, j)) += 1
            } // for
        } // for
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create MaxSpanningTree from conditional mutual information
     */
    def maxSpanningTree (ch: Array[SET[Int]], elabel: Map[(Int, Int), Double]): MinSpanningTree =
    {
        val g = new MGraph (ch, Array.ofDim(n), elabel)
        new MinSpanningTree (g, false, false)     // param 2 = false means max spanning tree
    } // maxSpanningTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent vector.
     */
    def computeVcp ()
    {
        vcp.set (1)                              // set default value count to 1
        for (j <- 0 until n if (fset(j) && parent(j) > -1)) vcp(j) = vc(parent(j))
    } // computeVcp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform smoothing operations on the learned parameters by using Dirichlet priors
     *  to compute the posterior probabilities of the parameters given the training dataset.
     *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.178.8884&rep=rep1&type=pdf
     *  @param testSize  size of the test size
     */
    private def smoothParam (testSize: Int = 0)
    {
        for (i <- 0 until k) {
            p_C(i) *= m / (m + N0)
            p_C(i) += N0 * k / (m + N0)
            for (j <- 0 until n if fset(j)) {
                val pj = parent(j)
                for (xj <- 0 until vc(j); xp <- 0 until vcp(j)) {

                    val f_px = if (pj > -1) f_CX(i, pj, xp) else f_C(i)

                    //NOTE: two alternative priors, may work better for some datasets
                    //val theta0 = f_CXP(i, j, xj, xp) / (md - testSize)
//                    val theta0 = f_CX(i, j, xj) / (md - testSize)
                    val theta0 = f_X(j, xj) / (md - testSize)

                    p_X_CP(i, j, xj, xp) *= (f_px / (f_px + N0))
                    p_X_CP(i, j, xj, xp) += (N0 / (f_px + N0) * theta0)
                } // for
            } // for
        } // for
    } // smoothParam

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z        the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val prob = new VectorD (p_C)
        for (i <- 0 until k; j <- 0 until n if fset(j)) {
                prob(i) *=  (if (parent(j) > -1) p_X_CP(i, j, z(j), z(parent(j)))    // P(X_j = z_j | C = i), parent
                             else                p_X_CP(i, j, z(j), 0))              // P(X_j = z_j | C = i), no parent (other than the class)
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()             // class with the highest relative posterior probability
        (best, cn(best), prob(best))          // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables.
     */
    def reset ()
    {
        f_C.set (0)
        f_CX.set (0)
        f_X.set (0)
        f_CXZ.set (0)
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent.
     */
    override def getParent: VectorI = parent

} // TANBayes0 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes0` object is the companion object for the `TANBayes0` class.
 */
object TANBayes0
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TANBayes0` object, passing 'x' and 'y' together in one matrix.
     *
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               me: Double = me_default, vc: VectoI = null, smooth: Boolean = true) =
    {
        new TANBayes0 (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn,
                      me, vc)
    } // apply

} // TANBayes0 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The same classifier but uses an optimized cross-validation technique.
 *  -----------------------------------------------------------------------------
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param me  use m-estimates (me == 0 => regular MLE estimates)
 *  @param vc  the value count (number of distinct values) for each feature
 */
class TANBayes (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                me: Double = me_default, private var vc: VectoI = null)
        extends TANBayes0 (x, y, fn, k, cn, me, vc)
{
    private val DEBUG  = false                                     // debug flag

    if (vc == null) vc = new VectorI (vca.length, vca)

    private val g_f_CXZ = new HMatrix5 [Int] (k, n, n, vca, vca)   // global joint frequency (using entire dataset) of C, X, and Z, where X, Z are features/columns
    private val g_f_CX  = new HMatrix3 [Int] (k, n, vca)           // global joint frequency of C and X
    private val g_f_C   = new VectorI (k)                          // global frequency of C
    private val g_f_X   = new HMatrix2[Int] (n, vca)               // global frequency of X

    additive = false

    if (DEBUG) {
        println ("value count vc      = " + vc)
        println ("value count vcp     = " + vcp)
        println ("parent features par = " + parent)
    } // if

    frequenciesAll ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute frequency counts using the entire data matrix
     */
    def frequenciesAll ()
    {
        for (i <- 0 until m) {
            val yi = y(i)
            g_f_C(yi) += 1
            for (j <- 0 until n if fset(j)) {
                g_f_X(j, x(i, j)) += 1
                g_f_CX(yi, j, x(i, j)) += 1
                for (j2 <- j+1 until n if fset(j2)) g_f_CXZ(yi, j, j2, x(i, j), x(i, j2)) += 1
            } // for
        } // for

        for (c <- 0 until k; j <- 0 until n if fset(j); j2 <- j+1 until n if fset(j2); xj <- 0 until vc(j); xj2 <- 0 until vc(j2)) {
            g_f_CXZ(c, j2, j, xj2, xj) = g_f_CXZ(c, j, j2, xj, xj2)
        } // for
    } // frequenciesAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decrement frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i  the index for current data row
     */
    protected override def updateFreq (i: Int)
    {
        val yi   = y(i)                                       // get the class for ith row
        f_C(yi) -= 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) {
            f_X(j, x(i, j)) -= 1
            f_CX (yi, j, x(i, j)) -= 1
            for (j2 <- j+1 until n if fset(j2)) {
                f_CXZ (yi, j, j2, x(i, j), x(i, j2)) -= 1
                f_CXZ (yi, j2, j, x(i, j2), x(i, j)) -= 1
            } // for
        } // for
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables from the global frequencies.
     */
    override def reset ()
    {
        for (i <- 0 until k) {
            f_C(i) = g_f_C(i)
            for (j <- x.range2 if fset(j); xj <- 0 until vc(j)) {
                if (i == 0) f_X(j, xj) = g_f_X(j, xj)
                f_CX(i, j, xj) = g_f_CX(i, j, xj)
                for (j2 <- j+1 until n if fset(j2); xj2 <- 0 until vc(j2)) {
                    f_CXZ(i, j, j2, xj, xj2) = g_f_CXZ(i, j, j2, xj, xj2)
                    f_CXZ(i, j2, j, xj2, xj) = f_CXZ(i, j, j2, xj, xj2)
                } // for
            } // for
        } // for
    } // reset

} // TANBayes class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes` object is the companion object for the `TANBayes` class.
 */
object TANBayes
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TANBayes` object, passing 'x' and 'y' together in one matrix.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param vc     the value count (number of distinct values) for each feature
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String],
               me: Double = me_default, vc: VectoI = null) =
    {
        new TANBayes (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn, me, vc)
    } // apply

} // TANBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest` object is used to test the `TANBayes0` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-AugNaiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.TANBayesTest
 */
object TANBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                x0 x1 x2
    val x = new MatrixI((10, 3), 1, 0, 1,               // data matrix
                                 1, 0, 1,
                                 1, 0, 1,
                                 0, 0, 1,
                                 0, 0, 0,
                                 0, 1, 0,
                                 0, 1, 0,
                                 0, 1, 1,
                                 1, 1, 0,
                                 1, 0, 0)

    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)      // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")          // feature/variable names
    val cn = Array ("No", "Yes")                        // class names

    println("xy = " + (x :^+ y))
    println("---------------------------------------------------------------")

    val tan0 = new TANBayes0 (x, y, fn, 2, cn)          // create the classifier
    val tan  = new TANBayes  (x, y, fn, 2, cn)           // create the classifier

    // train the classifier ---------------------------------------------------
    tan0.train ()
    tan.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1)                          // existing data vector to classify
    val z2 = VectorI (1, 1, 1)                          // new data vector to classify
    println ("Use tan0 to classify (" + z1 + ") = " + tan0.classify (z1))
    println ("Use tan  to classify (" + z1 + ") = " + tan.classify (z1))
    println ("Use tan0 to classify (" + z2 + ") = " + tan0.classify (z2))
    println ("Use tan  to classify (" + z2 + ") = " + tan.classify (z2))

    println ("tan0 cv accu = " + tan0.crossValidateRand())  // cross validate the classifier
    println ("tan  cv accu = " + tan.crossValidateRand())   // cross validate the classifier

} // TANBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest2` object is used to test the `TANBayes0` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.classifier.TANBayesTest2
 */
object TANBayesTest2 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                 x0 x1  y
    val xy = new MatrixI((10, 3), 1, 1, 1,
                                  1, 1, 1,
                                  1, 0, 1,
                                  1, 0, 1,
                                  1, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  0, 0, 0,
                                  0, 0, 0)

    val fn = Array ("Fast", "Strong")                   // feature names
    val cn = Array ("No", "Yes")                        // class names

    println("xy = " + xy)
    println("---------------------------------------------------------------")

    val tan0 = TANBayes0 (xy, fn, 2, cn, 1, null)       // create the classifier
    val tan  = TANBayes  (xy, fn, 2, cn, 1, null)        // create the classifier

    // train the classifier ---------------------------------------------------
    tan0.train ()
    tan.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0) // new data vector to classify
    println ("Use tan0 to classify (" + z + ") = " + tan0.classify (z))
    println ("Use tan  to classify (" + z + ") = " + tan.classify (z))

    println ("tan0 cv accu = " + tan0.crossValidateRand())    // cross validate the classifier
    println ("tan  cv accu = " + tan.crossValidateRand())     // cross validate the classifier

} // TANBayesTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest3` object is used to test the `TANBayes0` class.
 *  > run-main scalation.analytics.classifier.TANBayesTest3
 */
object TANBayesTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.slice(0, xy.dim2 - 1).toArray
    val cn = Array ("p", "e")                                   // class names
    val k  = 2

    println("---------------------------------------------------------------")
    val tan0 = TANBayes0 (xy, fn, k, cn)                        // create the classifier
    val tan  = TANBayes  (xy, fn, k, cn)                         // create the classifier
    println("tan0 cv accu = " + tan0.crossValidateRand())       // cross validate the classifier
    println("tan  cv accu = " + tan.crossValidateRand())        // cross validate the classifier

    tan0.featureSelection ()
    tan.featureSelection ()

    println ("After feature selection")
    println("tan0 cv accu = " + tan0.crossValidateRand())       // cross validate the classifier
    println("tan  cv accu = " + tan.crossValidateRand())        // cross validate the classifier

} // TANBayesTest3 object
