
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng, Zhe Jin
 *  @version 1.6
 *  @date    Mon Jul 27 01:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.collection.mutable.{Set => SET, Map}

import scalation.columnar_db.Relation
import scalation.graph_db.{MGraph, MinSpanningTree, Pair}
import scalation.linalgebra.{MatriI, MatrixI, VectorD, VectoI, VectorI}
import scalation.linalgebra.gen.{HMatrix2, HMatrix3, HMatrix4, HMatrix5}
import scalation.util.banner

import BayesClassifier.me_default
import ClassifierInt.pullResponse

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes0` object is the companion object for the `TANBayes0` class.
 */
object TANBayes0
{
    private val N0 = 3.0                      // parameter needed for smoothing

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TANBayes0` object, passing 'x' and 'y' together in one matrix.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param cn     the class names
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param vc     the value count (number of distinct values) for each feature
     */
    def apply (xy: MatriI, fn: Strings, k: Int, cn: Strings,
               me: Double = me_default, vc: Array [Int] = null) =
    {
        val (x, y) = pullResponse (xy)
        new TANBayes0 (x, y, fn, k, cn, me, vc)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform smoothing operations on the learned parameters by using Dirichlet priors
     *  to compute the posterior probabilities of the parameters given the training dataset.
     *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.178.8884&rep=rep1&type=pdf
     *  @see www.cs.technion.ac.il/~dang/journal_papers/friedman1997Bayesian.pdf
     *  @param k          the number of class values/labels for y
     *  @param n          the total number of features/x-variables
     *  @param fset       the selected features
     *  @param parent     the parent for each feature
     *  @param vc         the value count
     *  @param vcp        the value count for parent
     *  @param nu_y       the frequqncy of class y
     *  @param nu_X       the frequency of each feature X = [x_0, ... x_n-1]
     *  @param nu_Xy      the joint frequency of X and y
     *  @param trainSize  the size of the training dataset
     *  @param p_XyP      the conditional probability of X given y and P(arent) - to be updated
     */
    def smoothP (k: Int, n: Int, fset: Array [Boolean], parent: VectorI,
                        vc: Array [Int], vcp: Array [Int], trainSize: Double,
                        nu_y: VectorI, nu_X: HMatrix2 [Int], nu_Xy: HMatrix3 [Int],
                        p_XyP: HMatrix4 [Double])
    {
        for (i <- 0 until k; j <- 0 until n if fset(j)) {
            val pj = parent(j)
            for (xj <- 0 until vc(j); xp <- 0 until vcp(j)) {
                val nu_px  = if (pj > -1) nu_Xy(i, pj, xp) else nu_y(i)
                val theta0 = nu_X(j, xj) / trainSize

                p_XyP(i, j, xj, xp) *= nu_px / (nu_px + N0)
                p_XyP(i, j, xj, xp) += N0 / (nu_px + N0) * theta0
            } // for
        } // for
    } // smoothP

} // TANBayes0 object

import TANBayes0.smoothP

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes0` class implements an Integer-Based Tree Augmented Naive Bayes
 *  Classifier,  which is a commonly used such classifier for discrete input data.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier supports limited dependency between features/variables.
 *-----------------------------------------------------------------------------
 *  This classifier uses the standard cross-validation technique.
 *-----------------------------------------------------------------------------
 *  @param x    the integer-valued data vectors stored as rows of a matrix
 *  @param y    the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn_  the names for all features/variables
 *  @param k    the number of classes
 *  @param cn_  the names for all classes
 *  @param me   use m-estimates (me == 0 => regular MLE estimates)
 *  @param vc   the value count (number of distinct values) for each feature
 */
class TANBayes0 (x: MatriI, y: VectoI, fn_ : Strings = null, k: Int = 2, cn_ : Strings = null,
                 me: Double = me_default, protected var vc: Array [Int] = null)
      extends BayesClassifier (x, y, fn_, k, cn_)
{
    private val DEBUG  = false                            // debug flag

    if (cn.length != k) flaw ("constructor", "# class names != # classes")

    protected var parent = new VectorI (n)                // vector holding the parent for each feature/variable
    protected val vcp    = Array.ofDim [Int] (n)          // value count for the parent

    protected val nu_XyP = new HMatrix4 [Int] (k, n)      // conditional frequency counts for variable/feature j: xj
    protected val p_XyP  = new HMatrix4 [Double] (k, n)   // conditional probabilities for variable/feature j: xj

    if (vc == null) {
        shiftToZero; vc = vc_fromData                     // set value counts from data
    } // if

    nu_X   = new HMatrix2 [Int] (n, vc)                   // local frequency of X = [x_0, ... x_n-1]
    nu_Xy  = new HMatrix3 [Int] (k, n, vc)                // local joint frequency of X and y
    nu_XyZ = new HMatrix5 [Int] (k, n, n, vc, vc)         // local joint frequency (using partial dataset, i.e. when using cross validation)
                                                          // of X, y and Z where X, Z are features/columns
    if (DEBUG) {
        println ("value count vc      = " + vc.deep)
        println ("value count vcp     = " + vcp.deep)
        println ("parents of features = " + parent)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param itest  indices of the instances considered as testing data
     */
    def train (itest: Ints): TANBayes0 =
    {
        val idx = if (additive) 0 until m diff itest else itest
        computeParent (idx)                             // frequency computations are also done here
        computeVcp ()
        nu_XyP.alloc (vc, vcp)
        p_XyP.alloc (vc, vcp)
        copyFreqXyP ()
        train2 ()
        if (smooth) smoothP (k, n, fset, parent, vc, vcp, md - itest.size, nu_y, nu_X, nu_Xy, p_XyP)
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     */
    private def train2 ()
    {
        p_y = nu_y.toDouble / md                                     // probability for class yi
        for (i <- 0 until k; j <- 0 until n if fset(j)) {            // for each class yi & feature xj
            val me_vc = me / vc(j).toDouble
            for (xj <- 0 until vc(j); xp <- 0 until vcp(j)) {
                val d = if (parent(j) > -1) nu_Xy(i, parent(j), xp)
                        else                nu_y(i)
                // for each value for feature j: xj, par(j): xp
                p_XyP(i, j, xj, xp) = (nu_XyP(i, j, xj, xp) + me_vc) / (d + me)
            } // for
        } // for

        if (DEBUG) {
            println ("p_y   = " + p_y)                 // P(y = c)
            println ("p_XyP = " + p_XyP)               // P(X_j = x | y = c)
        } // if
    } // train2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature x_i is only a possible candidate for parent of feature x_j if
     *  i < j
     *  @param idx   indicies of either training or testing region
     */
    def computeParent (idx: Ints)
    {
        val cmiMx = calcCMI (idx, vc)
        for (j1 <- 0 until n if fset(j1); j2 <- 0 until j1 if fset(j2)) cmiMx(j1, j2) = cmiMx(j2, j1)

        val ch     = Array.fill (n)(SET [Int] ())
        val elabel = Map [Pair, Double] ()

        for (i <- 0 until n if fset(i); j <- i + 1 until n if fset(j)) { ch(i) += j; elabel += new Pair(i, j) -> cmiMx(i, j) }

        parent = VectorI (maxSpanningTree (ch, elabel).makeITree ())
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent.
     */
    override def getParent: VectoI = parent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone/copy the values from global freq variables into local ones.
     *  Only the joint frequencies of Class, X-feature, and its Parent needs
     *  to be copied for parameter learning purposes.
     */
    private def copyFreqXyP ()
    {
        for (i <- 0 until k; j <- x.range2 if fset(j); xj <- 0 until vc(j); xp <- 0 until vcp(j)) {
            nu_XyP(i, j, xj, xp) = if (parent(j) > -1) nu_XyZ(i, j, parent(j), xj, xp)
                                   else                nu_Xy(i, j, xj)
        } // for
    } // copyFreqXyP

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Increment frequency counters used in CMI calculations based on the 'i'th
     *  row of the data matrix.
     *  @param i  the index for current data row
     */
    protected def updateFreq (i: Int)
    {
        val yi    = y(i)                                       // get the class for ith row
        nu_y(yi) += 1                                          // increment frequency for class yi
        for (j <- x.range2 if fset(j)) {                       // for each feature/variable xj
            nu_X(j, x(i, j))      += 1                         // increment frequency for xj 
            nu_Xy(yi, j, x(i, j)) += 1                         // increment frequency for xj, yi
            for (j2 <- j+1 until n if fset(j2)) {              // for each feature/variable xj2
                nu_XyZ(yi, j, j2, x(i, j), x(i, j2)) += 1      // increment frequency for xj, yi, xj2
                nu_XyZ(yi, j2, j, x(i, j2), x(i, j)) += 1      // increment frequency for xj2, yi, xj
            } // for
        } // for
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create MaxSpanningTree from conditional mutual information.
     *  @param ch      the adjacency set
     *  @param elabel  the edge labels/weights
     */
    def maxSpanningTree (ch: Array [SET [Int]], elabel: Map [Pair, Double]): MinSpanningTree =
    {
        val g = new MGraph (ch, Array.ofDim (n), elabel)
        new MinSpanningTree (g, false, false)         // param 2 = false means max spanning tree
    } // maxSpanningTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent vector.
     *  Let 1 be the default value count when there is no parent.
     */
    def computeVcp ()
    {
        for (j <- 0 until n) {
            vcp(j) = if (fset(j) && parent(j) > -1) vc(parent(j)) else 1
        } // for
    } // computeVcp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val prob = new VectorD (p_y)
        for (i <- 0 until k; j <- 0 until n if fset(j)) {
            prob(i) *= (if (parent(j) > -1) p_XyP(i, j, z(j), z(parent(j)))    // P(X_j = z_j | X_p = z_p, y = c), x-parent
                        else                p_XyP(i, j, z(j), 0))              // P(X_j = z_j | x_p = z_p, y = c), no x-parent
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()                // class with the highest relative posterior probability
        (best, cn(best), prob(best))             // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables.
     */
    def reset ()
    {
        nu_y.set (0)
        nu_X.set (0)
        nu_Xy.set (0)
        nu_XyZ.set (0)
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the conditional probability tables by iterating over the features/variables.
     */
    def printConditionalProb ()
    {
        for (j <- 0 until p_XyP.dim2) {
            println (s"ConditionalProb for x$j = ${p_XyP(j)}")
        } // for
    } // printConditionalProb

} // TANBayes0 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The same classifier but uses an optimized cross-validation technique.
 *  -----------------------------------------------------------------------------
 *  @param x    the integer-valued data vectors stored as rows of a matrix
 *  @param y    the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn_  the names for all features/variables
 *  @param k    the number of classes
 *  @param cn_  the names for all classes
 *  @param me   use m-estimates (me == 0 => regular MLE estimates)
 *  @param vc_  the value count (number of distinct values) for each feature
 */
class TANBayes (x: MatriI, y: VectoI, fn_ : Strings = null, k: Int = 2, cn_ : Strings = null,
                me: Double = me_default, vc_ : Array [Int] = null)
        extends TANBayes0 (x, y, fn_, k, cn_, me, vc_)
{
    private val DEBUG    = false                                   // debug flag
    private val g_nu_y   = new VectorI (k)                         // global frequency of y
    private val g_nu_X   = new HMatrix2 [Int] (n, vc)              // global frequency of X = [x_0, ... x_n-1]
    private val g_nu_Xy  = new HMatrix3 [Int] (k, n, vc)           // global joint frequency of X and y
    private val g_nu_XyZ = new HMatrix5 [Int] (k, n, n, vc, vc)    // global joint frequency (using entire dataset) of
                                                                   // X, y and Z where X, Z are features/columns
    additive = false

    if (DEBUG) {
        println ("value count vc      = " + vc.deep)
        println ("value count vcp     = " + vcp.deep)
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
            g_nu_y(yi) += 1
            for (j <- 0 until n if fset(j)) {
                g_nu_X(j, x(i, j)) += 1
                g_nu_Xy(yi, j, x(i, j)) += 1
                for (j2 <- j+1 until n if fset(j2)) g_nu_XyZ(yi, j, j2, x(i, j), x(i, j2)) += 1
            } // for
        } // for

        for (c <- 0 until k; j <- 0 until n if fset(j); j2 <- j+1 until n if fset(j2); xj <- 0 until vc(j); xj2 <- 0 until vc(j2)) {
            g_nu_XyZ(c, j2, j, xj2, xj) = g_nu_XyZ(c, j, j2, xj, xj2)
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
        nu_y(yi) -= 1                                          // decrement frequency for class yi
        for (j <- x.range2 if fset(j)) {
            nu_X(j, x(i, j))      -= 1
            nu_Xy(yi, j, x(i, j)) -= 1
            for (j2 <- j+1 until n if fset(j2)) {
                nu_XyZ(yi, j, j2, x(i, j), x(i, j2)) -= 1
                nu_XyZ(yi, j2, j, x(i, j2), x(i, j)) -= 1
            } // for
        } // for
    } // updateFreq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables from the global frequencies.
     */
    override def reset ()
    {
        for (i <- 0 until k) {
            nu_y(i) = g_nu_y(i)
            for (j <- x.range2 if fset(j); xj <- 0 until vc(j)) {
                if (i == 0) nu_X(j, xj) = g_nu_X(j, xj)
                nu_Xy(i, j, xj) = g_nu_Xy(i, j, xj)
                for (j2 <- j+1 until n if fset(j2); xj2 <- 0 until vc(j2)) {
                    nu_XyZ(i, j, j2, xj, xj2) = g_nu_XyZ(i, j, j2, xj, xj2)
                    nu_XyZ(i, j2, j, xj2, xj) = nu_XyZ(i, j, j2, xj, xj2)
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
    def apply (xy: MatriI, fn: Strings, k: Int, cn: Strings,
               me: Double = me_default, vc: Array [Int] = null) =
    {
        val (x, y) = pullResponse (xy)
        new TANBayes (x, y, fn, k, cn, me, vc)
    } // apply

} // TANBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest` object is used to test the 'TANBayes' class.
 *  > runMain scalation.analytics.classifier.TANBayesTest
 */
object TANBayesTest extends App
{
    import ExampleTennis._

    banner ("Tennis Example")
    println ("xy = " + xy)
    println ("-" * 60)

    val tb0 = TANBayes0 (xy, fn, k, cn)                                 // create a classifier tb0
    val tb  = TANBayes  (xy, fn, k, cn)                                 // create a classifier tb
    tb0.train ()                                                        // train the classifier tb0
    tb.train ()                                                         // train the classifier tb
    tb.printClassProb ()                                                // print class probabilities
    tb.printConditionalProb ()                                          // print conditional probabilities

    val z = VectorI (2, 2, 1, 1)                                        // new data vector to classify
    banner (s"Classify $z")
    println (s"Use tb0 to classify (z) = ${tb0.classify (z)}")
    println (s"Use tb  to classify (z) = ${tb.classify (z)}")

    banner ("All Test")
    val x  = xy.sliceCol (0, xy.dim2 - 1)                               // data matrix
    val y  = xy.col (xy.dim2 - 1)                                       // response/class label vector
    val yp = new VectorI (xy.dim1)                                      // predicted class label vector
    for (i <- x.range1) {
        yp(i) = tb.classify (x(i))._1
        println (s"Use tb  to classify (${x(i)}) = ${yp(i)}")
    } // for
    val cm  = new ConfusionMat (y, yp, k)                               // confusion matrix
    println ("Confusion Matrix = " + cm.confusion)
    println ("accuracy         = " + cm.accuracy)
    println ("prec-recall      = " + cm.prec_recl)

    banner ("Cross-validation")
    println ("tb0 cv accu = " + tb0.crossValidateRand (10, true))
    println ("tb  cv accu = " + tb.crossValidateRand (10, true))

} // TANBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest2` object is used to test the `TANBayes0` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-AugNaiveBayes-example.pdf
 *  > runMain scalation.analytics.classifier.TANBayesTest2
 */
object TANBayesTest2 extends App
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

    val yp = tan.classify (x)
    println (tan.fitMap (y, yp))

} // TANBayesTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest3` object is used to test the `TANBayes0` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > runMain scalation.analytics.classifier.TANBayesTest3
 */
object TANBayesTest3 extends App
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

} // TANBayesTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest4` object is used to test the `TANBayes0` class.
 *  > runMain scalation.analytics.classifier.TANBayesTest4
 */
object TANBayesTest4 extends App
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

} // TANBayesTest4 object

