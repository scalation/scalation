
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 * @version 1.2
 * @date    Mon Jul 27 01:27:00 EDT 2015
 * @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier.par

import java.util.concurrent.ForkJoinPool

import scala.collection.mutable.{Map, Set => SET}
import scala.collection.parallel.ForkJoinTaskSupport
import scala.math.min

import scalation.graphalytics.Pair
import scalation.graphalytics.mutable.{MGraph, MinSpanningTree}
import scalation.linalgebra._
import scalation.linalgebra.gen.{HMatrix3, HMatrix4, HMatrix5}
import scalation.relalgebra.Relation

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes` class implements an Integer-Based Tree Augmented Naive Bayes
 *  Classifier,  which is a commonly used such classifier for discrete input data.
 *  The classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional probabilities.  The
 *  classifier supports limited dependency between features/variables.
 *-----------------------------------------------------------------------------
 *  @param x      the integer-valued data vectors stored as rows of a matrix
 *  @param y      the class vector, where y(l) = class for row l of the matrix, x(l)
 *  @param fn     the names for all features/variables
 *  @param k      the number of classes
 *  @param cn     the names for all classes
 *  @param vc     the value count (number of distinct values) for each feature
 *  @param me     use m-estimates (me == 0 => regular MLE estimates)
 *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
 */
class TANBayes (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                thres: Double = 0.3, me: Int = 3, private var vc: VectoI = null)
      extends BayesClassifier (x, y, fn, k, cn)
{
    private val DEBUG  = false                               // debug flag
    private val cor    = calcCorrelation                     // feature correlation matrix
    private var parent = new VectorI (n)                     // vector holding the parent for each feature/variable
    private val vcp    = new VectorI (n)                     // value count for the parent

    private val popC  = new VectorI (k)                      // frequency counts for classes 0, ..., k-1
    private val probC = new VectorD (k)                      // probabilities for classes 0, ..., k-1
    private val popX  = new HMatrix4 [Int] (k, n)            // conditional frequency counts for variable/feature j: xj
    private val probX = new HMatrix4 [Double] (k, n)         // conditional probabilities for variable/feature j: xj

    if (vc == null) vc = vc_fromData                         // set to default for binary data (2)
    println (vc)


    if (DEBUG) {
        println ("value count vc      = " + vc)
        println ("value count vcp     = " + vcp)
        println ("correlation matrix  = " + cor)
        println ("parent features par = " + parent)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the model with feature order and selection.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def buildModel (testStart: Int = 0, testEnd: Int = 0): (Array [Boolean], DAG) =
    {
        computeParent ()
        computeVcp ()
        popX.alloc (vc, vcp)
        probX.alloc (vc, vcp)
        val pp: Traversable[Array[Int]] = for (p <- parent) yield Array (p)
        (Array.fill (n)(true), new DAG (pp.toArray))
    } // buildModel

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the parent of each feature based on the correlation matrix.
     *  Feature x_i is only a possible candidate for parent of feature x_j if i < j.
     */
    def computeParent ()
    {
        val countC   = new VectorD (k)                                           // count the number of classes in each class
        var countXYC = new HMatrix5 [Double] (n, n, k, vc.toArray, vc.toArray)   // countXYC count the number where X=x,Y=y,C=c
        var countXC  = new HMatrix3 [Double] (k, n, vc.toArray)                  // countXC count the number where X=x,C=c

        val ch = Array.ofDim [SET[Int]] (n)
        val elabel = Map [Pair, Double] ()
//      parent(0) = -1                                                           // feature 0 does not have a parent

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compute frequency counts for each value in each variable.
         *  FIX - don't hardcode 8
         */
        def frequencies ()
        {
            val endworkers = 8
            val size      = (m / endworkers) + 1
            val countCw   = Array.ofDim [VectorD] (endworkers)
            val countXCw  = Array.ofDim [HMatrix3 [Double]](endworkers)
            val countXYCw = Array.ofDim [HMatrix5 [Double]](endworkers)
            println ("workers = " + endworkers)

            for (w <- 0 until endworkers) {
                countCw (w)   = new VectorD (k)
                countXCw (w)  = new HMatrix3 [Double] (k, n)
                countXYCw (w) = new HMatrix5 [Double] (k, n, n, vc.toArray, vc.toArray)
                countXCw (w).alloc (vc.toArray)
            } // for

            val temprange = (0 until endworkers).par
            temprange.tasksupport = new ForkJoinTaskSupport (new ForkJoinPool (endworkers))
            for (w <- temprange) {
                for (i <- w * (size) until min ((w + 1) * size, m)) {
                    countCw (w)(y (i)) += 1
                    for (j <- 0 until n) {
                        countXCw (w)(y (i), j, x (i, j)) += 1
                        for (t <- 1 until n) countXYCw (w)(y (i), j, t, x (i, j), x (i, t)) += 1
                    } // for
                } // for
            } // for
            for (w <- 0 until endworkers) {
                countC  += countCw (w)
                countXC += countXCw (w)
                countXYC = countXYCw(w)  + countXYCw (w)
            } // for
        } // frequencies

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compute frequency counts for each value in each variable
         */
        def probabilities: (VectorD, HMatrix3 [Double], HMatrix5 [Double]) =
        {
            for (i <- 0 until k; j <- 0 until n; t <- 0 until vc (j)) countXC (i, j, t) = (countXC (i, j, t) + (me.toDouble) / m) / (m + me)
            for (i <- 0 until n; j <- 0 until n; t <- 0 until k; p <- 0 until vc (i); q <- 0 until vc (j)) {
                countXYC (t, i, j, p, q) = (countXYC (t, i, j, p, q) + (me.toDouble / m)) / (m + me)
            } // for
            (countC / m, countXC, countXYC)
        } // frequencies

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Create MinSpanningTree from conditional mutual information
         */
        def minSpanningTree (ip: MatrixD): MinSpanningTree =
        {
            for (i <- 0 until n) ch (i) = SET ((i + 1 until n): _*)
            for (i <- 0 until n; j <- i + 1 until n) elabel += new Pair (i, j) -> ip (i, j)
            val g = new MGraph (ch, Array.ofDim (n), elabel)
            println ("elabel=   " + elabel)
            new MinSpanningTree (g, false, false)
        } // minSpanningTree

        frequencies ()
        val probs = probabilities
        val ipxyz = condMutualInformation (probs._1, probs._2, probs._3)
        parent = VectorI (minSpanningTree (ipxyz).makeITree ())
    } // computeParent

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value counts of each parent feature based on the parent vector.
     */
    def computeVcp ()
    {
        vcp.set (1)                                      // set default value count to 1
        for (j <- 0 until n if (parent (j) > -1)) vcp (j) = vc (parent (j))
    } // computeVcp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     * Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     * training data.
     * @param testStart  starting index of test region (inclusive) used in cross-validation
     * @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def frequencies (testStart: Int, testEnd: Int)
    {
        for (l <- 0 until m if l < testStart || l >= testEnd) {
            // l = lth row of data matrix x
            val i = y (l)                               // get the class
            popC (i) += 1                               // increment ith class
            for (j <- 0 until n) {
                if (parent (j) > -1) popX (i, j, x (l, j), x (l, parent (j))) += 1
                else popX (i, j, x (l, j), 0) += 1
            } // for
        } // for

        if (DEBUG) {
            println ("popC = " + popC)                  // #(C = i)
            println ("popX = " + popX)                  // #(X_j = x & C = i)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     * conditional probabilities for X_j.
     * @param testStart  starting index of test region (inclusive) used in cross-validation.
     * @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def train (testStart: Int = 0, testEnd: Int = 0)
    {
        frequencies (testStart, testEnd)                // compute frequencies skipping test region

        for (i <- 0 until k) {
            // for each class i
            val pci = popC (i).toDouble                // population of class i
            probC (i) = pci / md                       // probability of class i

            for (j <- 0 until n) {                     // for each feature j
                val me_vc = me / vc (j).toDouble
                for (xj <- 0 until vc (j); xp <- 0 until vcp (j)) {
                    // for each value for feature j: xj, par(j): xp
                    probX (i, j, xj, xp) = (popX (i, j, xj, xp) + me_vc) / (pci + me)
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC = " + probC)               // P(C = i)
            println ("probX = " + probX)               // P(X_j = x | C = i)
        } // if
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        val prob = new VectorD (k)
        for (i <- 0 until k) {
            prob (i) = probC (i)                       // P(C = i)
            for (j <- 0 until n) {
                prob (i) *= (if (parent (j) > -1) probX (i, j, z (j), z (parent (j)))
                // P(X_j = z_j | C = i), parent
                else probX (i, j, z (j), 0))           // P(X_j = z_j | C = i), no parent
            } // for
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()                      // class with the highest relative posterior probability
        (best, cn (best), prob(best))                  // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        popC.set (0)
        probC.set (0)
        popX.clear ()
        probX.clear ()
        popX.alloc (vc, vcp)
        probX.alloc (vc, vcp)
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parent.
     */
    def getParent: VectorI = parent

} // TANBayes class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayes` object is the companion object for the `TANBayes` class.
 */
object TANBayes
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'TANBayes object, passing 'x' and 'y' together in one table.
     * @param xy     the data vectors along with their classifications stored as rows of a matrix
     * @param fn     the names of the features
     * @param k      the number of classes
     * @param vc     the value count (number of distinct values) for each feature
     * @param me     use m-estimates (me == 0 => regular MLE estimates)
     * @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatriI, fn: Array[String], k: Int, cn: Array[String],
               thres: Double = 0.3, me: Int = 3, vc: VectoI = null) =
    {
        new TANBayes (xy (0 until xy.dim1, 0 until xy.dim2 - 1), xy.col (xy.dim2 - 1), fn, k, cn,
                      thres, me, vc)
    } // apply

} // TANBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest` object is used to test the `TANBayes` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-AugNaiveBayes-example.pdf
 *  > run-main scalation.analytics.classifier.par.TANBayesTest
 */
object TANBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                 x0 x1 x2
    val x = new MatrixI ((10, 3), 1, 0, 1,                   // data matrix
                                  1, 0, 1,
                                  1, 0, 1,
                                  0, 0, 1,
                                  0, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  1, 1, 0,
                                  1, 0, 0)

    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)            // classification vector: 0(No), 1(Yes))

    val fn = Array ("Color", "Type", "Origin")                // feature/variable names

    val cn = Array ("No", "Yes")                              // class names

    println ("xy = " + (x :^+ y))
    println ("---------------------------------------------------------------")

    val anb = new TANBayes (x, y, fn, 2, cn)                  // create the classifier

    // train the classifier ---------------------------------------------------
    anb.train ()
    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1)                                // existing data vector to classify
    val z2 = VectorI (1, 1, 1)                                // new data vector to classify
    println ("classify (" + z1 + ") = " + anb.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + anb.classify (z2) + "\n")
    anb.crossValidate ()                                       // cross validate the classifier

} // TANBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest2` object is used to test the `TANBayes` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.classifier.par.TANBayesTest2
 */
object TANBayesTest2 extends App
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

    val fn = Array ("Fast", "Strong")                         // feature names
    val cn = Array ("No", "Yes")                              // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val anb = TANBayes (xy, fn, 2, cn, 0.3, 3, null)          // create the classifier
    anb.computeParent ()

    // train the classifier ---------------------------------------------------
    anb.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                    // new data vector to classify
    println ("classify (" + z + ") = " + anb.classify (z) + "\n")
    anb.crossValidate ()                                      // cross validate the classifier

} // TANBayesTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TANBayesTest3` object is used to test the `TANBayes` class.
 *  > run-main scalation.analytics.classifier.par.TANBayesTest3
 */
object TANBayesTest3 extends App
{
    val filename = BASE_DIR + "breast-cancer.arff"
    var data = Relation (filename, -1, null)
    val xy = data.toMatriI2 (null)
    val fn = data.colName.toArray
    val cn = Array ("p", "e")                                 // class names
    val k  = 2

    println ("---------------------------------------------------------------")
    val anb = TANBayes (xy, fn, 2, cn,0.3 , 1, null)          // create the classifier
    anb.buildModel ()
    anb.train ()
    anb.crossValidate ()

} // TANBayesTest3 object

