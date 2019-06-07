
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.2
 *  @date    Mon Jul 27 01:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.abs

import scalation.linalgebra.{MatrixI, VectorD, VectorI}
import scalation.linalgebra.gen.HMatrix4

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AugNaiveBayes` class implements an Integer-Based Tree Augmented Naive Bayes
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
class AugNaiveBayes (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
                     private var vc: VectorI = null, me: Int = 3, thres: Double = 0.3)
        extends ClassifierInt (x, y, fn, k, cn)
{
    private val DEBUG = true                          // debug flag
    private val cor   = calcCorrelation               // feature correlation matrix
    private val par   = new VectorI (n)               // vector holding the parent for each feature/variable

    private val popC  = new VectorI (k)               // frequency counts for classes 0, ..., k-1
    private val probC = new VectorD (k)               // probabilities for classes 0, ..., k-1
    private val popX  = new HMatrix4 [Int] (k, n)     // conditional frequency counts for variable/feature j: xj
    private val probX = new HMatrix4 [Double] (k, n)  // conditional probabilities for variable/feature j: xj

    // init 'par': feature x_i is only a possible candidate for parent of feature x_j if i < j

    par(0) = -1                                       // feature 0 does not have a parent
    for (i <- 1 until n) {
        val correl = cor(i).map ((x: Double) => abs (x))
        par(i) = if (correl.max () > thres) correl.argmax () else -1
    } // for

    if (vc == null) vc = vc_default                   // set to default for binary data (2)
    popX.alloc (vc(), par)
    probX.alloc (vc(), par)

    if (DEBUG) {
        println ("value count vc      = " + vc)
        println ("correlation matrix  = " + cor)
        println ("parent features par = " + par)
    } // if

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     *  Only the test region from 'testStart' to 'testEnd' is skipped, the rest is
     *  training data.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def frequencies (testStart: Int, testEnd: Int)
    {
        for (l <- 0 until m if l < testStart || l >= testEnd) {    // l = lth row of data matrix x
            val i = y(l)                                           // get the class
            popC(i) += 1                                           // increment ith class
            for (j <- 0 until n) {
                if (par(j) > -1) popX(i, j, x(l, j), x(l, par(j))) += 1
                else             popX(i, j, x(l, j), 0) += 1
            } // for
        } // for

        if (DEBUG) {
            println ("popC = " + popC)                             // #(C = i)
            println ("popX = " + popX)                             // #(X_j = x & C = i)
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region. (exclusive) used in cross-validation.
     */
    def train (testStart: Int = 0, testEnd: Int = 0)
    {
        frequencies (testStart, testEnd)                           // compute frequencies skipping test region

        for (i <- 0 until k) {                                     // for each class i
            val pci = popC(i).toDouble                             // population of class i
            probC(i) = pci / md                                    // probability of class i

            for (j <- 0 until n) {                                 // for each feature j
                val me_vc = me / vc(j).toDouble
                for (xj <- 0 until vc(j)) {                        // for each value for feature j: xj
                    if (par(j) > -1) {
                        for (xp <- 0 until vc(par(j))) {           // for each value for par(j): xp
                            probX(i, j, xj, xp) = (popX(i, j, xj, xp) + me_vc) / (pci + me)
                        } //for
                    } else {
                        probX(i, j, xj, 0) = (popX(i, j, xj, 0) + me_vc) / (pci + me)
                    } // if
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC = " + probC)                           // P(C = i)
            println ("probX = " + probX)                           // P(X_j = x | C = i)
        } // if
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectorI): (Int, String) =
    {
        val prob = new VectorD (k)
        for (i <- 0 until k) {
            prob(i) = probC(i)                                               // P(C = i)
            for (j <- 0 until n) {
                prob(i) *= (if (par(j) > -1) probX(i, j, z(j), z(par(j)))    // P(X_j = z_j | C = i), parent
                            else             probX(i, j, z(j), 0))           // P(X_j = z_j | C = i), no parent
            } // for
        } // for
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()             // class with the highest relative posterior probability
        (best, cn(best))                      // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset()
    {
        popC.set (0)
        probC.set (0)
        popX.clear ()
        probX.clear ()
        popX.alloc (vc(), par)
        probX.alloc (vc(), par)
    } // reset

} // AugNaiveBayes class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `AugNaiveBayes` is the companion object for the `AugNaiveBayes` class.
 */
object AugNaiveBayes
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'AugNaiveBayes object, passing 'x' and 'y' together in one table.
     *  @param xy     the data vectors along with their classifications stored as rows of a matrix
     *  @param fn     the names of the features
     *  @param k      the number of classes
     *  @param vc     the value count (number of distinct values) for each feature
     *  @param me     use m-estimates (me == 0 => regular MLE estimates)
     *  @param thres  the correlation threshold between 2 features for possible parent-child relationship
     */
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String],
               vc: VectorI = null, me: Int = 3, thres: Double = 0.3) =
    {
        new AugNaiveBayes (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1), fn, k, cn,
                           vc, me, thres)
    } // apply

} // AugNaiveBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AugNaiveBayesTest` object is used to test the `AugNaiveBayes` class.
 *  Classify whether a car is more likely to be stolen (1) or not (1).
 *  @see www.inf.u-szeged.hu/~ormandi/ai2/06-AugNaiveBayes-example.pdf
 *  > run-main scalation.analytics.AugNaiveBayesTest
 */
object AugNaiveBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                 x0 x1 x2
    val x = new MatrixI ((10, 3), 1, 0, 1,              // data matrix
                                  1, 0, 1,
                                  1, 0, 1,
                                  0, 0, 1,
                                  0, 0, 0,
                                  0, 1, 0,
                                  0, 1, 0,
                                  0, 1, 1,
                                  1, 1, 0,
                                  1, 0, 0)

    val y  = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)     // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")          // feature/variable names
    val cn = Array ("No", "Yes")                        // class names

    println ("xy = " + (x :+ y))
    println ("---------------------------------------------------------------")

    val anb = new AugNaiveBayes (x, y, fn, 2, cn)       // create the classifier

    // train the classifier ---------------------------------------------------
    anb.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1)         // existing data vector to classify
    val z2 = VectorI (1, 1, 1)         // new data vector to classify
    println ("classify (" + z1 + ") = " + anb.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + anb.classify (z2) + "\n")

    // cross validate the classifier
    anb.crossValidate ()

} // AugNaiveBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AugNaiveBayesTest2` object is used to test the `AugNaiveBayes` class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 *  > run-main scalation.analytics.AugNaiveBayesTest2
 */
object AugNaiveBayesTest2 extends App
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

    val fn = Array ("Fast", "Strong")                  // feature names
    val cn = Array ("No", "Yes")                       // class names

    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val anb = AugNaiveBayes (xy, fn, 2, cn, null, 0)   // create the classifier

    // train the classifier ---------------------------------------------------
    anb.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                             // new data vector to classify
    println ("classify (" + z + ") = " + anb.classify (z) + "\n")

    // cross validate the classifier
    anb.crossValidate ()

} // AugNaiveBayesTest2 object

