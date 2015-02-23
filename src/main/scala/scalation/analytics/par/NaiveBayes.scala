
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sat Sep  8 13:53:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.par

import math.round

import scalation.analytics.ClassifierInt
import scalation.linalgebra.{MatrixI, VectorD, VectorI}
import scalation.linalgebra.par.MatrixD
import scalation.stat.StatVector
import scalation.util.Error

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
 *  This version uses parallel processing to speed up execution.
 *  @param x   the integer-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param vc  the value count (number of distinct values) for each feature
 *  @param me  use m-estimates (me == 0 => regular MLE estimates)
 */
class NaiveBayes (x: MatrixI, y: VectorI, fn: Array [String], k: Int, cn: Array [String],
                    private var vc: VectorI = null, me: Int = 3)
      extends ClassifierInt (x, y, fn, k, cn)
{
    private val DEBUG = true                       // debug flag

    private val popC  = new VectorI (k)            // frequency counts for classes 0, ..., k-1
    private val popX  = Array.ofDim [MatrixI] (n)  // conditional frequency counts for variable/feature j
    private val probC = new VectorD (k)            // probabilities for classes 0, ..., k-1
    private val probX = Array.ofDim [MatrixD] (n)  // conditional probabilities for variable/feature j

    if (vc == null) vc = vc_default                // set to default for binary data (2)
    println ("vc = " + vc)
    for (j <- 0 until n) {
        popX(j)  = new MatrixI (vc(j), k)          // hold #(X_j = v & C = i)
        probX(j) = new MatrixD (vc(j), k)          // hold P(X_j = v | C = i)
    } // for

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the correlation of the feature vectors (fea).  If the correlations
     *  are too high, the independence assumption may be dubious.
     */
    def checkCorrelation
    {
        val fea = for (j <- 0 until n) yield new StatVector (x.col(j))
        val cor = new MatrixD (n, n)
        for (j1 <- 0 until n; j2 <- 0 until j1) cor(j1, j2) = fea(j1) corr fea(j2)
        println ("correlation matrix = " + cor)
    } // checkCorrelation

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequencies for 'y' having class 'i' and 'x' for cases 0, 1, ...
     */
    def frequencies ()
    {
        for (l <- 0 until m) {
            val i = y(l)                            // get the class
            popC(i) += 1
            for (j <- 0 until n) { 
                popX(j)(x(l, j), i) += 1
            } // for
        } // for

        if (DEBUG) {
            println ("popC   = " + popC)                   // #(C = i)
            for (j <- 0 until n) {
                println (fn(j) + ": popX(" + j + ") = " + popX(j))   // #(X_j = v & C = i)
            } // for
        } // if
    } // frequencies

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier by computing the probabilities for C, and the
     *  conditional probabilities for X_j.
     */
    def train ()
    {
        frequencies ()
        for (i <- 0 until k) {                           // for each class i
            val pci  = popC(i).toDouble
            probC(i) = pci / md
            for (j <- 0 until n) {                       // for each feature j
                val me_vc = me / vc(j).toDouble
                for (v <- 0 until vc(j)) {               // for each value v for feature j
                    probX(j)(v, i) = (popX(j)(v, i) + me_vc) / (pci + me)
                } // for
            } // for
        } // for

        if (DEBUG) {
            println ("probC   = " + probC)                   // P(C = i)
            for (j <- 0 until n) {
                println (fn(j) + ": probX(" + j + ") = " + probX(j))   // P(X_j = v | C = i)
            } // for
        } // if
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a discrete data vector 'z', classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectorI): Tuple2 [Int, String] =
    {
        val prob = new VectorD (k)
        for (i <- 0 until k) {
            prob(i) = probC(i)                                   // P(C = i)
            for (j <- 0 until n) prob(i) *= probX(j)(z(j), i)    // P(X_j = z_j | C = i)
        } // for
        println ("prob = " + prob)
        val best = prob.argmax ()           // class with the highest relative posterior probability
        (best, cn(best))                    // return the best class and its name
    } // classify

} // NaiveBayes class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `NaiveBayes` is the companion object for the `NaiveBayes` class.
 */
object NaiveBayes
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'NaiveBayes` object, passing 'x' and 'y' together in one table.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     *  @param vc  the value count (number of distinct values) for each feature
     *  @param me  use m-estimates (me == 0 => regular MLE estimates)
     */
    def apply (xy: MatrixI, fn: Array [String], k: Int, cn: Array [String], vc: VectorI = null, me: Int = 3) =
    {
        new NaiveBayes (xy(0 until xy.dim1, 0 until xy.dim2-1), xy.col(xy.dim2-1), fn, k, cn, vc, me)
    } // apply

} // NaiveBayes object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest` object is used to test the 'NaiveBayes' class.
 ** Ex: Classify whether a car is more likely to be stolen (1) or not (1).
 *  http://www.inf.u-szeged.hu/~ormandi/ai2/06-naiveBayes-example.pdf
 */
object NaiveBayesTest extends App
{
    // x0: Color:   Red (1), Yellow (0)
    // x1: Type:    SUV (1), Sports (0)
    // x2: Origin:  Domestic (1), Imported (0)
    // features:                 x0  x1  x2
    val x = new MatrixI ((10, 3), 1,  0,  1,                     // data matrix
                                  1,  0,  1,
                                  1,  0,  1,
                                  0,  0,  1,
                                  0,  0,  0,
                                  0,  1,  0,
                                  0,  1,  0,
                                  0,  1,  1,
                                  1,  1,  0,
                                  1,  0,  0)
    val y = VectorI (1, 0, 1, 0, 1, 0, 1, 0, 0, 1)               // classification vector: 0(No), 1(Yes))
    val fn = Array ("Color", "Type", "Origin")                   // feature/variable names
    val cn = Array ("No", "Yes")                                 // class names

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val bnb = new NaiveBayes (x, y, fn, 2, cn)                // create the classifier            

    // train the classifier ---------------------------------------------------
    bnb.train ()

    // test sample ------------------------------------------------------------
    val z1 = VectorI (1, 0, 1)                                   // new data vector to classify
    val z2 = VectorI (1, 1, 1)                                   // new data vector to classify
    println ("classify (" + z1 + ") = " + bnb.classify (z1) + "\n")
    println ("classify (" + z2 + ") = " + bnb.classify (z2) + "\n")

} // NaiveBayesTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesTest2` object is used to test the 'NaiveBayes' class.
 *  Given whether a person is Fast and/or Strong, classify them as making C = 1
 *  or not making C = 0 the football team.
 */
object NaiveBayesTest2 extends App
{
    // training-set -----------------------------------------------------------
    // x0: Fast
    // x1: Strong
    // y:  Classification (No/0, Yes/1)
    // features:                  x0  x1   y
    val xy = new MatrixI ((10, 3), 1,  1,  1,
                                   1,  1,  1,
                                   1,  0,  1,
                                   1,  0,  1,
                                   1,  0,  0,
                                   0,  1,  0,
                                   0,  1,  0,
                                   0,  1,  1,
                                   0,  0,  0,
                                   0,  0,  0)

    val fn = Array ("Fast", "Strong")
    println ("xy = " + xy)
    println ("---------------------------------------------------------------")

    val bnb = NaiveBayes (xy, fn, 2, null, null, 0)     // create the classifier            

    // train the classifier ---------------------------------------------------
    bnb.train ()

    // test sample ------------------------------------------------------------
    val z = VectorI (1, 0)                                     // new data vector to classify
    println ("classify (" + z + ") = " + bnb.classify (z) + "\n")

} // NaiveBayesTest2 object

