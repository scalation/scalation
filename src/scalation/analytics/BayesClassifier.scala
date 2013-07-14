
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Sep  8 13:53:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.{ceil, floor}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra_gen.Vectors.VectorI
import scalation.math.DoubleWithExp._
import scalation.random.Normal
import scalation.stat.StatVector
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class implements a Naive Gaussian Bayes Classifier.  The classifier is
 *  trained using a data matrix x and a classification vector y.  Each data vector
 *  in the matrix is classified into one of k classes numbered 0, ..., k-1.
 *  Prior probabilities are calculated based on the population of each class in
 *  the training-set.  Relative posterior probabilities are computed by multiplying
 *  these by values computed using conditional density functions based on the Normal
 *  (Gaussian) distribution.  The classifier is naive, because it assumes feature
 *  independence and therefore simply multiplies the conditional densities.
 *  @param x  the data vectors stored as rows of a matrix
 *  @param y  the class vector, where y_i = class for row i of the matrix x
 *  @param k  the number of classes
 */
class BayesClassifier (x: MatrixD, y: Array [Int], k: Int = 2)
      extends Classifier with Error
{
    if (k >= x.dim1) flaw ("constructor", "k must be less than the training-set size")

    private val DEBUG   = false                 // debug flag
    private val EPSILON = 1E-9                  // number close to zero
    private val m       = x.dim1                // the number of data vectors in training-set
    private val n       = x.dim2                // the number of features
    private val md      = m.toDouble            // training-set size as a Double
    private val nd      = n.toDouble            // feature-set size as a Double

    private val pop  = new VectorD (k)          // numbers in class 0, ..., k-1
    private val mean = new MatrixD (k, n)       // mean for each class, feature
    private val varc = new MatrixD (k, n)       // variance for each class, feature

    private val cd   = Array.ofDim [Double => Double] (k, n)  // conditional density functions
    private var prob: VectorD = null

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
    /** Calculate statistics (sample mean and sample variance) for each class
     *  by feature.
     */
    def calcStats ()
    {
        for (i <- 0 until m) {                  // for each data vector in training-set
            val c = y(i)                        // given classification for ith data vector
            pop(c) += 1.0                        // count the number in each class
            for (j <- 0 until n) {              // for each feature
                val d = x(i, j)                 // jth data value
                mean(c, j) += d                 // running total for sum
                varc(c, j) += d * d             // running total for sum of squares
            } // for
        } // for 
    
        for (c <- 0 until k) {                  // for each class
            val pc = pop(c)                     // population of class c in training-set
            for (j <- 0 until n) {              // for each feature
                mean(c, j) /= pc                                             // compute mean
                varc(c, j) =  (varc(c, j) - pc * mean(c, j)~^2) / (pc - 1.0)  // compute variance
            } // for
        } // for
    
        if (DEBUG) {
            println ("pop  = " + pop)           // population vector (k classes)
            println ("prob = " + prob)          // probability vector (k classes)
            println ("mean = " + mean)          // mean matrix (k classes, n features)
            println ("varc = " + varc)          // variance matrix (k classes, n features)
        } // if
    } // calcStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the counts for each interval in the histogram.
     *  @param x_j  the vector for feature j given class c.
     *  @param intervals  the number intervals
     */
    def calcHistogram (x_j: VectorD, intervals: Int): VectorD =
    {
        val minVal = floor (x_j.min ())
        val maxVal = ceil (x_j.max () + EPSILON)
        val intWid = (maxVal - minVal) / intervals.toDouble
        val h      = new VectorD (intervals)
        for (xx <- x_j) {
            val i = (floor ((xx - minVal) / intWid)).toInt
            h(i) += 1.0
        } // for
        h
    } // calcHistogram

    // use Discrete distribution based on histogram

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier, i.e., calculate statistics and create conditional
     *  density (cd) functions.  Assumes that conditional densities follow the
     *  Normal (Gaussian) distribution.
     */
    def train ()
    {
        calcStats ()
        for (c <- 0 until k; j <- 0 until n) {
            cd(c)(j) = (z_j => Normal (mean(c, j), varc(c, j)).pf (z_j))
        } // for
        prob = pop / md           // probability = class population / training-set size
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a continuous data vector z, classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  @param z  the data vector to classify
     */
    def classify (z: VectorD): Int =
    {
        for (c <- 0 until k; j <- 0 until n) prob(c) *= cd(c)(j)(z(j))
        println ("prob = " + prob)
        prob.argmax ()           // class with the highest relative posterior probability
    } // classify

} // BayesClassifier class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the BayesClassifier class.  Ex: Classify whether
 *  a person is male (M) or female (F) based on the measured features.
 *  @see http://en.wikipedia.org/wiki/Naive_Bayes_classifier
 */
object BayesClassifierTest extends App
{
    // training-set -----------------------------------------------------------
    // features:                 height, weight, foot-size
    val x = new MatrixD ((8, 3), 6.00,   180.0,   12.0,     // data matrix
                                 5.92,   190.0,   11.0,
                                 5.58,   170.0,   12.0,
                                 5.92,   165.0,   10.0,
                                 5.00,   100.0,    6.0,
                                 5.50,   150.0,    8.0,
                                 5.42,   130.0,    7.0,
                                 5.75,   150.0,    9.0)
    val y = Array (0, 0, 0, 0, 1, 1, 1, 1)          // classification vector: 0(M), 1(F))
    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    // check independence assumption ------------------------------------------
    val cl = new BayesClassifier (x, y)                   // create the classifier            
    cl.checkCorrelation

    // train the classifier ---------------------------------------------------
    cl.train ()

    // test sample ------------------------------------------------------------
    val z = new VectorD (6.0, 130, 8.0)                     // new data vector to classify
    println ("--- classify " + z + " = " + cl.classify (z) + "\n")

} // BayesClassifierTest object

