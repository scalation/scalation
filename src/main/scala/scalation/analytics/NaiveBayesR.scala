
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Sep  8 13:53:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.{ceil, floor}

import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectorI}
import scalation.math._
import scalation.random.Normal
import scalation.stat.StatVector

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesR` class implements a Gaussian Naive Bayes Classifier, which
 *  is the most commonly used such classifier for continuous input data.  The
 *  classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Prior probabilities are calculated based on the population of
 *  each class in the training-set.  Relative posterior probabilities are computed
 *  by multiplying these by values computed using conditional density functions
 *  based on the Normal (Gaussian) distribution.  The classifier is naive, because
 *  it assumes feature independence and therefore simply multiplies the conditional
 *  densities.
 *-----------------------------------------------------------------------------
 *  @param x   the real-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y(l) = class for row l of the matrix x, x(l)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
class NaiveBayesR (x: MatrixD, y: VectorI, fn: Array [String], k: Int, cn: Array [String])
      extends ClassifierReal (x, y, fn, k, cn)
{
    private val DEBUG   = false                 // debug flag
    private val EPSILON = 1E-9                  // number close to zero

    private val pop  = new VectorD (k)          // numbers in class 0, ..., k-1
    private val mean = new MatrixD (k, n)       // mean for each class, feature
    private val varc = new MatrixD (k, n)       // variance for each class, feature

    private val cd   = Array.ofDim [Double => Double] (k, n)  // conditional density functions
    private var prob: VectorD = null

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the correlation matrix for the feature vectors 'fea'.
     *  If the correlations are too high, the independence assumption may be dubious.
     */
    def calcCorrelation: MatriD =
    {
        val fea = for (j <- 0 until n) yield new StatVector (x.col(j))
        val cor = new MatrixD (n, n)
        for (j1 <- 0 until n; j2 <- 0 until j1) cor(j1, j2) = fea(j1) corr fea(j2)
        cor
    } // calcCorrelation

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate statistics (sample mean and sample variance) for each class
     *  by feature.
     */
    def calcStats ()
    {
        for (i <- 0 until m) {                  // for each data vector in training-set
            val c = y(i)                        // given classification for ith data vector
            pop(c) += 1.0                       // count the number in each class
            for (j <- 0 until n) {              // for each feature
                val d = x(i, j)                 // jth data value
                mean(c, j) += d                 // running total for sum
                varc(c, j) += d * d             // running total for sum of squares
            } // for
        } // for 
    
        for (c <- 0 until k) {                  // for each class
            val pc = pop(c)                     // population of class c in training-set
            for (j <- 0 until n) {              // for each feature
                mean(c, j) /= pc                                               // compute mean
                varc(c, j) =  (varc(c, j) - pc * mean(c, j)~^2) / (pc - 1.0)   // compute variance
            } // for
        } // for
    
        if (DEBUG) {
            println ("fn   = " + fn)            // feature names
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
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def train (testStart: Int, testEnd: Int)    // FIX - use parameters 
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
    def classify (z: VectorD): Tuple2 [Int, String] =
    {
        for (c <- 0 until k; j <- 0 until n) prob(c) *= cd(c)(j)(z(j))
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()             // class with the highest relative posterior probability
        (best, cn(best))                      // return the best class and its name
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        pop.set (0)
        mean.set (0)
        varc.set (0)
    } // reset

} // NaiveBayesR class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesRTest` object is used to test the 'NaiveBayesR' class.
 ** Ex: Classify whether a person is male (M) or female (F) based on the measured features.
 *  @see http://en.wikipedia.org/wiki/Naive_Bayes_classifier
 */
object NaiveBayesRTest extends App
{
    // training-set -----------------------------------------------------------
    // x0: Height
    // x1: Weight
    // x2: Foot-size
    // features:                   x0       x1     x2
    val x = new MatrixD ((8, 3), 6.00,   180.0,  12.0,          // data matrix
                                 5.92,   190.0,  11.0,
                                 5.58,   170.0,  12.0,
                                 5.92,   165.0,  10.0,
                                 5.00,   100.0,   6.0,
                                 5.50,   150.0,   8.0,
                                 5.42,   130.0,   7.0,
                                 5.75,   150.0,   9.0)
    val y  = VectorI (0, 0, 0, 0, 1, 1, 1, 1)                   // classification vector: 0(M), 1(F))
    val fn = Array ("Height", "Weight", "Foot-size")            // feature/value names
    val cn = Array ("M", "F")                                   // class names

    println ("x = " + x)
    println ("y = " + y)
    println ("---------------------------------------------------------------")

    val nbr = new NaiveBayesR (x, y, fn, 2, cn)                 // create the classifier            

    // check independence assumption ------------------------------------------
    println ("cor = " + nbr.calcCorrelation)

    // train the classifier ---------------------------------------------------
    nbr.train ()

    // test sample ------------------------------------------------------------
    val z = VectorD (6.0, 130, 8.0)                             // new data vector to classify
    println ("--- classify " + z + " = " + nbr.classify (z) + "\n")

} // NaiveBayesRTest object

