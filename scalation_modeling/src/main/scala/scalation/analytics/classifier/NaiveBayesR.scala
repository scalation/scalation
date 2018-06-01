
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sat Sep  8 13:53:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.{ceil, floor}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.random.Normal
import scalation.stat.vectorD2StatVector
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesR` class implements a Gaussian Naive Bayes Classifier, which
 *  is the most commonly used such classifier for continuous input data.  The
 *  classifier is trained using a data matrix 'x' and a classification vector 'y'.
 *  Each data vector in the matrix is classified into one of 'k' classes numbered
 *  0, ..., k-1.  Class probabilities are calculated based on the frequency of
 *  each class in the training-set.  Relative probabilities are computed  by
 *  multiplying these by values computed using conditional density functions
 *  based on the Normal (Gaussian) distribution.  The classifier is naive, because
 *  it assumes feature independence and therefore simply multiplies the conditional
 *  densities.
 *-----------------------------------------------------------------------------
 *  @param x   the real-valued data vectors stored as rows of a matrix
 *  @param y   the class vector, where y_i = class for row i of the matrix x, x(i)
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 */
class NaiveBayesR (x: MatriD, y: VectoI, fn: Array [String], k: Int = 2,
                   cn: Array [String] = Array ("no", "yes"))
      extends ClassifierReal (x, y, fn, k, cn)
{
    private val DEBUG   = false                   // debug flag
    private val EPSILON = 1E-9                    // number close to zero
    private val cor     = calcCorrelation         // feature correlation matrix

    private val nu_y = new VectorD (k)            // frequency counts for classes 0, ..., k-1
    private val mean = new MatrixD (k, n)         // mean for each class, feature
    private val varc = new MatrixD (k, n)         // variance for each class, feature

    private val cd   = Array.ofDim [Double => Double] (k, n)  // conditional density functions
    private var prob: VectoD = null

    if (DEBUG) println ("correlation matrix = " + cor)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate statistics (sample mean and sample variance) for each class
     *  by feature.
     */
    def calcStats ()
    {
        for (i <- 0 until m) {                  // for each data vector in training-set
            val c    = y(i)                     // given classification for ith data vector
            nu_y(c) += 1.0                      // count the number in each class
            for (j <- 0 until n) {              // for each feature
                val d = x(i, j)                 // jth data value
                mean(c, j) += d                 // running total for sum
                varc(c, j) += d * d             // running total for sum of squares
            } // for
        } // for 
    
        for (c <- 0 until k) {                  // for each class
            val mc = nu_y(c)                    // frequency of class c in training-set
            for (j <- 0 until n) {              // for each feature
                mean(c, j) /= mc                                                 // compute mean
                val mean_cj = mean(c, j)
                varc(c, j)  = (varc(c, j) - mc * mean_cj*mean_cj) / (mc - 1.0)   // compute variance - FIX - check
            } // for
        } // for
    
        if (DEBUG) {
            println ("fn   = " + fn)            // feature names
            println ("nu_y = " + nu_y)          // frequency vector (k classes)
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
    def calcHistogram (x_j: VectoD, intervals: Int): VectoD =
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
     *  density 'cd' functions.  Assumes that conditional densities follow the
     *  Normal (Gaussian) distribution.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation
     */
    def train (itest: IndexedSeq [Int]): NaiveBayesR =   // FIX - use parameters 
    {
        calcStats ()
        for (c <- 0 until k; j <- 0 until n) {
            cd(c)(j) = (z_j => Normal (mean(c, j), varc(c, j)).pf (z_j))
        } // for
        prob = nu_y / md              // probability = class frequency / training-set size
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a continuous data vector z, classify it returning the class number
     *  (0, ..., k-1) with the highest relative posterior probability.
     *  Return the best class, its name and its relative probability.
     *  @param z  the data vector to classify
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        for (c <- 0 until k; j <- 0 until n) prob(c) *= cd(c)(j)(z(j))
        if (DEBUG) println ("prob = " + prob)
        val best = prob.argmax ()            // class with the highest relative posterior probability
        (best, cn(best), prob(best))         // return the best class, its name and its probability
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        nu_y.set (0)
        mean.set (0)
        varc.set (0)
    } // reset

} // NaiveBayesR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `NaiveBayesR` is the companion object for the `NaiveBayesR` class.
 */
object NaiveBayesR
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NaiveBayesR` object, passing 'x' and 'y' together in one matrix.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names of the features
     *  @param k   the number of classes
     */
    def apply (xy: MatriD, fn: Array [String], k: Int = 2, cn: Array [String] = Array ("no", "yes")): NaiveBayesR =
    {
        new NaiveBayesR (xy(0 until xy.dim1, 0 until xy.dim2 - 1), xy.col(xy.dim2 - 1).toInt, fn, k, cn)
    } // apply

} // NaiveBayesR object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesRTest` object is used to test the `NaiveBayesR` class.
 *  @see people.revoledu.com/kardi/tutorial/LDA/Numerical%20Example.html
 *  > runMain scalation.analytics.classifier.NaiveBayesRTest
 */
object NaiveBayesRTest extends App
{
    // features/variable:
    // x1: curvature
    // x2: diameter
    // y:  classification: pass (0), fail (1)
    //                            x1    x2    y
    val xy = new MatrixD ((7, 3), 2.95, 6.63, 0,
                                  2.53, 7.79, 0,
                                  3.57, 5.65, 0,
                                  3.16, 5.47, 0,
                                  2.58, 4.46, 1,
                                  2.16, 6.22, 1,
                                  3.27, 3.52, 1)

    val fn = Array ("curvature", "diameter")                   // feature names
    val cn = Array ("pass", "fail")                            // class names
    val nbr = NaiveBayesR (xy, fn, 2, cn)                       // create NaiveBayesR classifier
    nbr.train ()

    banner ("classify")
    val z  = VectorD (2.81, 5.46)
    println (s"classify ($z) = ${nbr.classify (z)}")

    banner ("test")
    val x  = xy.sliceCol (0, 2)
    val y  = xy.col (2).toInt
    val yp = nbr.classify (xy.sliceCol (0, xy.dim2-1))
    println (nbr.fitLabel)
    println (nbr.fit (y, yp))

    val t = VectorD.range (0, x.dim1)
    new Plot (t, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. t")
    new Plot (x.col(0), y.toDouble, yp.toDouble, "y(black)/yp(red) vs. x1")
    new Plot (x.col(1), y.toDouble, yp.toDouble, "y(black)/yp(red) vs. x2")

} // NaiveBayesRTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NaiveBayesRTest2` object is used to test the `NaiveBayesR` class.
 *  Ex: Classify whether a person is male (M) or female (F) based on the measured features.
 *  @see en.wikipedia.org/wiki/Naive_Bayes_classifier
 *  > runMain scalation.analytics.classifier.NaiveBayesRTest2
 */
object NaiveBayesRTest2 extends App
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

} // NaiveBayesRTest2 object

