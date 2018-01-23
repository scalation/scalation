
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Jul 29 14:44:36 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see brilliant.org/wiki/gaussian-mixture-model
 */

package scalation.analytics.classifier

import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectoI, VectoD, VectorD}
import scalation.random.{Normal, RandomVecI}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GMM` class is used for univariate Gaussian Mixture Models.  Given a
 *  sample, thought to be generated according to 'k' Normal distributions, estimate
 *  the values for the 'mu' and 'sig2' parameters for the Normal distributions.
 *  Given a new  value, determine which class (0, ..., k-1) it is most likely to
 *  have come from.
 *  FIX: need a class for multivariate Gaussian Mixture Models.
 *  FIX: need to adapt for clustering.
 *-----------------------------------------------------------------------------
 *  @param x  the data vector
 *  @param k  the number of components in the mixture
 */
class GMM (x: VectoD, k: Int = 3)
      extends Classifier
{
    private val DEBUG    = true                                      // debug flag
    private val TOL      = 1E-5                                      // tolerance for loop termination
    private val MAX_ITER = 500                                       // maximum number iterations
    private val n        = x.dim                                     // number of samples
    private val krange   = 0 until k                                 // k range
    private val samp     = RandomVecI (k, n-1).igen                  // pick a sample of x indices
    private val mu       = VectorD (for (j <- krange) yield x(j))    // let init means be these x values
    private val sig2     = new VectorD (k); sig2.set (x.variance)    // set init variance to overll variance
    private val phi      = new VectorD (k); phi.set (1.0 / k)        // set int weight to 1/k
    private val gamma    = new MatrixD (n, k)                        // hold conditional probabilities

    private var mu0: VectoD = null                                   // to hold previous mu values

    private var normal: Seq [Normal] = null                          // Normal (Gaussian) distributions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the Expectation (E) Step in the EM algoithm.
     */
    def exp_step ()
    {
        normal = for (j <- 0 until k) yield Normal (mu(j), sig2 (j))
        for (i <- x.range) {
            val w_density = VectorD (for (j <- 0 until k) yield normal(j).pf (x(i)))
            val t_density = w_density.sum
            for (j <- 0 until k) gamma (i, j) = w_density (j) / t_density
        } // for
    } // exp_step

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the Maximumization (M) Step in the EM algoithm.
     */
    def max_step ()
    {
        for (j <- 0 until k) {
            val gamma_j = gamma.col(j)
            val sum_j   = gamma_j.sum
            phi(j)      = sum_j / n
            mu0         = mu.copy ()
            mu(j)       = (gamma_j dot x) / sum_j
            sig2(j)     = (gamma_j dot (x - mu(j))~^2) / sum_j
        } // for
    } // max_step

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model to determine values for the parameter vectors 'mu' and 'sig2'.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def train (testStart: Int, testEnd: Int)
    {
        breakable { for (it <- 1 to MAX_ITER) {
           exp_step ()
           max_step ()
           if (DEBUG) {
               println (s"EM iteration $it:")
               println (s"mu   = $mu")
               println (s"sig2 = $sig2")
           } // if
           if ((mu - mu0).norm1 < TOL) break
        }} // breakable for
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the first point in vector 'z'.
     *  @param z  the vector to be classified.
     */
    def classify (z: VectoD): (Int, String, Double) = 
    {
        val p = VectorD (for (j <- 0 until k) yield normal(j).pf (z(0)))
        if (DEBUG) println (s"p = $p")
        val c = p.argmax ()
        (c, "C" + c, p(c))
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the first point in vector 'z'.
     *  @param z  the vector to be classified.
     */
    def classify (z: VectoI): (Int, String, Double) = classify (z.toDouble)

    def test (testStart: Int, testEnd: Int): Double = ???

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset ...  FIX
     */
    def reset () {}

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the feature set.
     */
    def size: Int = k

} // GMM class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GMMTest` object is used to test the `GMM` class.
 *  > runMain scalation.analytics.classifier.GMMTest
 */
object GMMTest extends App
{
    println ("GMMTest")

    val n = 100000
    val k = 3
    val normal = for (j <- 0 until k) yield Normal (j+1.0, j+1.0)
    val x = VectorD (for (i <- 0 until n) yield normal(i % k).gen)
    val gmm = new GMM (x, k)
    gmm.train ()
    println ("predict = " + gmm.classify (VectorD (4.0)))

} // GMMTest object

