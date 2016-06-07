
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep 28 11:18:16 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import scala.math.{ceil, sqrt}

import scalation.linalgebra.mem_mapped.{VectorD, VectorI}
import scalation.math.double_exp
import scalation.random.Quantile.studentTInv
import scalation.util.MM_SortingD.imedian

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_StatVector` value class provides methods for computing statistics
 *  on data vectors.  Both maximum likelihood and unbiased estimators are
 *  supported.  Unbiased should only be used on sample (not population) data.
 *  Ex:  It can be used to support the Method of Independent Replications (MIR).
 *  For efficiency, `MM_StatVector` is a value class that enriches the `VectorD`.
 *  The corresponding implicit conversion in the `stat` package object.
 *  @see stackoverflow.com/questions/14861862/how-do-you-enrich-value-classes-without-overhead
 *-----------------------------------------------------------------------------
 *  @param self  the underlying object to be accessed via the 'self' accessor
 */
class MM_StatVector (val self: VectorD)
      extends AnyVal
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-median ('k'-th smallest value) of 'self' vector.  Setting
     *  'k = (dim+1)/2' gives the regular median.
     *  @param k  the type of median (k-th smallest value)
     */
    def median (k: Int = (self.dim+1)/2): Double = imedian (self(), k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the averaged median, which is the median when 'dim' is odd and
     *  the average of the median and the next'k'-median when 'dim' is even.
     */
    def amedian: Double = if (self.dim % 2 == 0) (median () + median ((self.dim+2)/2)) / 2.0
                          else                    median ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sample covariance of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def cov (y: VectorD): Double = ((self dot y) - self.sum * y.sum / self.nd) / (self.nd-1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population covariance of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def pcov (y: VectorD): Double = ((self dot y) - self.sum * y.sum / self.nd) / self.nd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-lag auto-covariance of 'self' vector.
     *  @param k  the lag parameter
     */
    def acov (k: Int = 1): Double =
    {
        val mu  = self.mean
        val num = self.dim - k
        var sum = 0.0
        for (i <- 0 until num) sum += (self(i) - mu) * (self(i+k) - mu)
        sum / num
    } // acov

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Pearson's correlation of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def corr (y: VectorD): Double = cov (y) / sqrt (self.variance * y.variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population Pearson's correlation of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def pcorr (y: VectorD): Double = pcov (y) / sqrt (self.pvariance * y.pvariance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Spearman's rank correlation of 'self' vector with vector 'y'.
     *  @see  en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
     *  @param y  the other vector
     */
    def scorr (y: VectorD): Double = 
    {
        1.0 - 6.0 * (self.rank - y.rank).normSq / (self.nd * (self.nd*self.nd - 1.0))
    } // scorr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-lag auto-correlation of 'self' vector.
     *  @param k  the lag parameter
     */
    def acorr (k: Int = 1): Double = acov (k) / self.variance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the standard deviation of 'self' vector.
     */
    def stddev: Double = sqrt (self.variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population standard deviation of 'self' vector.
     *  @see VectorD for pvariance
     */
    def pstddev: Double = sqrt (self.pvariance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean square (ms) of 'self' vector.
     */
    def ms: Double = self.normSq / self.nd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the root mean square (rms) of 'self' vector.
     */
    def rms: Double = sqrt (ms)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the skewness of 'self' vector.  Negative skewness indicates the
     *  distribution is elongated on the left, zero skewness indicates it is
     *  symmetric, and positive skewness indicates it is elongated on the right.
     *  <p>
     *      E(X - μ)^3 / σ^3
     *  <p>
     *  @see www.mathworks.com/help/stats/skewness.html
     *  @param unbiased  whether to correct for bias
     */
    def skew (unbiased: Boolean = false): Double =
    {
        val n = self.nd
        val s = (self - self.mean).map ((x: Double) => x~^3).sum / (n * pstddev~^3)
        if (unbiased) s * sqrt (n * (n-1.0)) / (n-2.0)
        else s
    } // skew

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the kurtosis of 'self' vector.  High kurtosis (> 3) indicates a 
     *  distribution with heavier tails than a Normal distribution.
     *  <p>
     *      E(X - μ)^4 / σ^4
     *  <p>
     *  @see www.mathworks.com/help/stats/kurtosis.html
     *  @param unbiased  whether to shift the result so Normal is at 0 rather than 3
     */
    def kurtosis (unbiased: Boolean = false): Double =
    {
        val n = self.nd
        val s = (self - self.mean).map ((x: Double) => x~^4).sum / (self.nd * self.pvariance~^2)
        if (unbiased) (n-1.0) * ((n+1.0) * s - 3.0 * (n-1.0)) / ((n-2.0) * (n-3.0)) + 3.0
        else s
    } // kurtosis

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the product of the critical value from the t-distribution and the
     *  standard deviation of the vector.
     *  @param p  the confidence level
     */
    def t_sigma (p: Double = .95): Double =
    {
        val df = self.dim - 1                           // degrees of freedom
        if (df < 1) println ("interval, must have at least 2 observations")
        val pp = 1.0 - (1.0 - p) / 2.0             // e.g., .95 --> .975 (two tails)
        val t  = studentTInv (pp, df)
        t * stddev
    } // t_sigma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the confidence interval half-width for the given confidence level.
     *  The Confidence Interval (CI) is on the mean, i.e., CI = [mean +/- interval].
     *  @param p  the confidence level
     */
    def interval (p: Double = .95): Double = t_sigma (p) / sqrt (self.nd)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the relative precision, i.e., the ratio of the confidence interval
     *  half-width and the mean.
     *  @param p  the confidence level
     */
    def precision (p: Double = .95): Double = interval (p) / self.mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine if the Confidence Interval (CI) on the mean is tight enough.
     *  @param threshold  the cut-off value for CI to be considered tight
     *  @param p          the confidence level
     */
    def precise (threshold: Double = .2, p: Double = .95): Boolean = precision (p) <= threshold

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a standardized version of the vector by subtracting the mean and
     *  dividing by the standard deviation (e.g., Normal -> Standard Normal).
     */
    def standardize: VectorD = (self - self.mean) / stddev

} // MM_StatVector class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_StatVectorTest` object is used to test the `MM_StatVector` class.
 *  > run-main scalation.stat.MM_StatVectorTest
 */
object MM_StatVectorTest extends App
{
    import Conversions._

    val w = VectorD (1.1650, 0.6268, 0.0751, 0.3516, -0.6965)
    val x = VectorD (1.0, 2.0, 3.0, 4.0, 6.0, 5.0)
    val y = VectorD (2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    println ("x           = " + x)
    println ("y           = " + y)
    println ("x.min       = " + x.min ())         // minimum (from VectorD)
    println ("x.max       = " + x.max ())         // maximum (from VectorD)
    println ("x.mean      = " + x.mean)           // mean (from VectorD)
    println ("x.variance  = " + x.variance)       // variance (from VectorD)
    println ("x.pvariance = " + x.pvariance)      // population variance (from VectorD)

    println ("x.median ()   = " + x.median ())      // median
    println ("x.amedian     = " + x.amedian)        // averaged median
    println ("x.cov (y)     = " + x.cov (y))        // covariance
    println ("x.pcov (y)    = " + x.pcov (y))       // population covariance
    println ("x.corr (y)    = " + x.corr (y))       // correlation (Pearson)
    println ("x.pcorr (y)   = " + x.pcorr (y))      // population correlation (Pearson)
    println ("x.rank        = " + x.rank)           // rank order
    println ("x.scorr (y)   = " + x.scorr (y))      // correlation (Spearman)
    println ("x.acorr (1)   = " + x.acorr (1))      // auto-correlation
    println ("x.stddev      = " + x.stddev)         // standard deviation
    println ("x.ms          = " + x.ms)             // mean square
    println ("x.rms         = " + x.rms)            // root mean square
    println ("x.skew ()     = " + x.skew ())        // skewness
    println ("x.kurtosis () = " + x.kurtosis ())    // kurtosis
    println ("x.interval    = " + x.interval ())    // confidence interval (half width)
    println ("x.precision   = " + x.precision ())   // relative precision

    println ("w.skew ()     = " + w.skew ())        // skewness
    println ("w.kurtosis () = " + w.kurtosis ())    // kurtosis

    val z = x.standardize                           // standardized version of vector
    println ("z.mean   = " + z.mean)                // mean (should be 0)
    println ("z.stddev = " + z.stddev)              // standard deviation (should be 1)

} // MM_StatVectorTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_StatVectorTest2` object provides an example of how to use the `MM_StatVector`
 *  class to implement the Method of Independent Replications (MIR) following
 *  a simple two-stage procedure.
 */
object MM_StatVectorTest2 extends App
{
    import scalation.random.Random
    import Conversions._ 

    val rp  = 0.2                          // target for relative precision
    val rng = Random ()

    val n1  = 10                           // number of replications for stage 1
    var rep = new VectorD (n1)
    for (i <- rep.indices) rep(i) = 100.0 * rng.gen
    println ("CI (" + n1 + ") = [ " + rep.mean + " +/- " + rep.interval () + " ]")
    println ("relative precision = " + rep.precision ())

    // estimate the total number replications requires to achieve target rp
    val n = ceil ((rep.t_sigma () / (rp * rep.mean))~^2.0).toInt

    if (n > n1) {                          // run n - n1 more replications for stage 2
        rep = rep.expand (n - n1)
        for (i <- n1 until n) rep(i) = 100.0 * rng.gen
        println ("CI (" + n + ") = [ " + rep.mean + " +/- " + rep.interval () + " ]")
        println ("relative precision = " + rep.precision ())
    } // if

} // MM_StatVectorTest2 object

