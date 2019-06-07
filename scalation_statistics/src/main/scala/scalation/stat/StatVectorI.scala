
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Jun 19 13:14:37 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import scala.math.{ceil, sqrt}
import scala.collection.mutable.HashMap

import scalation.linalgebra.{VectoI, VectorI}
import scalation.math.{double_exp, int_exp}
import scalation.random.Quantile.{normalInv, studentTInv}
import scalation.util.SortingI.imedian

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorI` value class provides methods for computing statistics
 *  on integer values data vectors.  Both maximum likelihood and unbiased estimators
 *  are supported.  Unbiased should only be used on sample (not population) data.
 *  For efficiency, `StatVectorI` is a value class that enriches the `VectorI`.
 *  The corresponding implicit conversion in the `stat` package object.
 *  @see stackoverflow.com/questions/14861862/how-do-you-enrich-value-classes-without-overhead
 *-----------------------------------------------------------------------------
 *  @param self  the underlying object to be accessed via the 'self' accessor
 */
class StatVectorI (val self: VectorI)
      extends AnyVal
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-median ('k'-th smallest value) of 'self' vector.  Setting
     *  'k = (dim+1)/2' gives the regular median.
     *  @param k  the type of median (k-th smallest value)
     */
    def median (k: Int = (self.dim+1)/2): Int = imedian (self().toArray, k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mode of the 'self' vector, i.e., the value that occurs most frequently.
     */
    def mode: Int =
    {
        val zmap = HashMap [Int, Int] ()                     // map value z -> count zc
        var (mz, mc) = (0, 0)                                // max occuring value and its count
        for (z <- self) {
            val zc = zmap.getOrElse (z, 0) + 1               // find count and increment
            zmap  += z -> zc                                 // update map
            if (zc > mc) { mz = z; mc = zc }                 // save if count is larger
        } // for
        if (mc == 0) println ("StatVectorI.mode: no elements were found in the vector")
        mz
    } // mode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sample covariance of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def cov (y: VectorI): Double = ((self dot y) - self.sum * y.sum / self.nd) / (self.nd-1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population covariance of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def pcov (y: VectorI): Double = ((self dot y) - self.sum * y.sum / self.nd) / self.nd

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
    def corr (y: VectorI): Double =
    {
        val c = cov (y) / sqrt (self.variance * y.variance)
        if (c.isNaN) if (self == y) 1.0 else 0.0 else c
    } // corr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population Pearson's correlation of 'self' vector with vector 'y'.
     *  Note:  should only differ from 'corr' due to round-off errors and NaN issue.
     *  @param y  the other vector
     */
    def pcorr (y: VectorI): Double = pcov (y) / sqrt (self.pvariance * y.pvariance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Spearman's rank correlation of 'self' vector with vector 'y'.
     *  @see  en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
     *  @param y  the other vector
     */
    def scorr (y: VectorI): Double = 
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
     *  @see VectorI for variance
     */
    def stddev: Double = sqrt (self.variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population standard deviation of 'self' vector.
     *  @see VectorI for pvariance
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
        val s = (self.toDouble - self.mean).map ((x: Double) => x~^3).sum / (n * pstddev~^3)
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
        val s = (self.toDouble - self.mean).map ((x: Double) => x~^4).sum / (self.nd * self.pvariance~^2)
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
        val df = self.dim - 1                                // degrees of freedom
        if (df < 1) println ("interval, must have at least 2 observations")
        val pp = 1.0 - (1.0 - p) / 2.0                       // e.g., .95 --> .975 (two tails)
        val t  = studentTInv (pp, df)
        t * stddev
    } // t_sigma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the product of the critical value from the z-distribution (Standard
     *  Normal) and the standard deviation of the vector.
     *  @param p  the confidence level
     */
    def z_sigma (p: Double = .95): Double =
    {
        val pp = 1.0 - (1.0 - p) / 2.0                       // e.g., .95 --> .975 (two tails)
        val z  = normalInv (pp)
        z * stddev
    } // z_sigma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the confidence interval half-width for the given confidence level.
     *  The Confidence Interval (CI) is on the mean, i.e., CI = [mean +/- interval].
     *  @param p  the confidence level
     */
    def interval (p: Double = .95): Double = t_sigma (p) / sqrt (self.nd)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the confidence interval half-width for the given confidence level.
     *  The Confidence Interval (CI) is on the mean, i.e., CI = [mean +/- interval].
     *  This method assumes that the population standard deviation is known.
     *  uses the Standard Normal distribution.
     *  @param sig  the population standard deviation
     *  @param p    the confidence level
     */
    def interval2 (sig: Double, p: Double = .95): Double = z_sigma (p) / sqrt (self.nd)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the confidence interval as (lower, upper) after calling either
     *  interval (unknown standard deviation) or interval2 (know standard deviation).
     *  @param mu_  the sample mean
     *  @param ihw  the interval half width
     */
    def ci (mu_ : Double, ihw: Double): (Double, Double) = (mu_ - ihw, mu_ + ihw)

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

} // StatVectorI class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorITest` object is used to test the `StatVectorI` class.
 *  @see www.mathworks.com/help/stats/skewness.html
 *  > runMain scalation.stat.StatVectorITest
 */
object StatVectorITest extends App
{
    val x = VectorI (1, 2, 3, 4, 3, 5)
    val y = VectorI (2, 3, 4, 5, 6, 7)

    println ("x             = " + x)
    println ("y             = " + y)
    println ("x.min         = " + x.min ())         // minimum (from VectorI)
    println ("x.max         = " + x.max ())         // maximum (from VectorI)
    println ("x.mean        = " + x.mean)           // mean (from VectorI)
    println ("x.variance    = " + x.variance)       // variance (from VectorI)
    println ("x.pvariance   = " + x.pvariance)      // population variance (from VectorI)

    println ("x.median ()   = " + x.median ())      // median
    println ("x.mode        = " + x.mode)           // mode
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

    println ("x.skew ()     = " + x.skew ())        // skewness
    println ("x.kurtosis () = " + x.kurtosis ())    // kurtosis

} // StatVectorITest object

