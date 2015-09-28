
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Aug 26 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.{ceil, sqrt}

import scalation.linalgebra.{VectorD, VectorI}
import scalation.math._
import scalation.random.Random
import scalation.random.Quantile.studentTInv
import scalation.util.SortingD.{imedian, iqsort}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVector` class provides methods for computing common statistics on a
 *  data vector.  Both maximum likelihood (the default) and unbiased estimators
 *  are supported.  Unbiased should only be used on sample (not population) data.
 *  Ex:  It can be used to support the Method of Independent Replications (MIR).
 *  @param dim       the dimension/size of the vector
 *  @param unbiased  whether the estimators are restricted to be unbiased.
 */
class StatVector (dim: Int, private var unbiased: Boolean = false)
      extends VectorD (dim)
{
    /** The number of samples
     */
    private val n = dim.toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a StatVector from a vector of type VectorD (vector of Doubles).
     *  @param u  the vector used to initialize StatVector
     */
    def this (u: VectorD)
    {
        this (u.dim)
        setAll (u())
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a StatVector from a vector of type VectorI (vector of Ints).
     *  @param u  the vector used to initialize StatVector
     */
    def this (u: VectorI)
    {
        this (u.dim)
        for (i <- 0 until dim) v(i) = u(i).toDouble
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle/flip the bias flag for the estimators.
     */
    def toggleBias () { unbiased = ! unbiased }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of this stat vector by 'more' elements.
     *  @param more  the number of new elements to add
     */
    override def expand (more: Int = dim): StatVector = new StatVector (super.expand (more))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The denominator for sample variance and covariance is one less for
     *  unbiased 'n-1' vs. maximum likelihood 'n' estimators.  Also use 'n' for
     *  population statistics.
     */
    private def den: Double = if (unbiased) n - 1.0 else n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the number of samples.
     */
    def num: Int = dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-median ('k'-th smallest value) of 'this' vector.  Setting
     *  'k = (dim+1)/2' gives the regular median.
     *  @param k  the type of median (k-th smallest value)
     */
    def median (k: Int = (dim+1)/2): Double = imedian (this(), k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the averaged median, which is the median when dim is odd and
     *  the average of the median and the next'k'-median when dim is even.
     */
    def amedian: Double = if (dim % 2 == 0) (median () + median ((dim+2)/2)) / 2.0
                          else               median ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the variance of 'this' vector.
     */
    def variance: Double = (normSq - sum * sum / n) / den 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the covariance of 'this' vector with vector 'y'.
     *  @param y  the other vector
     */
    def cov (y: VectorD): Double = ((this dot y) - sum * y.sum / n) / den

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the k-lag auto-covariance of 'this' vector.
     *  @param k  the lag parameter
     */
    def acov (k: Int = 1): Double =
    {
        val mu  = mean
        val num = dim - k
        var sum = 0.0
        for (i <- 0 until num) sum += (v(i) - mu) * (v(i+k) - mu)
        sum / num
    } // acov

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Pearson's correlation of 'this' vector with vector 'y'.
     *  @see 
     *  @param y  the other vector
     */
    def corr (y: StatVector): Double = cov (y) / sqrt (variance * y.variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish the rank order of the elements in 'this' vector, e.g.,
     *  (8.0, 2.0, 4.0, 6.0) is (3, 0, 1, 2).
     */
    def rank: VectorI = new VectorI (iqsort (this()))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Spearman's rank correlation of 'this' vector with vector 'y'.
     *  @see  http://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient
     *  @param y  the other vector
     */
    def scorr (y: StatVector): Double = 
    {
        1.0 - 6.0 * (rank - y.rank).normSq / (n * (n*n - 1.0))
    } // scorr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-lag auto-correlation of 'this' vector.
     *  @param k  the lag parameter
     */
    def acorr (k: Int = 1): Double = acov (k) / variance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the standard deviation of 'this' vector.
     */
    def stddev: Double = sqrt (variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean square (ms) of 'this' vector.
     */
    def ms: Double = normSq / n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the root mean square (rms) of 'this' vector.
     */
    def rms: Double = sqrt (ms)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the skewness of 'this' vector.  Negative skewness indicates the
     *  distribution is elongated on the left, zero skewness indicates it is
     *  symmetric, and positive skewness indicates it is elongated on the right.
     *  @see http://www.mathworks.com/help/stats/skewness.html
     */
    def skew: Double =
    {
        val s = (this - mean).sum~^3 / (n * stddev~^3)
        if (unbiased) s * sqrt (n * (n-1.0)) / (n-2.0) else s
    } // skew

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the product of critical value from the t-distribution and the
     *  standard deviation.
     *  @param p  the confidence level
     */
    def t_sigma (p: Double = .95): Double =
    {
        val df = dim - 1                           // degrees of freedom
        if (df < 1) flaw ("interval", "must have at least 2 observations")
        val pp = 1.0 - (1.0 - p) / 2.0             // e.g., .95 --> .975 (two tails)
        val t  = studentTInv (pp, df)
        t * stddev
    } // t_sigma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the confidence interval half-width for the given confidence level.
     *  The Confidence Interval (CI) is on the mean, i.e., CI = [mean +/- interval].
     *  @param p  the confidence level
     */
    def interval (p: Double = .95): Double = t_sigma (p) / sqrt (dim.toDouble)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the relative precision, i.e., the ratio of the confidence interval
     *  half-width and the mean.
     *  @param p  the confidence level
     */
    def precision (p: Double = .95): Double = interval (p) / mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine if the Confidence Interval (CI) on the mean is tight enough.
     *  @param threshold  the cut-off value for CI to be considered tight
     *  @param p          the confidence level
     */
    def precise (threshold: Double = .2, p: Double = .95): Boolean = precision (p) <= threshold

} // StatVector class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVector` companion object provides methods for building `StatVector`s.
 */
object StatVector
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `StatVector` from one or more values (repeated values Double*).
     *  @param x   the first value
     *  @param xs  the rest of the values (zero or more additional values)
     */
    def apply (x: Double, xs: Double*): StatVector =
    {
        val u = new StatVector (1 + xs.length)
        u(0) = x
        for (i <- 1 until u.dim) u(i) = xs(i-1)
        u
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `StatVector` from an Array.
     *  @param xa  the array used to initialize StatVector
     */
    def apply (xa: Array [Double]): StatVector =
    {
        val u = new StatVector (xa.length)
        u.setAll (xa)
        u
    } // apply

} // StatVector object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest` object is used to test the `StatVector` class.
 */
object StatVectorTest extends App
{
    val x = StatVector (1.0, 2.0, 3.0, 4.0, 6.0, 5.0)
    val y = StatVector (2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    println ("x           = " + x)
    println ("y           = " + y)
    println ("x.min       = " + x.min ())         // minimum (from VectorD)
    println ("x.max       = " + x.max ())         // maximum (from VectorD)
    println ("x.mean      = " + x.mean)           // mean
    println ("x.median () = " + x.median ())      // median
    println ("x.amedian   = " + x.amedian)        // averaged median
    println ("x.variance  = " + x.variance)       // variance
    println ("x.cov (y)   = " + x.cov (y))        // covariance
    println ("x.corr (y)  = " + x.corr (y))       // correlation (Pearson)
    println ("x.rank      = " + x.rank)           // rank order
    println ("x.scorr (y) = " + x.scorr (y))      // correlation (Spearman)
    println ("x.acorr (1) = " + x.acorr (1))      // auto-correlation
    println ("x.stddev    = " + x.stddev)         // standard deviation
    println ("x.ms        = " + x.ms)             // mean square
    println ("x.rms       = " + x.rms)            // root mean square
    println ("x.skew      = " + x.skew)           // skewness
    println ("x.interval  = " + x.interval ())    // confidence interval (half width)
    println ("x.precision = " + x.precision ())   // relative precision

} // StatVectorTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest2` object provides an example of how to use the `StatVector`
 *  class to implement the Method of Independent Replications (MIR) following
 *  a simple two-stage procedure.
 */
object StatVectorTest2 extends App
{
    val rp  = 0.2                          // target for relative precision
    val rng = Random ()

    val n1  = 10                           // number of replications for stage 1
    var rep = new StatVector (n1)
    for (i <- rep.indices) rep(i) = 100.0 * rng.gen
    println ("CI (" + n1 + ") = [ " + rep.mean + " +/- " + rep.interval () + " ]")
    println ("relative precision = " + rep.precision ())

    // estimate the total number replications requires to achieve target rp
    val n = ceil ((rep.t_sigma () / (rp * rep.mean))~^2.0).toInt

    if (n > n1) {                          // run n - n1 more replications for stage 2
        rep    = rep.expand (n - n1)
        for (i <- n1 until n) rep(i) = 100.0 * rng.gen
        println ("CI (" + n + ") = [ " + rep.mean + " +/- " + rep.interval () + " ]")
        println ("relative precision = " + rep.precision ())
    } // if

} // StatVectorTest2 object

