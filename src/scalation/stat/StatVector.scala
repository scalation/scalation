
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Aug 26 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.sqrt

import scalation.linalgebra.VectorD
import scalation.math.DoubleWithExp._
import scalation.random.Quantile.studentTInv

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The StatVector class provides methods for computing common statistics on a
 *  data vector.  Both maximum likelihood (the default) and unbiased estimators
 *  are supported.  Unbiased should only be used on sample (not population) data.
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
    /** Construct a StatVector from an Array.
     *  @param u  the array to initialize StatVector
    def this (u: Array [Double])
    {
        this (u.length)
        setAll (u)
    } // constructor
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a StatVector from two or more values (repeated values Double*).
     *  @param u0  the first value
     *  @param u1  the second value
     *  @param u   the rest of the values (zero or more additional values)
     */
    def this (u0: Double, u1: Double, u: Double*)
    {
        this (u.length + 2)
        this(0) = u0; this(1) = u1
        for (i <- 2 until dim) this(i) = u(i - 2)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a StatVector from a VectorN [Double], i.e., VectorD.
     *  @param u  the vector to initialize StatVector
     */
    def this (u: VectorD)
    {
        this (u.dim)
        setAll (u())
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle/flip the bias flag for the estimators.
     */
    def toggleBias () { unbiased = ! unbiased }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The denominator for sample variance and covariance is one less for
     *  unbiased (n-1) vs. maximum likelihood (n) estimators.  Also use n for
     *  population statistics.
     */
    private def den: Double = if (unbiased) n - 1.0 else n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the number of samples.
     */
    def num: Int = dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of this vector.
     */
    def mean: Double = sum / n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the median (middle value) of this vector.  For odd size, it is
     *  the middle element, for even, it is the larger of the two middle elements.
     *  FIX:  need a more efficient algorithm
     */
    def median: Double = { sort (); v(dim / 2) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the variance of this vector.
     */
    def variance: Double = (normSq - sum * sum / n) / den 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the covariance of this vector with vector y.
     *  @param y  the other vector
     */
    def cov (y: VectorD): Double = ((this dot y) - sum * y.sum / n) / den

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the k-lag auto-covariance of this vector.
     *  @param k  the lag parameter
     */
    def acov (k: Int = 1): Double =
    {
        val mu  = mean
        val num = dim - k
        var sum = 0.0
        for (i <- 0 until num) sum += (v(i) - mu) * (v(i+k) - mu)
        sum / num
    } // acorr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Pearson's correlation of this vector with vector y.
     *  @param y  the other vector
     */
    def corr (y: StatVector): Double = cov (y) / sqrt (variance * y.variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the k-lag auto-correlation of this vector.
     *  @param k  the lag parameter
     */
    def acorr (k: Int = 1): Double = acov (k) / variance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the standard deviation of this vector.
     */
    def stddev: Double = sqrt (variance)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean square (ms) of this vector.
     */
    def ms: Double = normSq / n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the root mean square (rms) of this vector.
     */
    def rms: Double = sqrt (ms)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the skewness of this vector.  Negative skewness indicates the
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
    /** Compute the confidence interval half-width for the given confidence level.
     *  @param p  the confidence level
     */
    def interval (p: Double = .95): Double =
    {
        val df = dim - 1              // degrees of freedom
        if (df < 1) flaw ("interval", "must have at least 2 observations")
        val pp = 1 - (1 - p) / 2.0     // e.g., .95 --> .975 (two tails)
        val t  = studentTInv (pp, df)
        t * stddev / sqrt (dim.toDouble)
    } // interval

} // StatVector class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test the StatVector class.
 */
object StatVectorTest extends App
{
    val x = new StatVector (1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    val y = new StatVector (2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    println ("x           = " + x)
    println ("y           = " + y)
    println ("x.min       = " + x.min ())       // minimum (from VectorD)
    println ("x.max       = " + x.max ())       // maximum (from VectorD)
    println ("x.mean      = " + x.mean)         // mean
    println ("x.median    = " + x.median)       // median
    println ("x.variance  = " + x.variance)     // variance
    println ("x.cov (y)   = " + x.cov (y))      // covariance
    println ("x.corr (y)  = " + x.corr (y))     // correlation
    println ("x.acorr (1) = " + x.acorr (1))    // auto-correlation
    println ("x.stddev    = " + x.stddev)       // standard deviation
    println ("x.ms        = " + x.ms)           // mean square
    println ("x.rms       = " + x.rms)          // root mean square
    println ("x.skew      = " + x.skew)         // skewness
    println ("x.interval  = " + x.interval ())  // confidence interval

} // StatVectorTest object

