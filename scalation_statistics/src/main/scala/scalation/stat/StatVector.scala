
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug 26 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import scala.math.{ceil, sqrt}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectorI}
import scalation.linalgebra.MatrixD.eye
import scalation.math.double_exp
import scalation.random.CDF.{normalCDF, studentTCDF}
import scalation.random.Normal
import scalation.random.Quantile.{normalInv, studentTInv}
import scalation.util.banner
import scalation.util.SortingD.imedian

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVector` value class provides methods for computing statistics
 *  on data vectors.  Both maximum likelihood and unbiased estimators are
 *  supported.  Unbiased should only be used on sample (not population) data.
 *  Ex:  It can be used to support the Method of Independent Replications (MIR).
 *  For efficiency, `StatVector` is a value class that enriches the `VectorD`.
 *  The corresponding implicit conversion in the `stat` package object.
 *  @see stackoverflow.com/questions/14861862/how-do-you-enrich-value-classes-without-overhead
 *-----------------------------------------------------------------------------
 *  @param self  the underlying object to be accessed via the 'self' accessor
 */
class StatVector (val self: VectorD)
      extends AnyVal
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'k'-median ('k'-th smallest value) of 'self' vector.  Setting
     *  'k = (dim+1)/2' gives the regular median.
     *  @param k  the type of median (k-th smallest value)
     */
    def median (k: Int = (self.dim+1)/2): Double = imedian (self().toArray, k)

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
     *  If either variance is zero, will result in Not-a-Number (NaN), ruturn
     *  one if the vectors are the same, or -0 (indicating undefined).
     *  @param y  the other vector
     */
    def corr (y: VectorD): Double =
    {
        val c = cov (y) / sqrt (self.variance * y.variance)
        if (c.isNaN) if (self == y) 1.0 else -0.0 else c
    } // corr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population Pearson's correlation of 'self' vector with vector 'y'.
     *  Note:  should only differ from 'corr' due to round-off errors and NaN issue.
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
     *  @see VectorD for variance
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
     *  This method uses the Student t-distribution.
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a standardized version of the vector by subtracting the mean and
     *  dividing by the standard deviation (e.g., Normal -> Standard Normal).
     */
    def standardize: VectorD = (self - self.mean) / stddev

} // StatVector class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVector` companion object extends statistics vector operations to matrices.
 */
object StatVector
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean vector containing the means of each column of matrix 'x'.
     *  @param x  the matrix whose column means are sought
     *
    def mean (x: MatrixD): VectorD = VectorD (for (j <- x.range2) yield x.col(j).mean)
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center the input matrix 'x' to zero mean, column-wise, by subtracting the mean.
     *  @param x     the input matrix to center
     *  @param mu_x  the vector of column means of matrix x
     */
    def center (x: MatriD, mu_x: VectoD): MatriD = x - mu_x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sample covariance matrix for the columns of matrix 'x'.
     *  @param x  the matrix whose column covariances are sought
     */
    def cov (x: MatrixD): MatrixD =
    {
        val z = x - x.mean
        (z.t * z) / (x.dim1.toDouble - 1.0)
    } // cov

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the population covariance matrix for the columns of matrix 'x'.
     *  @param x  the matrix whose column columns covariances are sought
     */
    def pcov (x: MatrixD): MatrixD =
    {
        val z = x - x.mean
        (z.t * z) / x.dim1.toDouble
    } // pcov

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the correlation matrix for the columns of matrix 'x'.
     *  If either variance is zero (column i, column j), will result in Not-a-Number (NaN),
     *  return one if the vectors are the same, or -0 (indicating undefined).
     *  Note:  sample vs. population results in essentailly the same values.
     *  @see the related 'cos' function.
     *  @param x  the matrix whose column-column correlations are sought
     */
    def corr (x: MatrixD): MatrixD =
    {
        val covv = cov (x)                               // sample covariance matrix
        val cor  = eye (covv.dim1)                       // correlation matrix

        for (i <- covv.range1) {
            val var_i = covv (i, i)                      // variance of column i
            for (j <- 0 until i) {
                cor(i, j) = covv (i, j) / sqrt (var_i * covv (j, j))
                if (cor(i, j).isNaN) cor(i, j) = if (x(i) == x(j)) 1.0 else -0.0
                cor(j, i) = cor (i, j)
            } // for
        } // for
        cor
    } // corr

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cosine similarity matrix for the columns of matrix 'x'.
     *  If the vectors are centered, will give the correlation.
     *  @see stats.stackexchange.com/questions/97051/building-the-connection-between-cosine-similarity-and-correlation-in-r
     *  @param x  the matrix whose column-column cosines are sought
     */
    def cos (x: MatrixD): MatrixD =
    {
        val cs = eye (x.dim2)                       // cosine matrix

        for (i <- cs.range1) {
            val y  = x.col (i)                      // ith column vector
            val ny = y.norm
            for (j <- 0 until i) {
                val z  = x.col (j)                  // jth column vector
                val nz = z.norm
                cs(i, j) = (y dot z) / (ny * nz)
                cs(j, i) = cs (i, j)
            } // for
        } // for
        cs
    } // cos

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the first-order auto-regressive correlation matrix for vector
     *  (e.g., time series) 'y'.
     *  @see halweb.uc3m.es/esp/Personal/personas/durban/esp/web/notes/gls.pdf
     *  @param y  the vector whose auto-regressive correlation matrix is sought
     */
    def corrMat (y: VectorD): MatrixD =
    {
        val p  = y.acorr ()                        // first-order auto-correlation
        val cv = eye (y.dim)                       // correlation on main diagonal (md) is 1
        for (i <- cv.range1; j <- 0 until i) {
            val c = p ~^ (i-j)                     // correlation drops with distance from md
            cv(i, j) = c; cv(j, i) = c             // symmetric matrix
        } // for
        cv
    } // corrMat

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the first-order auto-regressive covariance matrix for vector
     *  (e.g., time series) 'y'.
     *  @see halweb.uc3m.es/esp/Personal/personas/durban/esp/web/notes/gls.pdf
     *  @param y  the vector whose auto-regressive covariance matrix is sought
     */
    def covMat (y: VectorD): MatrixD = corrMat (y) * y.variance

} // StatVector object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest` object is used to test the `StatVector` class.
 *  @see www.mathworks.com/help/stats/skewness.html
 *  > runMain scalation.stat.StatVectorTest
 */
object StatVectorTest extends App
{
    val w = VectorD (1.1650, 0.6268, 0.0751, 0.3516, -0.6965)
    val x = VectorD (1.0, 2.0, 3.0, 4.0, 6.0, 5.0)
    val y = VectorD (2.0, 3.0, 4.0, 5.0, 6.0, 7.0)

    println ("x             = " + x)
    println ("y             = " + y)
    println ("x.min         = " + x.min ())         // minimum (from VectorD)
    println ("x.max         = " + x.max ())         // maximum (from VectorD)
    println ("x.mean        = " + x.mean)           // mean (from VectorD)
    println ("x.variance    = " + x.variance)       // variance (from VectorD)
    println ("x.pvariance   = " + x.pvariance)      // population variance (from VectorD)

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

} // StatVectorTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest2` object provides an example of how to use the `StatVector`
 *  class to implement the Method of Independent Replications (MIR) following
 *  a simple two-stage procedure.
 *  > runMain scalation.stat.StatVectorTest2
 */
object StatVectorTest2 extends App
{
    import scalation.random.Random

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

} // StatVectorTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest3` object is used to test the `StatVector` companion object
 *  by computing covariance, correlation and cosine matrices.
 *  @see www.itl.nist.gov/div898/handbook/pmc/section5/pmc541.htm
 *  > runMain scalation.stat.StatVectorTest3
 */
object StatVectorTest3 extends App
{
    import StatVector._

    val x = new MatrixD ((5, 3), 4.0, 2.0, 0.60,
                                 4.2, 2.1, 0.59,
                                 3.9, 2.0, 0.58,
                                 4.3, 2.1, 0.62,
                                 4.1, 2.2, 0.63)

    val mu = x.mean
    val cx = center (x, mu)
                                 
    println ("-" * 60)
    println (s"x           = $x}")                   // x matrix
    println ("-" * 60)
    println (s"x.mean      = $mu")                   // mean vector
    println ("-" * 60)
    println (s"cov (x)     = ${cov (x)}")            // sample covariance matrix
    println ("-" * 60)
    println (s"pcov (x)    = ${pcov (x)}")           // population covariance matrix
    println ("-" * 60)
    println (s"corr (x)    = ${corr (x)}")           // sample correlation matrix
    println ("-" * 60)
    println (s"center (x)  = $cx")                   // mean centered
    println ("-" * 60)
    println (s"cx + mu - x = ${cx + mu - x}")        // difference
    println ("-" * 60)

} // StatVectorTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest4` object is used to test the `StatVector` companion object
 *  by computing covariance, correlation and cosine matrices.
 *  @see www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/corrmatr.htm
 *  > runMain scalation.stat.StatVectorTest4
 */
object StatVectorTest4 extends App
{
    import StatVector._

    val x = new MatrixD ((9, 4), 42.2, 11.2, 31.9, 167.1,
                                 48.6, 10.6, 13.2, 174.4,
                                 42.6, 10.6, 28.7, 160.8,
                                 39.0, 10.4, 26.1, 162.0,
                                 34.7,  9.3, 30.1, 140.8,
                                 44.5, 10.8,  8.5, 174.6,
                                 39.1, 10.7, 24.3, 163.7,
                                 40.1, 10.0, 18.6, 174.5,
                                 45.9, 12.0, 20.4, 185.7)

    val mu = x.mean
    val cx = center (x, mu)

    println ("-" * 60)
    println (s"x           = $x}")                   // x matrix
    println ("-" * 60)
    println (s"x.mean      = $mu")                   // mean vector
    println ("-" * 60)
    println (s"cov (x)     = ${cov (x)}")            // sample covariance matrix
    println ("-" * 60)
    println (s"pcov (x)    = ${pcov (x)}")           // population covariance matrix
    println ("-" * 60)
    println (s"corr (x)    = ${corr (x)}")           // correlation matrix
    println ("-" * 60)
    println (s"cos (x)     = ${cos (x)}")            // cosine matrix
    assert (cos (x - x.mean) == corr (x))
    println ("-" * 60)
    println (s"center (x)  = $cx")                   // mean centered
    println ("-" * 60)
    println (s"cx + mu - x = ${cx + mu - x}")        // difference
    println ("-" * 60)

} // StatVectorTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest5` object is used to test the `StatVector` companion object
 *  by analyzing a time-series vector.
 *  > runMain scalation.stat.StatVectorTest5
 */
object StatVectorTest5 extends App
{
    import scalation.random.RNGStream.ranStream                // randomly pick a stream
    import StatVector._

    val noise = Normal (stream = ranStream)                    // random variate generator
    val y     = new VectorD (10)
    y(0)      = noise.gen
    for (i <- 1 until y.dim) y(i) = 0.5 * y(i-1) + noise.gen   // auto-correlated vector

    println ("y           = " + y)
    println ("y.variance  = " + y.variance)
    println ("y.acorr ()  = " + y.acorr ())
    println ("corrMat (y) = " + corrMat (y))
    println ("covMat (y)  = " + covMat (y))

} // StatVectorTest5


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest6` object is used to test the `StatVector` class by
 *  computing point and interval statistics on a data vector/sample.
 *  @see Data Science Using ScalaTion, Chapter 2, Example Problems
 *  > runMain scalation.stat.StatVectorTest6
 */
object StatVectorTest6 extends App
{
    import scalation.plot.Plot

    val (mu, sig) = (70.0, 8.0)                          // population mean and standard deviation
    val m   = 100                                        // sample size
    val t   = VectorD.range (0, m)                       // time/index vector
    val rvg = Normal (mu, sig * sig)                     // Normal random variate generator

    val sample = new VectorD (m)                         // vector to hold sample
    for (i <- sample.range) sample(i) = rvg.gen          // sample from Normal distribution
    val (mu_, sig_) = (sample.mean, sample.stddev)       // sample mean and standard deviation
    println (s"(mu_, sig_) = ($mu_, $sig_)")
    new Plot (t, sample)
    new Histogram (sample)

    val ihw = 2 * sig / sqrt (m)                         // interval half width
    val interval = (mu_ - ihw, mu_ + ihw)                // confidence interval
    println (s"interval = $interval")

    println (s"1 - F(2) = ${1 - normalCDF (2)}")

} // StatVectorTest6


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVectorTest7` object is used to test the `StatVector` class by 
 *  computing point and interval statistics on a data vector/sample.
 *  It tests the success rates of confidence intervals based on the z- and t-distributions.
 *  @see Data Science Using ScalaTion, Chapter 2, Example Problems
 *  > runMain scalation.stat.StatVectorTest7
 */
object StatVectorTest7 extends App
{
    val (mu, sig) = (70.0, 8.0)                          // population mean and standard deviation
    val iter = 10000                                     // number iterations (samples)
    val m    = 20                                        // sample size
    val t    = VectorD.range (0, m)                      // time/index vector
    val rvg  = Normal (mu, sig * sig)                    // Normal random variate generator
    var count, count2 = 0

    banner ("Test Confidence Interval z- vs. t-distribution")
    for (it <- 1 to iter) {
        val sample = new VectorD (m)                     // vector to hold sample
        for (i <- sample.range) sample(i) = rvg.gen      // sample from Normal distribution
        val (mu_, sig_) = (sample.mean, sample.stddev)   // sample mean and standard deviation
//      println (s"(mu_, sig_) = ($mu_, $sig_)")

        val interv = sample.interval ()                  // interval half width: t-distribution
        val ci     = sample.ci (mu_, interv)             // confidence interval
        val inside = ci._1 <= mu && mu <= ci._2

        val interv2 = sample.interval2 (sig_)            // interval half width: z-distribution
        val ci2     = sample.ci (mu_, interv2)           // confidence interval
        val inside2 = ci2._1 <= mu && mu <= ci2._2

//      println (s"Case $it: Is mu in interval  = $mu in $ci?  $inside")
//      println (s"Case $it: Is mu in interval2 = $mu in $ci2? $inside2")
        if (inside)  count  += 1
        if (inside2) count2 += 1
        Thread.sleep (1)
    } // for
    banner (s"mu was inside the confidence interval  in $count of $iter samples")
    banner (s"mu was inside the confidence interval2 in $count2 of $iter samples")

} // StatVectorTest7

