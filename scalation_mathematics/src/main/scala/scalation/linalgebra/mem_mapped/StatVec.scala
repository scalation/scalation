
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Jan  8 13:02:32 EST 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  VectorD - min, max, mean and variance
 *  StatVec - covariance, correlation and standard deviation
 *  stat.StatVector - many statistical functions
 */

package scalation.linalgebra.mem_mapped

import scala.math.sqrt

import MatrixD.eye

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVec` value class provides methods for computing statistics
 *  on data vectors.  Both maximum likelihood and unbiased estimators are
 *  supported.  Unbiased should only be used on sample (not population) data.
 *  Ex:  It can be used to support the Method of Independent Replications (MIR).
 *  For efficiency, `StatVec` is a value class that enriches the `VectorD`.
 *  The corresponding implicit conversion in the package object.
 *  @see `stat.StatVector` for more complete statistical functions
 *  @see stackoverflow.com/questions/14861862/how-do-you-enrich-value-classes-without-overhead
 *-----------------------------------------------------------------------------
 *  @param self  the underlying object to be accessed via the 'self' accessor
 */
class StatVec (val self: VectorD)
      extends AnyVal
{
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
    /** Compute Pearson's correlation of 'self' vector with vector 'y'.
     *  @param y  the other vector
     */
    def corr (y: VectorD): Double =
    {
        val c = cov (y) / sqrt (self.variance * y.variance)
        if (c.isNaN) if (self == y) 1.0 else 0.0 else c
    } // corr

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population Pearson's correlation of 'self' vector with vector 'y'.
     *  Note:  should only differ from 'corr' due to round-off errors and NaN issue.
     *  @param y  the other vector
     */
    def pcorr (y: VectorD): Double = pcov (y) / sqrt (self.pvariance * y.pvariance)

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
    /** Produce a standardized version of the vector by subtracting the mean and
     *  dividing by the standard deviation (e.g., Normal -> Standard Normal).
     */
    def standardize: VectorD = (self - self.mean) / stddev

} // StatVec class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatVecTest` object is used to test the `StatVec` class.
 *  > runMain scalation.linalgebra.StatVecTest
 */
object StatVecTest extends App
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

    println ("x.cov (y)     = " + x.cov (y))        // covariance
    println ("x.pcov (y)    = " + x.pcov (y))       // population covariance
    println ("x.corr (y)    = " + x.corr (y))       // correlation (Pearson)
    println ("x.pcorr (y)   = " + x.pcorr (y))      // population correlation (Pearson)
    println ("x.stddev      = " + x.stddev)         // standard deviation
    println ("x.pstddev     = " + x.pstddev)        // population standard deviation

    val z = x.standardize                           // standardized version of vector
    println ("z.mean   = " + z.mean)                // mean (should be 0)
    println ("z.stddev = " + z.stddev)              // standard deviation (should be 1)

} // StatVecTest object

