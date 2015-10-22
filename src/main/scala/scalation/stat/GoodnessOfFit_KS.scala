
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Oct 20 13:28:57 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *-----------------------------------------------------------------------------
 *  GoodnessOfFit_KS:  KS goodness of fit test
 */

//  U N D E R   D E V E L O P M E N T 

package scalation.stat

import scala.math.{floor, round, sqrt}

import scalation.linalgebra.VectorD
import scalation.random.{Distribution, Quantile, Variate}
import scalation.random.CDF.buildEmpiricalCDF
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KS` object provides a method to approximate the critical
 *  values for the KS Test.
 */
object GoodnessOfFit_KS
{
    private val b = Array (0.37872256037043, 1.30748185078790, 0.08861783849346)

    private val c = Array (-0.37782822932809,  1.67819837908004,
                           -3.02959249450445,  2.80015798142101,
                           -1.39874347510845,  0.40466213484419,
                           -0.06353440854207,  0.00287462087623,
                            0.00069650013110, -0.00011872227037,
                            0.00000575586834)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the critical value for a KS Test using the Lilliefors approximation.
     *  Caveat:  assumes alpha = .05 and is only accurate to two digits.
     *  @see www.utdallas.edu/~herve/Abdi-Lillie2007-pretty.pdf
     *  FIX - use a more flexible and accurate approximation.
     *  @param dm  the maximum distance between empirical and theoretical distribution
     *  @param n   the number of data points
     */
    def lilliefors (dm: Double, n: Int): Double =
    {
        val b1n = b(1) + n
        val dm2 = 1.0 / (dm * dm)
        val a = ( -b1n + sqrt (b1n*b1n - 4.0 * b(2) * (b(0) - dm2)) ) / (2.0 * b(2))
        var aa = a 
        var sum = c(0)
        for (i <- 1 to 10) { sum += c(i) * aa; aa *= a }
        sum
    } // lilliefors

} // GoodnessOfFit_KS object

import GoodnessOfFit_KS.lilliefors

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KS` class is used to fit data to probability distibutions.
 *  It uses the Kolmogorov-Srminov goodness of fit test.
 *  @see www.eg.bucknell.edu/~xmeng/Course/CS6337/Note/master/node66.html
 *  Compute the following for each unique data point 'x_i'.
 *  <p>
 *      d_max = max_i {| Fe(x_i) - F(x_i)|}
 *  <p>
 *  where Fe(.) is the empirical distribution and F(.) is the theorectical distribution.
 *  @param d             the sample data points
 *  @param makeStandard  whether to transform the data to zero mean and unit standard deviation
 */
class GoodnessOfFit_KS (private var d: VectorD, makeStandard: Boolean = true)
      extends Error
{
    private val DEBUG  = true                                    // debug flag
    private val n      = d.dim                                   // number of sample data points
    private val sqrt_n = sqrt (n)                                // square root of n

    if (makeStandard) d = d.standardize

    private val eCDF = buildEmpiricalCDF (d)                    // Empirical CDF for data vector d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a KS goodness of fit test, matching the CDF of thegiven data 'd' with
     *  the random variable's Cummulative Distribritution Function (CDF).
     *  @see www.usna.edu/Users/math/dphillip/sa421.f13/chapter02.pdf
     *  @param cdf    the Cummulative Distribritution Function to test
     *  @param parms  the parameters for the distribution
     */
    def fit (cdf: Distribution, parms: Array [Int] = Array ()): Boolean =
    {
        println ("-------------------------------------------------------------")
        println ("Test goodness of fit for " + cdf.getClass.getSimpleName ())
        println ("-------------------------------------------------------------")

        val ey = eCDF._2
        val y = VectorD ((for (x: Double <- eCDF._1) yield cdf (x, parms)).toSeq)

        val d_max = (ey - y).mag                          // maximum absolute difference

        if (DEBUG) {
            println ("x  = " + eCDF._1)                   // x coordinates
            println ("ey = " + ey)                        // empirical distribution Fe(x_i)
            println ("y  = " + y)                         // theoretical distribution F(x_i)
        } // if

        val critical = lilliefors (d_max, n)              // approximate critical value
        println (s"d_max = $d_max <=? critical = $critical")
        d_max <= critical                                 // false => null hypothesis rejected
    } // fit

} // GoodnessOfFit_KS class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KSTest` object is used to test the `GoodnessOfFit_KS` class.
 *  @see www.seattlecentral.edu/qelp/sets/057/057.html
 *  > run-main scalation.stat.GoodnessOfFit_KSTest
 */
object GoodnessOfFit_KSTest extends App
{
    import scalation.random.CDF.{normalCDF, uniformCDF}

    val d = VectorD (36.0, 37.0, 38.0, 38.0, 39.0, 39.0, 40.0, 40.0, 40.0, 40.0,
                     41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 42.0, 42.0, 42.0, 42.0,
                     42.0, 42.0, 42.0, 43.0, 43.0, 43.0, 43.0, 43.0, 43.0, 43.0,
                     43.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0,
                     45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0,
                     46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0,
                     47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 48.0,
                     48.0, 48.0, 48.0, 48.0, 48.0, 48.0, 48.0, 49.0, 49.0, 49.0,
                     49.0, 49.0, 49.0, 49.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0,
                     51.0, 51.0, 51.0, 51.0, 52.0, 52.0, 53.0, 53.0, 54.0, 55.0)

    val dmin  = d.min ()         // the minimum
    val dmax  = d.max ()         // the minimum
    val dmu   = d.mean           // the mean
    val dsig2 = d.variance       // the variance
    val dsig  = sqrt (dsig2)     // the standard deviation

    println ("-------------------------------------------------------------")
    println (" Basic Statistics")
    println ("-------------------------------------------------------------")
    println ("n     = " + d.dim)
    println ("dmin  = " + dmin)
    println ("dmax  = " + dmax)
    println ("dmu   = " + dmu)
    println ("dsig2 = " + dsig2)
    println ("dsig  = " + dsig)
    println ("-------------------------------------------------------------")

    val gof1 = new GoodnessOfFit_KS (d, false)
    println ("fit = " + gof1.fit (uniformCDF, Array (dmin.toInt, dmax.toInt)))
    
    val gof2 = new GoodnessOfFit_KS (d)
    println ("fit = " + gof2.fit (normalCDF))

} // GoodnessOfFit_KSTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KSTest2` object is used to test the `GoodnessOfFit_KS` object.
 *  @see www.utdallas.edu/~herve/Abdi-Lillie2007-pretty.pdf
 *  > run-main scalation.stat.GoodnessOfFit_KSTest2
 */
object GoodnessOfFit_KSTest2 extends App
{
     println ("lilliefors = " + lilliefors (0.1030, 50))

} // GoodnessOfFit_KSTest2

