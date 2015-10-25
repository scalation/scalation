
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Oct 20 13:28:57 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *-----------------------------------------------------------------------------
 *  GoodnessOfFit_KS:  KS goodness of fit test
 *  @see also `GoodnessOfFit_CS`, `GoodnessOfFit_CS2`
 */

//  U N D E R   D E V E L O P M E N T 

package scalation.stat

import scala.math.{floor, round, sqrt}

import scalation.linalgebra.{MatrixD, SVD, VectorD}
import scalation.math.double_exp
import scalation.math.Combinatorics.fac
import scalation.random.{Distribution, Parameters, Quantile, Variate}
import scalation.random.CDF.buildEmpiricalCDF
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KS` object provides a method to approximate the critical
 *  values for the KS Test.
 */
object GoodnessOfFit_KS
{
    private val DEBUG = true              // debug flag

    private val b = Array (0.37872256037043, 1.30748185078790, 0.08861783849346)

    private val c = Array (-0.37782822932809,  1.67819837908004,
                           -3.02959249450445,  2.80015798142101,
                           -1.39874347510845,  0.40466213484419,
                           -0.06353440854207,  0.00287462087623,
                            0.00069650013110, -0.00011872227037,
                            0.00000575586834)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the critical value for the KS Test using the Lilliefors approximation.
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

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cummulative Distribution Function for the critical values
     *  for the KS Test.
     *  @see sa-ijas.stat.unipd.it/sites/sa-ijas.stat.unipd.it/files/IJAS_3-4_2009_07_Facchinetti.pdf
     *  FIX - it's returning NaN
     *  @param dm  the maximum distance between empirical and theoretical distribution
     *  @param n   the number of data points
     */
    def facchinettiCDF (dm: Double, n: Int): Double =
    {
        // parameters definition
        val nD = n * dm
        val m1 = round (n*dm + 0.5).toInt
        val m2 = round (n - n*dm - 0.5).toInt
        val l1 = round (2.0*n*dm + 0.5).toInt
        val n1 = n * (1 + dm)
        val n2 = n * (1 - dm)

        // B matrix -> bb
        val bb = new MatrixD (2*(n+1), 2*(n+1))

        // B11 matrix
        for (k <- m1+1 to n+1) bb(k, k) = 1.0
        for (r <- m1 to n-1; k <- r+1 to n) {
            bb(k+1, r+1) = fac(n-r)/(fac(k-r)*fac(n-k))*(((k-r)/(n1-r))~^(k-r))*(((n1-k)/(n1-r))~^(n-k))
        } // for

        // B22 matrix
        for (k <- n+2 to n+2+m2) bb(k, k) = 1.0
        for (r <- 0 to m2-1; k <- r+1 to m2) {
            bb(k+n+2, r+n+2) = fac(n-r)/(fac(k-r)*fac(n-k))*(((k-r)/(n2-r))~^(k-r))*(((n2-k)/(n2-r))~^(n-k))
        } // for

        // B21 matrix
        for (r <- m1 to m2; k <- r to m2) {
            bb(k+n+2, r+1) = fac(n-r)/(fac(k-r)*fac(n-k))*(((k-r+2*nD)/(n1-r))~^(k-r))*(((n2-k)/(n1-r))~^(n-k))
        } // for

        // B12 matrix
        for (r <- 0 to n-l1; k <- l1+r to n) {
            bb(k+1,r+n+2) = fac(n-r)/(fac(k-r)*fac(n-k))*(((k-r-2*nD)/(n2-r))~^(k-r))*(((n1-k)/(n2-r))~^(n-k))
        } // for

        // C vector -> cc
        val cc = new VectorD (2*(n+1))
        for (k <- m1 to n) cc(k+1) = fac(n)/(fac(k)*fac(n-k))*(((k-nD)/n)~^k)*(((n1-k)/n)~^(n-k))      // C1 vector
        for (k <- 0 to m2) cc(n+2+k) = fac(n)/(fac(k)*fac(n-k))*(((k+nD)/n)~^k)*(((n2-k)/n)~^(n-k))    // C2 vector

        if (DEBUG) { println ("bb = " + bb); println ("cc = " + cc) }

        // system solution
        val bb_fac = new SVD (bb)                // Use Singular Value Decomposition: pinv (bb)
        val z      = bb_fac.solve (cc)
        if (DEBUG) println ("z = " + z)
        val alpha  = z.sum
        1.0 - alpha                              // return cdf
    } // facchinettiCDF

} // GoodnessOfFit_KS object

import GoodnessOfFit_KS._

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
    def fit (cdf: Distribution, parms: Parameters = null): Boolean =
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
    println ("fit = " + gof1.fit (uniformCDF, Vector (dmin, dmax)))
    
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


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KSTest3` object is used to test the `GoodnessOfFit_KS` object.
 *  @see sa-ijas.stat.unipd.it/sites/sa-ijas.stat.unipd.it/files/IJAS_3-4_2009_07_Facchinetti.pdf
 *  > run-main scalation.stat.GoodnessOfFit_KSTest3
 */
object GoodnessOfFit_KSTest3 extends App
{
     println ("facchinettiCDF = " + facchinettiCDF (0.50945, 5))   // answer = .10

} // GoodnessOfFit_KSTest3

