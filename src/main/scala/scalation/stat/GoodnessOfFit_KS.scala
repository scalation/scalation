
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Oct 20 13:28:57 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *-----------------------------------------------------------------------------
 *  GoodnessOfFit_KS:  KS goodness of fit test
 *  @see also `GoodnessOfFit_CS`, `GoodnessOfFit_CS2`
 */

package scalation.stat

import scala.math.{exp, min, pow, sqrt}
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot
import scalation.random.CDF._
import scalation.random.{Distribution, Parameters}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoodnessOfFit_KS` object provides methods to approximate the critical
 *  values/p-values for the KS Test.
 *  <p>
 *      P(D_n < d)
 *  <p>
 *  @see www.jstatsoft.org/article/view/v008i18/kolmo.pdf
 *  @see sa-ijas.stat.unipd.it/sites/sa-ijas.stat.unipd.it/files/IJAS_3-4_2009_07_Facchinetti.pdf
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
     *  @param d  the maximum distance between empirical and theoretical distribution
     *  @param n  the number of data points
     */
    def lilliefors (d: Double, n: Int): Double =
    {
        val b1n = b(1) + n
        val dm2 = 1.0 / (d * d)
        val a = ( -b1n + sqrt (b1n*b1n - 4.0 * b(2) * (b(0) - dm2)) ) / (2.0 * b(2))
        var aa = a 
        var sum = c(0)
        for (i <- 1 to 10) { sum += c(i) * aa; aa *= a }
        sum
    } // lilliefors

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cummulative Distribution Function (CDF) for 'P(D_n < d)'.
     *  It can used for p-values or critical values for the KS test.
     *  Translated from C code given in paper below.
     *  @see www.jstatsoft.org/article/view/v008i18/kolmo.pdf
     *  @param d  the maximum distance between empirical and theoretical distribution
     *  @param n  the number of data points
     */
    def ksCDF (d: Double, n: Int): Double =
    {
        var s = d*d*n
        if (s > 7.24 || (s > 3.76 && n > 99)) {
            return 1.0 - 2.9 * exp (-(2.000071 + 0.331/sqrt(n) + 1.409/n) * s)
        } // if

        val k  = (n*d).toInt + 1
        val m  = 2*k - 1
        val h  = k - n*d
        val hh = new MatrixD (m, m)
        for (i <- 0 until m; j <- 0 to min (i+1, m-1)) hh(i, j) = 1.0

        for (i <- 0 until m) {
            hh(i, 0)   -= pow (h, i+1)                    // first column
            hh(m-1, i) -= pow (h, m-i)                    // last row
        } // for

        hh(m-1, 0) += (if (2*h - 1 > 0) pow (2*h - 1, m) else 0.0)
        for (i <- 0 until m; j <- 0 until m) {
            if (i-j+1 > 0) for (g <- 1 to i-j+1) hh(i, j) /= g
        } // for

        val eH = 0
        var (qq, eQ) = mPower (hh, eH, n)
        s = qq(k-1, k-1)
        println("Critical Value:" + s)
        for (i <- 1 to n) {
            s = s*i / n
            if (s < 1e-140) { s *= 1e140; eQ -= 140 }
        } // for
        s * pow (10.0, eQ)
    } // ksCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide and conquer algorithm to raise matrix 'a' to the 'n'-th power,
     *  returning 'v = a^n' and 'eV' = final exponential shift.
     *  Translated from C code given in paper below.
     *  @see www.jstatsoft.org/article/view/v008i18/kolmo.pdf
     *  @param a   the matrix whose n-th power is sought
     *  @patam eA  the initial shift on the exponent
     *  @param n   the power to raise the matrix a to
     */
    private def mPower (a: MatrixD, eA: Int, n: Int): Tuple2 [MatrixD, Int] =
    {
        if (n == 1) return (new MatrixD (a), eA)
        var (v, eV) = mPower (a ,eA, n/2)
        val (b, eB) = (v * v, 2 * eV)
        if (n % 2 == 0) { v = b; eV = eB }
        else { v = a * b; eV = eA + eB }
        val mid = a.dim1 / 2
        if (v(mid, mid) > 1e140) { v *= 1e-140; eV += 140 }
        (v, eV)
    } // mPower

} // GoodnessOfFit_KS object

import scalation.stat.GoodnessOfFit_KS._

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

//    private val eCDF = buildEmpiricalCDF (d)                    // Empirical CDF for data vector d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a KS goodness of fit test, matching the CDF of the given data 'd' with
     *  the random variable's Cummulative Distribritution Function (CDF).
     *  @see www.usna.edu/Users/math/dphillip/sa421.f13/chapter02.pdf
     *  @param cdf    the Cummulative Distribritution Function to test
     *  @param parms  the parameters for the distribution
     */
    def fit (cdf: Distribution, parms: Parameters = null): Double =
    {
        println ("-------------------------------------------------------------")
        println ("Test goodness of fit for " + cdf.getClass.getSimpleName ())
        println ("-------------------------------------------------------------")

//        val ey = eCDF._2
//        val y = VectorD ((for (x: Double <- eCDF._1) yield cdf (x, parms)).toSeq)

//        val d_max = (ey - y).mag                          // maximum absolute difference
        val nn = d.dim.toDouble
        d.sort()

        val f = VectorD.range(0, d.dim)/nn
        val cdfDi = d.map((value: Double) => cdf(value, parms))
        println(cdfDi)
        new Plot(VectorD.range(0, cdfDi.dim),cdfDi)
        val x = cdfDi - f
        val x2 = cdfDi.map((value: Double) => 1/n - value)
        println("DMAX:" + Math.max(x.max(), x2.max()))

        val d_max_new = d.indices.map( i => {
            val cdfDi = cdf(d(i), parms)
            Math.max(Math.abs(cdfDi - i/nn) , Math.abs((i +1)/nn - cdfDi))
        }).max

//        if (DEBUG) {
//            println ("x  = " + eCDF._1)                   // x coordinates
//            println ("ey = " + ey)                        // empirical distribution Fe(x_i)
//            println ("y  = " + y)                         // theoretical distribution F(x_i)
//        } // if

/*******
        val critical = lilliefors (d_max, n)              // approximate critical value
        println (s"d_max = $d_max <=? critical = $critical")
        d_max <= critical                                 // false => null hypothesis rejected
*******/

//        println("D_MAX:" + d_max)
        println("D_MAX_NEW:" + d_max_new)
        ksCDF (d_max_new, n)                                   // p-value for the KS Test
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
 *  @see www.jstatsoft.org/article/view/v008i18/kolmo.pdf
 *  > run-main scalation.stat.GoodnessOfFit_KSTest3
 */
object GoodnessOfFit_KSTest3 extends App
{
     println ("ksCDF = " + ksCDF (0.2, 10))     // p-value answer = 0.251281

} // GoodnessOfFit_KSTest3

object GoodnessOfFit_KSTest4 extends App{
    val d = VectorD(-0.62645381,0.18364332,-0.83562861,1.59528080,0.32950777,-0.82046838,0.48742905,0.73832471,0.57578135,-0.30538839,1.51178117,0.38984324,-0.62124058,-2.21469989,1.12493092,-0.04493361,-0.01619026,0.94383621,0.82122120,0.59390132,0.91897737,0.78213630,0.07456498,-1.98935170,0.61982575,-0.05612874,-0.15579551,-1.47075238,-0.47815006,0.41794156,1.35867955,-0.10278773,0.38767161,-0.05380504,-1.37705956,-0.41499456,-0.39428995,-0.05931340,1.10002537,0.76317575,-0.16452360,-0.25336168,0.69696338,0.55666320,-0.68875569,-0.70749516,0.36458196,0.76853292,-0.11234621,0.88110773)


    val gof2 = new GoodnessOfFit_KS (d)
    println ("fit = " + gof2.fit (normalCDF))
}
