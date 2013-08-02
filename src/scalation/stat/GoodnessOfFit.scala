
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Jan 26 22:05:46 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.{floor, round, sqrt}

import scalation.math.DoubleWithExp._
import scalation.random._
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to fit data to probability distibutions.
 *  @param d          the sample data points
 *  @param dmin       the minimum value for d
 *  @param dmax       the maximum value for d
 *  @param intervals  the number of intervals for the data's histogram
 */
class GoodnessOfFit (d: StatVector, dmin: Double, dmax: Double, intervals: Int = 10)
      extends Error
{
    private val EPSILON = 1E-9                                  // number close to zero
    private val n       = d.dim                                 // number of sample data points
    private val ratio   = intervals / (dmax - dmin + EPSILON)   // intervals to data range ratio

    if (n < 5 * intervals) flaw ("constructor", "not enough data to fit distribution")

    private val histo = new Array [Int] (intervals)             // histogram
    for (i <- 0 until n) {
        val j = floor ((d(i) - dmin) * ratio).toInt
        if (0 <= j && j < intervals) histo(j) += 1              // add to count for interval j
        else println ("lost value = " + d(i))
    } // for

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a Chi-square goodness of fit test, matching the histogram of the
     *  given data d with the random variable's probability function pf (pdf).
     *  @param rv  the random variate to test
     */
    def fit (rv: Variate): Boolean =
    {
        println ("-------------------------------------------------------------")
        println ("Test goodness of fit for " + rv.getClass.getSimpleName ())
        println ("-------------------------------------------------------------")

        var x    = 0.0             // x coordinate
        var o    = 0.0             // observed value: height of histogram
        var e    = 0.0             // expected value: pf (x)
        var chi2 = 0.0             // ChiSquare statistic
        var nz   = 0              // number of nonzero intervals

        for (j <- 0 until intervals) {
            x = j / ratio + dmin
            o = histo(j)
            e = round (n * rv.pf (x + .5) / ratio)
            if (e >= 4) { chi2 += (o - e)~^2.0 / e; nz += 1 }         // big enough
            println ("\thisto (" + x + ") = " + o + " : " + e + " ")
        } // for

        nz -= 1                   // degrees of freedom (dof) is one less
        if (nz < 2)  flaw ("fit", "insufficient degrees of freedom")
        if (nz > 49) nz = 49
        val cutoff = Quantile.chiSquareInv (0.95, nz)
        println ("\nchi2 = " + chi2 + " : chi2(0.95, " + nz + ") = " + cutoff)
        chi2 <= cutoff
    } // fit

} // GoodnessOfFit class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the GoodnessOfFit class.
 *  @see http://www.seattlecentral.edu/qelp/sets/057/057.html
 */
object GoodnessOfFitTest extends App
{
    val d = new StatVector (36.0, 37.0, 38.0, 38.0, 39.0, 39.0, 40.0, 40.0, 40.0, 40.0,
                            41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 42.0, 42.0, 42.0, 42.0,
                            42.0, 42.0, 42.0, 43.0, 43.0, 43.0, 43.0, 43.0, 43.0, 43.0,
                            43.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0,
                            45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0,
                            46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0,
                            47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 48.0,
                            48.0, 48.0, 48.0, 48.0, 48.0, 48.0, 48.0, 49.0, 49.0, 49.0,
                            49.0, 49.0, 49.0, 49.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0,
                            51.0, 51.0, 51.0, 51.0, 52.0, 52.0, 53.0, 53.0, 54.0, 55.0)

    d.toggleBias ()              // use unbiased estimators on sample data

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

    val gof = new GoodnessOfFit (d, dmin, dmax)

    val uniform = new Uniform (dmin, dmax)
    println ("fit = " + gof.fit (uniform))
    
    val normal = new Normal (dmu, dsig2)
    println ("fit = " + gof.fit (normal))

} // GoodnessOfFitTest object

