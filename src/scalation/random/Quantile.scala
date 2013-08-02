
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scala.math.{abs, log, sqrt}

import scalation.math.Combinatorics.rBetaF
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object contains methods to compute inverse Cumulative Distribution
 *  Functions (iCDF's) for the popular sampling distributions:
 *  StandardNormal, StudentT, ChiSquare and Fisher.
 *  For a given CDF F and quantile p, compute x such that the F(x) = p.
 */
object Quantile
       extends Error
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the p-th quantile for the "standard normal distribution" function.
     *  This function returns an approximation of the "inverse" cumulative
     *  standard normal distribution function.  I.e., given p, it returns
     *  an approximation to the x satisfying p = P{Z <= x} where Z is a
     *  random variable from the standard normal distribution.
     *  The algorithm uses a minimax approximation by rational functions
     *  and the result has a relative error whose absolute value is less
     *  than 1.15e-9.
     *  Author:      Peter J. Acklam (Adapted to Scala by John Miller)
     *  (Javascript version by Alankar Misra @ Digital Sutras (alankar@digitalsutras.com))
     *  Time-stamp:  2003-05-05 05:15:14
     *  E-mail:      pjacklam@online.no
     *  WWW URL:     http://home.online.no/~pjacklam
     *  @param p  the p-th quantile, e.g., .95 (95%)
     */
    def normalInv (p: Double = .95): Double =
    {
        if (p < 0 || p > 1) flaw ("normalInv", "parameter p must be in the range [0, 1]")

        // Coefficients in rational approximations
        val a = Array(-3.969683028665376e+01,  2.209460984245205e+02,
                      -2.759285104469687e+02,  1.383577518672690e+02,
                      -3.066479806614716e+01,  2.506628277459239e+00)

        val b = Array(-5.447609879822406e+01,  1.615858368580409e+02,
                      -1.556989798598866e+02,  6.680131188771972e+01,
                      -1.328068155288572e+01)

        val c = Array(-7.784894002430293e-03, -3.223964580411365e-01,
                      -2.400758277161838e+00, -2.549732539343734e+00,
                       4.374664141464968e+00,  2.938163982698783e+00)

        val d = Array (7.784695709041462e-03,  3.224671290700398e-01,
                       2.445134137142996e+00,  3.754408661907416e+00)

        // Define break-points
        val plow  = 0.02425
        val phigh = 1 - plow

        // Rational approximation for lower region:
        if (p < plow) {
             val q  = sqrt(-2*log(p))
             return (((((c(0)*q + c(1))*q + c(2))*q + c(3))*q + c(4))*q + c(5)) /
                        ((((d(0)*q + d(1))*q + d(2))*q + d(3))*q + 1)
        } // if

        // Rational approximation for upper region:
        if (phigh < p) {
             val q  = sqrt(-2*log(1-p))
             return -(((((c(0)*q + c(1))*q + c(2))*q + c(3))*q + c(4))*q + c(5)) /
                         ((((d(0)*q + d(1))*q + d(2))*q + d(3))*q + 1)
        } // if

        // Rational approximation for central region:
        val q = p - 0.5
        val r = q*q
        (((((a(0)*r + a(1))*r + a(2))*r + a(3))*r + a(4))*r + a(5))*q /
            (((((b(0)*r + b(1))*r + b(2))*r + b(3))*r + b(4))*r + 1)
    } // normalInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the p-th quantile for "Student's t distribution" function.
     *  This function returns an approximation of the "inverse" cumulative
     *  Student's t distribution function.  I.e., given p, it returns
     *  an approximation to the x satisfying p = P{T <= x} where T is a
     *  random variable from Student's t distribution.
     *  From K. Pawlikowski (www.cosc.canterbury.ac.nz).
     *  This function computes the upper p-th quantile of the t distribution (the
     *  value of t for which the area under the curve from t to +infinity is equal
     *  to p). It is a transliteration of the 'STUDTP' function given in Appendix C
     *  of "Principles of Discrete Event Simulation", G. S. Fishman, Wiley, 1978.
     *  @param p   the p-th quantile, e.g., 95 (95%)
     *  @param df  the degrees of freedom
     */
    def studentTInv (p: Double = .95, df: Int = 10): Double =
    {
        if (p < 0 || p > 1) flaw ("studentTInv", "parameter p must be in the range [0, 1]")
        if (df <= 0)        flaw ("studentTInv", "parameter df must be positive")

        val z1 = abs (normalInv (p))
        val z2 = z1 * z1

        val h = Array [Double] (
              0.25 * z1 * (z2 + 1.0),
              0.010416667 * z1 * ((5.0 * z2 + 16.0) * z2 + 3.0),
              0.002604167 * z1 * (((3.0 * z2 + 19.0) * z2 + 17.0) * z2 - 15.0),
              0.000010851 * z1 * ((((79.0 * z2 + 776.0) * z2 + 1482.0) * z2 - 1920.0) * z2 - 945.0) )

        var x = 0.0
        for (i <- h.length - 1 to 0 by -1) x = (x + h(i)) / df
        if (p >= 0.5) z1 + x else -(z1 + x)
    } // studentTInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the ChiSquare
     *  distribution by numerically integrating the ChiSquare probability
     *  density function (pdf).  See Variate.scala.
     *  @param x   the x coordinate
     *  @param df  the degrees of freedom
     */
    def chiSquareCDF (x: Double, df: Int = 4): Double =
    {
        if (df <= 0) flaw ("chiSquareCDF", "parameter df must be positive")
        if (x < 0)   flaw ("chiSquareCDF", "parameter x must be nonnegative")

        val chi  = ChiSquare (df)  // ChiSquare distribution
        val step = 0.0001
        var sum  = 0.0
        var xx   = 0.0
        var y1   = 0.0
        var y2   = chi.pf (0.0)     // pdf for ChiSquare distribution
        while (xx <= x && sum < 1.0) {
            y1  = y2
            xx  += step
            y2  = chi.pf (xx)
            sum += step * (y1 + y2) / 2.0
        } // while
        sum
    } // chiSquareCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the p-th quantile for "ChiSquare distribution" function using
     *  bisection search of the CDF.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param df  the degrees of freedom
     */
    def chiSquareInv (p: Double = .95, df: Int = 4): Double =
    {
        if (p < 0 || p > 1)      flaw ("chiSquareInv", "parameter p must be in the range [0, 1]")
        if (df <= 0 || df >= 50) flaw ("chiSquareInv", "parameter df must be in the set {1, 2, ..., 49}")

        var x1   = 0.0            // lower limit
        var x2   = 8.0 * df       // upper limit
        var x    = 0.0            // x coordinate
        var y    = 0.0            // y coordinate
        var cont = true          // continue searching
        while (cont) {
            x = (x1 + x2) / 2.0
            y = chiSquareCDF (x, df)
            // println ("x = " + x + " y = " + y + " p = " + p)
            if (y + .0005 < p)      x1 = x
            else if (y - .0005 > p) x2 = x
            else                    cont = false  // done
        } // while
        x
    } // chiSquareInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Fisher (F)
     *  distribution using beta functions.
     *  @param p    the p-th quantile, e.g., .95 (95%)
     *  @param df1  the degrees of freedom 1
     *  @param df2  the degrees of freedom 2
     */
    def fisherCDF (p: Double, df1: Int, df2: Int): Double =
    {
        rBetaF (df1 * p / ((df1 * p) + df2), df1 / 2.0, df2 / 2.0)
    } // fisherCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Fisher (F)
     *  distribution using beta functions.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param df  the pair of degrees of freedom (df1, df2)
     */
    def fisherCDF (p: Double, df: Tuple2 [Int, Int]): Double =
    {
        fisherCDF (p, df._1, df._2)
    } // fisherCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the p-th quantile for "Fisher (F) distribution" function using
     *  bisection search of the CDF.
     *  @param p    the p-th quantile, e.g., .95 (95%)
     *  @param df1  the degrees of freedom 1
     *  @param df2  the degrees of freedom 2
     */
    def fisherInv (p: Double = .95, df1: Int = 4, df2: Int = 5): Double =
    {
        if (p < 0 || p > 1)       flaw ("fisherInv", "parameter p must be in the range [0, 1]")
        if (df1 <= 0 || df2 <= 0) flaw ("fisherInv", "parameters df1 and df2 must be positive")

        var x1   = 0.0            // lower limit
        var x2   = 1.0E6          // upper limit
        var x    = 0.0            // x coordinate
        var y    = 0.0            // y coordinate
        var cont = true          // continue searching
        while (cont) {
            x = (x1 + x2) / 2.0
            y = fisherCDF (x, df1, df2)
            // println ("x = " + x + " y = " + y + " p = " + p)
            if (y + .0005 < p)      x1 = x
            else if (y - .0005 > p) x2 = x
            else                    cont = false              // done
        } // while
        x
    } // fisherInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the p-th quantile for "Fisher (F) distribution" function.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param df  the pair of degrees of freedom (df1 and df2)
     */
    def fisherInv (p: Double, df: Tuple2 [Int, Int]): Double =
    {
        fisherInv (p, df._1, df._2)
    } // fisherInv

} // Quantile object

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object tests the Quantile object.
 */
object QuantileTest extends App
{
    import Quantile._

    println ("\nTest the normalInv function")
    println ("normalInv (.50)  = " + normalInv (.50))
    println ("normalInv (.90)  = " + normalInv (.90))
    println ("normalInv (.95)  = " + normalInv (.95))
    println ("normalInv (.975) = " + normalInv (.975))
    println ("normalInv (.99)  = " + normalInv (.99))
    println ("normalInv (.995) = " + normalInv (.995))

    println ("\nTest the studentTInv function")
    println ("studentTInv (.50)  = " + studentTInv (.50))
    println ("studentTInv (.90)  = " + studentTInv (.90))
    println ("studentTInv (.95)  = " + studentTInv (.95))
    println ("studentTInv (.975) = " + studentTInv (.975))
    println ("studentTInv (.99)  = " + studentTInv (.99))
    println ("studentTInv (.995) = " + studentTInv (.995))

    val df = 49
    println ("\nTest the chiSquareCDF function")
    for (i <- 0 to 10) {
        println ("chiSquareCDF (" + i + ", df)  = " + chiSquareCDF (i, df))
    } // for

    println ("\nTest the chiSquareInv function")
    println ("chiSquareInv (.50, " + df + ")  = " + chiSquareInv (.50, df))
    println ("chiSquareInv (.90, " + df + ")  = " + chiSquareInv (.90, df))
    println ("chiSquareInv (.95, " + df + ")  = " + chiSquareInv (.95, df))
    println ("chiSquareInv (.975, " + df + ") = " + chiSquareInv (.975, df))
    println ("chiSquareInv (.99, " + df + ")  = " + chiSquareInv (.99, df))
    println ("chiSquareInv (.995, " + df + ") = " + chiSquareInv (.995, df))

    val _df = (49, 11)
    println ("\nTest the chiSquareCDF function")
    for (i <- 0 to 10) {
        println ("fisherCDF (" + i + ", _df)  = " + fisherCDF (i, _df))
    } // for

    println ("\nTest the fisherInv function")
    println ("fisherInv (.50, " + _df + ")  = " + fisherInv (.50, _df))
    println ("fisherInv (.90, " + _df + ")  = " + fisherInv (.90, _df))
    println ("fisherInv (.95, " + _df + ")  = " + fisherInv (.95, _df))
    println ("fisherInv (.975, " + _df + ") = " + fisherInv (.975, _df))
    println ("fisherInv (.99, " + _df + ")  = " + fisherInv (.99, _df))
    println ("fisherInv (.995, " + _df + ") = " + fisherInv (.995, _df))

} // QuantileTest object

