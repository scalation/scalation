
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scala.math.{abs, exp, log, Pi, sqrt}

import scalation.linalgebra.VectorD
import scalation.math.{double_exp, cot}
import scalation.math.Combinatorics.rBetaF
import scalation.math.ExtremeD._
import scalation.plot.Plot
import scalation.util.Error

import CDF.{buildEmpiricalCDF, chiSquareCDF, fisherCDF}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Quantile` object contains methods for computing 'Finv', the "inverse"
 *  Cumulative Distribution Functions (iCDF's) for popular sampling distributions:
 *  `StandardNormal`, `StudentT`, `ChiSquare` and `Fisher`.
 *  For a given CDF 'F' and probability/quantile 'p', compute 'x' such that 'F(x) = p'.
 *  The iCDF may be thought of as giving value of 'x' for which the area under the
 *  curve from -infinity to 'x' of the probability density function (pdf) is equal to 'p'.
 */
object Quantile
       extends Error
{
    /** Pi divided by 2
     */
    private val Pi_by_2 = Pi / 2.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the probability 'p' is out of range (giving NaN) or
     *  extreme, either close to 0 (giving -infinity) or 1 (giving +infinity). 
     *  Return (true, special-value) for these cases.
     *  @param p      the p-th quantile, e.g., .95 (95%)
     *  @param x_min  the smallest value in the distribution's domain
     */
    def check (p: Double, x_min: Double = NEGATIVE_INFINITY): Tuple2 [Boolean, Double] =
    {
        if (p < 0.0 || p > 1.0) {
            flaw ("check", "probability parameter p must be in the range [0, 1]")
            return (true, NaN)
        } // if
        if (p =~ 0.0) return (true, x_min)                     // smallest value, defaults to -infinity
        if (p =~ 1.0) return (true, POSITIVE_INFINITY)         // +infinity
        (false, 0.0)                                           // in usual range (0, 1)
    } // check

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for the Uniform distribution function.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param pr  parameters for the end-points of the `Uniform` distribution
     */
    def uniformInv (p: Double, pr: Parameters = null): Double =
    {
        val (a, b) = if (pr == null) (0.0, 1.0) else (pr(0), pr(1))
        a + p * (b - a)
    } // uniformInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for the Exponential distribution function.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param pr  parameter for the rate
     */
    def exponentialInv (p: Double, pr: Parameters = null): Double =
    {
        val λ = if (pr == null) 1.0 else pr(0)
        -log (1.0 - p) / λ
    } // exponentialInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for the Empirical distribution function.
     *  @param p     the p-th quantile, e.g., .95 (95%)
     *  @param eCDF  the empirical CDF
     */
    def empiricalInv (p: Double, eCDF: Tuple2 [VectorD, VectorD]): Double =
    {
        eCDF._1 (eCDF._2.indexWhere (p <= _))
    } // empiricalInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for the Empirical distribution function.
     *  @param p     the p-th quantile, e.g., .95 (95%)
     *  @param data  parameters as data
     */
    def empiricalInv (p: Double, data: Parameters): Double =
    {
        val eCDF = buildEmpiricalCDF (VectorD (data))
        eCDF._1 (eCDF._2.indexWhere (p <= _))
    } // empiricalInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for the "standard normal distribution" function.
     *  @author Peter J. Acklam (Adapted to Scala by John Miller)
     *-------------------------------------------------------------------------
     *  This function returns an approximation of the "inverse" cumulative
     *  standard normal distribution function, i.e., given 'p', it returns an
     *  approximation to the 'x' satisfying
     *  <p>
     *      p = F(x) = P(Z <= x)
     *  <p>
     *  where Z is a random variable from the standard normal distribution.
     *  The algorithm uses a minimax approximation by rational functions and the
     *  result has a relative error whose absolute value is less than 1.15e-9.
     *-------------------------------------------------------------------------
     *  Author:      Peter J. Acklam
     *  Time-stamp:  2002-06-09 18:45:44 +0200
     *  E-mail:      jacklam@math.uio.no (pjacklam@online.no)
     *  WWW URL:     http://www.math.uio.no/~jacklam
     *  @see home.online.no/~pjacklam/notes/invnorm/impl/sprouse/ltqnorm.c
     *-------------------------------------------------------------------------
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param pr  parameter for the distribution (currently not used)
     */
    def normalInv (p: Double = .95, pr: Parameters = null): Double =
    {
        val extreme = check (p)                        // handle extreme cases
        if (extreme._1) return extreme._2

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
    /** Compute the 'p'-th quantile for "Student's t" distribution function.
     *  @author Alan Miller (Adapted to Scala by John Miller)
     *-------------------------------------------------------------------------
     *  This function returns an approximation of the "inverse" cumulative
     *  Student's t distribution function, i.e., given 'p', it returns an
     *  approximation to the x satisfying
     *  <p>
     *      p = F(x) = P(T <= x)
     *  <p>
     *  where T is a random variable from Student's t distribution.
     *-------------------------------------------------------------------------
     *  The function calculates the quantiles of "Student's t" distribution
     *  based on a translation from Algol by Alan Miller, CSIRO Division of
     *  Mathematics & Statistics, Clayton, Victoria 3169, Australia of:
     *
     *  Algorithm 396: Student's t-quantiles by G.W. Hill
     *  Comm. A.C.M., vol.13(10), 619-620, October 1970
     *  @see wp.csiro.au/alanmiller/toms/cacm396.f90
     *-------------------------------------------------------------------------
     *  @param p   the p-th quantile, e.g., 95 (95%)
     *  @param pr  parameter for the degrees of freedom
     */
    def studentTInv (p: Double = .95, pr: Parameters = null): Double =
    {
        val extreme = check (p)                        // handle extreme cases
        if (extreme._1) return extreme._2

        val sign = if (p < 0.5) -1.0 else 1.0

        val df = if (pr == null) 9 else pr(0).toInt
        if (df <= 0) { flaw ("studentTInv", "parameter df must be positive"); return NaN }
        if (df == 1) return -cot (p * Pi)
        if (df == 2) return sign * sqrt (1.0 / (2.0 * p * (1.0 - p)) - 2.0)

        val a = 1.0 / (df - 0.5)
        val b = 48.0 / (a * a)
        var c = ((20700.0 * a / b - 98.0) * a - 16.0) * a + 96.36
        val d = ((94.5 / (b + c) - 3.0) / b + 1.0) * sqrt (a * Pi_by_2) * df
        var x = 2.0 * d * p
        var y = x ~^ (2.0 / df)

        if (y > 0.05 + a) {
            x = normalInv (p)           // asymptotic inverse expansion about normal
            y = x * x
            if (df < 5.0) c += 0.3 * (df - 4.5) * (x + 0.6)
            c = (((0.05 * d * x - 5.0) * x - 7.0) * x - 2.0) * x + b + c
            y = (((((0.4 * y + 6.3) * y + 36.0) * y + 94.5) / c - y  - 3.0) / b + 1.0) * x
            y = a * y * y
            y = if (y > 0.002) exp (y) - 1.0 else 0.5 * y * y + y
        } else {
            y = ((1.0 / (((df + 6.0) / (df * y) - 0.089 * d - 0.822) * (df + 2.0) * 3.0) +
                0.5 / (df + 4.0))  * y - 1.0) * (df + 1.0) / (df + 2.0) + 1.0 / y
        } // if
        sign * sqrt (df * y)
    } // studentTInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for "Student's t" distribution function.
     *  This algorithm is less accurate than the one above.
     *-------------------------------------------------------------------------
     *  It is a transliteration of the 'STUDTP' function given in Appendix C
     *  @see "Principles of Discrete Event Simulation", G. S. Fishman, Wiley, 1978.
     *-------------------------------------------------------------------------
     *  @param p   the p-th quantile, e.g., 95 (95%)
     *  @param pr  parameter for the degrees of freedom
     */
    def studentTInv2 (p: Double = .95, pr: Parameters = null): Double =
    {
        val extreme = check (p)                        // handle extreme cases
        if (extreme._1) return extreme._2

        val sign = if (p < 0.5) -1.0 else 1.0

        val df = if (pr == null) 9 else pr(0).toInt
        if (df <= 0) { flaw ("studentTInv", "parameter df must be strictly positive"); return NaN }
        if (df == 1) return -cot (p * Pi)
        if (df == 2) return sign * sqrt (1.0 / (2.0 * p * (1.0 - p)) - 2.0)

        val z1 = abs (normalInv (p))
        val z2 = z1 * z1

        val h = Array [Double] (
              0.25 * z1 * (z2 + 1.0),
              0.010416667 * z1 * ((5.0 * z2 + 16.0) * z2 + 3.0),
              0.002604167 * z1 * (((3.0 * z2 + 19.0) * z2 + 17.0) * z2 - 15.0),
              0.000010851 * z1 * ((((79.0 * z2 + 776.0) * z2 + 1482.0) * z2 - 1920.0) * z2 - 945.0) )

        var x = 0.0
        for (i <- h.length - 1 to 0 by -1) x = (x + h(i)) / df.toDouble
        if (p >= 0.5) z1 + x else -(z1 + x)
    } // studentTInv2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for "Student's t" distribution function.
     *  @param p   the p-th quantile, e.g., 95 (95%)
     *  @param df  the degrees of freedom
     */
    def studentTInv (p: Double, df: Int): Double =
    {
        studentTInv (p, Vector (df.toDouble))
    } // studentTInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for "ChiSquare distribution" function using
     *  bisection search of the CDF.
     *  FIX: need a faster algorithm
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param pr  parameter for the degrees of freedom
     */
    def chiSquareInv (p: Double = .95, pr: Parameters = null): Double =
    {
        val extreme = check (p, 0.0)                   // handle extreme cases
        if (extreme._1) return extreme._2

        val df = if (pr == null) 9 else pr(0).toInt
        if (df <= 0 || df >= 50) {
            flaw ("chiSquareInv", "parameter df must be in the set {1, 2, ..., 49}")
            return NaN
        } // if

        var x1   = 0.0           // lower limit
        var x2   = 8.0 * df      // upper limit
        var x    = 0.0           // x coordinate
        var y    = 0.0           // y coordinate
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
    /** Compute the 'p'-th quantile for "ChiSquare distribution" function.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param df  the degrees of freedom
     */
    def chiSquareInv (p: Double, df: Int): Double =
    {
        chiSquareInv (p, Vector (df.toDouble))
    } // chiSquareInv

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'p'-th quantile for "Fisher (F) distribution" function using
     *  bisection search of the CDF.
     *  FIX: need a faster algorithm
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param pr  parameters for the degrees of freedom (numerator, denominator)
     */
    def fisherInv (p: Double = .95, pr: Parameters = null): Double =
    {
        val extreme = check (p, 0.0)                   // handle extreme cases
        if (extreme._1) return extreme._2

        val df1 = if (pr == null) 2 else pr(0).toInt          // degrees of freedom 1
        val df2 = if (pr == null) 9 else pr(1).toInt          // degrees of freedom 2
        if (df1 <= 0 || df2 <= 0) {
            flaw ("fisherInv", "parameters df1 and df2 must be strictly positive")
            return NaN
        } // if

        var x1   = 0.0            // lower limit
        var x2   = 1.0E6          // upper limit
        var x    = 0.0            // x coordinate
        var y    = 0.0            // y coordinate
        var cont = true           // continue searching
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
    /** Compute the 'p'-th quantile for "Fisher (F) distribution" function.
     *  @param p   the p-th quantile, e.g., .95 (95%)
     *  @param df  the pair of degrees of freedom ('df1' and 'df2')
     */
    def fisherInv (p: Double, df: Tuple2 [Int, Int]): Double =
    {
        fisherInv (p, Vector (df._1.toDouble, df._2.toDouble))
    } // fisherInv

} // Quantile object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest` trait provides methods for testing the `Quantile` object.
 */
trait QuantileTest
{
    import Quantile._

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given iCDF 'fi' over a range of 'p' values for the given parameters
     *  e.g., degrees of freedom 'df'.
     *  @param fi     the iCDF 'Finv(.)'
     *  @param name   the name of iCDF 'Finv(.)'
     *  @param pr     the parameters for the distribution, e.g., degrees of freedom
     */
    def test_df (fi: Distribution, name: String, pr: Parameters = null)
    {
        println ("-----------------------------------------------------------")
        println (s"Test the $name function")
        val n = 40
        val p = new VectorD (n)
        val x = new VectorD (n)
        for (i <- 1 until n) {
            p(i) = i / n.toDouble
            x(i) = fi(p(i), pr)
            println (s"$name (${p(i)}, $pr)\t = ${x(i)}")
        } // for
        new Plot (p, x, null, name + ": x = Finv(p)")
    } // test_df

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given iCDF with name 'icdf'
     *  @param icdf  the name of the inverse CDF to test
     */
    def test (icdf: String)
    {
        icdf match {

        case "uniformInv" =>
            test_df (uniformInv, icdf)

        case "exponentialInv" =>
            test_df (exponentialInv, icdf)

        case "empiricalInv" =>
            test_df (empiricalInv, icdf, Vector (2.0, 1.0, 2.0, 3.0, 2.0))

        case "normalInv" =>
            test_df (normalInv, icdf)

        case "studentTInv" =>
            for (df <- 1 to 30) test_df (studentTInv, icdf, Vector (df))

        case "chiSquareInv" =>
            for (df <- 1 to 30) test_df (chiSquareInv, icdf, Vector (df))

        case "fisherInv" =>
            for (df1 <- 1 to 10; df2 <- 1 to 10)
                test_df (fisherInv, icdf, Vector (df1, df2))

        case _ =>
            println (s"distribution $icdf currently is not supported")
        } // match
    } // test

} // QuantileTest trait


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_Uniform` object is used to test the 'uniformInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_Uniform
 */
object QuantileTest_Uniform extends App with QuantileTest { test ("uniformInv") }


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_Exponential` object is used to test the 'exponentialInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_Exponential
 */
object QuantileTest_Exponential extends App with QuantileTest { test ("exponentialInv") }


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_Empirical` object is used to test the 'empiricalInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_Empirical
 */
object QuantileTest_Empirical extends App with QuantileTest { test ("empiricalInv") }


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_Normal` object is used to test the 'normalInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_Normal
 */
object QuantileTest_Normal extends App with QuantileTest { test ("normalInv") }


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_StudentT` object is used to test the 'studentTInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_StudentT
 */
object QuantileTest_StudentT extends App with QuantileTest { test ("studentTInv") }


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_ChiSquare` object is used to test the 'chiSquareInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_ChiSquare
 */
object QuantileTest_ChiSquare extends App with QuantileTest { test ("chiSquareInv") }


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuantileTest_Fisher` object is used to test the 'fisherInv' method
 *  in the `Quantile` object.
 *  > run-main scalation.random.QuantileTest_Fisher
 */
object QuantileTest_Fisher extends App with QuantileTest { test ("fisherInv") }

