
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael E. Cotterell
 *  @version 1.2
 *  @date    Fri Jul 24 14:35:58 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scala.math.{abs, atan, exp, Pi, pow, sqrt}

import scalation.math.Combinatorics.{rBetaF, rBetaC}
import scalation.math.ExtremeD._
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDF` object contains methods for computing 'F(x)', the Cumulative
 *  Distribution Functions (CDF's) for popular sampling distributions:
 *  StandardNormal, StudentT, ChiSquare and Fisher.
 *  For a given CDF 'F' with argument 'x', compute 'p = F(x)'.
 */
object CDF
       extends Error
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    /** Compute the Cumulative Distribution Function (CDF) for the Normal
     *  distribution using a composite fifth-order Gauss-Legendre quadrature.
     *  @author John D. Cook (Adapted to Scala by Michael E. Cotterell)
     *  @see http://www.johndcook.com/blog/cpp_phi/jam
     *  @see [AS 1965] Abramowitz & Stegun. "Handbook of Mathematical Functions"
     *       (June) (1965) 
     *  @param x   the x coordinate, argument to F(x)
     */  
    def normalCDF (x: Double): Double =
    {
        val a1 =  0.254829592
        val a2 = -0.284496736
        val a3 =  1.421413741
        val a4 = -1.453152027
        val a5 =  1.061405429
        val p  =  0.3275911

        val sign = if (x < 0) -1 else 1
        val y = abs (x) / sqrt (2.0);

        // A&S formula 7.1.26
        val t = 1.0 / (1.0 + p * y);
        val z = 1.0 - (((((a5*t + a4) * t) + a3) * t + a2) * t + a1) * t * exp (-y*y);

        0.5 * (1.0 + sign * z);
    } // normalCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Normal
     *  distribution.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the array of degrees of freedom Array (), not used
     */
    def normalCDF (x: Double, df: Array [Int] = Array ()): Double =
    {
        normalCDF (x)
    } // normalCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for "Student's t"
     *  distribution.
     *  @author Michael Cotterell
     *  @see [JKB 1995] Johnson, Kotz & Balakrishnan "Continuous Univariate 
     *       Distributions" (Volume 2) (2nd Edition) (Chapter 28) (1995) 
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the degrees of freedom (must be > 0)
     */    
    def studentTCDF (x: Double, df: Int = 4): Double =
    {
        if (df <= 0) {
            flaw ("studentTCDF", "parameter df must be strictly positive")
            return NaN
        } // if

        if (df == 1) {                           // Cauchy CDF
            0.5 + (1.0/Pi) * atan (x)
        } else if (df == 2) {                    // Explicit Formula
            0.5 + (x/2.0) * pow (2.0 + x*x, -0.5)
        } else if (df < 2*x*x) {                 // [JKB 1995]
            val z = 0.5 * rBetaF (df / (df + x*x), 0.5*df, 0.5)
            if (x > 0) 1.0 - z else z
        } else if (df < 30) {                    // [JKB 1995]
            val z = 0.5 * rBetaC (x*x / (df + x*x), 0.5, 0.5*df)
            if (x > 0) 1.0 - z else z
        } else {                                 // Ordinary Normal Approximation (ONA)
            normalCDF (x)                  
        } // if
    } // studentTCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for "Student's t"
     *  distribution.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the array of degrees of freedom Array (), one value
     */
    def studentTCDF (x: Double, df: Array [Int]): Double =
    {
        studentTCDF (x, df(0))
    } // studentTCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the ChiSquare
     *  distribution by numerically integrating the ChiSquare probability
     *  density function (pdf).  See Variate.scala.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the degrees of freedom
     */
    def chiSquareCDF (x: Double, df: Int = 4): Double =
    {
        if (x < 0.0) {
            flaw ("chiSquareCDF", "parameter x should be nonnegative")
            return 0.0
        } // if
        if (df <= 0) {
            flaw ("chiSquareCDF", "parameter df must be strictly positive")
            return NaN
        } // if

        val chi  = ChiSquare (df)        // ChiSquare distribution
        val step = 0.0001
        var sum  = 0.0
        var xx   = EPSILON               // small number (machine epsilon)
        var y1   = 0.0
        var y2   = chi.pf (xx)           // pdf for ChiSquare distribution
        while (xx <= x && sum < 1.0) {
            y1  = y2
            xx  += step
            y2  = chi.pf (xx)
            sum += step * (y1 + y2) / 2.0
        } // while
        sum
    } // chiSquareCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the ChiSquare
     *  distribution.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the array of degrees of freedom Array (), one value
     */
    def chiSquareCDF (x: Double, df: Array [Int]): Double =
    {
        chiSquareCDF (x, df(0))
    } // chiSquareCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Fisher (F)
     *  distribution using beta functions.
     *  @param x    the x coordinate, argument to F(x)
     *  @param df1  the degrees of freedom 1
     *  @param df2  the degrees of freedom 2
     */
    def fisherCDF (x: Double, df1: Int, df2: Int): Double =
    {
        if (x < 0.0) {
            flaw ("fisherCDF", "parameter x should be nonnegative")
            return 0.0
        } // if
        if (df1 <= 0 || df2 <= 0) {
            flaw ("fisherCDF", "parameters df1 and df2 must be strictly positive")
            return NaN
        } // if

        rBetaF (df1 * x / ((df1 * x) + df2), df1 / 2.0, df2 / 2.0)
    } // fisherCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Fisher (F)
     *  distribution using beta functions.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the pair of degrees of freedom (df1, df2)
     */
    def fisherCDF (x: Double, df: Tuple2 [Int, Int]): Double =
    {
        fisherCDF (x, df._1, df._2)
    } // fisherCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Fisher (F)
     *  distribution using beta functions.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the array of degrees of freedom Array (df1, df2)
     */
    def fisherCDF (x: Double, df: Array [Int]): Double =
    {
        fisherCDF (x, df(0), df(1))
    } // fisherCDF

} // CDF object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest` trait provides methods for testing the `CDF` object.
 */
trait CDFTest
{
    import CDF._

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given CDF 'ff' over a range of 'x' values for the given degrees
     *  of freedom (df).
     *  @param ff     the CDF F(.)
     *  @param name   the name of CDF F(.)
     *  @param x_min  the minimum of value of x to test
     *  @param x_max  the maximum of value of x to test
     *  @param df     the array giving the degrees of freedom
     */
    def test_df (ff: Distribution, name: String, x_min: Double, x_max: Double, df: Array [Int] = Array ())
    {
        println ("-----------------------------------------------------------")
        println (s"Test the $name function")
        val n = 16
        val dx = (x_max - x_min) / n.toDouble
        for (i <- 0 to n) {
            val x = x_min + i * dx
            println (s"$name ($x, ${df.deep})\t = ${ff(x, df)}")
        } // for
    } // test_df

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given CDF with name 'cdf'
     *  @param cdf  the name of the CDF to test
     */
    def test (cdf: String)
    {
        cdf match {
        case "normalCDF" =>
            test_df (CDF.normalCDF, "normalCDF", -4.0, 4.0)
        case "studentTCDF" =>
            for (df <- 1 to 30)
                test_df (studentTCDF, "studentTCDF", -4.0, 4.0, Array (df))
        case "chiSquareCDF" =>
            for (df <- 1 to 30)
                test_df (chiSquareCDF, "chiSquareCDF", 0.0, 2.0*df, Array (df))
        case _ =>
            for (df1 <- 1 to 10; df2 <- 1 to 10)
                test_df (fisherCDF, "fisherCDF", 0.0, 4.0*(df1/df2.toDouble), Array (df1, df2))
        } // match
    } // test

} // CDFTest trait

 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_Normal` object is used to test the 'normalCDF' method in the
 *  `CDF` object.
 *  > run-main scalation.random.CDFTest_Normal
 */
object CDFTest_Normal extends App with CDFTest { test ("normalCDF") }

 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_StudentT` object is used to test the 'studentTCDF' method in the
 *  `CDF` object.
 *  > run-main scalation.random.CDFTest_StudentT
 */
object CDFTest_StudentT extends App with CDFTest { test ("studentTCDF") }

 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_ChiSquare` object is used to test the 'chiSquareCDF' method in the
 *  `CDF` object.
 *  > run-main scalation.random.CDFTest_ChiSquare
 */
object CDFTest_ChiSquare extends App with CDFTest { test ("chiSquareCDF") }

 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_Fisher` object is used to test the 'fisherCDF' method in the
 *  `CDF` object.
 *  > run-main scalation.random.CDFTest_Fisher
 */
object CDFTest_Fisher extends App with CDFTest { test ("fisherCDF") }

