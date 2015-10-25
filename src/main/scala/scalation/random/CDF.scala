
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael E. Cotterell
 *  @version 1.2
 *  @date    Fri Jul 24 14:35:58 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scala.collection.mutable.ArrayBuffer
import scala.math.{abs, atan, exp, Pi, pow, sqrt}

import scalation.linalgebra.VectorD
import scalation.math.Combinatorics.{rBetaF, rBetaC}
import scalation.math.nexp
import scalation.math.ExtremeD._
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDF` object contains methods for computing 'F(x)', the Cumulative
 *  Distribution Functions (CDF's) for popular sampling distributions:
 *  StandardNormal, StudentT, ChiSquare and Fisher, as well as the Uniform
 *  Distribution. 
 *  For a given CDF 'F' with argument 'x', compute 'p = F(x)'.
 */
object CDF
       extends Error
{
    /** Type definition for parameters to a distribution
     */
    type Parameters = Vector [Double]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) 'F(x)' for the
     *  Uniform distribution.
     *  @param x  the x coordinate, argument to F(x)
     *  @param b  the upper endpoint of the uniform distribution 
     *  @param a  the lower endpoint of the uniform distribution 
     */
    def uniformCDF (x: Double, a: Double, b: Double): Double =
    {
        if (a >= b) flaw ("uniformCDF", "requires parameter b > parameter a")
        if (x <= a)     0.0
        else if (x < b) (x - a) / (b - a)
        else            1.0
    } // uniformCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) 'F(x)' for the
     *  Uniform distribution.
     *  @param x   the x coordinate, argument to F(x)
     *  @param pr  parameters giving the endpoints of the uniform distribution 
     */
    def uniformCDF (x: Double, pr: Parameters = null): Double =
    {
        val (a, b) = if (pr == null) (0.0, 1.0) else (pr(0), pr(1))
        uniformCDF (x, a, b)
    } // uniformCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) 'F(x)' for the
     *  Exponentail distribution.
     *  @param x  the x coordinate, argument to F(x)
     *  @param λ  the rate parameter
     */
    def exponentialCDF (x: Double, λ: Double): Double =
    {
        if (λ <= 0.0) flaw ("exponentialCDF", "requires parameter lambda λ > 0")
        if (x > 0) 1.0 - nexp (λ * x) else 0.0
    } // exponentialCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) 'F(x)' for the
     *  Exponentail distribution.
     *  @param x     the x coordinate, argument to F(x)
     *  @param parm  parameter giving the rate 
     */
    def exponentialCDF (x: Double, pr: Parameters = null): Double =
    {
        val λ = if (pr == null) 1.0 else pr(0)
        exponentialCDF (x, λ)
    } // exponentialCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    /** Build an empirical CDF from input data vector 'x'.
     *  Ex: x = (2, 1, 2, 3, 2) -> cdf = ((1, .2), (2, .6), (3, .2))
     *  @param x  the input data vector
     */
    def buildEmpiricalCDF (x: VectorD): Tuple2 [VectorD, VectorD] =
    {
        val zbuf = ArrayBuffer [Double] ()
        val cbuf = ArrayBuffer [Double] ()
        val z = x
        z.sort
        for (i <- z.indices) {
            if (i == 0 || z(i) != z(i-1)) {
               zbuf += z(i)
               cbuf += 1.0
            } else {
               cbuf(cbuf.size - 1) += 1.0
            } // if
        } // for
        (VectorD (zbuf.toSeq), VectorD (cbuf.toSeq).cumulate / x.dim.toDouble)
    } // buildEmpiricalCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) 'F(x)' for the
     *  Empirical distribution 'eCDF'.
     *  @param x     the x coordinate, argument to F(x)
     *  @param eCDF  the Empirical CDF
     */
    def empiricalCDF (x: Double, eCDF: Tuple2 [VectorD, VectorD]): Double =
    {
        if (x < eCDF._1(0)) 0.0
        else if (x < eCDF._1(eCDF._1.dim-1)) eCDF._2 (eCDF._1.indexWhere (_ > x) - 1)
        else 1.0
    } // empiricalCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    /** Compute the Cumulative Distribution Function (CDF) 'F(x)' for the Standard
     *  Normal distribution using the Hart function.  Recoded in Scala from C code
     *  @see stackoverflow.com/questions/2328258/cumulative-normal-distribution-function-in-c-c
     *  which was recoded from VB code.
     *  @see www.codeplanet.eu/files/download/accuratecumnorm.pdf
     *  @param x   the x coordinate, argument to F(x)
     */  
    def normalCDF (x: Double): Double =
    {
        val z = abs (x)
        if (z > 37.0) return 0.0

        val RT2PI = 2.506628274631           // sqrt (4.0* acos (0.0))
        val SPLIT = 7.07106781186547

        val N0 = 220.206867912376
        val N1 = 221.213596169931
        val N2 = 112.079291497871
        val N3 = 33.912866078383
        val N4 = 6.37396220353165
        val N5 = 0.700383064443688
        val N6 = 3.52624965998911e-02
        val M0 = 440.413735824752

        val M1 = 793.826512519948
        val M2 = 637.333633378831
        val M3 = 296.564248779674
        val M4 = 86.7807322029461
        val M5 = 16.064177579207
        val M6 = 1.75566716318264
        val M7 = 8.83883476483184e-02

        val e = exp (-z * z / 2.0)

        val c = if (z < SPLIT) {
            val n = (((((N6*z + N5)*z + N4)*z + N3)*z + N2)*z + N1)*z + N0
            val d = ((((((M7*z + M6)*z + M5)*z + M4)*z + M3)*z + M2)*z + M1)*z + M0
            e * n / d
        } else {
            val f = z + 1.0 / (z + 2.0/(z + 3.0/(z + 4.0/(z + 13.0/20.0))))
            e / (RT2PI * f)
        } // if

        if (x <= 0.0) c else 1.0 - c
    } // normalCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::    
    /** Compute the Cumulative Distribution Function (CDF) for the Standard Normal
     *  distribution using a composite fifth-order Gauss-Legendre quadrature.
     *  @author John D. Cook (Adapted to Scala by Michael E. Cotterell)
     *  @see www.johndcook.com/blog/cpp_phi
     *  @see [AS 1965] Abramowitz & Stegun. "Handbook of Mathematical Functions"
     *       (June) (1965) 
     *  @param x   the x coordinate, argument to F(x)
     *
    def normalCDF (x: Double): Double =
    {
        val a1 =  0.254829592
        val a2 = -0.284496736
        val a3 =  1.421413741
        val a4 = -1.453152027
        val a5 =  1.061405429
        val p  =  0.3275911

        val sign = if (x < 0) -1 else 1
        val y = abs (x) / sqrt (2.0)

        // A&S formula 7.1.26
        val t = 1.0 / (1.0 + p * y)
        val z = 1.0 - (((((a5*t + a4) * t) + a3) * t + a2) * t + a1) * t * exp (-y*y)

        0.5 * (1.0 + sign * z)
    } // normalCDF
     */  

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Normal
     *  distribution.
     *  @param x   the x coordinate, argument to F(x)
     *  @param pr  parameters for the mean and standard deviation
     */
    def normalCDF (x: Double, pr: Parameters = null): Double =
    {
        val (μ, σ) = if (pr == null) (0.0, 1.0) else (pr(0), pr(1))
        if (σ <= 0.0) flaw ("normalCDF", "requires parameter σ > 0")
        normalCDF ((x - μ) / σ)
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
    def studentTCDF (x: Double, df: Int): Double =
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
     *  @param pr  parameter for the degrees of freedom
     */
    def studentTCDF (x: Double, pr: Parameters = null): Double =
    {
        val df = if (pr == null) 9 else pr(0).toInt
        studentTCDF (x, df)
    } // studentTCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the ChiSquare
     *  distribution by numerically integrating the ChiSquare probability
     *  density function (pdf).  See Variate.scala.
     *  @param x   the x coordinate, argument to F(x)
     *  @param df  the degrees of freedom
     */
    def chiSquareCDF (x: Double, df: Int): Double =
    {
        if (x < 0.0) {
            flaw ("chiSquareCDF", "coordinate x should be nonnegative")
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
     *  @param df  parameter for the degrees of freedom
     */
    def chiSquareCDF (x: Double, pr: Parameters = null): Double =
    {
        val df = if (pr == null) 9 else pr(0).toInt
        chiSquareCDF (x, df)
    } // chiSquareCDF

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Cumulative Distribution Function (CDF) for the Fisher (F)
     *  distribution using beta functions.
     *  @param x    the x coordinate, argument to F(x)
     *  @param df1  the degrees of freedom 1 (numerator)
     *  @param df2  the degrees of freedom 2 (denominator)
     */
    def fisherCDF (x: Double, df1: Int, df2: Int): Double =
    {
        if (x < 0.0) {
            flaw ("fisherCDF", "coordinate x should be nonnegative")
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
     *  @param df  parameters for degrees of freedom numerator and denominator
     */
    def fisherCDF (x: Double, pr: Parameters = null): Double =
    {
        val (df1, df2) = if (pr == null) (2, 9) else (pr(0).toInt, pr(1).toInt)
        fisherCDF (x, df1, df2)
    } // fisherCDF

} // CDF object

import CDF._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest` trait provides methods for testing the `CDF` object.
 */
trait CDFTest
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given CDF 'ff' over a range of 'x' values for the given degrees
     *  of freedom (df).
     *  @param ff     the CDF F(.)
     *  @param name   the name of CDF F(.)
     *  @param x_min  the minimum of value of x to test
     *  @param x_max  the maximum of value of x to test
     *  @param pr     parameters for the distribution, e.g., the degrees of freedom
     */
    def test_df (ff: Distribution, name: String, x_min: Double, x_max: Double, pr: Parameters = null)
    {
        println ("-----------------------------------------------------------")
        println (s"Test the $name function")
        val n = 32
        val x = new VectorD (n + 1)
        val p = new VectorD (n + 1)
        val dx = (x_max - x_min) / n.toDouble
        for (i <- 0 to n) {
            x(i) = x_min + i * dx
            p(i) = ff(x(i), pr)
            println (s"$name (${x(i)}, $pr)\t = ${p(i)}")
        } // for
        new Plot (x, p, null, name + ": p = F(x)")
    } // test_df

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the given CDF with name 'cdf'
     *  @param cdf  the name of the CDF to test
     */
    def test (cdf: String)
    {
        cdf match {

        case "uniformCDF" =>
            test_df (uniformCDF, cdf, -0.5, 1.5)

        case "exponentialCDF" =>
            test_df (exponentialCDF, cdf, 0.0, 4.0)

        case "empiricalCDF" =>
            println (s"distribution $cdf currently is not yet implemented")

        case "normalCDF" =>
            test_df (normalCDF, cdf, -4.0, 4.0)

        case "studentTCDF" =>
            for (df <- 1 to 30) test_df (studentTCDF, cdf, -4.0, 4.0, Vector (df))

        case "chiSquareCDF" =>
            for (df <- 1 to 30) test_df (chiSquareCDF, cdf, 0.0, 2.0*df, Vector (df))

        case "fisherCDF" =>
            for (df1 <- 1 to 10; df2 <- 1 to 10)
                test_df (fisherCDF, cdf, 0.0, 4.0*(df1/df2.toDouble), Vector (df1, df2))

        case _ =>
            println (s"distribution $cdf currently is not supported")
        } // match
    } // test

} // CDFTest trait

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_Uniform` object is used to test the 'UniformCDF' method in the
 *  `CDF` object.
 *  > run-main scalation.random.CDFTest_Uniform
 */
object CDFTest_Uniform extends App with CDFTest { test ("uniformCDF") }

 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_Exponential` object is used to test the 'ExponentialCDF' method in the
 *  `CDF` object.
 *  > run-main scalation.random.CDFTest_Exponential
 */
object CDFTest_Exponential extends App with CDFTest { test ("exponentialCDF") }

 

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CDFTest_Empirical` object is used to test the 'buildEmpiricalCDF' method
 *  in the `CDF` object.
 *  > run-main scalation.random.CDFTest_Empirical
 */
object CDFTest_Empirical extends App
{
    val eCDF = buildEmpiricalCDF (VectorD (2.0, 1.0, 2.0, 3.0, 2.0))
    println ("F(x)   = " + eCDF)
    for (i <- 1 to 7) println (s"empiricalCDF (${i/2.0}, eCDF) = ${empiricalCDF (i/2.0, eCDF)}")

} // CDFTest_Empirical
     
 
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

