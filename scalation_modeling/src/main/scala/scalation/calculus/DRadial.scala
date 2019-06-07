
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.6
 *  @date    Sun Nov 12 12:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see en.wikipedia.org/wiki/Radial_series
 */

package scalation.calculus

import scala.Double.NaN
import scala.math._

import scalation.linalgebra.VectorD
import scalation.math.double_exp

import RadialType._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DRadial` class provides Radial basis functions with derivatives.
 *  Such basis functions are useful are useful in Neural Networks and Support
 *  Vector Classification.
 *  @param centers      a list of centers
 *  @param radialType_  the type of the Radial Basis Function to be used
 *  @param γ_           shape parameter
 *  @param k_           the polynomial power of the radius used in Poly Harmonic Spline
 */
class DRadial (centers: VectorD = VectorD (0.0), radialType_ : RadialType = GAUSSIAN,
               γ_ : Double = 1.0, k_ : Int = 2)
        extends Radial (centers, radialType_, γ_, k_) with DBasisFunction
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The derivative value of the Gaussian Radial Basis Function with radius 'r'
     *  @param n  order of the derivative
     *  @param r  the radius
     */
    private def dGaussian (n: Int, r: Double): Double =
    {
        n match {
            case 0 => gaussian (r)
            case 1 => -2.0 * γ~^2  * r * exp (-((γ*r)~^2))
            case 2 => 2.0 * γ~^2 * exp (-((γ*r)~^2)) * (2.0 * γ~^2 * r~^2 - 1.0)
            case 3 => -4.0 * γ~^4 * r * exp (-((γ*r)~^2)) * (2.0 * γ~^2 * r~^2 - 3.0)
            case _ => { flaw("dGaussian", "Only derivatives up to order 3 are currently supported"); NaN }
        } // match
    } // dGaussian

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The derivative value of the Multi Quadratic Radial Basis Function with radius 'r'
     *  @param n  order of the derivative
     *  @param r  the radius
     */
    private def dMultiQuadratic (n: Int, r: Double): Double =
    {
        n match {
            case 0 => multiQuadratic (r)
            case 1 => γ~^2 * r / sqrt (1.0 + (γ*r)~^2)
            case 2 => γ~^2 / (1.0 + (γ*r)~^2)~^1.5
            case 3 => 3.0 * γ~^2 * r / (1.0 + (γ*r)~^2)~^2.5
            case _ => { flaw("dMultiQuadratic", "Only derivatives up to order 3 are currently supported"); NaN }
        } // match
    } // dMultiQuadratic

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The derivative value of the Inverse Quadratic Radial Basis Function with radius 'r'
     *  @param n  order of the derivative
     *  @param r  the radius
     */
    private def dInverseQuadratic (n: Int, r: Double): Double =
    {
        n match {
            case 0 => inverseQuadratic (r)
            case 1 => -2.0 * γ~^2 * r / (1.0 + (γ*r)~^2)~^2
            case 2 => (6.0 * γ~^4 * r~^2 - 2 * γ~^2) / (1.0 + (γ*r)~^2)~^3
            case 3 => -24.0 * γ~^4 * r * ((γ*r)~^2 - 1.0) / (1.0 + (γ*r)~^2)~^4
            case _ => { flaw("dInverseQuadratic", "Only derivatives up to order 3 are currently supported"); NaN }
        } // match
    } // dInverseQuadratic

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The derivative value of the Inverse Multi Quadratic Radial Basis Function with radius 'r'
     *  @param n  order of the derivative
     *  @param r  the radius
     */
    private def dInverseMultiQuadratic (n: Int, r: Double): Double =
    {
        n match {
            case 0 => inverseMultiQuadratic (r)
            case 1 => -γ~^2 * r / (1.0 + (γ*r)~^2)~^1.5
            case 2 => (2.0 * γ~^4 * r~^2 - γ~^2) / (1.0 + (γ*r)~^2)~^2.5
            case 3 => (9.0 * γ~^4 * r - 6 * γ~^6 * r~^3) / (1.0 + (γ*r)~^2)~^3.5
            case _ => { flaw("dInverseQuadratic", "Only derivatives up to order 3 are currently supported"); NaN }
        } // match
    } // dInverseMultiQuadratic

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The derivative value of the Poly Harmonic Spline Radial Basis Function with radius 'r'
     *  @param n  order of the derivative
     *  @param r  the radius
     *  @see      en.wikipedia.org/wiki/Polyharmonic_spline
     */
    private def dPolyHarmonicSpline (n: Int, r: Double, k: Int = k): Double =
    {
        if      (n == 0) return polyHarmonicSpline (r, k)
        else if (n > 3 || n < 0) {
            flaw("dPolyHarmonicSpline", "Only derivatives up to order 3 are currently supported")
            return NaN
        } // if

        if (r == 0.0) return 0.0

        if (k % 2 == 0) {
            n match {
                case 1 => r~^(k-1) * (k * log(r) + 1)
                case 2 => r~^(k-2) * (k~^2 * log(r) - k * log(r) + 2.0 * k - 1.0)
                case 3 => r~^(k-3) * ((k~^2 - 3*k + 2) * k * log(r) + 3*k~^2 - 6*k + 2)
            } // match
        } else {
            n match {
                case 1 => k * r ~^ (k - 1)
                case 2 => (k - 1) * k * r ~^ (k - 2)
                case 3 => (k - 2) * (k - 1) * k * r ~^ (k - 3)
            } // match
        } // if

    } // polyHarmonicSpline

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The derivative value of the Thin Plate Spline Radial Basis Function with radius 'r'
     *  @param n  order of the derivative
     *  @param r  the radius
     */
    private def dThinPlateSpline (n: Int, r: Double) = dPolyHarmonicSpline(n, r, 2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the nth derivative basis function by calling dnbf(n)(m)(j) only.
     *  @param n  the order of the derivative
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def dnbf (n: Int)(m: Int)(j: Int)(t: Double): Double =
    {
        if (j >= m) { flaw("bf", s"The $j-th radial basis function doesn't exist, IndexOutOfBounds"); return NaN }

        val c = centers(j)
        val r = abs (t - c)      // computing the Euclidean distance between time point t and center c, L2 norm in general

        radialType match {
            case GAUSSIAN                => dGaussian (n, r)
            case MULTI_QUADRATIC         => dMultiQuadratic (n, r)
            case INVERSE_QUADRATIC       => dInverseQuadratic (n, r)
            case INVERSE_MULTI_QUADRATIC => dInverseMultiQuadratic (n, r)
            case POLYHARMONIC_SPLINE     => dPolyHarmonicSpline (n, r)
            case THIN_PLATE_SPLINE       => dThinPlateSpline (n, r)
            case _                       => dGaussian (n, r)
        } // match
    } // dnbf

} // DRadial Class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DRadialTest` object is used to test the `DRadial` class.
 *  > runMain scalation.calculus.DRadialTest
 */
object DRadialTest extends App
{
    import DBasisFunction.plot

    val t = VectorD.range(-300, 300)/100
//  val c = VectorD (-2.8 to 2.8 by 0.4)
    val c = VectorD.range (-7, 8) * 0.4
    val m = c.size

    val rbf = new DRadial(c, GAUSSIAN)
    plot (rbf, m, t)
    rbf.setRadialType (MULTI_QUADRATIC)
    plot (rbf, m, t)
    rbf.setRadialType (INVERSE_QUADRATIC)
    plot (rbf, m, t)
    rbf.setRadialType (INVERSE_MULTI_QUADRATIC)
    plot (rbf, m, t)
    rbf.setRadialType (POLYHARMONIC_SPLINE)
    plot (rbf, m, t)
    rbf.setRadialType (THIN_PLATE_SPLINE)
    plot (rbf, m, t)

} // RadialTest object

