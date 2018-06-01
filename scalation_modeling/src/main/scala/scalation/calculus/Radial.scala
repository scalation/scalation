
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.5
 *  @date    Wed Nov 15 12:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see en.wikipedia.org/wiki/Radial_basis_function
 */

package scalation.calculus

import scala.Double.NaN
import scala.math.{abs, exp, log, sqrt}

import scalation.linalgebra.VectorD
import scalation.math.double_exp
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Enumerations defining various Basis Functions to be used
 */
object RadialType extends Enumeration
{
    type RadialType = Value
    val GAUSSIAN, MULTI_QUADRATIC, INVERSE_QUADRATIC, INVERSE_MULTI_QUADRATIC,
    POLYHARMONIC_SPLINE, THIN_PLATE_SPLINE = Value
} // SmoothingMethod

import RadialType._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Radial` class provides Radial basis functions. Such basis functions
 *  are useful are useful in Neural Networks and Support Vector Classification.
 *-----------------------------------------------------------------------------
 *  @param centers      a list of centers
 *  @param radialType   the type of the Radial Basis Function to be used
 *  @param γ            shape parameter
 *  @param k            the polynomial power of the radius used in Poly Harmonic Spline
 */
class Radial (centers: VectorD = VectorD (0.0), protected var radialType: RadialType = GAUSSIAN,
              protected var γ: Double = 1.0, protected var k: Int = 2)
      extends BasisFunction with Error
{
    private val DEBUG = true                                // debug flag
    private val m     = centers.size                        // the order of the radial basis functions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Change the type of the Raidal Basis Function
     *  @param rt  the new Radial Basis Function Type to change to
     */
    def setRadialType (rt: RadialType) = radialType = rt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Change the type of the Gamma parameter
     *  @param gamma  the gamma parameter to change to
     */
    def setGamma (gamma: Double) = γ = gamma

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Change the type of the Gamma parameter
     *  @param k_  the gamma parameter to change to
     */
    def setK (k_ : Int) =
    {
        if (k_ < 1) flaw ("setK", "k must be >= 1")
        else        k = k_
    } // setK

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The value of the Gaussian Radial Basis Function with radius 'r'
     *  @param r  the radius
     */
    protected def gaussian (r: Double) = exp (-((γ*r)~^2))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The value of the Multi Quadratic Radial Basis Function with radius 'r'
     *  @param r  the radius
     */
    protected def multiQuadratic (r: Double) = sqrt (1.0 + (γ*r)~^2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The value of the Inverse Quadratic Radial Basis Function with radius 'r'
     *  @param r  the radius
     */
    protected def inverseQuadratic (r: Double) = 1.0 / (1.0 + (γ*r)~^2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The value of the Inverse Multi Quadratic Radial Basis Function with radius 'r'
     *  @param r  the radius
     */
    protected def inverseMultiQuadratic (r: Double) = 1.0 / sqrt (1.0 + (γ*r)~^2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The value of the Poly Harmonic Spline Radial Basis Function with radius 'r'
     *  @param r  the radius
     *  @see      en.wikipedia.org/wiki/Polyharmonic_spline
     */
    protected def polyHarmonicSpline (r: Double, k: Int = k): Double =
    {
        if (r == 0.0) return 0.0
        if (k % 2 == 0) r~^k * log (r)
        else            r~^k
    } // polyHarmonicSpline

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The value of the Thin Plate Spline Radial Basis Function with radius 'r'
     *  @param r  the radius
     */
    protected def thinPlateSpline (r: Double) = polyHarmonicSpline(r, 2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the basis function by calling bf(m)(j) only.
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def bf (m: Int = m)(j: Int)(t: Double): Double =
    {
        if (j >= m) { flaw("bf", s"The $j-th radial basis function doesn't exist, IndexOutOfBounds"); return NaN }

        val c = centers(j)
        val r = abs (t - c)      // computing the Euclidean distance between time point t and center c, L2 norm in general

        radialType match {
            case GAUSSIAN                => gaussian (r)
            case MULTI_QUADRATIC         => multiQuadratic (r)
            case INVERSE_QUADRATIC       => inverseQuadratic (r)
            case INVERSE_MULTI_QUADRATIC => inverseMultiQuadratic (r)
            case POLYHARMONIC_SPLINE     => polyHarmonicSpline (r)
            case THIN_PLATE_SPLINE       => thinPlateSpline (r)
            case _                       => gaussian (r)
        } // match
    } // bf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of Radial basis functions for a specified order
     *  @param m  the order of the basis function
     */
    def size (m: Int = m) = m

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The range for the Radial basis functions
     *  @param m  the order of the spline
     */
    def range (m: Int = m) = 0 until m

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieves the order of the this B_Spline
     */
    def getOrder = m

} // Radial class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RadialTest` object is used to test the `Radial` class.
 *  > runMain scalation.calculus.RadialTest
 */
object RadialTest extends App
{
    import BasisFunction.plot

    val t = VectorD.range (-300, 300)/100
    val c = VectorD (-2.8 to 2.8 by 0.4)
    val m = c.size

    val rbf = new Radial (c, GAUSSIAN)
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

