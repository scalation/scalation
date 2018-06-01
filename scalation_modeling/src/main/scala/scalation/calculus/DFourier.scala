
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.5
 *  @date    Sun Nov 12 12:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see https://en.wikipedia.org/wiki/Fourier_series
 */

package scalation.calculus

import scala.Double.NaN
import scala.math.{Pi, cos, sin}
import scalation.linalgebra.VectorD
import scalation.math.double_exp
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DFourier` class provides Fourier basis functions with derivatives.
 *  Such basis functions are useful are useful for fitting periodic data in
 *  Functional Data Analysis.
 *  @see en.wikipedia.org/wiki/Fourier_series
 *  @param w     the fundamental frequency parameter
 *  @param mMax  the number of sin/cos pairs to be used in the basis function
 */
class DFourier (w: Double = 2.0 * Pi, mMax: Int = 4)
      extends Fourier (w, mMax) with DBasisFunction
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain the value of nth derivative of the m-th order 'j'-th basis function at time 't'.
     *  Or alternatively, obtain the nth derivative basis function by calling dnbf(n)(m)(j) only.
     *  Ex: val x = dnbf(n)(m)(j)(t) retrieves the nth derivative value of the j-th basis function at 't'.
     *      val f = dnbf(n)(m)(j)    retrieves the nth derivative of the j-th basis function.
     *  @param n  the order of the derivative
     *  @param m  the order of the basis function
     *  @param j  indicates which basis function
     *  @param t  the time parameter
     */
    def dnbf (n: Int)(m: Int)(j: Int)(t: Double): Double =
    {
        val c  = (j+1)/2 * w                            // constant for the inner parts of the sin/cos functions
        val cn = c~^n                                   // c^n
        val cSin = if (n/2 % 2 == 0)     cn else -cn    // multiplicative constant for derivatives of Sines
        val cCos = if ((n+1)/2 % 2 == 0) cn else -cn    // multiplicative constant for derivatives of Cosines

        if      (j > 2*m)    NaN                        // invalid input, j must be <= 2m
        else if (j == 0)     if (n > 0) 0.0 else 1.0
        else if (j % 2 == 0) if (n % 2 == 0) cSin * sin (c * t) else cSin * cos (c * t)
        else                 if (n % 2 == 0) cCos * cos (c * t) else cCos * sin (c * t)
    } // dnbf

} // DFourier Class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DFourierTest` object is used to test the `DFourier` class.
 *  > runMain scalation.calculus.DFourierTest
 */
object DFourierTest extends App
{
    val m = 1
    val dfour = new DFourier ()
    val t = VectorD(0.0 to 5 by 0.01)

    def x (n: Int, tt: Double) = (0 until 2*m+1).map(j => dfour.dnbf(n)(m)(j)(tt)).sum

    val y   = for (tt <- t) yield x(0, tt)
    val dy  = for (tt <- t) yield x(1, tt)
    val d2y = for (tt <- t) yield x(2, tt)

    new Plot (t, dy, y, "dy vs y")
    new Plot (t, d2y, dy, "d2y vs dy")

} // FourierTest object

