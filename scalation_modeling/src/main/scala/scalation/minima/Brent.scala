
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
 *  @version 1.5
 *  @date    Fri Aug  4 12:18:00 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.abs

import scalation.calculus.Differential.{Ⅾ, Δ}
import scalation.math.FunctionS2S

import FunctionSelector._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Brent` class is used to find roots (zeros) for a one-dimensional
 *  (scalar) function 'g'.  Depending on the FunctionSelector, it can find zeros
 *  for derivatives or finite differences, which may indicate optima for function 'g'.
 *  The code is directly translated from the following:
 *  @see math.haifa.ac.il/ronn/NA/NAprogs/brent.java
 *  @param g     the function to find minima of
 *  @param root  find the root for function, derivative or finite difference
 */
class Brent (g: FunctionS2S, root: FunctionSelector = FiniteDifference)
{
    private val EPSILON  = 1E-9                          // a number close to zero
    private val f        = root match {                  // find roots for
                           case Function   => g          // function (zeros)
                           case Derivative => Ⅾ (g) _    // derivative (optima)
                           case _          => Δ (g) _    // finite difference (optima, noise)
                           } // match

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for/find a root within the bounds of 'a_' and 'b_'
     *  @param a_  the lower bound
     *  @param b_  the upper bound
     */
    def solve (a_ : Double,  b_ : Double): Double =
    {
        var (a, b) = (a_, b_)                            // bounds on x
        var c, d = a                                     // old bounds
        var s = 0.0                                      // point between a and b
        var (fa, fb, fc, fs) = (f(a), f(b), f(c), 0.0)   // functional values

        def swap ()
        {
            var t = a; a = b; b = t                      // swap a and b
            t = fa; fa = fb; fb = t                      // swap fa and fb
        } // swap

        if (fa * fb >= 0) return Double.NaN              // need opposite signs

        if (abs (fa) < abs (fb)) swap ()                 // want positive to negative

        var mflag = true                                 // modification flag
        var it    = 0                                    // iteration counter

        while (abs (fb) > EPSILON && abs (b - a) > EPSILON) {
            s = if (fa != fc && fb != fc) a * fb * fc / ((fa - fb) * (fa - fc)) +
                                          b * fa * fc / ((fb - fa) * (fb - fc)) +
                                          c * fa * fb / ((fc - fa) * (fc - fb))
                else b - fb * (b - a) / (fb - fa)

            if ((s < (3 * (a + b) / 4) || s > b) || (mflag && abs (s-b) >= (abs (b-c) / 2)) ||
                                                   (!mflag && abs (s-b) >= (abs (c-d) / 2))) {
                s = (a + b) / 2
                mflag = true
            } else {
                mflag = false
            } // if

            fs = f(s); d = c; c  = b; fc = fb            // update points

            if ((fa * fs) < 0.0) b = s else a = s        // one of bounds inward

            if (abs (fa) < abs (fb)) swap ()             // again want positive to negative
            it += 1
        } // while

        printf ("it = %d, solution x = %10.4f\n", it, b)
        b
    } // solve

} // Brent class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BrentTest` object is used to test the `Brent` class.
 *  > runMain scalation.minima.BrentTest
 */
object BrentTest extends App
{
    def f (x: Double): Double = (x - 4) * (x - 4)

    val br = new Brent (f)

    br.solve (3.0, 5.0)

} // BrentTest object

