 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Apr  2 18:46:44 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.abs
import scala.util.control.Breaks.{breakable, break}

import scalation.calculus.Calculus.derivative
import scalation.math.FunctionS2S

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to find roots (zeros) for a one-dimensional (scalar)
 *  function f.  If f is the derivative of some other function g, then this
 *  technique can be used to find optima for g.
 *  Caveat:  Use Conjugate Gradient or Quasi-Newton for complex optimizations.
 *  @param f  the function to find roots of
 */
class NewtonRaphson (f: FunctionS2S)
{
    private val EPSILON  = 1E-9            // a number close to zero
    private val MAX_ITER = 100             // the maximum number of iterations

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for/find a root close to the starting point/guess x0.  
     *  @param x0  the starting point/guess
     */
    def solve (x0: Double): Double =
    {
        var x    = x0                                   // current point
        var f_x  = -1.0                                 // function value at x
        var df_x = -1.0                                 // derivative value at x
        breakable { for (it <- 0 until MAX_ITER) {
            f_x = f(x)
            if (abs (f_x) < EPSILON) break              // close to zero => quit
            df_x = derivative (f, x)               
            if (abs (df_x) < EPSILON) df_x = EPSILON    // derivative is too small
            x -= f_x / df_x                             // subtract the ratio
        }} // for
        printf ("solution x = %10.4f, df = %10.4f\n", x, f(x))
        x
    } // solve

} // NewtonRaphson


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the NewtonRaphson class.
 */
object NewtonRaphsonTest extends App
{
    def f (x: Double): Double = 2.0 * (x - 3.0)

    val nr = new NewtonRaphson (f)
    
    nr.solve (0.0)

} // NewtonRaphsonTest

