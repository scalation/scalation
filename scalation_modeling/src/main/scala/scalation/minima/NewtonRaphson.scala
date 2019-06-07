 
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Apr  2 18:46:44 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.{abs, sqrt}
import scala.util.control.Breaks.{breakable, break}

import scalation.calculus.Differential.{Ⅾ, Δ}
import scalation.math.FunctionS2S

import FunctionSelector._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NewtonRaphson` class is used to find roots (zeros) for a one-dimensional
 *  (scalar) function 'g'.  Depending on the FunctionSelector, it can find zeros
 *  for derivatives or finite differences, which may indicate optima for function 'g'.
 *  @param g     the function to find roots/optima of
 *  @param root  find the root for function, derivative or finite difference
 */
class NewtonRaphson (g: FunctionS2S, root: FunctionSelector = FiniteDifference)
{
    private val EPSILON  = 1E-9                         // a number close to zero
    private val MAX_ITER = 100                          // the maximum number of iterations
    private val f        = root match {                 // find roots for
                           case Function   => g         // function (zeros)
                           case Derivative => Ⅾ (g) _   // derivative (optima)
                           case _          => Δ (g) _   // finite difference (optima, noise)
                           } // match

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for/find a root close to the starting point/guess 'x0'.  
     *  @param x0  the starting point/guess
     */
    def solve (x0: Double): Double =
    {
        var x    = x0                                   // current point
        var f_x  = -1.0                                 // function value at x
        var df_x = -1.0                                 // derivative value at x

        breakable { for (it <- 0 until MAX_ITER) {
            f_x = f(x)
            if (abs (f_x) < EPSILON) {print (s"it = $it, "); break }  // close to zero => quit
            df_x = Ⅾ (f)(x)               
            if (abs (df_x) < EPSILON) df_x = EPSILON    // derivative is too small
            x -= f_x / df_x                             // subtract the ratio
        }} // for

        printf ("solution x = %10.4f, df = %10.4f\n", x, f(x))
        x
    } // solve

} // NewtonRaphson class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NewtonRaphsonTest` object is used to test the `NewtonRaphson` class.
 *  > runMain scalation.minima.NewtonRaphsonTest
 */
object NewtonRaphsonTest extends App
{
//  def f (x: Double): Double = 2.0 * (x*x - 3.0)
    def f (x: Double): Double = (x - 4) * (x - 4)

    val nr = new NewtonRaphson (f)
    
    nr.solve (0.0)

} // NewtonRaphsonTest object

