
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import math.{abs, max, pow}

import scalation.calculus.Calculus.{FunctionV2S, gradient, gradientD}
import scalation.linalgebra.VectorD
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class solves unconstrained Non-Linear Programming (NLP) problems using
 *  the Steepest Descent algorithm.  Given a function 'f' and a starting point 'x',
 *  the algorithm computes the gradient and takes steps in the opposite direction.
 *  The algorithm iterates until it converges.  The class assumes that partial
 *  derivative functions are not availble unless explicitly given via the
 *  setDerivatives method.
 *
 *  dir_k = -gradient (x)
 *
 *  minimize    f(x)
 *
 *  @param f        the vector-to-scalar objective function
 *  @param exactLS  whether to use exact (e.g., GoldenLS)
 *                            or inexact (e.g., WolfeLS) Line Search
 */
class SteepestDescent (f: FunctionV2S, exactLS: Boolean = true)
      extends Minimizer with Error
{
    private val DEBUG    = true                 // debug flag
    private val WEIGHT   = 1000.0               // weight on penalty for constraint violation
    private var given    = false                // default: functions for partials are not given

    private var df: Array [FunctionV2S] = null  // array of partials

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the partial derivative functions.  If these functions are available,
     *  they are more efficient and more accurate than estimating the values
     *  using difference quotients (the default approach).
     *  @param partials  the array of partial derivative functions
     */
    def setDerivatives (partials: Array [FunctionV2S]) { df = partials; given = true } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact (GoldenSectionLS) or inexact (WolfeLS) line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    {
        def f_1D (z: Double): Double = f(x + dir * z)     // create a 1D function
        val ls = if (exactLS) new GoldenSectionLS (f_1D)  // Golden Section line search
                 else new WolfeLS (f_1D, .0001, .1)       // Wolfe line search (c1 = .0001, c2 = .1)
        ls.search (step)                                  // perform a line search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the Steepest Descent
     *  algorithm.
     *  @param x0     the starting point
     *  @param step   the initial step size
     *  @param toler  the tolerence
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD =
    {
        var x    = x0                                  // current point
        var f_x  = f(x)                                // objective function at current point
        var y: VectorD = null                          // next point
        var f_y  = 0.0                                 // objective function at next point
        var dir  = if (given) -gradientD (df, x)       // initial direction is -gradient: use partials
                   else -gradient (f, x)               //                                 estimate gradient
        var dist = 1.0                                 // distance between current and next point
        var down = true                                // moving down flag

        for (k <- 1 to MAX_ITER if down && dist > toler && dir.normSq > toler) {

            y   = x + dir * lineSearch (x, dir, step)  // determine the next point
            f_y = f(y)                                 // objective function value for next point
            dir = if (given) -gradientD (df, y)        // next search direction: use partials
                  else -gradient (f, y)                //                        estimate gradient

            if (DEBUG) println ("solve: k = " + k + ", y = " + y + ", f_y = " + f_y + ", dir = " + dir)

            dist = (x - y).normSq                      // calc the distance between current and next point
            down = f_y < f_x                           // still moving down?
            if (down) { x = y; f_x = f_y }             // make the next point, the current point
        } // for
        x                                              // return the current point
    } // solve

} // SteepestDescent class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SteepestDescent class.
 */
object SteepestDescentTest extends App
{
    var x0 = VectorD (0.0, 0.0)                     // starting point
    var x: VectorD = null                           // optimal solution

    println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
    def f (x: VectorD): Double = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
    val solver = new SteepestDescent (f)
    x = solver.solve (x0)
    println ("optimal solution = " + x + ", objective value = " + f(x))

    println ("\nProblem 2 (with partials): (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
    x0 = VectorD (0.0, 0.0)
    def df_dx0 (x: VectorD): Double = 2.0 * x(0) - 4.0
    def df_dx1 (x: VectorD): Double = 2.0 * x(1) - 6.0
    solver.setDerivatives (Array [FunctionV2S] (df_dx0, df_dx1))
    x = solver.solve (x0)
    println ("optimal solution = " + x + ", objective value = " + f(x))

    println ("\nProblem 3: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
    // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
    x0 = VectorD (0.0, 0.0)
    def f3 (x: VectorD): Double = x(0)/4.0 + 5.0*x(0)*x(0) + pow(x(0),4) -
                                  9.0*x(0)*x(0)*x(1) + 3.0*x(1)*x(1) + 2.0*pow(x(1),4)
    val solver3 = new SteepestDescent (f3)
    x = solver3.solve (x0)
    println ("optimal solution = " + x + ", objective value = " + f3(x))

} // SteepestDescentTest

