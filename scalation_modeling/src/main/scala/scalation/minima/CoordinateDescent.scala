
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Apr 18 11:58:39 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see arxiv.org/pdf/1502.04759.pdf
 */

package scalation.minima

import scala.math.{abs, max, pow}

import scalation.linalgebra.FunctionV2S
import scalation.linalgebra.VectorD
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoordinateDescent` class solves unconstrained Non-Linear Programming (NLP)
 *  problems using the Coordinate Descent algorithm.  Given a function 'f' and a
 *  starting point 'x0', the algorithm picks coordinate directions (cyclically) and
 *  takes steps in the those directions.  The algorithm iterates until it converges.
 *
 *  dir_k = kth coordinate direction
 *
 *  minimize    f(x)
 *
 *  @param f        the vector-to-scalar objective function
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 */
class CoordinateDescent (f: FunctionV2S, exactLS: Boolean = true)
      extends Minimizer with Error
{
    private val DEBUG    = true                 // debug flag
    private val WEIGHT   = 1000.0               // weight on penalty for constraint violation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact 'GoldenSectionLS' or inexact 'WolfeLS' line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    {
        def f_1D (z: Double): Double = f(x + dir * z)         // create a 1D function
        val ls = if (exactLS) new GoldenSectionLS (f_1D)      // Golden Section line search
                 else new WolfeLS (f_1D, .0001, .1)           // Wolfe line search (c1 = .0001, c2 = .1)
        ls.search (step)                                      // perform a line search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the Coordinate Descent
     *  algorithm.
     *  @param x0     the starting point
     *  @param step   the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD =
    {
        val n    = x0.dim
        var x    = x0                                         // current point
        var f_x  = f(x)                                       // objective function at current point
        var y: VectorD = null                                 // next point
        var f_y  = 0.0                                        // objective function at next point
        val dir  = new VectorD (n)                            // adjust direction by cycling thru coordinates
        var dist = 1.0                                        // distance between current and next point
        var down = true                                       // moving down flag

        for (k <- 1 to MAX_ITER if down && dist > toler) {
            
            for (fb <- 1 to -1 by -2; j <- 0 until n) {        // cycle thru coordinates - establish direction
                            
                if (j > 0) dir(j-1) = 0.0
                dir(j) = fb                                   // set direction forward of backward by fb
                y      = x + dir * lineSearch (x, dir, step)  // determine the next point
                f_y    = f(y)                                 // objective function value for next point

                if (DEBUG) println ("solve: k = " + k + ", y = " + y + ", f_y = " + f_y + ", dir = " + dir)

                dist = (x - y).normSq                         // calc the distance between current and next point
                down = f_y < f_x                              // still moving down?
                if (down) { x = y; f_x = f_y }                // make the next point, the current point
            } // for
        } // for
        x                                                     // return the current point
    } // solve

} // CoordinateDescent class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoordinateDescentTest` object is used to test the `CoordinateDescent` class.
 *  > runMain scalation.minima.CoordinateDescentTest
 */
object CoordinateDescentTest extends App
{
    var x0 = VectorD (0.0, 0.0)                     // starting point
    var x: VectorD = null                           // optimal solution

    println ("\nProblem 1: (x_0 - 3)^2 + (x_1 - 4)^2 + 1") 
    def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    var solver = new CoordinateDescent (f)
    x = solver.solve (x0)
    println ("optimal solution = " + x + ", objective value = " + f(x))

    println ("\nMinimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def g (x: VectorD): Double = pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    solver = new CoordinateDescent (g)
    x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value g(x) = " + g(x))

    println ("\nProblem 3: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
    // @see math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
    x0 = VectorD (0.0, 0.0)
    def f3 (x: VectorD): Double = x(0)/4.0 + 5.0*x(0)*x(0) + pow(x(0),4) -
                                  9.0*x(0)*x(0)*x(1) + 3.0*x(1)*x(1) + 2.0*pow(x(1),4)
    solver = new CoordinateDescent (f3)
    x = solver.solve (x0)
    println ("optimal solution = " + x + ", objective value = " + f3(x))

} // CoordinateDescentTest

