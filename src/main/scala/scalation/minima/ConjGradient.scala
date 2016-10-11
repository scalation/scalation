
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Sep 30 13:37:32 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  @see http://www.neos-guide.org/NEOS/index.php/Nonlinear_Conjugate_Gradient_Method
 */

package scalation.minima

import scala.math.{abs, max, pow}

import scalation.calculus.Differential.{FunctionV2S, gradient, gradientD}
import scalation.linalgebra.VectorD
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConjGradient` class implements the Polak-Ribiere Conjugate Gradient (PR-CG)
 *  Algorithm for solving Non-Linear  Programming (NLP) problems.  PR-CG determines
 *  a search direction as a weighted combination of the steepest descent direction
 *  (-gradient) and the previous direction.  The weighting is set by the beta function,
 *  which for this implementation used the Polak-Ribiere technique.
 * 
 *  dir_k = -gradient (x) + beta * dir_k-1 
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0    [ optionally g(x) == 0 ]
 *
 *  @param f        the objective function to be minimized
 *  @param g        the constraint function to be satisfied, if any
 *  @param ineq     whether the constraint function must satisfy inequality or equality
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 */
class ConjGradient (f: FunctionV2S, g: FunctionV2S = null,
                    ineq: Boolean = true, exactLS: Boolean = true)
      extends Minimizer with Error
{
    private val DEBUG    = true                  // the debug flag
    private val WEIGHT   = 1000.0                // weight on penalty for constraint violation

    private var df: Array [FunctionV2S] = null   // array of partials

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the beta function using the Polak-Ribiere (PR) technique.  The
     *  function determines how much of the prior direction is mixed in with -gradient.
     *  @param gr1  the gradient at the current point
     *  @param gr2  the gradient at the next point
     */
    def beta (gr1: VectorD, gr2: VectorD): Double =
    {
        max (0.0, (gr2 dot (gr2 - gr1)) / (gr1.normSq + EPSILON))    // PR-CG (Polak-Ribiere)
    } // beta

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the partial derivative functions.  If these functions are available,
     *  they are more efficient and more accurate than estimating the values
     *  using difference quotients (the default approach).
     *  @param partials  the array of partial derivative functions
     */
    def setDerivatives (partials: Array [FunctionV2S])
    {
        if (g != null) flaw ("setDerivatives", "only works for unconstrained problems")
        df = partials       // use given functions for partial derivatives
    } // setDerivatives

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function f plus a weighted penalty based on the constraint
     *  function g.
     *  @param x  the coordinate values of the current point
     */
    override def fg (x: VectorD): Double =
    {
        val f_x = f(x)
        if (g == null) {                  // unconstrained
            f_x
        } else {                          // constrained, g(x) <= 0
            val penalty = if (ineq) max (g(x), 0.0) else abs (g(x))
            f_x + abs (f_x) * WEIGHT * penalty * penalty
        } // if
    } // fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact 'GoldenSectionLS' or inexact 'WolfeLS' line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    {
        def f_1D (z: Double): Double = fg(x + dir * z)    // create a 1D function
        val ls = if (exactLS) new GoldenSectionLS (f_1D)  // Golden Section line search
                 else new WolfeLS (f_1D, .0001, .1)       // Wolfe line search (c1 = .0001, c2 = .1)
        ls.search (step)                                  // perform a line search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the PR-CG algorithm.
     *  To use explicit functions for gradient, replace 'gradient (fg, x)' with
     *  'gradientD (df,  x)'.
     *  @param x0     the starting point 
     *  @param step   the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD =
    {
        var x    = x0                                  // current point
        var f_x  = fg(x)                               // objective function at current point
        var y:   VectorD = null                        // next point
        var f_y  = 0.0                                 // objective function at next point
        var dir  = -gradient (fg, x)                   // initial direction is -gradient
        var dir0: VectorD = null                       // keep the previous direction
        var dist = 1.0                                 // distance between current and next point
        var down = true                                // moving down flag

        for (k <- 1 to MAX_ITER if down && dist > toler && dir.normSq > toler) {

            y   = x + dir * lineSearch (x, dir, step)  // determine the next point
            f_y = fg(y)                                // objective function value for next point
            dir0 = dir                                 // save the current direction
            dir = -gradient (fg, y)                    // next search direction using Steepest-Descent
            if (k > 1) dir += dir0 * beta (dir0, dir)  // modify search direction using PR-CG

            if (DEBUG) println ("solve: k = " + k + ", y = " + y + ", f_y = " + f_y + ", dir = " + dir)

            dist = (x - y).normSq                      // calc the distance between current and next point
            down = f_y < f_x                           // still moving down?
            if (down) { x = y; f_x = f_y }             // make the next point, the current point
        } // for
        x                                              // return the current point
    } // solve

} // ConjGradient class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConjGradientTest` object is used to test the `ConjGradient` class.
 */
object ConjGradientTest extends App
{
    val x0 = new VectorD (2)

    println ("\nMinimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    var solver = new ConjGradient (f)
    var x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))

    println ("\nMinimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def g (x: VectorD): Double = pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    solver = new ConjGradient (g)
    x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value g(x) = " + g(x))

} // ConjGradientTest object

