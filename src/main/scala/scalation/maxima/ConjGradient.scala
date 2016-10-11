
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Sep 30 13:37:32 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  @see http://www.neos-guide.org/NEOS/index.php/Nonlinear_Conjugate_Gradient_Method
 */

package scalation.maxima

import scala.math.{abs, max, pow}

import scalation.calculus.Differential.{FunctionV2S, gradient, gradientD}
import scalation.linalgebra.VectorD
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConjGradient` implements the Polak-Ribiere Conjugate Gradient (PR-CG) Algorithm
 *  for solving Non-Linear Programming (NLP) problems.  PR-CG determines a search
 *  direction as a weighted combination of the steepest descent direction (-gradient)
 *  and the previous direction.  The weighting is set by the beta function, which for
 *  this implementation used the Polak-Ribiere technique.
 * 
 *  dir_k = -gradient (x) + beta * dir_k-1 
 *
 *  maximize    f(x)
 *  subject to  g(x) <= 0    [ optionally g(x) == 0 ]
 *
 *  @param f     the objective function to be maximized
 *  @param g     the constraint function to be satisfied, if any
 *  @param ineq  whether the constraint function must satisfy inequality or equality
 */
class ConjGradient (f: FunctionV2S, g: FunctionV2S = null, ineq: Boolean = true)
      extends Error
{
    private val DEBUG    = true              // the debug flag
    private val EPSILON  = 1E-9              // number close to zero
    private val MAX_ITER = 500               // maximum number of iterations
    private val WEIGHT   = 1000.0            // weight on penalty for constraint violation

    private var df: Array [FunctionV2S] = null   // array of partials
    private var pr_cg = true                 // use PR-CG (true) or Steepest-Descent (false)

    type Pair = Tuple2 [VectorD, VectorD]    // pair of vectors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the Steepest-Descent algorithm rather than the default PR-CG algorithm.
     */
    def setSteepest () { pr_cg = false }

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
    /** The objective function f re-scaled by a weighted penalty, if constrained.
     *  @param x  the coordinate values of the current point
     */
    def fg (x: VectorD): Double =
    {
        if (g == null) {                  // unconstrained
            f(x)
        } else {                          // constrained, g(x) <= 0
            val penalty = if (ineq) max (g(x), 0.0) else abs (g(x))
            f(x) * (1.0 - WEIGHT * penalty * penalty)
        } // if
    } // fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact (e.g., 'WolfeLS' or exact (e.g., 'GoldenSectionLS' line search
     *  in the direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x    the current point
     *  @param dir  the direction to move in
     */
    def lineSearch (x: VectorD, dir: VectorD): Double =
    {
        def f_1D (z: Double): Double = fg(x + dir * z)  // create a 1D function
//      val ls = new WolfeLS (f_1D, .0001, .1)          // Wolfe line search (c1 = .0001, c2 = .1)
        val ls = new GoldenSectionLS (f_1D)             // Golden Section line search
        ls.search ()                                    // perform a line search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the following Non-Linear Programming (NLP) problem using PR-CG:
     *  max { f(x) | g(x) <= 0 }.  To use explicit functions for gradient,
     *  replace 'gradient (fg, x._1 + s)' with 'gradientD (df,  x._1 + s)'.
     *  @param x0  the starting point 
     */
    def solve (x0: VectorD): VectorD =
    {
        var x:  Pair = (x0, gradient (fg, x0))         // current (point, gradient)
        var xx: Pair = (null, null)                    // next (point, gradient)
        var s:  VectorD = null                         // step vector
        var dir = x._2                                // initial direction is gradient
        var dir0: VectorD = null                       // keep the previous direction

        for (k <- 1 to MAX_ITER if x._2.normSq > EPSILON) {
            s   = dir * lineSearch (x._1, dir)         // update step vector
            xx  = (x._1 + s, gradient (fg, x._1 + s))  // compute the next point
            dir = -xx._2                               // next search direction using Steepest-Descent
            if (pr_cg && k > 1) {
                dir += dir0 * beta (xx._2, x._2)       // modify search direction using PR-CG
            } // if
            if (DEBUG) println ("solve: (k = " + k + ") move from " + x._1 + " to " + xx._1
                              + " where fg(xx._1) = " + fg(xx._1))
            dir0 = dir                                 // save the current direction
            x = xx                                     // make the next point, the current point
        } // for
        x._1                                           // return the current point
    } // solve

} // ConjGradient class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConjGradientTest` object is used to test the `ConjGradient` class.
 */
object ConjGradientTest extends App
{
    val x0 = new VectorD (2)

    println ("\nMaximize: 10 - (x_0 - 3)^2 - (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = 10.0 - (x(0) - 3.0) * (x(0) - 3.0) - (x(1) - 4.0) * (x(1) - 4.0)
    var solver = new ConjGradient (f)
    var x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))

    println ("\nMaximize: 10 - x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2")
    def g (x: VectorD): Double = 10.0 - pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0)
    solver = new ConjGradient (g)
    x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value g(x) = " + g(x))

} // ConjGradientTest object

