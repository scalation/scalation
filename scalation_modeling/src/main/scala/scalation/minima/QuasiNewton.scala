
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Fri Sep 30 13:37:32 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  @see The Superlinear Convergence of a Modified BFGS-Type Method for Unconstrained Optimization 
 *  @see On the Robustness of Conjugate-Gradient Methods and Quasi-Newton Methods
 *  @see Limited Memory BFGS for Nonsmooth Optimization
 *  @see http://en.wikipedia.org/wiki/BFGS_method
 *  @see http://www.personal.psu.edu/cxg286/Math555.pdf
 *  @see http://people.orie.cornell.edu/aslewis/publications/bfgs_inexactLS.pdf
 *  @see http://people.orie.cornell.edu/aslewis/publications/bfgs_exactLS.pdf
 */

package scalation.minima

import scala.math.{abs, max, pow}

import scalation.calculus.Differential.∇
import scalation.linalgebra.FunctionV2S
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra.MatrixD.{eye, outer}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuasiNewton` the class implements the Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  Quasi-Newton Algorithm for solving Non-Linear Programming (NLP) problems.
 *  BFGS determines a search direction by  deflecting the steepest descent direction
 *  vector (opposite the gradient) by *  multiplying it by a matrix that approximates
 *  the inverse Hessian.  Note, this  implementation may be set up to work with the matrix
 *  'b' (approximate Hessian) or directly with the 'binv' matrix (the inverse of 'b').
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0   [ optionally g(x) == 0 ]
 *
 *  @param f        the objective function to be minimized
 *  @param g        the constraint function to be satisfied, if any
 *  @param ineq     whether the constraint is treated as inequality (default) or equality
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 */
class QuasiNewton (f: FunctionV2S, g: FunctionV2S = null,
                   ineq: Boolean = true, exactLS: Boolean = false)
      extends Minimizer with Error
{
    private val DEBUG    = false                            // the debug flag
    private val WEIGHT   = 1000.0                           // weight on penalty for constraint violation

    private var df: Array [FunctionV2S] = null              // array of partials
    private var b: MatrixD    = null                        // approx. Hessian matrix (use b or binv)
    private var binv: MatrixD = null                        // inverse of approx. Hessian matrix
    private var bfgs          = true                        // use BFGS (true) or Gradient Descent (false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the Gradient Descent algorithm rather than the default BFGS algorithm.
     */
    def setSteepest () { bfgs = false }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the 'b' matrix, whose inverse is used to deflect -gradient to a
     *  better direction than steepest descent (-gradient).
     *  @param s  the step vector (next point - current point)
     *  @param y  the difference in the gradients (next - current)
     */
//  def updateB (s: VectorD, y: VectorD)
//  {
//      var sy = s dot y                                    // dot product of s and y
//      if (abs (sy) < TOL) sy = TOL
//      val sb = s * b
//      b += outer (y, y) / sy - outer (sb, sb) / (sb dot s)
//  } // updateB

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the 'binv' matrix, which is used to deflect -gradient to a better
     *  search direction than steepest descent (-gradient).
     *  Compute the 'binv' matrix directly using the Sherman–Morrison formula.
     *  @see http://en.wikipedia.org/wiki/BFGS_method
     *  @param s  the step vector (next point - current point)
     *  @param y  the difference in the gradients (next - current)
     */
    def updateBinv (s: VectorD, y: VectorD)
    {
        var sy = s dot y                                    // dot product of s and y
        if (abs (sy) < TOL) sy = TOL
        val binvy = binv * y
        binv +=  (outer (s, s) * (sy + (binvy dot y))) / (sy * sy) -
                 (outer (binvy, s) + outer (s, binvy)) / sy
    } // updateBinv

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
        if (g == null) {                                    // unconstrained
            f_x
        } else {                                            // constrained, g(x) <= 0
            val penalty = if (ineq) max (g(x), 0.0) else abs (g(x))
            f_x + abs (f_x) * WEIGHT * penalty * penalty
        } // if
    } // fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact 'GoldenSectionLS' or inexact 'WolfeLS' Line Search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  Default to 
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    {
        def f_1D (z: Double): Double = fg(x + dir * z)      // create a 1D function
        val ls = if (exactLS) new GoldenSectionLS (f_1D)    // Golden Section Line Search
                 else new WolfeLS (f_1D)                    // Wolfe line search ((c1 = .0001, c2 = .9)
        ls.search (step)                                    // perform a Line Search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the following Non-Linear Programming (NLP) problem using BFGS:
     *  min { f(x) | g(x) <= 0 }.  To use explicit functions for gradient,
     *  replace 'gradient (fg, x._1 + s)' with 'gradientD (df,  x._1 + s)'.
     *  @param x0     the starting point 
     *  @param step_  the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step_ : Double = STEP, toler: Double = TOL): VectorD =
    {
        if (DEBUG) println ("QuasiNewton.solve: starting at x0 = " + x0)

        var step = step_                                    // set the current step size
        var x    = (x0, ∇ (fg, x0))                         // current (point, gradient)
        var xx:  Pair    = (null, null)                     // next (point, gradient)
        var dir: VectorD = null                             // initial direction is -gradient
        var s:   VectorD = null                             // step vector

        binv = eye (x0.dim)                                 // inverse of approx. Hessian matrix

        if (DEBUG) println ("solve: ||gradient||^2 = " + x._2.normSq)

        var mgn         = 0.0                               // mean gradient normSq
        var diff        = 0.0                               // diff between current and next point
        val diffTol     = toler * toler                     // tolerance for changes in diff
        var count       = 0                                 // number of times mgn stayed roughly same (< diffTol)
        val maxCount    = 10                                // max number of times mgn stayed roughly same => terminate
        val n           = x0.dim                            // size of the parameter vector
        var goodGrad    = true                              // good gradient value flag (not NaN nor infinity)
        var xn: VectorD = null                              // next value for x (point)

        for (k <- 1 to MAX_ITER) {
            if (goodGrad) dir = if (bfgs) -(binv * x._2) else -x._2
            s  = dir * lineSearch (x._1, dir, step)         // update step vector
            xn = x._1 + s                                   // next x point
            if (goodGrad) {
                for (xx_i <- xn if (xx_i.isNaN || xx_i.isInfinite)) return x._1
                diff = (xn - x._1).normSq / n               // measure of distance moved
            } // if
            xx = (xn, ∇ (fg, xn))                           // compute the next point
            mgn = xx._2.normSq / n                          // compute mean gradient normSq
            if (DEBUG) println (s"current mean gradient normSq = $mgn")

            if (mgn.isNaN || mgn.isInfinite) { 
                goodGrad = false                            // gradient blew up
                step /= 2.0                                 // halve the step size 
            } else if (mgn < toler || count > maxCount) {
                return xx._1                                // return when vanished gradient or haven't moved
            } else if (goodGrad) {
                if (diff < diffTol) count += 1              // increment no movement counter
                if (step < step_)   step  *= 1.5            // increase step size by 50%
            } else  {
                goodGrad = true                             // gradient is currently fine
            } // if

            if (goodGrad) {
                if (bfgs)  updateBinv (s, xx._2 - x._2)     // update the deflection matrix 'binv'
                if (DEBUG) println ("solve: (k = " + k + ") move from " + x._1 + " to " + xx._1
                        + " where fg(xx._1) = " + fg(xx._1))
                x = xx                                      // make the next point the current point
            } // if
        } // for
        x._1                                                // return the current point
    } // solve

} // QuasiNewton class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuasiNewtonTest` object is used to test the `QuasiNewton` class.
 *  > runMain scalation.minima.QuasiNewtonTest
 */
object QuasiNewtonTest extends App
{
    def x0 = new VectorD (2)

    println ("\nMinimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    var solver = new QuasiNewton (f)
    var x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value f(x) = " + f(x))

    println ("\nMinimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def g (x: VectorD): Double = pow (x(0), 4.0) + (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + 1.0
    solver = new QuasiNewton (g)
    x = solver.solve (x0)
    println ("optimal solution x = " + x + " with an objective value g(x) = " + g(x))

} // QuasiNewtonTest object

