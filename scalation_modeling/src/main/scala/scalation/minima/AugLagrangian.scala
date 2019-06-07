
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author John Miller
 *  @version 1.6
 *  Fri Feb  8 15:58:37 EST 2019
 *  @see LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.abs
import scala.util.control.Breaks._

import scalation.linalgebra._
import scalation.math.double_exp
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AugLagrangian` class implements the Augmented Lagrangian Method for
 *  solving equality constrained optimization problems.
 *  Minimize objective function 'f' subject to constraint 'h' to find an optimal
 *  solution for 'x'.
 *  <p>
 *      min  f(x)
 *      s.t. h(x) = 0
 *
 *      f = objective function
 *      h = equality contraint
 *      x = solution vector
 *  <p>
 *  Note: the hyper-parameters 'eta' and 'p0' will need to be tuned per problem.
 *  @see `AugLagrangianTest` for how to set up 'f', 'h' and 'grad' functions
 */
object AugLagrangian
{
    type Gradient = (VectoD, Double, Double) => VectoD

    private val DEBUG    = true                         // debug flag
    private val MAX_ITER = 100                          // maximum number of iterations
    private val EPSILON  = 1e-6                         // tolerance
    private val eta      = 0.08                         // learning rate
    private val p0       = 0.25                         // initial penalty for constraint violation

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optimal solution to the equality constrained optimization problem.
     *  @param x     initial guess for solution vector 
     *  @param f     the objective function to be minimized
     *  @param h     the equality constraint
     *  @param grad  the gradient of Lagranian (must be specified by caller)
     */
    def solve (x: VectoD, f: FunctionV_2S, h: FunctionV_2S, grad: Gradient): (VectoD, MatriD) =
    {
        val t  = new MatrixD (MAX_ITER, x.dim)          // storage for x's trajectory (for viz, may remove)
        var p  = p0                                     // initial penalty (p = p0)
        var l  = 0.0                                    // initial value for Lagrange multiplier
        var fx = Double.MaxValue                        // current functional value

        breakable { for (k <- 1 to MAX_ITER) {
            l -= p * h(x)                               // update Lagrange multiplier, comment out for Penalty Method
            x -= grad (x, p, l) * eta                   // move opposite the gradient
            t(k-1) = x.copy                             // save point in trajectory
            val fx2 = f(x)                              // compute new objective function value
            if (DEBUG) println (s"$k: x = $x, f(x) = $fx2, p = $p, l = $l")
            if (abs (fx2 - fx) < EPSILON) break         // stopping rule: little change to objective value
            fx = fx2                                    // make new value the current value
            p += p0                                     // increase the penalty
        }} // breakable for
        (x, t)                                          // return optimal solution and trajectory
    } // solve

} // AugLagrangian object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AugLagrangianTest` object tests the `AugLagrangian` object using a simple
 *  equality constrained optimization problem defined by functions 'f' and 'h'.
 *  Caller must also supply the gradient of the Augmented Lagrangian 'grad'.
 *  > runMain scalation.minima.AugLagrangianTest
 */
object AugLagrangianTest extends App
{
    val MAX_ITER = 30

    // function to optimize
    def f(x: VectoD): Double = (x(0) - 4)~^2 + (x(1) - 2)~^2

    // equality constraint to maintain
    def h(x: VectoD): Double = x(0) - x(1)

    // gradient of the Augmented Lagrangian
    def grad (x: VectoD, p: Double, l: Double): VectoD = 
    {
        VectorD (2 * (x(0) - 4) + p * (x(0) - x(1)) - l,
                 2 * (x(1) - 2) - p * (x(0) - x(1)) + l)
    } // grad

    val x0 = new VectorD (2)                            // initial guess

    val (x, t) = AugLagrangian.solve (x0, f, h, grad)

    println (s"optimal solution x = $x")
    new Plot (t.col(0), t.col(1))

} // AugLagrangianTest object

