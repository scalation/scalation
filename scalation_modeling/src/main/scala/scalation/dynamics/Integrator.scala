
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Jan 30 13:19:22 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.dynamics

import scalation.linalgebra.VectorD
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Derivatives` object is used to define types of time derivative functions.
 */
object Derivatives
{
    /** Function type for derivative functions: f (t, y) where y is a scalar
     */
    type Derivative = (Double, Double) => Double

    /** Function type for derivative functions: f (t, y) where y is a vector
     */
    type DerivativeV = (Double, VectorD) => Double

} // Derivatives


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Integrator` trait provides a template for writing numerical integrators
 *  (e.g., Runge-Kutta 'RK4' or Dormand-Prince 'DOPRI') to produce trajectories for
 *  first-order Ordinary Differential Equations 'ODE's.  The ODE is of the form:
 *  <p>
 *      d/dt y(t) = f(t, y)  with initial condition y0 = y(t0)
 *  <p>
 *  If 'f' is a linear function of the form 'a(t) * y(t) + b(t)', then the ODE is
 *  linear, if 'a(t) = a' (i.e., a constant) the ODE has constant coefficients and
 *  if 'b(t) = 0' the ODE is homogeneous.  Note this package provides a solver (not
 *  an integrator) as an option for linear, constant coefficient, homogeneous,
 *  first-order ODE.
 *  @see scalation.dynamics.LinearDiffEq.scala
 */
trait Integrator
      extends Error
{
    import Derivatives.{Derivative, DerivativeV}

    /** The default step size for the t dimension
     */
    protected val defaultStepSize = .01

    /** Estimate of the error in calculating y
     */
    protected var error = 0.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use numerical integration to compute the trajectory of an unknown, time-
     *  dependent function y(t) governed by a first-order ODE of the form y(t)' = f(t, y),
     *  i.e., the time derivative of y(t) equals f(t, y).  The derivative function
     *  f(t, y) is integrated using a numerical integrator (e.g., Runge-Kutta) to
     *  return the value of y(t) at time t.  The derivative function takes a scalar t
     *  and a scalar y.
     *  @param f     the derivative function f(t, y)
     *  @param y0    the initial value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0.0, step: Double = defaultStepSize): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the integrate method to each derivative to compute the trajectory of
     *  a time-dependent vector function y(t) governed by a separable system of
     *  Ordinary Differential Equations (ODE's) where [f_j(t, y_j)] is an array of
     *  derivative functions.  Each derivative function takes a scalar t and a
     *  scalar y_j = y(j).
     *  @param f     the array of derivative functions [f_j(t, y_j)]
     *  @param y0    the initial value vector, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrateV (f: Array [Derivative], y0: VectorD, t: Double,
                    t0: Double = 0.0, step: Double = defaultStepSize): VectorD =
    {
        val n = y0.dim
        if (n != f.length) {
            flaw ("integrateV", "incompatible dimensions between f and y0")
            null
        } else {
            val y = new VectorD (n)
            for (i <- 0 until n) y(i) = integrate (f(i), y0(i), t, t0, step)
            y
        } // if
    } // integrateV

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use numerical integration to compute the trajectory of an unknown, time-
     *  dependent vector function y(t) governed by a system of first-order ODEs of
     *  the form y(t)' = f(t, y).  The j-th derivative in the array of derivative
     *  functions, [f_j(t, y)], takes a scalar t and a vector y (note the other
     *  integrate methods take a scalar t and a scalar y.
     *  @param f     the array of derivative functions [f_j(t, y)]
     *  @param y0    the initial value vector, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrateVV (f: Array [DerivativeV], y0: VectorD, t: Double,
                     t0: Double = 0.0, step: Double = defaultStepSize): VectorD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the error estimate.
     */
    def getError: Double = error

} // Integrator trait

