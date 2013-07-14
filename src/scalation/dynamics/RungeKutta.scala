
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Oct 25 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.dynamics

import math.{abs, E, pow, round}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**  Given an unknown, time-dependent function y(t) governed by an Ordinary
 *  Differential Equation (ODE) of the form y(t)' = f(t, y) where ' is d/dt,
 *  compute y(t) using a 4th-order Runge-Kutta Integrator (RK4).  Note: the
 *  integrateV method for a system of separable ODEs is mixed in from the
 *  Integrator trait.
 */
object RungeKutta
       extends Integrator
{
    import Derivatives.{Derivative, DerivativeV}

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t) governed by a differential equation using numerical integration
     *  of the derivative function f(t, y) using a 4th-order Runge-Kutta method to
     *  return the value of y(t) at time t.
     *  @param f     the derivative function f(t, y) where y is a scalar
     *  @param y0    the value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0.0, step: Double = defaultStepSize): Double =
    {
        val t_t0       = t - t0                                    // time interval
   	val steps: Int = (round (t_t0 / step)).asInstanceOf [Int]  // number of steps
   	var h          = t_t0 / steps.asInstanceOf [Double]        // adjusted step size
        var ti         = t0                                        // initialize ith time ti to t0
   	var y          = y0                                        // initialize y = f(t) to y0

        var a = 0.0; var b = 0.0; var c = 0.0; var d = 0.0

   	for (i <- 1 to steps) {
            ti += h                                       // take the next step
            if (ti > t) { h -= ti - t; ti = t }           // don't go past t

            a = f (ti, y)
            b = f (ti + h/2.0, y + a/2.0)
            c = f (ti + h/2.0, y + b/2.0)
            d = f (ti + h, y + c)
            y += h/6.0 * (a + 2*b + 2*c + d)

            if (abs (y) > Double.MaxValue / 10.0) flaw ("integrate", "probable overflow since y = " + y)
            if (i % 1000 == 0) println ("integrate: iteration " + i + " ti = " + ti + " y = " + y)
   	} // for

        y         // the value of the function at time t, y = f(t)
    } // integrate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t), a vector, governed by a system of differential equations using
     *  numerical integration of the derivative function f(t, y) using a 4th-order
     *  Runge-Kutta method to return the value of y(t) at time t.
     *  @param f     the array of derivative functions [f(t, y)] where y is a vector
     *  @param y0    the value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrateVV (f: Array [DerivativeV], y0: VectorD, t: Double,
                     t0: Double = 0.0, step: Double = defaultStepSize): VectorD =
    {
        val t_t0       = t - t0                                    // time interval
   	val steps: Int = (round (t_t0 / step)).asInstanceOf [Int]  // number of steps
   	var h          = t_t0 / steps.asInstanceOf [Double]        // adjusted step size
        var ti         = t0                                        // initialize ith time ti to t0
   	val y          = y0                                        // initialize y = f(t) to y0

        val a = new VectorD (y.dim)
        val b = new VectorD (y.dim)
        val c = new VectorD (y.dim)
        val d = new VectorD (y.dim)

   	for (i <- 1 to steps) {
            ti += h                                       // take the next step
            if (ti > t) { h -= ti - t; ti = t }           // don't go past t

            for (j <- 0 until y.dim) a(j) = f(j) (ti, y)
            for (j <- 0 until y.dim) b(j) = f(j) (ti + h/2.0, y + a * h/2.0)
            for (j <- 0 until y.dim) c(j) = f(j) (ti + h/2.0, y + b * h/2.0)
            for (j <- 0 until y.dim) d(j) = f(j) (ti + h, y + c * h)
            for (j <- 0 until y.dim) y(j) += h/6.0 * (a(j) + 2.0*b(j) + 2.0*c(j) + d(j))

            if (abs (y(0)) > Double.MaxValue / 10.0) flaw ("integrateVV", "probable overflow since y = " + y)
            if (i % 1000 == 0) println ("integrateVV: iteration " + i + " ti = " + ti + " y = " + y)
        } // for

        y         // the value of the function at time t, y = f(t)
    } // integrateVV

} // RungeKutta object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the RungeKutta object.
 */
object RungeKuttaTest extends App
{
    import Derivatives.{Derivative, DerivativeV}
    import RungeKutta._

    val y0 = 1.0
    val t  = 2.0

    def derv1 (t: Double, y: Double) = 2.0 * t  // solution to differential equation is t^2
    println ("\n==> at t = " + t + " y = " + integrate (derv1, y0, t))
    println ("\n==> t^2 + c = " + 5)

    def derv2 (t: Double, y: Double) = y       // solution to differential equation is e^t
    println ("\n==> at t = " + t + " y = " + integrate (derv2, y0, t))
    println ("\n==> e = " + E + " e^2 = " + E * E)

    def derv3 (t: Double, y: Double) = t + y   // solution to differential equation is ?
    println ("\n==> at t = " + t + " y = " + integrate (derv3, y0, t))

    println ("\n==> at t = " + t + " y = " + 
             integrateV (Array (derv1, derv2), new VectorD (1.0, 2.0), t))

    // @see http://www.mathworks.com/help/techdoc/ref/ode23.html (Example 1)

    def dx_dt (t: Double, p: VectorD) =  p(1) * p(2)
    def dy_dt (t: Double, p: VectorD) = -p(0) * p(2)
    def dz_dt (t: Double, p: VectorD) = -.51 * p(0) * p(1)
    val odes = Array [DerivativeV] (dx_dt, dy_dt, dz_dt)

    var ti  = .2
    var p   = new VectorD (0.0, 1.0, 1.0)
    val p_r = new MatrixD (61, 3); for (k <- 0 until p.dim) p_r(0, k) = p(k)
    var tt  = new VectorD (61); tt(0)  = 0.0
    for (i <- 1 to 60) {
        tt(i) = ti * i
        p = integrateVV (odes, p, ti)
        println ("\n==> at tt = " + tt(i) + " p = " + p)
        for (k <- 0 until p.dim) p_r(i, k) = p(k)
        //p_r(i) = p
    } // for

    new Plot (tt, p_r.col(0), p_r.col(1), "Plot p(0), p(1) vs. t")

} // RungeKuttaTest object

