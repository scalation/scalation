
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Mar 29 14:59:50 EDT 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.dynamics

import math.{abs, pow}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Given an unknown, time-dependent function y(t) governed by an Ordinary
 *  Differential Equation (ODE) of the form y(t)' = f(t, y) where ' is d/dt,
 *  compute y(t) using a (4,5)-order Dormand-Prince Integrator (DOPRI).  Note:
 *  the integrateV method for a system of separable ODEs is mixed in from the
 *  Integrator trait.
 *  @see http://adorio-research.org/wordpress/?p=6565
 */
object DormandPrince
       extends Integrator
{
    import Derivatives.{Derivative, DerivativeV}

    /** Butcher tableau @see http://en.wikipedia.org/wiki/Dormand–Prince_method
     */
    val a21 = 1./5.
    val a31 = 3./40.;       val a32 = 9./40.
    val a41 = 44./45.;      val a42 = -56./15.;       val a43 = 32./9.
    val a51 = 19372./6561.; val a52 = -25360./2187.;  val a53 = 64448./6561.
    val a54 = -212./729.
    val a61 = 9017./3168.;  val a62 = -355./33.;      val a63 = 46732./5247.
    val a64 = 49./176.;     val a65 = -5103./18656.
    val a71 = 35./384.;     val a72 = 0.;             val a73 = 500./1113.
    val a74 = 125./192.;    val a75 = -2187./6784.;   val a76 = 11./84.
 
    val c2 = 1./5.
    val c3 = 3./10.
    val c4 = 4./5.
    val c5 = 8./9.
    val c6 = 1.
    val c7 = 1.
 
    val b1 = 35./384.
    val b2 = 0.
    val b3 = 500./1113.
    val b4 = 125./ 192.
    val b5 = -2187./6784.
    val b6 = 11./84.
    val b7 = 0.
 
    val b1p = 5179./57600.
    val b2p = 0.
    val b3p = 7571./16695.
    val b4p = 393./640.
    val b5p = -92097./339200.
    val b6p = 187./2100.
    val b7p = 1./40.

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t) governed by a differential equation using numerical integration
     *  of the derivative function f(t, y) using a (4,5)-order Dormand-Prince method to
     *  return the value of y(t) at time t.
     *  @param f     the derivative function f(t, y)
     *  @param y0    value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the middle step size
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0., step: Double = defaultStepSize): Double =
    {
        integrate2 (f, y0, t, .5*step, 2.*step, t0)
    } // integrate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t) governed by a differential equation using numerical integration
     *  of the derivative function f(t, y) using a (4,5)-order Dormand-Prince method to
     *  return the value of y(t) at time t.  The method provides more customization
     *  options.
     *  @param f         the derivative function f(t, y)
     *  @param y0        value of the y-function at time t0, y0 = y(t0)
     *  @param t         the time value at which to compute y(t)
     *  @param hmin      the minimum step size
     *  @param hmax      the maximum step size
     *  @param t0        the initial time
     *  @param tol       the tolerance
     *  @param maxSteps  the maximum number of steps
     */
    def integrate2 (f: Derivative, y0: Double, t: Double, hmin: Double, hmax: Double,
                   t0: Double = 0., tol: Double = 1E-5, maxSteps: Int = 1000): Double =
    {
        var ti    = t0
        var y     = y0
        var h     = hmax
        var delta = 0.

        var k1    = 0.
        var k2    = 0.
        var k3    = 0.
        var k4    = 0.
        var k5    = 0.
        var k6    = 0.
        var k7    = 0.
 
        for (i <- 1 to maxSteps) {
            k1 = f (ti,          y)
            k2 = f (ti + c2 * h, y + (a21*k1) * h)
            k3 = f (ti + c3 * h, y + (a31*k1 + a32*k2) * h)
            k4 = f (ti + c4 * h, y + (a41*k1 + a42*k2 + a43*k3) * h)
            k5 = f (ti + c5 * h, y + (a51*k1 + a52*k2 + a53*k3 + a54*k4) * h)
            k6 = f (ti +      h, y + (a61*k1 + a62*k2 + a63*k3 + a64*k4 + a65*k5) * h)
            k7 = f (ti +      h, y + (a71*k1 + a72*k2 + a73*k3 + a74*k4 + a75*k5 + a76*k6) * h)
 
            error = abs ( (b1-b1p) * k1 + (b3-b3p) * k3 + (b4-b4p) * k4 +
                          (b5-b5p) * k5 + (b6-b6p) * k6 + (b7-b7p) * k7 )
 
            delta = 0.84 * pow (tol / error, .2)   // error control
            if (error < tol) {
                ti += h
                y  += h * (b1*k1 + b3*k3 + b4*k4 + b5*k5 + b6*k6)
            } // if
 
            if (delta <= .1)       h *= .1
            else if (delta >= 4. ) h *= 4.
            else                   h *= delta
 
            if (h > hmax) h = hmax
 
            if (ti >= t)         return y
            else if (ti + h > t) h = t - ti
            else if (h < hmin)   return y
       } // for
 
       y         // the value of the function at time t, y = f(t)

    } // integrate2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t), a vector, governed by a system of differential equations using
     *  numerical integration of the derivative function f(t, y) using a (4,5)-order
     *  Dormand-Prince method to return the value of y(t) at time t.
     *  @param f     the array of derivative functions [f(t, y)] where y is a vector
     *  @param y0    the value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrateVV (f: Array [DerivativeV], y0: VectorD, t: Double,
                     t0: Double = 0., step: Double = defaultStepSize): VectorD =
    {
        val maxSteps = 1000
        val hmin     = .2 * step
        val hmax     = 2. * step
        val tol      = 1E-5

        var ti    = t0
        var y     = y0
        var h     = hmax
        var delta = 0.

        val k1 = new VectorD (y.dim)
        val k2 = new VectorD (y.dim)
        val k3 = new VectorD (y.dim)
        val k4 = new VectorD (y.dim)
        val k5 = new VectorD (y.dim)
        val k6 = new VectorD (y.dim)
        val k7 = new VectorD (y.dim)

        for (i <- 1 to maxSteps) {
            for (j <- 0 until y.dim) k1(j) = f(j) (ti,          y)
            for (j <- 0 until y.dim) k2(j) = f(j) (ti + c2 * h, y + (k1*a21) * h)
            for (j <- 0 until y.dim) k3(j) = f(j) (ti + c3 * h, y + (k1*a31 + k2*a32) * h)
            for (j <- 0 until y.dim) k4(j) = f(j) (ti + c4 * h, y + (k1*a41 + k2*a42 + k3*a43) * h)
            for (j <- 0 until y.dim) k5(j) = f(j) (ti + c5 * h, y + (k1*a51 + k2*a52 + k3*a53 + k4*a54) * h)
            for (j <- 0 until y.dim) k6(j) = f(j) (ti +      h, y + (k1*a61 + k2*a62 + k3*a63 + k4*a64 + k5*a65) * h)
            for (j <- 0 until y.dim) k7(j) = f(j) (ti +      h, y + (k1*a71 + k2*a72 + k3*a73 + k4*a74 + k5*a75 + k6*a76) * h)

            error = abs ( (b1-b1p) * k1.norm + (b3-b3p) * k3.norm + (b4-b4p) * k4.norm +
                          (b5-b5p) * k5.norm + (b6-b6p) * k6.norm + (b7-b7p) * k7.norm )

            delta = 0.84 * pow (tol / error, .2)   // error control
            if (error < tol) {
                ti += h
                for (j <- 0 until y.dim) y(j)  += h * (b1*k1(j) + b3*k3(j) + b4*k4(j) + b5*k5(j) + b6*k6(j))
            } // if

            if (delta <= .1)       h *= .1
            else if (delta >= 4. ) h *= 4.
            else                   h *= delta

            if (h > hmax) h = hmax

            if (ti >= t)         return y
            else if (ti + h > t) h = t - ti
            else if (h < hmin)   return y
       } // for

       y         // the value of the function at time t, y = f(t)

    } // integrateVV
 
} // DormandPrince object 


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the DormandPrince object.
 */
object DormandPrinceTest extends App
{
    import Derivatives.{Derivative, DerivativeV}
    import DormandPrince._

    def derv1 (t: Double, y: Double) = t + y
    val y0   = 1.24
    val t    = 1.0
    val hmin = 0.01
    val hmax = 1.0

    //def integrate (f: Derivative, y0: Double, t: Double, hmin: Double, hmax: Double,
    //               t0: Double = 0., tol: Double = 1E-5, maxSteps: Int = 1000): Double =
    println ("\n==> at t = " + t + " y = " + integrate2 (derv1, y0, t, hmin, hmax))

    // @see http://www.mathworks.com/help/techdoc/ref/ode23.html (Example 1)

    def dx_dt (t: Double, p: VectorD) =  p(1) * p(2)
    def dy_dt (t: Double, p: VectorD) = -p(0) * p(2)
    def dz_dt (t: Double, p: VectorD) = -.51 * p(0) * p(1)
    val odes = Array [DerivativeV] (dx_dt, dy_dt, dz_dt)

    var ti  = .2
    var p   = new VectorD (0., 1., 1.)
    val p_r = new MatrixD (61, 3); for (k <- 0 until p.dim) p_r(0, k) = p(k)
    var tt  = new VectorD (61); tt(0) = 0.
    for (i <- 1 to 60) {
        tt(i) = ti * i
        p = integrateVV (odes, p, ti)
        println ("\n==> at tt = " + tt(i) + " p = " + p)
        for (k <- 0 until p.dim) p_r(i, k) = p(k)
        //p_r(i) = p
    } // for

    new Plot (tt, p_r.col(0), p_r.col(1), "Plot p(0), p(1) vs. t")

} // DormandPrinceTest object

