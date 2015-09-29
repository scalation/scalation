
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Mar 28 13:43:50 EDT 2013
 *  @see     LICENSE (MIT style license file)
 *  @see     www.ita.uni-heidelberg.de/~dullemond/lectures/.../Chapter_3.pdf
 *  @see     www2.hawaii.edu/~norbert/CompPhys/chapter14.pdf
 */

package scalation.dynamics_pde

import math.{abs, ceil, exp}

import scalation.calculus.Calculus.FunctionS2S
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FirstOrderPDE` class is used to solve first order partial differential
 *  equations like the Advection Equation.  Let 'u(x, t)' = concentration in a fluid
 *  with velocity 'v' at position '0 <= x <= xm' and time 't' > 0.  Numerically solve the
 *  <p>
 *  Advection Equation:      u_t + v(x, t) * u_x = 0
 *  with initial conditions  u(x, 0) = ic(x)
 *      boundary conditions  (u(0, t), u(xm, t)) = bc
 *  <p>
 *  @param v   the velocity field function v(x, t)
 *  @param dt  delta t
 *  @param dx  delta x
 *  @param xm  the length of the column
 *  @param ic  the initial conditions as a function of position x
 *  @param bc  the boundary conditions as a 2-tuple for endpoints 0 and xm
 */
class FirstOrderPDE (v: (Double, Double) => Double, dt: Double, dx: Double, xm: Double,
                     ic: FunctionS2S, bc: Tuple2 [Double, Double])
      extends Error
{
    private val nx = (ceil ((xm) / dx)).toInt + 1            // number of x values in grid
    private val r  = dt / dx                                 // multiplier in recurrence equation
    private var u  = new VectorD (nx)                        // old concentration vectors
    private var uu = new VectorD (nx)                        // new concentration vectors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the concentration of the column at time t, returning the vector of
     *  concentration representing the concentration profile of column over its length.
     *  This method uses an explicit finite difference technique to solve the PDE.
     *  L-W is the Lax-Wendroff scheme which has second-order accuracy.
     *  @see   math.nju.edu.cn/~qzh/numPDE.pdf
     *  @param te  the time the solution is desired (t-end)
     */
    def solve (te: Double): VectorD =
    {
        var j1 = 0                                           // current time index
        var j2 = 1                                           // next time index
        val nt  = (ceil (te / dt)).toInt + 1                 // number of t values in grid
        for (i <- 0 until nx) u(i) = ic (i*dx)               // apply initial conditions
        u(0) = bc._1                                         // apply boundary conditions, left only

        println ("at t =  " + 0.0 + ": \tu  = " + u)

        for (j <- 1 until nt) {                              // iterative over t = j*dt
            val t = j*dt                                     // current time t
            for (i <- 1 until nx-1) {                        // iterative over x = i*dx
                val x = i*dx                                 // current position x
                val a = r * v(x, t)
                                                             // explicit recurrence equation
//              uu(i) = u(i) - a * (u(i) - u(i-1))
//              uu(i) = .5 * ((1. - a) * u(i+1) + (1. + a) * u(i-1))
//              uu(i) = u(i) - .5 * a * u(i+1) + .5 * a * u(i-1)
                uu(i) = u(i) - .5 * a * (u(i+1) - u(i-1)) + .5 * a*a * (u(i+1) - 2.0*u(i) + u(i-1))  // L-W

            } // for

            println ("at t = " + "%5.2f".format (t) + ": \tuu = " + uu)
            uu(0)  = bc._1                                   // apply left boundary condition
            var ut = uu; uu = u; u = ut                      // swap new and old references
        } // for
        u                                                    // return the concentration vector
    } // solve

} // FirstOrderPDE


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FirstOrderPDETest` object is used to test the `FirstOrderPDE` class.
 *  Numerically solve the Advection Equation:  du/dt + v(x, t) * du/dx = 0
 */
object FirstOrderPDETest extends App
{
    val dt = 1.0                                               // delta t in sec
    val dx = 1.0                                               // delta x in cm
    val xm = 100.0                                             // length of column in cm

    def ic (x: Double): Double = if (x < 30.0) 1.0 else 0.0    // initial conditions
    val bc = (1.0, 0.0)                                        // boundary conditions - only a left bc

    def v (x: Double, t: Double): Double = 1.0                 // the velocity field

    val pde = new FirstOrderPDE (v, dt, dx, xm, ic, bc)

    println (" Explicit Finite Difference ------------------------------------------")
    println ("solution = " + pde.solve (30.0))          // solve for t = 1, 2, ... sec

} // FirstOrderPDETest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FirstOrderPDETest2` object is used to test the `FirstOrderPDE` class.
 *  Numerically solve the Advection Equation:  du/dt + v(x, t) * du/dx = 0
 */
object FirstOrderPDETest2 extends App
{
    val dt = .2                                              // delta t in sec
    val dx = .2                                              // delta x in cm
    val xm = 3.0                                             // length/height of column in cm
    val d  = 0.5                                             // decay width
    val x0 = 0.0

    def ic (x: Double): Double =                             // initial conditions
    {
        xm / (1.0 + exp ((x - x0) / d))                      // logistic function
    } // ic

    for (i <- 0 to 15) printf ("x = %4.1f, \t%6.3f\n", i*dx, ic (i*dx)) 

    val bc = (ic (0.0), 0.0)                                 // boundary conditions - only a left bc

    def v (x: Double, t: Double): Double = 1.0               // the velocity field

    val pde = new FirstOrderPDE (v, dt, dx, xm, ic, bc)

    println (" Explicit Finite Difference ------------------------------------------")
    println ("solution = " + pde.solve (4.0))          // solve for t = 2., .4 ... sec

} // FirstOrderPDETest2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FirstOrderPDETest3` object is used to test the `FirstOrderPDE` class.
 *  Numerically solve the Advection Equation:  du/dt + v(x, t) * du/dx = 0
 *  @see www.public.asu.edu/~hhuang38/pde_slides_numerical.pdf
 */
object FirstOrderPDETest3 extends App
{
    val EPSILON = 1E-9                                       // a value close to zero

    val dt = .1                                              // delta t in sec
    val dx = .2                                              // delta x in cm
    val xm = 2.0                                             // length/height of column in cm

    def ic (x: Double): Double =                             // initial conditions
    {
        if (abs (x - .8) < EPSILON) 1.0 else 0.0
    } // ic

    val bc = (ic (0.0), 0.0)                                 // boundary conditions - only a left bc

    def v (x: Double, t: Double): Double = -1.0              // the velocity field

    val pde = new FirstOrderPDE (v, dt, dx, xm, ic, bc)

    println (" Explicit Finite Difference ------------------------------------------")
    println ("solution = " + pde.solve (2.0))         // solve for t = .1, .2, ... sec

} // FirstOrderPDETest3

