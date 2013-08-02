
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Mar  7 14:20:25 EST 2013
 *  @see     LICENSE (MIT style license file)
 *  @see     gwu.geverstine.com/pdenum.pdf
 */

package scalation.dynamics_pde

import math.ceil

import scalation.calculus.Calculus.FunctionS2S
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to solve parabolic partial differential equations like
 *  the Heat Equation.  Let u(x, t) = temperature of a rod at position 0 <= x <= xm
 *  and time t > 0.0  Numerically solve the
 *  Heat Equation:           u_t = k * u_xx
 *  with initial conditions  u(x, 0) = ic(x)
 *      boundary conditions  (u(0, t), u(xm, t)) = bc
 *  @param k   the thermal conductivity
 *  @param dt  delta t
 *  @param dx  delta x
 *  @param xm  the length of the rod
 *  @param ic  the initial conditions as a function of position x
 *  @param bc  the boundary conditions as a 2-tuple for endpoints 0 and xm
 */
class ParabolicPDE (k: Double, dt: Double, dx: Double, xm: Double,
                    ic: FunctionS2S, bc: Tuple2 [Double, Double])
      extends Error
{
    private val nx = (ceil ((xm) / dx)).toInt + 1            // number of x values in grid
    private val r  = k * dt / (dx * dx)                      // multiplier in recurrence equation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the temperature of the rod at time t, returning the vector of
     *  temperatures representing the temperature profile of the rod over its length.
     *  This method uses an explicit finite difference technique to solve the PDE.
     *  @param t  the time the solution is desired
     */
    def solve (t: Double): VectorD =
    {
        if (r >= .5) flaw ("solve", "multiplier r = " + r + " must be < .5 for stability")

        val u  = new MatrixD (nx, 2)                         // old/new temperatures as column vectors
        var j1 = 0                                           // current time index
        var j2 = 1                                           // next time index
        val nt  = (ceil (t / dt)).toInt + 1                  // number of t values in grid
        for (i <- 1 until nx-2) u(i, 0) = ic (i*dx)          // apply initial conditions
        u(0, 0) = bc._1; u(nx-1, 0) = bc._2                  // apply boundary conditions
        u(0, 1) = bc._1; u(nx-1, 1) = bc._2                  // apply boundary conditions (to both)

        println ("at t = " + 0.0 + ": \tu = " + u.col(j1))

        for (j <- 1 until nt) {                              // iterative over t = j*dt
            for (i <- 1 until nx-1) {                        // iterative over x = i*dx
                                                             // explicit recurrence equation
                u(i, j2) = u(i, j1) + r * (u(i-1, j1) - 2.0*u(i, j1) + u(i+1, j1))

            } // for
            println ("at t = " + j*dt + ": \tu = " + u.col(j2))
            j2 = j1; j1 = (j1 + 1) % 2                       // toggle indices (j2 <-> j1)
        } // for
        u.col(j1)                                            // return solution column vector at j1
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the temperature of the rod at time t, returning the vector of
     *  temperatures representing the temperature profile of the rod over its length.
     *  This method uses the implicit Crank-Nicolson technique to solve the PDE,
     *  which provides greater stability and accuracy.
     *  Implicit recurrence equation:
     *      -r*u(i-1, j2) + 2.0*(1.0+r)*u(i, j2) - r*u(i+1, j2) =
     *       r*u(i-1, j1) + 2.0*(1.0-r)*u(i, j1) + r*u(i+1, j1)
     *  This equation is solved simultaneously:  solve for u in mat * u = vec
     *  @see people.sc.fsu.edu/~jpeterson/5-CrankNicolson.pdf
     *  @param t  the time the solution is desired
     */
    def solveCN (t: Double): VectorD =
    {
        val u  = new VectorD (nx)                            // temperatures as a vector
        val nt = (ceil (t / dt)).toInt + 1                   // number of t values in grid
        for (i <- 1 until nx-2) u(i) = ic (i*dx)             // apply initial conditions
        u(0) = bc._1; u(nx-1) = bc._2                        // apply boundary conditions

        val mx  = nx - 2                                     // all x-values, except first and last
        val mat = formMatrix (r, mx)                         // coefficients for next time point
        val vec = new VectorD (mx)                           // to hold values from current time point

        println ("at t = " + 0.0 + ": \tu = " + u)

        for (j <- 1 until nt) {                              // iterative over time t = j*dt
            vec(0)    = 2.0*(1.0-r)*u(1) + r*u(2) + 2.0*r*bc._1
            vec(mx-1) = r*u(mx-1) + 2.0*(1.0-r)*u(mx) + 2.0*r*bc._2

            for (i <- 1 until mx-1) {                        // iterative over position x = i*dx
                vec(i) = r*u(i) + 2.0*(1.0-r)*u(i+1) + r*u(i+2)
            } // for

//          println ("mat = " + mat + "\nvec = " + vec)
            u(1 until nx-1) = mat.solve (vec)                // solve mat * u = vec
            println ("at t = " + j*dt + ": \tu = " + u)
        } // for
        u                                                    // return solution vector u
    } // solveCN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form the tridiagonal matrix that is used in the equation "mat * u = vec".
     *  @param r   the multiplier in the recurrence equation
     *  @param mx  the number of positions (x) excluding the first and last
     */
    private def formMatrix (r: Double, mx: Int): MatrixD =
    {
        val mat = new MatrixD (mx, mx)
        for (i <- 0 until mx) {
            if (i > 0) mat(i, i-1) = -r                       // sub-digonal
            mat(i, i) = 2.0*(1.0+r)                             // diagonal
            if (i < mx-1) mat(i, i+1) = -r                    // super-diagonal
        } // for
        mat
    } // formMatrix

} // ParabolicPDE


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the ParabolicPDE class.
 *  Numerically solve the Heat Equation:  du/dt = k * d^2u/dx^2.
 *  @see personales.unican.es/gutierjm/cursos/cornell/9_PDEs.pdf
 */
object ParabolicPDETest extends App
{
    val k  = 0.82                                     // thermal conductivity in cal/s*cm*0C
    val dt = 2.0                                       // delta t in sec
    val dx = 2.5                                      // delta x in cm
    val xm = 10.0                                      // length of rod in cm

    def ic (x: Double): Double = 0.0                   // initial conditions
    val bc = (100.0, 50.0)                              // boundary conditions

    val pde = new ParabolicPDE (k, dt, dx, xm, ic, bc)

    println (" Explicit Finite Difference ------------------------------------------")
    println ("solution = " + pde.solve (60.0))           // solve for t = 2, 4, ... sec

    println (" Implicit Crank-Nicholson --------------------------------------------")
    println ("solution = " + pde.solveCN (60.0))         // solve for t = 2, 4, ... sec

} // ParabolicPDETest

