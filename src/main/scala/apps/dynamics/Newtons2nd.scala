
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Jul 29 14:33:21 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.dynamics

import scala.math.{cos, sin, Pi}
import util.control.Breaks.{breakable, break}

import scalation.dynamics.{DormandPrince, RungeKutta}
import scalation.dynamics.Derivatives.Derivative
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Newtons2nd` object is used to illustrate the RungeKutta (RK) and DormandPrince
 *  (DP) ODE solvers by applying them to Newton's Second Law of Motion, f = ma = -gm.
 *  The flight of a golf ball is simulated from impact until the ball hits the
 *  ground.  Note, a more realistic simulation would take additional forces into
 *  account: drag, lift and spin.
 *  @see http://home2.fvcc.edu/~dhicketh/DiffEqns/Spring11projects/Brett_Burglund_Ryan_Street/Diff%20Q/pdfscreen/projectoutline.pdf
 *  @see http://claymore.engineer.gvsu.edu/~lait/312/golfball.pdf
 *  The accurracies of RK and DP versus the  exact solution (EX) are compared.
 */
object Newtons2nd extends App
{
    val n  = 100                                  // maximum number of time points
    val tm =   5.0                                // simulate for a maximum of tm seconds
    val g  =   9.80665                            // gravitational force (meters/second^2)
    val m  =  45.93                               // mass of a golf ball in grams
    val aa =  15.00                               // launch angle in degrees
    val ss = 100.00                               // swing speed in miles/hour
    val sf =   1.49                               // smash factor
    val s  = ss * sf * 1609.344 / 3600            // initial ball speed in meters/second
    val a  = aa * Pi / 180.0                      // launch angle in radians
    val p0 = VectorD (0.0, 0.0)                   // initial position (x, y) at time t0=0
    val v0 = VectorD (s * cos(a), s * sin(a))     // initial velocity (v_x, v_y) at t0

    println ("ball speed    s  = " + s)
    println ("launch angle  a  = " + a)
    println ("ball velocity v0 = " + v0)

    // define the system of Ordinary Differential Equations (ODEs)
    def dx_dt (t: Double, x: Double) = v0(0)              // ODE 1
    def dy_dt (t: Double, y: Double) = v0(1) - g * t      // ODE 2
    val odes: Array [Derivative] = Array (dx_dt, dy_dt)

    def exactSolution (t: Double) = VectorD (v0(0) * t, v0(1) * t - .5 * g * t * t)

    val p_r = new MatrixD (n, 2); p_r(0) = p0
    val p_d = new MatrixD (n, 2); p_d(0) = p0
    val p_e = new MatrixD (n, 2); p_e(0) = p0
    val tt  = new VectorD (n);    tt(0)  = 0.0

    val dt = tm / n                                       // time step
    var t  = dt                                           // next time point to examine

    breakable { for (i <- 1 to n) {
        p_e(i) = exactSolution (t)                        // compute new position using EX
        if (p_e(i, 1) < 0.0) { p_e(i, 1) = 0; break }     // quit after hitting the ground

        p_r(i) = RungeKutta.integrateV (odes, p0, t)      // compute new position using RK
        p_d(i) = DormandPrince.integrateV (odes, p0, t)   // compute new position using DP

        println ("> at t = " + "%4.1f".format (t) + " p_r = " + p_r(i) +
                 " p_d = " + p_d(i) + " p_e = " + p_e(i))
        tt(i) = t
        t += dt
    }} // for

    new Plot (tt, p_r.col(0), p_d.col(0), "Plot x vs. t (black-RK, red-DP)")
    new Plot (tt, p_r.col(1), p_d.col(1), "Plot y vs. t (black-RK, red-DP)")
    new Plot (tt, p_r.col(1), p_e.col(1), "Plot y vs. t (black-RK, red-EX)")
    new Plot (tt, p_d.col(1), p_e.col(1), "Plot y vs. t (black-DP, red-EX)")
    new Plot (p_d.col(0), p_d.col(1))

} // Newtons2nd object

