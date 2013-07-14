
/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Sun Sep 25 22:39:34 EDT 2011
 * @see     LICENSE (MIT style license file).
 * @compile scalac -cp ../../classes -d classes Pathway2.scala
 * @run     scala -cp ../../classes:classes dynamics.Pathway2
 */

package dynamics

import util.control.Breaks.{breakable, break}

import scalation.dynamics.{DormandPrince, RungeKutta}
import scalation.dynamics.Derivatives.DerivativeV
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot

/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
/** This object is used to simulate a simple plant metabolic pathway.
 *  @see Experimental and mathematical approaches to modeling plant metabolic networks
 *  Phytochemistry Vol. 68 (2007) pp. 2351–2374, Elsevier Science Direct
 */
object Pathway2 extends App
{
    val t0 = 0.0                        // initial time
    val tf = 720.0                      // final time
    val n  = 720                        // number of time steps
 
    val km1 = .8
    val km2 = .1
    val km3 = .004

    val vm1 = .0018
    val vm2 = .0018
    val vm3 = .000012

    // concentrations    A   B   C
    //                   0   1   2
    var c = new VectorD (1.0, 0.0, 0.0)

    // define the system of Ordinary Differential Equations (ODEs)

    def dA_dt (t: Double, c: VectorD) = -vm1 * c(0) / (c(0) + km1) - vm2 * c(0) / (c(0) + km2)
    def dB_dt (t: Double, c: VectorD) = vm1 * c(0) / (c(0) + km1) - vm3 * c(0) / (c(0) + km3)
    def dC_dt (t: Double, c: VectorD) = -vm3 * c(0) / (c(0) + km3) + vm2 * c(0) / (c(0) + km2)

    val odes: Array [DerivativeV] = Array (dA_dt, dB_dt, dC_dt)

    println ("dA_dt = " + dA_dt (0.0, c))
    println ("dB_dt = " + dB_dt (0.0, c))
    println ("dC_dt = " + dC_dt (0.0, c))

    println ("                           A,  B,   C")
    println ("> at t = " + "%6.3f".format (t0) + " c = " + c)
    val dt = tf / n                                 // time step
    var t  = t0 + dt                                // next time point to examine

    breakable { for (i <- 1 to n) {
//      c = RungeKutta.integrateVV (odes, c, dt)      // compute new concentrations using RK
        c = DormandPrince.integrateVV (odes, c, dt)   // compute new concentrations using DP

        println ("> at t = " + "%6.3f".format (t) + " c = " + c)
        t += dt
    }} // for

/*
    new Plot (tt, p_r.col(0), p_d.col(0), "Plot x vs. t (black-RK, red-DP)")
    new Plot (tt, p_r.col(1), p_d.col(1), "Plot y vs. t (black-RK, red-DP)")
    new Plot (tt, p_r.col(1), p_e.col(1), "Plot y vs. t (black-RK, red-EX)")
    new Plot (tt, p_d.col(1), p_e.col(1), "Plot y vs. t (black-DP, red-EX)")
    new Plot (p_d.col(0), p_d.col(1))
*/

} // Pathway2 object

