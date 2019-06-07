
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Jul 29 14:33:21 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.dynamics

import scala.util.control.Breaks.{breakable, break}

import scalation.dynamics.{DormandPrince, RungeKutta}
import scalation.dynamics.Derivatives.DerivativeV
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Pathway` object is used to simulate a simple chemical pathway for making H2O.
 *  @see the KINSOLVER Paper: www.cs.uga.edu/~thiab/paper25.pdf
 *  > runMain apps.dynamics.Pathway
 */
object Pathway extends App
{
    val t0 = 0.0                         // initial time
    val tf = 5.0                         // final time
    val n  = 100                         // number of time steps

    val kf = (1.0,  1.0,  0.5)     // forward reaction rates
    val kb = (0.02, 0.02, 0.01)    // backward reaction rates

    // concentrations   H2   O2   O    H     OH   H2O
    //                  0    1    2    3     4    5
    var c = VectorD   (6.0, 6.0, 0.0, 0.01, 0.0, 1.0)

    // define the system of Ordinary Differential Equations (ODEs)

    // d[H2]/dt                         = - kf1 [H2] [O] + kb1 [H] [OH] - kf3 [H2] [OH] + kb3 [H2O] [H]
    def dh2_dt (t: Double, c: VectorD)  = -kf._1*c(0)*c(2) + kb._1*c(3)*c(4) - kf._3*c(0)*c(4) + kb._3*c(5)*c(3) 

    // d[O2]/dt                         = - kf2 [H] [O2] + kb2 [O] [OH]
    def do2_dt (t: Double, c: VectorD)  = -kf._2*c(3)*c(1) + kb._2*c(2)*c(4)

    // d[O]/dt                          = - kf1 [H2] [O] + kb1 [H] [OH] + kf2 [H] [O2] - kb2 [O] [OH]
    def do_dt (t: Double, c: VectorD)   = -kf._1*c(0)*c(2) + kb._1*c(3)*c(4) + kf._2*c(3)*c(1) - kb._2*c(2)*c(4)

    // d[H]/dt                          = + kf1 [H2] [O] - kb1 [H] [OH] - kf2 [H] [O2] + kb2 [O] [OH] + kf3 [H2] [OH] - kb3 [H2O] [H]
    def dh_dt (t: Double, c: VectorD)   =  kf._1*c(0)*c(2) - kb._1*c(3)*c(4) - kf._2*c(3)*c(1) + kb._2*c(2)*c(4) + kf._3*c(0)*c(4) - kb._3*c(5)*c(3)

    // d[OH]/dt                         = + kf1 [H2] [O] - kb1 [H] [OH] + kf2 [H] [O2] - kb2 [O] [OH] - kf3 [H2] [OH] + kb3 [H2O] [H]
    def doh_dt (t: Double, c: VectorD)  =  kf._1*c(0)*c(2) - kb._1*c(3)*c(4) + kf._2*c(3)*c(1) - kb._2*c(1)*c(4) - kf._3*c(0)*c(4) + kb._3*c(5)*c(3)

    // d[H2O]/dt                        = + kf3 [H2] [OH] - kb3 [H2O] [H]
    def dh2o_dt (t: Double, c: VectorD) =  kf._3*c(0)*c(4) - kb._3*c(5)*c(3)

    val odes: Array [DerivativeV] = Array (dh2_dt, do2_dt, do_dt, dh_dt, doh_dt, dh2o_dt)

    println ("dh2_dt  = " + dh2_dt (0.0, c))
    println ("do2_dt  = " + do2_dt (0.0, c))
    println ("do_dt   = " + do_dt (0.0, c))
    println ("dh_dt   = " + dh_dt (0.0, c))
    println ("doh_dt  = " + doh_dt (0.0, c))
    println ("dh2o_dt = " + dh2o_dt (0.0, c))

    println ("                           H2,  O2,   O,    H,  OH,  H2O")
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

} // Pathway object

