
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Oct  9 21:24:35 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import math.abs

import scalation.process._
import scalation.linalgebra.{VectorD, VectorI}
import scalation.minima.QuasiNewton
import scalation.random.{Exponential, Uniform, Variate}
import scalation.util.Monitor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankOpt2` object performs Simulation Optimization 'SO' on a bank model to
 *  find the number of tellers that minimizes the overall cost function.  Cost is
 *  based on the daily pay for a teller (8 hours * 20 dollars per hour) and a
 *  cost based on customer wait time (10 dollars per minute of mean waiting time).
 *  > run-main apps.process.BankOpt2
 */
object BankOpt2 extends App
{
    val nStop   = 100                       // simulation stopping rule (100 customers)
    val maxReps = 2                         // run maxReps replications and average the results
    val cost    = (10000.0, 1.0)            // the cost tuple (teller speed, cost of waiting)
    var bm: BankModelOpt = null             // instance of a bank model

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computation of the objective function from the simulation results.  This
     *  involves (1) extracting and checking input parameters, (2) executing the
     *  simulation model and (3) performing cost analysis.
     *  @see "SoPT: Ontology for Simulation Optimization for Scientific Experiments"
     *  @param x  the vector of input parameters
     */
    def f (x: VectorD): Double =
    {
        val mu = x(0)
        if (mu <= 0.0) return g(x)
        val tellers  = 1
        var c        = 0.0
        var waitTime = 0.0

        for (reps <- 0 until maxReps) {
            println ("---------------------------------------------------------------")
            println ("simulate a bank with teller service mean = " + mu)
            println ("---------------------------------------------------------------")
            bm = new BankModelOpt ("bank", nStop, Uniform (1000, 3000), tellers,
                                   Exponential (mu), Uniform (900, 1100))
            bm.simulate ()
            val waitTime = bm.getStatistics(2).mean
            c += cost._1 / mu + cost._2 * waitTime                    // return overall cost
        } // for
        c /= maxReps.toDouble
        println ("for mu = " + mu + " with w_q = " + (waitTime / maxReps.toDouble) + " cost = " + c)
        c
    } // f

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computation of the constraint function
     *  @param x  the vector of input parameters
     */
    def g (x: VectorD): Double =
    {
        val mu = x(0)
        if (mu <= 0.0) (1.0 + mu * mu) else 0.0
    } // g

    val optimizer = new QuasiNewton (f, g)
    val x0 = VectorD (4000.0)
    val result = optimizer.solve (x0)
    println ("###############################################################")
    println ("optimal solution x = " + result)
    println ("###############################################################")

} // BankOpt2 object

