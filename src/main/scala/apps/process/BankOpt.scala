
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Oct  9 21:24:35 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.process._
import scalation.linalgebra.{VectorD, VectorI}
import scalation.minima.IntegerLocalSearch
//import scalation.minima.IntegerNLP
import scalation.random.{Uniform, Variate}
import scalation.util.Monitor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankOpt` object performs Simulation Optimization (SO) on a bank model to
 *  find the number of tellers that mimimizes the overall cost function.  Cost is
 *  based on the daily pay for a teller (8 hours * 20 dollars per hour) and a
 *  cost based on customer wait time (10 dollarsper minute of mean waiting time).
 */
object BankOpt extends App
{
    val nStop   = 100                       // simulation stopping rule (100 customers)
    val cost    = VectorD (160.0, 10.0)     // the cost coefficient vector
    val PENALTY = 1.0E8                     // penalty for infeasibility (e.g., -1 tellers)
    var bm: BankModelOpt = null             // instance of a bank model

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Computation of the objective function from the simulation results.  This
     *  involves (1) extracting and checking input parameters, (2) executing the
     *  simulation model and (3) performing cost analysis.
     *  @see "SoPT: Ontology for Simulation Optimization for Scientific Experiments"
     *  @param x  the vector of input parameters
     */
    def f (x: VectorI): Double =
    {
        val tellers = x(0)
        if (tellers < 1) return PENALTY * (1 - tellers)   // return based on penalty
        println ("---------------------------------------------------------------")
        println ("simulate a bank with " + tellers + " tellers")
        println ("---------------------------------------------------------------")
        bm = new BankModelOpt ("bank", nStop, Uniform (1000, 3000), tellers,
                               Uniform (9000, 11000), Uniform (900, 1100))
        bm.simulate ()
        val waitTime = bm.getStatistics(2).mean
        val response = VectorD (tellers, waitTime)
        cost dot response                                 // return overall cost
    } // f

    val optimizer = new IntegerLocalSearch (f)
//  val optimizer = new IntegerNLP (f, x0.dim)
    val x0 = new VectorI (1); x0.set (1)
    val result = optimizer.solve (x0)
    println ("###############################################################")
    println ("optimal solution x = " + result)
    println ("###############################################################")

} // BankOpt object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModelOpt` class defines a simple process-interaction model of a bank
 *  where service is provided by one or more tellers.
 *  @param name        the name of the bank model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nUnits      the number of service units (tellers)
 *  @param serviceRV   the service time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class BankModelOpt (name: String, nArrivals: Int, iArrivalRV: Variate,
                    nUnits: Int, serviceRV: Variate, moveRV: Variate)
      extends Model (name, 2, false)
{
    Monitor.traceOff ()

    val entry     = Source ("entry", this, Customer, 0, nArrivals, iArrivalRV, (120, 185))
    val tellerQ   = WaitQueue ("tellerQ", (250, 190))
    val teller    = Resource ("teller", tellerQ, nUnits, serviceRV, (320, 185))
    val door      = Sink ("door", (460, 185))
    val toTellerQ = new Transport ("toTellerQ", entry, tellerQ, moveRV)
    val toDoor    = new Transport ("toDoor", teller, door, moveRV)

    addComponent (entry, tellerQ, teller, door, toTellerQ, toDoor)

    case class Customer () extends SimActor ("c", this)
    {
        def act ()
        {
            toTellerQ.move ()
            if (teller.busy) tellerQ.waitIn ()
            teller.utilize ()
            teller.release ()
            toDoor.move ()
            door.leave ()
        } // act

    } // Customer

} // BankModelOpt class

