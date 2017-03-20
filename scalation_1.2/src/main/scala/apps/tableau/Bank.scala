
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Dec 27 15:41:58 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package apps.tableau

import scalation.model.Modelable
import scalation.random.Exponential
import scalation.tableau.Model

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bank` object defines a simple tableau model of a bank where service is
 *  provided by one teller and models an M/M/1 queue.  In this case, the default
 *  'simulate' method in `scalation.tableau.Model` is sufficient and need not be
 *  overridden.
 *  run-main apps.tableau.Bank
 */
object Bank extends App with Modelable
{
    val stream     = 1                                   // random number stream  (0 to 99)
    val lambda     = 6.0                                 // customer arrival rate (per hr)
    val mu         = 7.5                                 // customer service rate (per hr)
    val maxCusts   = 10                                  // stopping rule: at maxCusts
    val iArrivalRV = Exponential (HOUR/lambda, stream)   // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)       // service time random variate
    val label      = Array ("ID-0", "IArrival-1", "Arrival-2", "Start-3", "Service-4",
                            "End-5", "Wait-6", "Total-7")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the model `Bank`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val mm1 = new Model ("Bank", maxCusts, Array (iArrivalRV, serviceRV), label)
        mm1.simulate (startTime)
        mm1.report
    } // simulate

    simulate (0.0)

} // Bank

