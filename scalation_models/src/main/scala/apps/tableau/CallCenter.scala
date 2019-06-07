
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Dec 27 15:41:58 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package apps.tableau

import scalation.model.Modelable
import scalation.random.{Exponential, Variate}
import scalation.tableau.Model

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenter` object defines a simple tableau model of a Call Center where
 *  service is provided by one tele-service representative and models an M/M/1/1
 *  queue (i.e., no call waiting).  The default 'simulate' method provided by
 *  `scalation.tableau.Model` won't suffice and must be overridden in the
 *  `CallCenterModel` class.
 */
object CallCenter extends App with Modelable
{
    val stream     = 1                                   // random number stream (0 to 99)
    val lambda     = 6.0                                 // customer arrival rate (per hr)
    val mu         = 7.5                                 // customer service rate (per hr)
    val maxCalls   = 10                                  // stopping rule: at maxCalls
    val iArrivalRV = Exponential (HOUR/lambda, stream)   // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)       // service time random variate
    val label      = Array ("ID-0", "IArrival-1", "Arrival-2", "Start-3", "Service-4",
                            "End-5", "Wait-6", "Total-7")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `CallCenterModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val mm11 = new CallCenterModel ("CallCenter", maxCalls, Array (iArrivalRV, serviceRV),
                                         label)
        mm11.simulate (startTime)
        mm11.report
        mm11.save
    } // simulate

    simulate (0.0)

} // CallCenter object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel` class customizes `scalation.tableau.Model` for Call Center
 *  simulations by overriding the 'simulate' method.
 *  @param name   the name of simulation model
 *  @param m      the number entities to process before stopping
 *  @param rv     the random variate generators to use
 *  @param label  the column labels for the matrix
 */
class CallCenterModel (name: String, m: Int, rv: Array [Variate], label: Array [String])
      extends Model (name, m, rv, label)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform tableau-based simulation by recording timing information about
     *  the 'i'th entity in the 'i'th row of the matrix.
     *  @param startTime  the start time for the simulation
     */
    override def simulate (startTime: Double)
    {
        var l = 0                                                 // last established call
        for (i <- 1 to m) {
            table(i, 1)  = if (i == 1) startTime else rv(0).gen        // IArrival-1
            table(i, 2)  = table(i, 1) + table(i-1, 2)                 // Arrival-2
            val serviceT = rv(1).gen
            if (table(l, 5) <= table(i, 2)) {                     // call established
                table(i, 3) = table(i, 2); l = i                       // Start-3
                table(i, 4) = serviceT                                 // Service-4
                table(i, 5) = table(i, 3) + table(i, 4)                // End-5
                table(i, 6) = table(i, 3) - table(i, 2)                // Wait-6
                table(i, 7) = table(i, 5) - table(i, 2)                // Total-7
            } // if
        } // for
    } // simulate

} // CallCenterModel class

