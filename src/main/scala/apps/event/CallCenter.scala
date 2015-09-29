
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Aug 19 17:10:23 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.event

import scalation.event.{Entity, Event, Model}
import scalation.model.Modelable
import scalation.random.{Exponential, Variate}
import scalation.queueingnet.MMck_Queue
import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenter` object defines a particular scenario under which to execute
 *  the Call Center model.
 *  @see scalation.event.ModelTest for another example of test code.
 */
object CallCenter extends App with Modelable
{
    val stream     = 1                                  // random number stream (0 to 99)
    val lambda     = 6.0                                // call arrival rate (per hour)
    val mu         = 7.5                                // call service rate (per hour)
    val maxCalls   = 100                                // stopping rule: at maxCalls
    val iArrivalRV = Exponential (HOUR/lambda, stream)  // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)      // service time random variate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of `CallCenterModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        new CallCenterModel ("CallCenter", maxCalls, iArrivalRV, serviceRV)
    } // simulate

    simulate (0.0)

    //:: verify the results using an M/M/1/1 Queueing Model

    println ("\nVerification ...")
    val mm11 = new MMck_Queue (lambda/HOUR, mu/HOUR)
    mm11.check
    mm11.report

} // CallCenter object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel` class defines a simple Event-Scheduling model of a
 *  Call Center where service is provided by one tele-service representative and
 *  models an M/M/1/1 queue.
 *  @param name        the name of the simulation model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution (Random Variate)
 *  @param serviceRV   the service time distribution (Random Variate)
 */
class CallCenterModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                                                     serviceRV: Variate)
      extends Model (name)
{
    val t_a_stat  = new Statistic ("t_a")   // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")   // time in Service statistics

    var nArr      = 0.0                     // number of calls that have arrived
    var nIn       = 0.0                     // number of calls in progress
    var nOut      = 0.0                     // number of calls that finished and hung up
    var nLost     = 0.0                     // number of calls dropped

    addStats (t_a_stat, t_s_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events (MakeCall).
     *  @param call   the entity that arrives, in this case a call
     *  @param delay  the time delay for this event's occurrence
     */
    case class Arrival (call: Entity, delay: Double)
         extends Event (call, this, delay, t_a_stat)
    {
        def occur ()
        {
            if (nArr < nArrivals-1) {
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, CallCenterModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            } // if
            if (nIn == 0) {
                 schedule (Departure (call, call.serviceT))
            } // if
            nArr += 1                                         // update the current state
            if (nIn == 1) nLost += 1 else nIn = 1
        } // occur

    } // Arrival class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure` is a subclass of `Event` for handling departure events (HangUp).
     *  @param call   the entity that departs, in this case a call
     *  @param delay  the time delay for this event's occurrence
     *  @param stat   the statistics collection object for delay times
     */
    case class Departure (call: Entity, delay: Double)
         extends Event (call, this, delay, t_s_stat)
    {
        def occur ()
        {
            leave (call)                            // collects time in sYstem statistics
            nIn   = 0                                         // update the current state
            nOut += 1
        } // occur

    } // Departure class

    //:: start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    report (("nArr", nArr), ("nIn", nIn), ("nLost", nLost), ("nOut", nOut))
    reportStats

} // CallCenterModel class

