
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Aug 19 17:10:23 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package apps.event

import scalation.model.Modelable
import scalation.event.{CausalLink, Entity, Event, EventNode, Model}
import scalation.random.{Exponential, Variate}
import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenter2` object defines a particular scenario under which to execute
 *  the Call Center model.  It is the same as `CallCenter`, except that causal links
 *  are added to enable the model to be animated as an Event Graph.
 *  @see scalation.event.ModelTest for another example of test code.
 */
object CallCenter2 extends App with Modelable
{
    val stream     = 1                                  // random number stream (0 to 99)
    val lambda     = 6.0                                // call arrival rate (per hour)
    val mu         = 7.5                                // call service rate (per hour)
    val maxCalls   = 10                                 // stopping rule: at maxCalls
    val iArrivalRV = Exponential (HOUR/lambda, stream)  // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)      // service time random variate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of `CallCenterModel2`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        new CallCenterModel2 ("CallCenter2", maxCalls, iArrivalRV, serviceRV)
    } // simulate

    simulate (0.0)

} // CallCenter2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel2` class defines a simple Event Graph model of a
 *  Call Center where service is provided by one tele-service representative and
 *  models an M/M/1/1 queue.
 *  @param name        the name of the simulation model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution (Random Variate)
 *  @param serviceRV   the service time distribution (Random Variate)
 */
class CallCenterModel2 (name: String, nArrivals: Int, iArrivalRV: Variate,
                                                      serviceRV: Variate)
      extends Model (name, true)                      // true => turn on animation
{
    val t_a_stat  = new Statistic ("t_a")             // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")             // time in Service statistics
    val aLoc      = Array (150.0, 200.0, 50.0, 50.0)  // Arrival node location
    val dLoc      = Array (450.0, 200.0, 50.0, 50.0)  // Departure node location

    val aProto    = new EventNode (this, aLoc)        // prototype for all Arrival events
    val dProto    = new EventNode (this, dLoc)        // prototype for all Departure events

    val aLink = Array (CausalLink ("l_A2A", this, () => nArr < nArrivals-1, aProto),
                       CausalLink ("l_A2D", this, () => nIn == 0,           dProto))

    var nArr   = 0.0                           // number of calls that have arrived
    var nIn    = 0.0                           // number of calls in progress
    var nOut   = 0.0                           // number of calls that finished and hung up
    var nLost  = 0.0                           // number of calls dropped

    aProto.displayLinks (aLink)

    addStats (t_a_stat, t_s_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `EventNode` for handling arrival events (MakeCall).
     *  @param call   the entity that arrives, in this case a phone call
     *  @param delay  the time delay for this event's occurrence
     */
    case class Arrival (call: Entity, delay: Double)
         extends Event (call, this, delay, t_a_stat, aProto)
    {
        override def occur ()
        {
            if (aLink(0).condition ()) {
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, CallCenterModel2.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            } // if
            if (aLink(1).condition ()) {
                schedule (Departure (call, call.serviceT))
            } // if
            nArr += 1                                        // update the current state
            if (nIn == 1) nLost += 1 else nIn = 1
        } // occur

    } // Arrival class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure is a subclass of `EventNode` for handling departure events (HangUp).
     *  @param call   the entity that arrives, in this case a phone call
     *  @param delay  the time delay for this event's occurrence
     */
    case class Departure (call: Entity, delay: Double)
         extends Event (call, this, delay, t_s_stat, dProto)
    {
        override def occur ()
        {
            leave (call)                           // collects time in sYstem statistics
            nIn   = 0                                        // update the current state
            nOut += 1
        } // occur

    } // Departure class

    //:: start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    Thread.sleep (20000)                                      // wait on animation trace
    report (("nArr", nArr), ("nIn", nIn), ("nLost", nLost), ("nOut", nOut))
    reportStats

} // CallCenterModel2 class

