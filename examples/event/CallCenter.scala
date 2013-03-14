
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Fri Aug 19 17:10:23 EDT 2011
 * @see     LICENSE (MIT style license file).
 * @compile scalac -cp ../../classes -d classes CallCenter.scala
 * @run     scala -cp ../../classes:classes event.CallCenter
 */

package event

import scalation.event.{CausalLink, Entity, Event, Model}
import scalation.random.{Exponential, Variate}

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object defines a particular scenario under which to execute the CallCenter model.
 * @see scalation.event.ModelTest for another example of test code.
 */
object CallCenter extends App
{
    val lambda   = 6.0       // call arrival rate (per hour)
    val mu       = 7.50      // call service rate (per hour)
    val maxCalls = 1000      // stopping rule: simulate maxCalls calls
    new CallCenterModel ("CallCenter", maxCalls, Exponential (60./lambda), Exponential (60./mu))
} // CallCenter


/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This class defines a simple event-scheduling model of a CallCenter where service
 * is provided by a tele-service representative.
 * @param name        the name of the simulation model
 * @param nArrivals   the number of arrivals to generate (stopping condition)
 * @param iArrivalRV  the inter-arrival time distribution (Random Variable)
 * @param serviceRV   the service time distribution (Random Variable)
 */
class CallCenterModel (name: String, nArrivals: Int, arrivalRV: Variate, serviceRV: Variate)
      extends Model (name)
{
    //:: define the state variables for the simulation

    var nArr  = 0.    // number of calls that have arrived
    var nIn   = 0.    // number of calls in progress
    var nOut  = 0.    // number of calls that have finished and hung up
    var nLost = 0.    // number of calls dropped

    //:: define the nodes in the event graph (event prototypes)

    val protoArrival   = Arrival (null)       // prototype for all Arrival events
    val protoDeparture = Departure (null)     // prototype for all Departure events

    //:: define the edges in the event graph (causal links between events)

    val aLinks = Array (CausalLink ("link2A", this, () => nArr < nArrivals - 1, protoArrival,
                                    () => Arrival (null), arrivalRV),
                        CausalLink ("link2D", this, () => nIn == 0, protoDeparture,
                                    () => Departure (null), serviceRV))

    protoArrival.displayLinks (aLinks)

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Create a subclass of Event for Arrival events (MakeCall).
     * @param call  the entity that arrives, in this case a call
     */
    case class Arrival (call: Entity)
         extends Event (protoArrival, call, aLinks, this, Array (150., 200., 50., 50.))
    {
        override def occur ()
        {
            super.occur ()   // handle casual links
            nArr += 1        // update the current state
            if (nIn == 1) nLost += 1 else nIn = 1
        } // occur

    } // Arrival class

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Create a subclass of Event for Departure events. (HangUp)
     * @param call  the entity that departs, in this case a call
     */
    case class Departure (call: Entity)
         extends Event (protoDeparture, call, null, this,  Array (450., 200., 50., 50.))
    {
        override def occur ()
        {
            super.occur ()   // handle casual links
            nIn   = 0        // update the current state
            nOut += 1
        } // occur

    } // Departure class

    //:: start the simulation, passing the first priming event and the start time

    simulate (Arrival (null), 0.)
    Thread.sleep (20000)
    report (Array (("nArr", nArr), ("nIn", nIn), ("nLost", nLost), ("nOut", nOut)))
    report ("Arrival", aLinks)

} // CallCenterModel class

