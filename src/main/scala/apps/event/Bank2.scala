
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Dec 30 14:48:41 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package apps.event

import scalation.event.{CausalLink, Entity, Event, EventNode, Model, WaitQueue}
import scalation.model.Modelable
import scalation.random.{Exponential, Variate}
import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bank2` object defines a particular scenario under which to execute the
 *  Bank  model.  It is the same as `Bank`, except that causal links are added
 *  to enable the model to be animated as an Event Graph.
 *  @see scalation.event.ModelTest for another example of test code.
 *  > run-main apps.event.Bank2
 */
object Bank2 extends App with Modelable
{
    val stream     = 1                                  // random number stream (0 to 99)
    val lambda     = 6.0                                // customer arrival rate (per hour)
    val mu         = 7.5                                // customer service rate (per hour)
    val maxCusts   = 10                                 // stopping rule: simulate maxCusts
    val iArrivalRV = Exponential (HOUR/lambda, stream)  // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)      // service time random variate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of `BankModel2`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        new BankModel2 ("Bank2", maxCusts, iArrivalRV, serviceRV)
    } // simulate

    simulate (0.0)

} // Bank2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModel2` class defines a simple Event Graph model of a Bank where
 *  service is provided by one teller and models an M/M/1 queue.
 *  @param name        the name of the simulation model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param serviceRV   the service time distribution
 */
class BankModel2 (name: String, nArrivals: Int, iArrivalRV: Variate, serviceRV: Variate)
      extends Model (name, true)                      // true => turn on animation
{
    val t_a_stat  = new Statistic ("t_a")             // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")             // time in Service statistics
    val waitQueue = WaitQueue (this)                  // waiting queue that collects stats
    val aLoc      = Array (150.0, 200.0, 50.0, 50.0)  // Arrival event node location
    val dLoc      = Array (450.0, 200.0, 50.0, 50.0)  // Departure event node location

    val aProto    = new EventNode (this, aLoc)        // prototype for all Arrival events
    val dProto    = new EventNode (this, dLoc)        // prototype for all Departure events

    val aLink = Array (CausalLink ("l_A2A", this, () => nArr < nArrivals-1, aProto),
                       CausalLink ("l_A2D", this, () => nIn == 0,           dProto))
    val dLink = Array (CausalLink ("l_D2D", this, () => nIn > 1,            dProto))

    var nArr   = 0.0                           // number of customers that have arrived
    var nIn    = 0.0                           // number of customers in the bank
    var nOut   = 0.0                           // number of customers that finished & left

    aProto.displayLinks (aLink)
    dProto.displayLinks (dLink)

    addStats (t_a_stat, t_s_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `EventNode` for handling arrival events.
     *  @param customer  the entity that arrives, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Arrival (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_a_stat, aProto)
    {
        def occur ()
        {
            if (aLink(0).condition ()) {
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, BankModel2.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            } // if
            if (aLink(1).condition ()) {
                schedule (Departure (customer, customer.serviceT))
            } else {
                waitQueue += customer               // collects time in Queue statistics
            } // if
            nArr += 1                                        // update the current state
            nIn  += 1
        } // occur

    } // Arrival class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure` is a subclass of `EventNode` for handling departure events.
     *  @param customer  the entity that arrives, in this case a bank customer
     *  @param delay  the time delay for this event's occurrence
     */
    case class Departure (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_s_stat, dProto)
    {
        def occur ()
        {

            leave (customer)                       // collects time in sYstem statistics
            if (dLink(0).condition ()) {
                val nextService = waitQueue.dequeue ()        // first customer in queue
                schedule (Departure (nextService, nextService.serviceT))
            } // if
            nIn  -= 1                                        // update the current state
            nOut += 1
        } // occur

    } // Departure class

    //:: start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    Thread.sleep (20000)                                      // wait on animation trace
    report (("nArr", nArr), ("nIn", nIn), ("nOut", nOut))
    reportStats

} // BankModel2 class

