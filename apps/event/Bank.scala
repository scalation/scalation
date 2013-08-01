
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Wed Dec 30 14:48:41 EST 2009
 * @see     LICENSE (MIT style license file).
 * @compile scalac -cp ../../classes -d classes Bank.scala
 * @run     scala -cp ../../classes:classes event.Bank
 */

package event

import scalation.event.{CausalLink, Entity, Event, Model}
import scalation.random.{Uniform, Variate}

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object defines a particular scenario under which to execute the bank model.
 * @see scalation.event.ModelTest for another example of test code.
 */
object Bank extends App
{
    new BankModel ("bank", 100, Uniform (4000, 6000), Uniform (3000, 5000))
} // Bank

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This class defines a simple event-scheduling model of a bank where service is
 * provided by one teller.
 * @param name        the name of the simulation model
 * @param nArrivals   the number of arrivals to generate (stopping condition)
 * @param iArrivalRV  the inter-arrival time distribution
 * @param serviceRV   the service time distribution
 */
class BankModel (name: String, nArrivals: Int, arrivalRV: Variate, serviceRV: Variate)
      extends Model (name)
{
    //:: define the state variables for the simulation

    var nArr = 0.    // number of customers that have arrived
    var nIn  = 0.    // number of customers in the bank
    var nOut = 0.    // number of customers that have finished and left the bank

    //:: define the nodes in the event graph (event prototypes)

    val protoArrival   = Arrival (null)       // prototype for all Arrival events
    val protoDeparture = Departure (null)     // prototype for all Departure events

    //:: define the edges in the event graph (causal links between events)

    val aLinks = Array (CausalLink ("link2A", this, () => nArr < nArrivals, protoArrival,
                                    () => Arrival (null), arrivalRV),
                        CausalLink ("link2D", this, () => nIn == 0, protoDeparture,
                                    () => Departure (null), serviceRV))
    val dLinks = Array (CausalLink ("link2D", this, () => nIn > 1, protoDeparture,
                                    () => Departure (null), serviceRV))

    protoArrival.displayLinks (aLinks)
    protoDeparture.displayLinks (dLinks)

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Create a subclass of Event for Arrival events.
     * @param customer  the entity that arrives, in this case a customer
     */
    case class Arrival (customer: Entity)
         extends Event (protoArrival, customer, aLinks, this, Array (150., 200., 50., 50.))
    {
        override def occur ()
        {
            super.occur ()   // handle casual links
            nArr += 1        // update the current state
            nIn  += 1
        } // occur

    } // Arrival class

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Create a subclass of Event for Departure events.
     * @param customer  the entity that departs, in this case a customer
     */
    case class Departure (customer: Entity)
         extends Event (protoDeparture, customer, dLinks, this,  Array (450., 200., 50., 50.))
    {
        override def occur ()
        {
            super.occur ()   // handle casual links
            nIn  -= 1        // update the current state
            nOut += 1
        } // occur

    } // Departure class

    //:: start the simulation, passing the the first priming event and start time

    simulate (Arrival (null), 0.)
    Thread.sleep (2000)
    report (Array (("nArr", nArr), ("nIn", nIn), ("nOut", nOut)))
    report ("Arrival", aLinks)
    report ("Departure", dLinks)

} // BankModel class

