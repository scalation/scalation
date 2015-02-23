
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Wed Dec 30 14:48:41 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package apps.event

import scalation.model.Modelable
import scalation.event.{Entity, Event, Model, WaitQueue}
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.queueingnet.MMc_Queue
import scalation.random.{Exponential, Variate}
import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bank` object defines a particular scenario under which to execute the
 *  Bank model.
 *  @see scalation.event.ModelTest for another example of test code.
 */
object Bank extends App with Modelable
{
    val stream     = 1                                  // random number stream (0 to 99)
    val lambda     = 6.0                                // customer arrival rate (per hr)
    val mu         = 7.5                                // customer service rate (per hr)
    val maxCusts   = 10                                 // stopping rule: at maxCusts
    val iArrivalRV = Exponential (HOUR/lambda, stream)  // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)      // service time random variate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of `BankModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        new BankModel ("Bank", maxCusts, iArrivalRV, serviceRV)
    } // simulate

    simulate (0.0)

    //:: verify the results using an M/M/c Queueing Model

    println ("\nVerification ...")
    val mm1 = new MMc_Queue (lambda/HOUR, mu/HOUR)
    mm1.check
    mm1.report

} // Bank object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModel` class defines a simple Event-Scheduling model of a Bank where
 *  service is provided by one teller and models an M/M/1 queue.
 *  @param name        the name of the simulation model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param serviceRV   the service time distribution
 */
class BankModel (name: String, nArrivals: Int, iArrivalRV: Variate, serviceRV: Variate)
      extends Model (name)
{
    val t_a_stat  = new Statistic ("t_a")   // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")   // time in Service statistics
    val waitQueue = WaitQueue (this)        // waiting queue that collects stats

    var nArr      = 0.0                     // number of customers that have arrived
    var nIn       = 0.0                     // number of customers in the bank
    var nOut      = 0.0                     // number of customers that finished & left

    addStats (t_a_stat, t_s_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param customer  the entity that arrives, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Arrival (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_a_stat)
    {
        def occur ()
        {
            if (nArr < nArrivals-1) {
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, BankModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            } // if
            if (nIn == 0) {
                schedule (Departure (customer, customer.serviceT))
            } else {     
                waitQueue += customer               // collects time in Queue statistics
            } // if
            nArr += 1                                        // update the current state
            nIn  += 1
        } // occur

    } // Arrival class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure` is a subclass of `Event` for handling departure events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param customer  the entity that departs, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Departure (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_s_stat)
    {
        def occur ()
        {
            leave (customer)                       // collects time in sYstem statistics
            if (! waitQueue.isEmpty) {
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

    report (("nArr", nArr), ("nIn", nIn), ("nOut", nOut))
    reportStats

} // BankModel class

