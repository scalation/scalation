
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Mar  2 20:37:18 EST 2014
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
/** The `Machine` object defines a particular scenario under which to execute the
 *  Machine model.
 *  @see scalation.event.ModelTest for another example of test code.
 *  > runMain apps.event.Machine
 */
object Machine extends App with Modelable
{
    val stream     = 1                                  // random number stream (0 to 99)
    val lambda     = 6.0                                // part arrival rate (per hr)
    val mu         = 7.5                                // part service rate (per hr)
    val maxParts   = 10                                 // stopping rule: at maxParts
    val iArrivalRV = Exponential (HOUR/lambda, stream)  // inter-arrival time random var
    val serviceRV  = Exponential (HOUR/mu, stream)      // service time random variate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of `MachineModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        new MachineModel ("Machine", maxParts, iArrivalRV, serviceRV)
    } // simulate

    simulate (0.0)

} // Machine object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MachineModel` class defines an Event-Scheduling model of a two-stage
 *  manufacturing process, which consists of two machines in series M1 and M2.
 *  @param name        the name of the simulation model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param serviceRV   the service time distribution
 */
class MachineModel (name: String, nArrivals: Int, iArrivalRV: Variate, serviceRV: Variate)
      extends Model (name)
{
    val t_a_stat     = new Statistic ("t_a")      // time between Arrivals statistics
    val t_s1_stat    = new Statistic ("t_s1")     // time in Service statistics for M1
    val t_s2_stat    = new Statistic ("t_s1")     // time in Service statistics for M2
    val waitQueue_M1 = WaitQueue (this, "1", 10)  // waiting queue for M1 that collects stats
    val waitQueue_M2 = WaitQueue (this, "2", 10)  // waiting queue for M2 that collects stats

    var nArr = 0                                  // number of parts that have arrived
    var nIn  = 0                                  // number of parts in the system
    var nM1  = 0                                  // number of parts in service at M1
    var nM2  = 0                                  // number of parts in serice at M1
    var nOut = 0                                  // number of parts that finished & left

    addStats (t_a_stat, t_s1_stat, t_s2_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param part   the entity that arrives, in this case a part
     *  @param delay  the time delay for this event's occurrence
     */
    case class Arrival (part: Entity, delay: Double)
         extends Event (part, this, delay, t_a_stat)
    {
        def occur ()
        {
            if (nArr < nArrivals-1) {
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, MachineModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            } // if
            if (nM1 == 0) {
                nM1 = 1
                schedule (Departure_M1 (part, part.serviceT))
            } else {     
                waitQueue_M1 += part               // collects time in Queue statistics
            } // if
            nArr += 1                                        // update the current state
            nIn  += 1
        } // occur

    } // Arrival class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure_M1` is a subclass of `Event` for handling departure events
     *  from machine M1.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param part   the entity that departs, in this case a part
     *  @param delay  the time delay for this event's occurrence
     */
    case class Departure_M1 (part: Entity, delay: Double)
         extends Event (part, this, delay, t_s1_stat)
    {
        def occur ()
        {
            if (waitQueue_M1.isEmpty) {
                nM1 = 0
            } else {
                val nextService = waitQueue_M1.dequeue ()        // first part in queue
                schedule (Departure_M1 (nextService, nextService.serviceT))
            } // if
            if (nM2 == 0) {
                nM2 = 1
                schedule (Departure_M2 (part, part.serviceT))
            } else {     
                waitQueue_M2 += part               // collects time in Queue statistics
            } // if
        } // occur

    } // Departure_M1 class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure_M2` is a subclass of `Event` for handling departure events.
     *  from machine M2.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param part   the entity that departs, in this case a part
     *  @param delay  the time delay for this event's occurrence
     */
    case class Departure_M2 (part: Entity, delay: Double)
         extends Event (part, this, delay, t_s2_stat)
    {
        def occur ()
        {
            leave (part)                       // collects time in sYstem statistics
            if (waitQueue_M2.isEmpty) {
                nM2 = 0
            } else {
                val nextService = waitQueue_M2.dequeue ()        // first part in queue
                schedule (Departure_M2 (nextService, nextService.serviceT))
            } // if
            nIn  -= 1                                        // update the current state
            nOut += 1
        } // occur

    } // Departure_M2 class

    //:: start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    val nScrap = waitQueue_M1.barred + waitQueue_M2.barred
    report (("nArr", nArr), ("nIn", nIn), ("nOut", nOut), ("nScrap", nScrap))
    reportStats

} // MachineModel class

