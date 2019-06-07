
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Feb 28 14:31:14 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.model.Modelable
import scalation.process._
import scalation.random.{Exponential, Uniform, Variate}
import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bank2` object defines a particular scenario under which to execute the
 *  bank model.  This version reduces the impact of transports on simulation by
 *  (1) using the transport's 'jump' method rather than its 'move' method and
 *  (2) reducing the time through the transport by an order of magnitude.
 *  It also has animation turned off.
 *  @see scalation.process.ModelTest for another example of test code.
 *  > runMain apps.process.Bank2
 */
object Bank2 extends App with Modelable
{
    val lambda   = 6.0       // customer arrival rate (per hour)
    val mu       = 7.5       // customer service rate (per hour)
    val nTellers = 1         // the number of bank tellers (servers)
    val maxCusts = 100       // stopping rule: simulate maxCusts customers

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `BankModel2`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val bm = new BankModel2 ("Bank2", maxCusts, Exponential (HOUR/lambda), nTellers,
                                 Exponential (HOUR/mu), Uniform (.4*MINUTE, .6*MINUTE))
        time { bm.simulate (); bm.complete () }
    } // simulate

    simulate (0.0)
    Model.shutdown ()

} // Bank2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModel2` class defines a simple process-interaction model of a bank
 *  where service is provided by one or more tellers.
 *  @param name        the name of the bank model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nUnits      the number of service units (tellers)
 *  @param serviceRV   the service time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class BankModel2 (name: String, nArrivals: Int, iArrivalRV: Variate, nUnits: Int,
                  serviceRV: Variate, moveRV: Variate)
      extends Model (name, 2, false)
{
    val entry     = Source ("entry", this, Customer, 0, nArrivals, iArrivalRV, (100, 290))
    val tellerQ   = WaitQueue ("tellerQ", (330, 290))
    val teller    = Resource ("teller", tellerQ, nUnits, serviceRV, (350, 285))
    val door      = Sink ("door", (600, 290))
    val toTellerQ = new Transport ("toTellerQ", entry, tellerQ, moveRV)
    val toDoor    = new Transport ("toDoor", teller, door, moveRV)

    addComponent (entry, tellerQ, teller, door, toTellerQ, toDoor)

    case class Customer () extends SimActor ("c", this)
    {
        def act ()
        {
            toTellerQ.jump ()
            if (teller.busy) tellerQ.waitIn () else tellerQ.noWait ()
            teller.utilize ()
            teller.release ()
            toDoor.jump ()
            door.leave ()
        } // act

    } // Customer

} // BankModel2 class

