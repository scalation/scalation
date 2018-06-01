
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Mon Nov  2 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.model.Modelable
import scalation.process._
import scalation.random.{Exponential, Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bank` object defines a particular scenario under which to execute the
 *  bank model.
 *  @see scalation.process.ModelTest for another example of test code.
 * > runMain apps.process.Bank
 */
object Bank extends App with Modelable
{
    val lambda   = 6.0       // customer arrival rate (per hour)
    val mu       = 7.5       // customer service rate (per hour)
    val nTellers = 1         // the number of bank tellers (servers)
    val maxCusts = 50        // stopping rule: simulate maxCusts customers
    val aniRatio = 8.0       // the ratio of simulation speed vs. animation speed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `BankModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val bm = new BankModel ("Bank", maxCusts, Exponential (HOUR/lambda), nTellers,
                                Exponential (HOUR/mu), Uniform (4*MINUTE, 6*MINUTE), aniRatio)
        bm.simulate ()
    } // simulate

    simulate (0.0)

} // Bank object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModel` class defines a simple process-interaction model of a bank
 *  where service is provided by one or more tellers.
 *  @param name        the name of the bank model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nUnits      the number of service units (tellers)
 *  @param serviceRV   the service time distribution
 *  @param moveRV      the time distribution for motion along transports
 *  @param aniRatio    the ratio of simulation speed vs. animation speed
 */
class BankModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                 nUnits: Int, serviceRV: Variate, moveRV: Variate, aniRatio: Double)
      extends Model (name, 1, true, aniRatio)
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
            toTellerQ.move ()
            if (teller.busy) tellerQ.waitIn () else tellerQ.noWait ()
            teller.utilize ()
            teller.release ()
            toDoor.move ()
            door.leave ()
        } // act

    } // Customer

} // BankModel class

