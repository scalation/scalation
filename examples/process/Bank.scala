
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Nov  2 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *  @compile scalac -cp ../../classes -d classes Bank.scala
 *  @run     scala -cp ../../classes:classes process.Bank
 */

package process

import scalation.process._
import scalation.random.{Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object defines a particular scenario under which to execute the bank model.
 *  @see scalation.process.ModelTest for another example of test code.
 */
object Bank extends App
{
    val bm = new BankModel ("bank", 100, Uniform (4000, 6000), 2, Uniform (9000, 11000), Uniform (900, 1100))
    bm.simulate ()
} // Bank


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class defines a simple process-interaction model of a bank where service
 *  is provided by one or more tellers.
 *  @param name        the name of the bank model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nUnits      the number of service units (tellers)
 *  @param serviceRV   the service time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class BankModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                 nUnits: Int, serviceRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val entry     = new Source ("entry", this, Customer, nArrivals, iArrivalRV, (120.0, 185.0))
    val tellerQ   = new WaitQueue ("tellerQ", (250.0, 190.0))
    val teller    = new Resource ("teller", tellerQ, nUnits, serviceRV, (320.0, 185.0))
    val door      = new Sink ("door", (460.0, 190.0))
    val toTellerQ = new Transport ("entry2tellerQ", moveRV, entry, tellerQ)
    val toDoor    = new Transport ("teller2door", moveRV, teller, door)

    addComponents (List (entry, tellerQ, teller, door, toTellerQ, toDoor))

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

