
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Feb 28 14:31:14 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.model.Modelable
import scalation.process._
import scalation.random.{Exponential, Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenter` object defines a particular scenario under which to execute
 *  the call center model.
 *  @see scalation.process.ModelTest for another example of test code.
 */
object CallCenter extends App with Modelable
{
    val lambda   = 6.0       // customer arrival rate (per hour)
    val mu       = 7.5       // customer service rate (per hour)
    val nPhones  = 1         // the number of Phones (servers)
    val maxCalls = 50        // stopping rule: simulate maxCalls
    val aniRatio = 8.0       // the ratio of simulation speed vs. animation speed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `BankModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val bm = new CallCenterModel ("CallCenter", maxCalls, Exponential (HOUR/lambda), nPhones,
                                 Exponential (HOUR/mu), Uniform (.4*MINUTE, .6*MINUTE), aniRatio)
        bm.simulate ()
    } // simulate

    simulate (0.0)

} // CallCenter object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel` class defines a simple process-interaction model of a call
 *  center where service is provided by one or more tele-service representatives.
 *  @param name        the name of the call center model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nUnits      the number of service units (phones)
 *  @param serviceRV   the service time distribution
 *  @param moveRV      the time distribution for motion along transports
 *  @param aniRatio    the ratio of simulation speed vs. animation speed
 */
class CallCenterModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                       nUnits: Int, serviceRV: Variate, moveRV: Variate, aniRatio: Double)
      extends Model (name, aniRatio)
{
    val entry    = Source ("entry", this, Call, 0, nArrivals, iArrivalRV, (200, 290))
    val phone    = Resource ("phone", null, nUnits, serviceRV, (350, 285))
    val hangUp   = Sink ("hangUp", (500, 290))
    val drop     = Sink ("drop", (360, 440))
    val toPhone  = new Transport ("toPhone", entry, phone, moveRV)
    val toHangUp = new Transport ("toHangUp", phone, hangUp, moveRV)
    val toDrop   = new Transport ("toDrop", phone, drop, moveRV)

    addComponent (entry, phone, hangUp, drop, toPhone, toHangUp, toDrop)

    case class Call () extends SimActor ("c", this)
    {
        def act ()
        {
            toPhone.jump ()
            if (phone.busy) {
                toDrop.jump ()
                drop.leave ()
            } else {
                phone.utilize ()
                phone.release ()
                toHangUp.jump ()
                hangUp.leave ()
            } // if
        } // act

    } // Call

} // CallCenterModel class

