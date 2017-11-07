
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Nov  2 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.model.Modelable
import scalation.process._
import scalation.random.{Exponential, Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Machine` object defines a particular scenario under which to execute the
 *  machine model.
 *  > run-main apps.process.Machine
 */
object Machine extends App with Modelable
{
    val lambda   = 6.0       // part arrival rate (per hour)
    val mu       = 7.5       // part service rate (per hour)
    val nUnits   = 1         // the number of machines at each stage/station
    val maxParts = 50        // stopping rule: simulate maxParts parts

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation of the `MachineModel`.
     *  @param startTime  the start time for the simulation
     */
    def simulate (startTime: Double)
    {
        val mm = new MachineModel ("Machine", maxParts, Exponential (HOUR/lambda), nUnits, Exponential (HOUR/mu),
                                   Uniform (30*MINUTE-10, 30*MINUTE+10))
        mm.simulate ()
    } // simulate

    simulate (0.0)

} // Machine object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MachineModel` class defines a process-interaction model of a simple
 *  two-stage manufacturing process.
 *  @param name        the name of the machine model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param nUnits      the number of service units
 *  @param serviceRV   the service time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class MachineModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                    nUnits: Int, serviceRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val entry       = Source ("entry", this, Part, 0, nArrivals, iArrivalRV, (100, 390))
    val machine1Q   = WaitQueue ("machine1Q", (330, 390), 3)
    val machine1    = Resource ("machine1", machine1Q, nUnits, serviceRV, (350, 385))
    val machine2Q   = WaitQueue ("machine2Q", (530, 390), 3)
    val machine2    = Resource ("machine2", machine2Q, nUnits, serviceRV, (550, 385))
    val ship        = Sink ("ship", (800, 390))
    val scrap       = Sink ("scrap", (430, 600))
    val toMachine1Q = new Transport ("toMachine1Q", entry, machine1Q, moveRV)
    val toMachine2Q = new Transport ("toMachine2Q", machine1, machine2Q, moveRV)
    val toShip      = new Transport ("toShip", machine2, ship, moveRV)
    val toScrap1    = new Transport ("toScrap1", machine1Q, scrap, moveRV)
    val toScrap2    = new Transport ("toScrap2", machine2Q, scrap, moveRV)

    addComponent (entry, machine1Q, machine1, machine2Q, machine2, ship, scrap,
                  toMachine1Q, toMachine2Q, toShip, toScrap1, toScrap2)

    case class Part () extends SimActor ("p", this)
    {
        def act ()
        {
            toMachine1Q.move ()
            if (machine1.busy) {
                if (! machine1Q.waitIn ()) {
                   toScrap1.move ()
                   scrap.leave ()
                   return
                 } // if
            } else {
                machine1Q.noWait ()
            } // if
            machine1.utilize ()
            machine1.release ()

            toMachine2Q.move ()
            if (machine2.busy) {
                if (! machine2Q.waitIn ()) {
                   toScrap2.move ()
                   scrap.leave ()
                   return
                 } // if
            } else {
                machine2Q.noWait ()
            } // if
            machine2.utilize ()
            machine2.release ()

            toShip.move ()
            ship.leave ()
        } // act

    } // Part

} // MachimeModel class

