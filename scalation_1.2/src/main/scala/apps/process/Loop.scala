
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Feb 16 21:30:20 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.process._
import scalation.random.{Bernoulli, Sharp, Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Loop` object is used to run the `LoopModel` class.
 *  > run-main apps.process.Loop
 */
object Loop extends App 
{
    val maxCars = 50
    val rm = new LoopModel ("road", maxCars, Uniform (4000, 6000), Uniform (2900, 3100))
    rm.simulate ()

} // Loop object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LoopModel` class simulates a two-lane road in two directions, i.e., it
 *  has 2 West-bound lanes and 2 East-bound lanes.  It used a composite class called
 *  `Route`, which will have a `Transport` for each lane.
 *  @param name        the name of the loop model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class LoopModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val rng  = Bernoulli ()

    val src1 = Source ("src1", this, Car1, 0, nArrivals, iArrivalRV, (400, 500))
    val src2 = Source ("src2", this, Car2, 0, nArrivals, iArrivalRV, (400, 420))
    val jun1 = Junction ("jun1", this, Sharp (10.0), (850, 475))
    val jun2 = Junction ("jun2", this, Sharp (10.0), (870, 475))
    val snk1 = Sink ("snk1", (400, 450))
    val snk2 = Sink ("snk2", (400, 530))

    val road1a = new Route ("road1a", 2, src1, jun1, moveRV, false, 0.0, -0.8)
    val road1b = new Route ("road1b", 2, jun1, snk1, moveRV, false, 0.0, 0.8)
    val road2a = new Route ("road2a", 2, src2, jun2, moveRV, false, 0.0, 0.8)
    val road2b = new Route ("road2b", 2, jun2, snk2, moveRV, false, 0.0, -0.8)

    addComponent (src1, src2, jun1, jun2, snk1, snk2, road1a, road1b, road2a, road2b)

    case class Car1 () extends SimActor ("c1", this)      // for West-bound cars
    {
        def act ()
        {
            val i = rng.igen
            road1a.lane(i).move ()
            jun1.jump ()
            road1b.lane(i).move ()
            snk1.leave ()
        } // act

    } // Car1

    case class Car2 () extends SimActor ("c2", this)      // for East-bound cars
    {
        def act ()
        {
            val i = rng.igen
            road2a.lane(i).move ()
            jun2.jump ()
            road2b.lane(i).move ()
            snk2.leave ()
        } // act

    } // Car2

} // LoopModel class

