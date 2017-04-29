
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Feb 16 21:30:20 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import scalation.process._
import scalation.random.{Bernoulli, Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Road` object is used to run the `RoadModel` class.
 *  > run-main apps.process.Road
 */
object Road extends App 
{
    val maxCars = 50
    val rm = new RoadModel ("road", maxCars, Uniform (4000, 6000), Uniform (2900, 3100))
    rm.simulate ()

} // Road object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoadModel` class simulates a two-lane road in two directions, i.e., it
 *  has 2 West-bound lanes and 2 East-bound lanes.  It used a composite class called
 *  `Route`, which will have a `Transport` for each lane.
 *  @param name        the name of the bank model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param moveRV      the time distribution for motion along transports
 */
class RoadModel (name: String, nArrivals: Int, iArrivalRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val rng  = Bernoulli ()
    val src1 = Source ("src1", this, Car1, 0, nArrivals, iArrivalRV, (400, 220))
    val src2 = Source ("src2", this, Car2, 0, nArrivals, iArrivalRV, (100, 260))
    val snk1 = Sink ("snk1", (100, 220))
    val snk2 = Sink ("snk2", (400, 260))

    val road1 = new Route ("road1", 2, src1, snk1, moveRV)
    val road2 = new Route ("road2", 2, src2, snk2, moveRV)

    addComponent (src1, src2, snk1, snk2, road1, road2)

    case class Car1 () extends SimActor ("c1", this)      // for West-bound cars
    {
        def act ()
        {
            val i = rng.igen
            road1.lane(i).move ()
            snk1.leave ()
        } // act

    } // Car1

    case class Car2 () extends SimActor ("c2", this)      // for East-bound cars
    {
        def act ()
        {
            val i = rng.igen
            road2.lane(i).move ()
            snk2.leave ()
        } // act

    } // Car2

} // RoadModel class

