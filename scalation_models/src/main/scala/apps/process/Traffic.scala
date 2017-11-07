
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Feb 16 21:30:20 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package apps.process

import collection.mutable.ListBuffer

import scalation.process._
import scalation.random.{Bernoulli, Sharp, Uniform, Variate}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Traffic` object is used to run the `TrafficModel` class.
 *  > run-main apps.process.Traffic
 */
object Traffic extends App 
{
    val maxCars = 50
    val rm = new TrafficModel ("traffic", maxCars, Uniform (4000, 6000),
                               Sharp (8000), Sharp (6000), Uniform (2900, 3100))
    rm.simulate ()

} // Traffic object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrafficModel` class simulates an intersection with four traffic lights
 *  (`Gates`) and four roads.  Each road consists of two routes with one in each
 *  direction.  Each `Route` has two lanes (`Transport`s).
 *  @param name       the name of the bank model
 *  @param nArrivals  the number of arrivals to generate (stopping condition)
 *  @param iArrlRV    the inter-arrival time distribution
 *  @param onTimeRV   the on (green-light) time distribution for North-South traffic
 *  @param offTimeRV  the off (red-light) time distribution for North-South traffic
 *  @param moveRV     the time distribution for motion along transports
 */
class TrafficModel (name: String, nArrivals: Int, iArrRV: Variate,
                         onTimeRV: Variate, offTimeRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val rng  = Bernoulli ()

    val source = Source.group (this, Car, nArrivals, (800, 250), ("s1N", 0, iArrRV, (0, 0)),
                                                                 ("s1E", 1, iArrRV, (230, 200)),
                                                                 ("s1S", 2, iArrRV, (30, 400)),
                                                                 ("s1W", 3, iArrRV, (-200, 230)))

    val queue = WaitQueue.group ((800, 430), ("q1N", (0, 0)),
                                             ("q1E", (50, 20)),
                                             ("q1S", (30, 70)),
                                             ("q1W", (-20, 50)))

    val light = Gate.group (this, nArrivals, onTimeRV, offTimeRV, (800, 480), ("l1N", queue(0), (0, 0)),    // traffic from North
                                                                              ("l1E", queue(1), (0, -30)),
                                                                              ("l1S", queue(2), (30, -30)),
                                                                              ("l1W", queue(3), (30, 0)))

    val sink = Sink.group ((830, 250), ("k1N", (0, 0)),
                                       ("k1E", (200, 230)),
                                       ("k1S", (-30, 400)),
                                       ("k1W", (-230, 200)))

    val road = new ListBuffer [Route] ()
    for (i <- source.indices) {
        road += new Route ("ra" + i, 2, source(i), queue(i), moveRV)
        road += new Route ("rb" + i, 2, light(i),  sink((i + 2) % 4),  moveRV)
    } // for

    addComponents (source, queue, light, sink, road.toList)

    case class Car () extends SimActor ("c", this)
    {
        def act ()
        {
            val i = subtype                      // from North (0), East (1), South (2), West (3) 
            val l = rng.igen                     // select lane l
            road(i).lane(l).move ()
            if (light(i).shut) queue(i).waitIn ()
            road(i + 4).lane(l).move ()
            sink((i + 2) % 4).leave ()
        } // act

    } // Car

} // TrafficModel class

