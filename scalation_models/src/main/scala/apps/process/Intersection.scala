
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
/** The `Intersection` object is used to run the `IntersectionModel` class.
 *  > runMain apps.process.Intersection
 */
object Intersection extends App 
{
    val maxCars = 50
    val rm = new IntersectionModel ("intersection", maxCars, Uniform (4000, 6000),
                                    Sharp (8000), Sharp (6000), Uniform (2900, 3100))
    rm.simulate ()

} // Intersection object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntersectionModel` class simulates an intersection with four traffic lights
 *  (`Gates`) and four roads.  Each road consists of two routes with one in each
 *  direction.  Each `Route` has two lanes (`Transport`s).
 *  @param name        the name of the bank model
 *  @param nArrivals   the number of arrivals to generate (stopping condition)
 *  @param iArrivalRV  the inter-arrival time distribution
 *  @param onTimeRV    the on (green-light) time distribution for North-South traffic
 *  @param offTimeRV   the off (red-light) time distribution for North-South traffic
 *  @param moveRV      the time distribution for motion along transports
 */
class IntersectionModel (name: String, nArrivals: Int, iArrivalRV: Variate,
                         onTimeRV: Variate, offTimeRV: Variate, moveRV: Variate)
      extends Model (name)
{
    val rng  = Bernoulli ()

    val source = List (Source ("sr0", this, Car, 0, nArrivals, iArrivalRV, (800, 250)),
                       Source ("sr1", this, Car, 1, nArrivals, iArrivalRV, (1030, 450)),
                       Source ("sr2", this, Car, 2, nArrivals, iArrivalRV, (830, 650)),
                       Source ("sr3", this, Car, 3, nArrivals, iArrivalRV, (600, 480)))

    val queue = List (WaitQueue ("qu0", (800, 430)),
                      WaitQueue ("qu1", (850, 450)),
                      WaitQueue ("qu2", (830, 500)),
                      WaitQueue ("qu3", (780, 480)))

    val light = List (Gate ("lt0", this, queue(0), nArrivals, onTimeRV, offTimeRV, (800, 480)),
                      Gate ("lt1", this, queue(1), nArrivals, offTimeRV, onTimeRV, (800, 450), true),
                      Gate ("lt2", this, queue(2), nArrivals, onTimeRV, offTimeRV, (830, 450)),
                      Gate ("lt3", this, queue(3), nArrivals, offTimeRV, onTimeRV, (830, 480), true))

    val sink = List (Sink ("sn0", (800, 650)),
                     Sink ("sn1", (600, 450)),
                     Sink ("sn2", (830, 250)),
                     Sink ("sn3", (1030, 480)))

    val road = new ListBuffer [Route] ()
    for (i <- source.indices) {
        road += new Route ("ra" + i, 2, source(i), queue(i), moveRV)
        road += new Route ("rb" + i, 2, light(i),  sink(i),  moveRV)
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
            sink(i).leave ()
        } // act

    } // Car

} // IntersectionModel class

