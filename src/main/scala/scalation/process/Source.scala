
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import actors.Actor

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Source class is used to periodically inject entities (SimActors) into a
 *  running simulation model.  May act as an arrival generator.  Source is both
 *  a simulation Component and special SimActor and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param makeEntity    the function to make entities of a specified type
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param at            the location of the source (x, y, w, h)
 */
class Source (name: String, director: Model, makeEntity: () => SimActor, units: Int,
              iArrivalTime: Variate, at: Array [Double])
      extends SimActor (name, director) with Component
{
    initStats (name)
    setAt (at)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width (w) and height (h).
     *  @param name  the name of the source
     *  @param xy    the (x, y) coordinates for the top-left corner of the source.
     */
    def this (name: String, director: Model, makeEntity: () => SimActor, units: Int,
              iArrivalTime: Variate, xy: Tuple2 [Double, Double])
    {
        this (name, director, makeEntity, units, iArrivalTime, Array (xy._1, xy._2, 30., 30.))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this source as a node on the animation canvas.
     */
    def display ()
    {
        director.animate (this, CreateNode, limegreen, Ellipse (), at)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The Sources as a special SimActor will act over time to make entities
     *  (other SimActors).
     */
    def act ()
    {
        for (i <- 1 to units) {
            val actor = makeEntity ()
            trace (this, "generates", actor, director.clock)
            director.animate (actor, CreateToken, randomColor (actor.id), Ellipse (),
                     Array (at(0) + at(2) + RAD / 2., at(1) + at(3) / 2. - RAD))
            actor.schedule (0.0)
            if (i < units) {
                val duration = iArrivalTime.gen
                tally (duration)
                schedule (duration)
                yieldToDirector ()
            } else {
                yieldToDirector (true)
            } // if
            // Thread.sleep (500)
        } // for
    } // act

} // Source

