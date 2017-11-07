
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import java.util.concurrent.ConcurrentLinkedQueue 

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Source` class is used to periodically inject entities (`SimActors`) into a
 *  running simulation model.  May act as an arrival generator.  Source is both
 *  a simulation `Component` and special `SimActor` and therefore runs in own thread.
 *  @param name          the name of the source
 *  @param director      the director controlling the model
 *  @param makeEntity    the function to make entities of a specified type
 *  @param esubtype      indicator of the subtype of the entities to me made
 *  @param units         the number of entities to make
 *  @param iArrivalTime  the inter-arrival time distribution
 *  @param loc           the location of the source (x, y, w, h)
 */
class Source (name: String, director: Model, makeEntity: () => SimActor, esubtype: Int, units: Int,
              iArrivalTime: Variate, loc: Array [Double])
      extends SimActor (name, director) with Component
{
    initStats (name)
    at = loc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def this (name: String, director: Model, makeEntity: () => SimActor, esubtype: Int, units: Int,
              iArrivalTime: Variate, xy: Tuple2 [Double, Double])
    {
        this (name, director, makeEntity, esubtype, units, iArrivalTime, Array (xy._1, xy._2, 20.0, 20.0))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display 'this' source as a node on the animation canvas.
     */
    def display ()
    {
        director.animate (this, CreateNode, limegreen, Ellipse (), loc)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Source`s as special `SimActor` will act over time to make entities
     *  (other `SimActor`s).
     */
    def act ()
    {
        for (rep <- 1 to director.reps) {                                 // major loop - replications
            actTime = director.clock                                      // set to model start time

            breakable { for (i <- 1 to units) {                           // minor loop - make actors
                if (director.stopped) break                               // terminate source, simulation ended
                val actor = makeEntity ()                                 // make new actor
                actor.mySource = this                                     // actor's source
                actor.subtype  = esubtype                                 // set the entity subtype 
                trace (this, "generates", actor, director.clock)
                director.animate (actor, CreateToken, randomColor (actor.id), Ellipse (),
                         Array (loc(0) + loc(2) + RAD / 2.0, loc(1) + loc(3) / 2.0 - RAD))
                actor.schedule (0.0)

                if (i < units) {
                    val duration = iArrivalTime.gen
                    tally (duration)
                    schedule (duration)
                    yieldToDirector ()                            // yield and wait duration time units
                } // if
            }} // for

            if (rep < director.reps) {
                trace (this, "wait for next rep", director, director.clock)
                yieldToDirector ()                                // yield and wait for next replication
            } // if

        } // for

        trace (this, "terminates", null, director.clock)
        yieldToDirector (true)                                    // yield and terminate
    } // act

} // Source class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Source` companion object provides a builder method for sources.
 */
object Source
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a source using defaults for width 'w' and height 'h'.
     *  @param name          the name of the source
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param esubtype      indicator of the subtype of the entities to me made
     *  @param units         the number of entities to make
     *  @param iArrivalTime  the inter-arrival time distribution
     *  @param xy            the (x, y) coordinates for the top-left corner of the source.
     */
    def apply (name: String, director: Model, makeEntity: () => SimActor, esubtype: Int, units: Int,
              iArrivalTime: Variate, xy: Tuple2 [Int, Int]): Source =
    {
        new Source (name, director, makeEntity, esubtype, units, iArrivalTime,
                    Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sources using defaults for width 'w' and height 'h'.
     *  @param director      the director controlling the model
     *  @param makeEntity    the function to make entities of a specified type
     *  @param units         the number of entities to make
     *  @param xy            the (x, y) coordinates for the top-left corner of the reference source.
     *  @param src           repeated source specific info: name, subtype, distribution, offset
     */
    def group (director: Model, makeEntity: () => SimActor, units: Int, xy: Tuple2 [Int, Int],
               src: Tuple4 [String, Int, Variate, Tuple2 [Int, Int]]*): List [Source] =
    {
        val sourceGroup = new ListBuffer [Source] ()
        for (s <- src) sourceGroup += Source (s._1, director, makeEntity, s._2, units, s._3,
                                             (xy._1 + s._4._1, xy._2 + s._4._2))
        sourceGroup.toList
    } // group

} // Source object

