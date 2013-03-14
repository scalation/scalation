
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Rectangle
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Resource class provides services to entities (SimActors).
 *  It may or may not have an associated waiting queue.
 *  @param name         the name of the resource
 *  @param line         the line/queue where entities wait
 *  @param units        the number of service units (e.g., bank tellers)
 *  @param serviceTime  the service time distribution
 *  @param at           the location of the resource (x, y, w, h)
 */
class Resource (name: String, line: WaitQueue, units: Int, serviceTime: Variate,
                at: Array [Double])
      extends Component
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width (w) and height (h).
     *  @param name  the name of the resource
     *  @param xy    the (x, y) coordinates for the top-left corner of the resourse.
     */
    def this (name: String, line: WaitQueue, units: Int, serviceTime: Variate,
              xy: Tuple2 [Double, Double])
    {
        this (name, line, units, serviceTime, Array (xy._1, xy._2, 40., 30.))
    } // constructor

    initComponent (name, at)

    /** Number of service units of this Resource currently in use
     */
    private var inUse = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Resource.
     */
    def display ()
    {
        director.animate (this, CreateNode, orange, Rectangle (), at)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the Resource is busy (no units available).
     */
    def busy = inUse == units

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Utilize the Resource for a period of time (models an activity).
     */
    def utilize ()
    {
        if (busy) flaw ("utilize", "no units available")
        val actor    = director.theActor
        val duration = serviceTime.gen
        tally (duration)
        trace (this, "serves for " + duration, actor, actor.time)
        director.animate (actor, MoveToken, null, null, 
                 Array (at(0) + DIAM, at(1) + at(3) / 2. - RAD))
        inUse += 1
        actor.schedule (duration)
        actor.yieldToDirector ()
    } // utilize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Release the Resource after service is finished (also check waiting queue).
     */
    def release ()
    {
        val actor = director.theActor
        trace (this, "releases", actor, actor.time)
        if (line != null && ! line.isEmpty) {
            val waitingActor = line.dequeue ()
            waitingActor.schedule (0.0)
        } // if
        inUse -= 1
    } // release

} // Resource

