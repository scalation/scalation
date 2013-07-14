
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
class Resource (name: String, line: WaitQueue, private var units: Int, serviceTime: Variate,
                at: Array [Double])
      extends Component
{
    if (units < 0) flaw ("constructor", "resource may not have negative units")

    initComponent (name, at)

    /** Number of service units of this Resource currently in use (0 ... units)
     */
    private var inUse = 0

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Change the number of units in this resourse (e.g., add a teller).
     *  @param dUnits  the number of units to add (+ve) or remove (-ve)
     */
    def changeUnits (dUnits: Int)
    {
        val actor = director.theActor
        trace (this, "change units by " + dUnits, actor, actor.time)
        if (units + dUnits >= inUse) units += dUnits
        else flaw ("changeUnits", "attempt to reduce the resource to negative units")
    } // changeUnits

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
     *  The duration (service time) is randomly generated according to the
     *  Resource's service time distribution.
     */
    def utilize ()
    {
        if (busy) flaw ("utilize", "no units available")
        val actor    = director.theActor
        val duration = serviceTime.gen                   // randomly generate
        tally (duration)
        trace (this, "serves for " + duration, actor, actor.time)
        director.animate (actor, MoveToken, null, null, 
                 Array (at(0) + DIAM, at(1) + at(3) / 2. - RAD))
        if (inUse < units) inUse += 1
        else flaw ("utilize", "no units left")
        actor.schedule (duration)
        actor.yieldToDirector ()
    } // utilize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Utilize the Resource for a given period of time (models an activity).
     *  @param duration  the given service time
     */
    def utilize (duration: Double)
    {
        if (busy) flaw ("utilize", "no units available")
        val actor    = director.theActor
        tally (duration)
        trace (this, "serves for " + duration, actor, actor.time)
        director.animate (actor, MoveToken, null, null, 
                 Array (at(0) + DIAM, at(1) + at(3) / 2. - RAD))
        if (inUse < units) inUse += 1
        else flaw ("utilize", "no units left")
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
        if (inUse > 0) inUse -= 1
        else flaw ("release", "no units currently in use")
    } // release

} // Resource

