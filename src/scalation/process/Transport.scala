
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import math.abs

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.{QCurve, R2}
import scalation.scala2d.Colors._
import scalation.scala2d.QCurveCalc.computeControlPoint
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Transport class provides a pathway between two other components.
 *  The Components in a Model conceptually form a 'graph' in which the 'edges'
 *  are Transport objects and the 'nodes' are other Component objects.
 *  @param name      the name of the transport
 *  @param tripTime  the time to move down the transport
 *  @param from      the starting component
 *  @param to        the ending component
 *  @param bend      the bend or curvature of the transport (0 => line)
 */
class Transport (name: String, tripTime: Variate, from: Component, to: Component, bend: Double = 0.)
      extends Component
{
    initComponent (name, Array ())

    /** A number close to zero
     */
    private val EPSILON = 1E-7

    /** A shadow QCurve for computing locations as tokens move along the curve
     */
    private val curve = QCurve ()

    {
        val w1 = from.at(2)
        val h1 = from.at(3)
        var x1 = from.at(0)
        val y1 = from.at(1) + h1 / 2.
        
        val w2 = to.at(2)
        val h2 = to.at(3)
        var x2 = to.at(0)
        val y2 = to.at(1) + h2 / 2.

        if (x1 < x2) x1 += w1 else x2 += w2

        val p1 = R2 (x1, y1)
        val p2 = R2 (x2, y2)
        val pc = computeControlPoint (p1, p2, bend)

        if (abs (bend) < EPSILON) {
//          println (" 1. Transport.constructor: p1 = " + p1 + ", p2 = " + p2 + ", pc = " + pc)
            curve.setLine (p1, p2)
        } else {
//          println (" 2. Transport.constructor: p1 = " + p1 + ", p2 = " + p2 + ", pc = " + pc)
            curve.setLine (p1, p2, bend)
//          curve.setLine (p1, pc, p2)
//          println ("loc = " + curve.getFirst)
        } // if
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Transport.
     */
    def display ()
    {
        director.animate (this, CreateEdge, blue, QCurve (), from, to, Array (bend))
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Give the location of the curve to be its starting point.
     */
    override def at: Array [Double] =
    {
       val p1 = curve.getFirst
       Array (p1.x, p1.y)
    } // at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Jump the entity (SimActor) down this Transport.  Place it in the middle
     *  of the Transport/Edge/QCurve for the entire trip time.
     */
    def jump ()
    {
        val actor    = director.theActor
        val duration = tripTime.gen
        tally (duration)
        trace (this, "jumps for " + duration, actor, actor.time)

        val fAt = from.at
        val tAt = to.at
        director.animate (actor, MoveToken, null, null,
                 Array ((fAt(0) + tAt(0)) / 2. + DIAM, (fAt(1) + tAt(1)) / 2. + DIAM))
        actor.schedule (duration)
        actor.yieldToDirector ()
    } // jump

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (SimActor) smoothly down this Transport.  Repeatedely
     *  move it along the Transport/Edge/QCurve.  Caveat: tokens coordinates
     *  are computed using a shadow QCurve (same coordinates as the one that
     *  will be created by the animation engine).
     */
    def move ()
    {
        val actor    = director.theActor
        val duration = tripTime.gen
        tally (duration)
        trace (this, "moves for " + duration, actor, actor.time)

        val steps = 30                        // number of small steps on QCurve
        curve.setSteps (steps)
        var loc = curve.next (DIAM, DIAM)     // get the starting position for the entity/token
        for (i <- 1 to steps) {
            if (loc != null) {
                director.animate (actor, MoveToken, null, null, Array (loc.x, loc.y))
                actor.schedule (duration / steps.asInstanceOf [Double])
                actor.yieldToDirector ()
//              println ("Transport.move: -- before loc = " + loc)
                loc = curve.next (DIAM, DIAM)
//              println ("Transport.move: -- after  loc = " + loc)
            } // if
		
        } // for
    } // move
    
} // Transport

