
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scala.math.{abs, floor}

import scalation.animation.CommandType._
import scalation.linalgebra.VectorD
import scalation.random.{Discrete, Variate}
import scalation.scala2d.{QCurve, R2}
import scalation.scala2d.Colors._
import scalation.scala2d.QCurve.calcControlPoint
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Transport` class provides a pathway between two other components.
 *  The components in a `Model` conceptually form a 'graph' in which the 'edges'
 *  are `Transport`s and the 'nodes' are other `Component`s.
 *  @param name      the name of the transport
 *  @param from      the first/starting component
 *  @param to        the second/ending component
 *  @param motion    the speed/trip-time to move down the transport
 *  @param isSpeed   whether speed or trip-time is used for motion
 *  @param bend      the bend or curvature of the transport (0 => line)
 *  @param shift1    the x-y shift for the transport's first end-point (from-side)
 *  @param shift2    the x-y shift for the transport's second end-point (to-side)
 */
class Transport (name: String, val from: Component, val to: Component,
                 motion: Variate, isSpeed: Boolean = false,
                 bend: Double = 0.0, shift1: R2 = R2 (0.0, 0.0), shift2: R2 = R2 (0.0, 0.0))
      extends Component
{
    initComponent (name, Array ())

    private val DEBUG     = false                   // debug flag
    private val EPSILON   = 1E-7                    // number close to zero
    private val STEP_SIZE = 5                       // number of units/pixels to move per step

    val curve    = QCurve ()                        // shadow QCurve for computing locations as tokens move along curve
    val (p1, p2) = calcEndPoints ()                 // first and second endpoints
    val pc       = calcControlPoint (p1, p2, bend)  // control point (determines curvature)

    private var onTransport = 0                     // the number of entities/sim-actors on this Transport

    /** Random variate for selecting next direction, defaults to left (.25), straight (.50), right (.25)
     */
    var selector: Variate = Discrete (VectorD (0.25, 0.5, 0.25))

    if (DEBUG) println ("p1, pc, p2 = " + p1 + ", " + pc + ", " + p2)

    if (abs (bend) < EPSILON) {
        curve.setLine (p1, p2)
    } else {
        curve.setLine (p1, p2, bend)
//      curve.setLine (p1, pc, p2)
//      println ("loc = " + curve.getFirst)
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the coordinates of the two end-points.
     */
    private def calcEndPoints (): Tuple2 [R2, R2] =
    {
        val w1 = from.at(2)
        val h1 = from.at(3)
        var x1 = from.at(0) + 0.5 * w1 + shift1.getX ()
        val y1 = from.at(1) + 0.5 * h1 + shift1.getY ()

        val w2 = to.at(2)
        val h2 = to.at(3)
        var x2 = to.at(0) + 0.5 * w2 + shift2.getX ()
        val y2 = to.at(1) + 0.5 * h2 + shift2.getY ()

//      if (x1 < x2) x1 += w1 else x2 += w2
        (R2 (x1, y1), R2 (x2, y2))
    } // calcEndPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display 'this' transport.
     */
    def display ()
    {
//      director.animate (this, CreateEdge, blue, QCurve (), from, to, Array (bend))
        director.animate (this, CreateEdge, blue, QCurve (), from, to,
                          Array (p1.getX (), p1.getY (), pc.getX (), pc.getY (), p2.getX (), p2.getY ()))
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
    /** Jump the entity 'SimActor' down 'this' transport.  Place it in the middle
     *  of the `Transport`s `QCurve` for the entire trip time.
     */
    def jump ()
    {
        val actor    = director.theActor
        val duration = if (isSpeed) curve.length / motion.gen else motion.gen
        tally (duration)
        accum (onTransport)
        onTransport += 1
        trace (this, "jumps for " + duration, actor, director.clock)

        curve.traj = 0.5                         // jump to middle of curve
        val loc = curve.next (DIAM, DIAM)
        director.animate (actor, MoveToken, null, null, Array (loc.x, loc.y))

        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onTransport)
        onTransport -= 1
    } // jump

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity `SimActor` smoothly down 'this' transport.  Repeatedly
     *  move it along the `Transport`s `QCurve`.  Caveat: tokens coordinates
     *  are computed using a shadow `QCurve` (same coordinates as the one that
     *  will be created by the animation engine).
     */
    def move ()
    {
        val actor    = director.theActor
        val duration = if (isSpeed) curve.length / motion.gen else motion.gen
        tally (duration)
        accum (onTransport)
        onTransport += 1
        trace (this, "moves for " + duration, actor, director.clock)

        val steps = (floor (curve.length / STEP_SIZE)).toInt    // number of small steps on QCurve
        curve.setSteps (steps)
        curve.traj = actor.trajectory
        var loc = curve.next (DIAM, DIAM)        // get the starting position for the entity/token
        actor.trajectory = curve.traj

        for (i <- 1 to steps) {
            if (loc != null) {
                director.animate (actor, MoveToken, null, null, Array (loc.x, loc.y))
                actor.schedule (duration / steps.toDouble)
                actor.yieldToDirector ()
//              println ("Transport.move: -- before loc = " + loc)
                curve.traj = actor.trajectory
                loc = curve.next (DIAM, DIAM)
                actor.trajectory = curve.traj
//              println ("Transport.move: -- after  loc = " + loc)
            } // if
        } // for

        accum (onTransport)
        onTransport -= 1
        actor.trajectory = 0.0                   // reset for next transport
    } // move
    
} // Transport class

