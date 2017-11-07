
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Nov 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scala.collection.mutable.ListBuffer

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Junction` class provides a connector between two `Transport`s/`Route`s.
 *  Since `Lines` and `QCurves` have limitations (e.g., hard to make a loop back),
 *  a junction may be needed.
 *  @param name      the name of the junction
 *  @param director  the director controlling the model
 *  @param jTime     the jump-time through the junction
 *  @param at        the location of the junction (x, y, w, h)
 */
class Junction (name: String, director: Model, jTime: Variate, at: Array [Double])
      extends Component
{
    initComponent (name, at)

    private var onJunction = 0        // the number of entities/sim-actors on this Junction

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name      the name of the junction
     *  @param director  the director controlling the model
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the junction
     */
    def this (name: String, director: Model, jTime: Variate, xy: Tuple2 [Double, Double])
    {
        this (name, director, jTime, Array (xy._1, xy._2, 10.0, 10.0))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Junction.
     */
    def display ()
    {
        director.animate (this, CreateNode, purple, Ellipse (), at)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Jump the entity `SimActor` from the incoming "from" transport to the
     *  the middle of the junction.
     */
    def jump ()
    {
        val actor    = director.theActor
        val duration = jTime.gen
        tally (duration)
        accum (onJunction)
        onJunction += 1
        trace (this, "jump for " + duration, actor, director.clock)

        director.animate (actor, MoveToken, null, null, Array (at(0) + RAD, at(1) + RAD))
        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onJunction)
        onJunction -= 1
    } // jump
    
} // Junction class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Junction` companion object provides a builder method for sinks.
 */
object Junction
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a junction using defaults for width 'w' and height 'h'.
     *  @param name      the name of the junction
     *  @param director  the director controlling the model
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the junction
     */
    def apply (name: String, director: Model, jTime: Variate, xy: Tuple2 [Int, Int]): Junction =
    {
        new Junction (name, director, jTime,
                      Array (xy._1.toDouble, xy._2.toDouble, 10.0, 10.0))
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related junctions using defaults for width 'w' and height 'h'.
     *  @param director  the director controlling the model
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the reference junction.
     *  @param jnt       repeated junction specific info: name, offset
     */
    def group (director: Model, jTime: Variate, xy: Tuple2 [Int, Int],
               jnt: Tuple2 [String, Tuple2 [Int, Int]]*): List [Junction] =
    {
        val junctionGroup = new ListBuffer [Junction] ()
        for (j <- jnt) junctionGroup += Junction (j._1, director, jTime,
                                                 (xy._1 + j._2._1, xy._2 + j._2._2))
        junctionGroup.toList
    } // group

} // Junction object

