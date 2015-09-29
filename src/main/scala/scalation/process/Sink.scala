
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import collection.mutable.ListBuffer

import scalation.animation.CommandType._
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sink` class is used to terminate entities (SimActors) when they are finished.
 *  @param name  the name of the sink
 *  @param at    the location of the sink (x, y, w, h)
 */
class Sink (name: String, at: Array [Double])
      extends Component
{
    initComponent (name, at)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width (w) and heigth (h).
     *  @param name  the name of the sink
     *  @param xy    the (x, y) coordinates for the top-left corner of the sink.
     */
    def this (name: String, xy: Tuple2 [Double, Double])
    {
        this (name, Array (xy._1, xy._2, 20.0, 20.0))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Sink.
     */
    def display ()
    {
        director.animate (this, CreateNode, darkred, Ellipse (), at)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Leave the model, effectively terminating the entity (SimActor).
     */
    def leave ()
    {
        val actor = director.theActor
        tally (director.clock - actor.arrivalT)
        trace (this, "terminates", actor, director.clock)
        director.animate (actor, MoveToken, null, null, Array (at(0) + DIAM, at(1) + at(3) / 2.0 - RAD))

        actor.yieldToDirector (true)          // yield and terminate
    } // leave
    
} // Sink class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sink` companion object provides a builder method for sinks.
 */
object Sink
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a sink using defaults for width (w) and heigth (h).
     *  @param name  the name of the sink
     *  @param xy    the (x, y) coordinates for the top-left corner of the sink.
     */
    def apply (name: String, xy: Tuple2 [Int, Int]): Sink =
    {
        new Sink (name, Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
    } // apply

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sinks using defaults for width (w) and heigth (h).
     *  @param xy   the (x, y) coordinates for the top-left corner of the reference sink.
     *  @param snk  repeated sink specific info: name, offset
     */
    def group (xy: Tuple2 [Int, Int],
               snk: Tuple2 [String, Tuple2 [Int, Int]]*): List [Sink] =
    {
        val sinkGroup = new ListBuffer [Sink] ()
        for (s <- snk) sinkGroup += Sink (s._1, (xy._1 + s._2._1, xy._2 + s._2._2))
        sinkGroup.toList
    } // group

} // Sink object

