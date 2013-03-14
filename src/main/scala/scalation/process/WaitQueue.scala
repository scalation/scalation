
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import collection.mutable.Queue

import scalation.animation.CommandType._
import scalation.scala2d.Rectangle
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The WaitQueue class simply a wrapper for monitoring scala's Queue class.
 *  @param name  the name of the wait-queue
 *  @param at    the location of the wait-queue (x, y, w, h)
 */
class WaitQueue (name: String, at: Array [Double])
      extends Queue [SimActor] with Component
{
    initComponent (name, at)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width (w) and height (h).
     *  @param name  the name of the wait-queue
     *  @param xy    the (x, y) coordinates for the top-left corner of the wait-queue.
     */
    def this (name: String, xy: Tuple2 [Double, Double])
    {
        this (name, Array (xy._1, xy._2, 70., 20.))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation queue to display this WaitQueue.
     */
    def display ()
    {
        director.animate (this, CreateNode, cyan, Rectangle (), at)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait in the queue, recording the wait time.
     */
    def waitIn ()
    {
        val actor = director.theActor
        val timeIn = actor.time
        trace (this, "wait begins", actor, timeIn)
        enqueue (actor)
        actor.yieldToDirector ()
        val timeOut = director.clock
        tally (timeOut - timeIn)
        trace (this, "wait over", actor, timeOut)
    } // waitIn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Do not wait in the queue and record zero waiting time.  Call this method
     *  to get average waiting time for all actors.  If you just want the waiting
     *  time for those who wait, do not call this method.
     */
    def noWait ()
    {
        tally (0.)
    } // noWait

} // WaitQueue class

