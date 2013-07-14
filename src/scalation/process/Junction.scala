
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Nov 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.process

import scalation.animation.CommandType._
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.util.Monitor.trace

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Junction class provides a connector between two transports.  Since Lines
 *  and QCurves have limitation (e.g., hard to make a loop back), a junction may
 *  be needed.
 *  @param name  the name of the junction
 *  @param from  the incoming transport
 *  @param to    the outgoing transport
 */
class Junction (name: String, from: Transport, to: Transport)
               extends Component
{
    initComponent (name, Array ())    // FIX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Junction.
     */
    def display ()  // FIX
    {
        director.animate (this, CreateEdge, blue, Ellipse (), from, to)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move the entity (SimActor) from the incoming "from" transport to the
     *  outgoing "to" transport.
     */
    def move () // FIX
    {
        val actor = director.theActor
        val duration = 0.0
        tally (duration)
        trace (this, "moves for " + duration, actor, actor.time)
        val fAt = from.at
        val tAt = to.at
        director.animate (actor, MoveToken, null, null,
                 Array ((fAt(0) + tAt(0)) / 2. + DIAM, (fAt(1) + tAt(1)) / 2. + DIAM))
        actor.schedule (duration)
        actor.yieldToDirector ()
    } // move
    
} // Junction

