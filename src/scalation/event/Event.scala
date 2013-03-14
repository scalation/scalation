
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import scalation.animation.CommandType._
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._
import scalation.util.PQItem

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class provides facilities for defining simulation events.  Subclasses of
 *  of Event provide event-logic in their implementation of the occur method.
 *  Note: unique identification and the event/activation time (actTime) are mixed
 *  in via the PQItem trait.
 *  @param proto     the prototype (serves as node in animation) for this event
 *  @param entity    the entity involved in this event
 *  @param links     the causal links used to trigger other immediate/future events
 *  @param director  the controller/scheduler that this event is a part of
 *  @param at        the location of this event
 */
abstract class Event (val proto: Event, entity: Entity, links: Array [CausalLink],
                      director: Model, at: Array [Double] = Array ())
         extends PQItem with Ordered [Event]
{
     {
         if (proto == null) display ()  // the prototype event does not have a prototype
     } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times of the two events.
     */
    def compare (ev: Event): Int = ev.actTime.compare (actTime)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute the event where the event-logic is specified as follows:
     *  (i) Each model will define causal links that connect events. One event
     *  may schedule other events, depending upon whether the respective
     *  causal link evaluates to true.
     *  (ii) Each model defines its own events (e.g., Arrival, Departure) that
     *  must override this occur method to specify the state changes.  If such
     *  events may cause other events, they must call this occur method (via
     *  super.occur).
     */
    def occur ()
    {
        if (links != null) {
            for (arc <- links if arc.condition ()) {
                val delayTime = arc.delay.gen
                arc.tally (delayTime)
                director.schedule (arc.makeEvent (), delayTime)
            } // for
        } // if
    } // occur

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Event as a node.
     */
    def display ()
    {
        director.animate (this, CreateNode, blue, Ellipse (), at)
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this node's outgoing CausalLink's as edges.
     *  @param outLinks  this nodes outgoing causal links
     */
    def displayLinks (outLinks: Array [CausalLink])
    {
        for (l <- outLinks) l.display (this, l.causedEvent)
    } // displayLinks

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the event to a string.
     */
    override def toString = "Event ( " + getClass.getSimpleName () + ", " + me + ", " + entity + " )"

} // Event class

