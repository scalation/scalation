
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Dec 6 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.event

import scalation.animation.CommandType._
import scalation.scala2d.QCurve
import scalation.scala2d.Colors._
import scalation.util.Identifiable

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CausalLink` class provides casual links between events.  A causal link
 *  indicates that a "causing event" (from node) conditionally may triggers a
 *  "caused event" (to node).
 *  @param label        the name/label of the causal link
 *  @param director     the controller/scheduler that this causal link is a part of
 *  @param condition    the condition under which the link is triggered
 *  @param causedEvent  the event caused by this causal link
 */
case class CausalLink (label: String, director: Model, val condition: () => Boolean,
                       causedEvent: Event)
      extends Identifiable
{
    /** Default amount of bend in the `QCurve`
     */
    private val bend = .25

    name = label                  // set the name of the causal link

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this `CausalLink`.
     *  @param from  the starting event prototype
     *  @param to    the ending event prototype
     */
    def display (from: Event, to: Event)
    {
        if (from.id == to.id) {
            director.animate (this, CreateEdge, green, new QCurve (), from, to, Array (16.0 * bend))
        } else {
            director.animate (this, CreateEdge, green, new QCurve (), from, to, Array (bend))
        } // if
    } // display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the causal link to a string.
     */
    override def toString = "CausalLink( " + getClass.getSimpleName () + ", " + name + ")"

} // CausalLink class

