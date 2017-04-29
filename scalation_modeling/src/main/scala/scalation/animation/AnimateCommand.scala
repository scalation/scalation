
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Sep 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.animation

import scalation.scala2d.Colors.Color
import scalation.scala2d.Shapes.Shape

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CommandType` object implements a message which is passed from a simulation
 *  engine to the animation engine.  A message specifies one of the commands defined
 *  in the Animator interface.
 */
object CommandType extends Enumeration
{
    val CreateNode      = Value ("CreateNode")
    val CreateEdge      = Value ("CreateEdge")
    val CreateToken     = Value ("CreateToken")
    val DestroyNode     = Value ("DestroyNode")
    val DestroyEdge     = Value ("DestroyEdge")
    val DestroyToken    = Value ("DestroyToken")
    val MoveNode        = Value ("MoveNode")
    val MoveToken       = Value ("MoveToken")
    val MoveToken2Node  = Value ("MoveToken2Node")
    val MoveTokens2Node = Value ("MoveTokens2Node")
    val MoveToken2Edge  = Value ("MoveToken2Edge")
    val ScaleNode       = Value ("ScaleNode")
    val ScaleToken      = Value ("ScaleToken")
    val ScaleTokensAt   = Value ("ScaleTokensAt")
    val SetPaintNode    = Value ("SetPaintNode")
    val SetPaintEdge    = Value ("SetPaintEdge")
    val SetPaintToken   = Value ("SetPaintToken")
    val TimeDilation    = Value ("TimeDilation")
} // CommandType object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AnimateCommand` class provides a template for animation commands.
 *  @param action    the animation action to perform
 *  @param eid       the external id for the component acted upon
 *  @param shape     the shape of graph component (node, edge or token)
 *  @param label     the display label for the component
 *  @param primary   whether the component is primary (true) or secondary (false)
 *  @param color     the color of the component
 *  @param pts       the set points/dimensions giving the shapes location and size
 *  @param time      simulation time when the command is to be performed
 *  @param from_eid  the 'eid' of the origination node (only for edges)
 *  @param to_eid    the 'eid' of the destination node (only for edges)
 */
case class AnimateCommand (action: CommandType.Value, eid: Int, shape: Shape, label: String,
                           primary: Boolean, color: Color, pts: Array [Double], time: Double,
                           from_eid: Int = -1, to_eid: Int = -1)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method compares two `AnimateCommand` objects to see which one has
     *  the most recent timestamp.
     *  @param command2  the animate command to compare 'this' to
     */
    def compare (command2: AnimateCommand) = time compare command2.time

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the elements of the array and handle the null case.
     *  @param array  the array to be shown
     */
    def show (array: Array [Double]) = 
    {
       if (array == null) "Array ( null )" else array.deep
    } // show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the command to a string representation useful to printing/debugging.
     */
    override def toString =
    {
        val __ = " , "
        "AnimateCommand ( " + action + __ + eid + __ + shape + __ + label + __ + primary + __ +
                              color + __ + show (pts) + __ + time + __ + from_eid + __ + to_eid + " )"
    } // toString

} // AnimateCommand class

