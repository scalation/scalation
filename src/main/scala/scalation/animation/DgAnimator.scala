
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep 14 14:15:51 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.animation

import java.awt.Font
import java.util.concurrent.ConcurrentLinkedQueue 

import scala.math.round
import scala.util.control.Breaks.{breakable, break}

import scalation.animation.CommandType._
import scalation.scala2d.{Panel, VizFrame}
import scalation.scala2d.{CurvilinearShape, Ellipse, QCurve, Rectangle}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{Dimension, Graphics, Graphics2D, RectangularShape}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DgAnimator` class is an animation engine for animating graphs.
 *  For example, it can animate bipartite graphs to animate Petri Nets.
 *  @param _title    the title for the display frame
 *  @param fgColor   the foreground color
 *  @param bgColor   the background color
 *  @param aniRatio  the ratio of simulation speed vs. animation speed
 */
class DgAnimator (_title: String, fgColor: Color = black, bgColor: Color = white,
                  aniRatio: Double = 1.0)
      extends VizFrame (_title, null, 1200, 800) with Runnable with Error
{
    /** Clock for animation engine
     */
    private var clock = 0.0

    /** Stop time for animation engine
     */
    private var stopTime = 0.0

    /** Graph to animate
     */
    private val graph = new Dgraph ("Animated_Graph")

    /** Animation command processor
     */
    private val ani = new Animator (graph)

    /** Shared queue holding animation commands
     */    
    private val cmdQ = new ConcurrentLinkedQueue [AnimateCommand] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The canvas Panel is used to place shapes in the drawing region.
     */
    class Canvas extends Panel
    {
        setBackground (bgColor)
        val f = new Font ("Serif", Font.BOLD, 12)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the display panel component.
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics)
        {
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-reso;ution

            //:: Display the animation clock

            g2d.setFont (f)
            g2d.setPaint (fgColor)
            g2d.drawString ("CLOCK = " + "%10.3f".format(clock), 20, getH - 30)

            //:: Display all nodes in graph and tokens bound to these nodes.

            // println ("paintComponent: paint " + graph.nodes.length + " nodes")
            for (node <- graph.nodes) {
                g2d.setPaint (node.color)
                g2d.fill (node.shape)
                g2d.setPaint (black)
                g2d.draw (node.shape)
                val x = node.shape.getCenterX ().asInstanceOf [Float] - 20.0f
                val y = node.shape.getMaxY ().asInstanceOf [Float] + 12.0f
                g2d.drawString (node.label, x, y)
                for (token <- node.tokens) {
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                } // for
            } // for

            //:: Display all edges in graph and tokens bound to these edges.

            // println ("paintComponent: paint " + graph.edges.length + " edges")
            for (edge <- graph.edges) {
                g2d.setPaint (edge.color)
                g2d.draw (edge.shape)
                val x = edge.shape.getCenterX ().asInstanceOf [Float] - 30.0f
                val y = edge.shape.getCenterY ().asInstanceOf [Float]
                g2d.drawString (edge.label, x, y)
                for (token <- edge.tokens if token.shape.getWidth () > 0.0) {
                    g2d.setPaint (token.color)
                    g2d.fill (token.shape)
                } // for
            } // for

            //:: Display all free tokens in the graph.

            // println ("paintComponent: paint " + graph.freeTokens.length + " free tokens")
            for (token <- graph.freeTokens if token.shape.getWidth () > 0.0) {
                g2d.setPaint (token.color)
                g2d.fill (token.shape)
            } // for
        } // paintComponent

    } // Canvas class

    {
        getContentPane ().add (new Canvas ())
        setVisible (true)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invoke the animation command.
     *  @param c  the animation command to invoke
     */
    private def invokeCommand (c: AnimateCommand)
    {
        println ("DgAnimator.invokeCommand: " + c)

        c.action  match {
        case CreateNode =>
            ani.createNode (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.pts)
        case CreateEdge =>
            //ani.createEdge (c.eid, c.shape.asInstanceOf [QCurve], c.label, c.primary, c.color, c.from_eid, c.to_eid, c.pts)
            ani.createEdge (c.eid, c.shape.asInstanceOf [CurvilinearShape], c.label, c.primary, c.color, c.from_eid, c.to_eid, c.pts)
        case CreateToken =>
            ani.createToken (c.eid, c.shape.asInstanceOf [RectangularShape], c.label, c.primary, c.color, c.from_eid, c.pts)
        case DestroyNode =>
            ani.destroyNode (c.eid)
        case DestroyEdge =>
            ani.destroyEdge (c.eid)
        case DestroyToken =>
            ani.destroyToken (c.eid)
        case MoveNode =>
            ani.moveNode (c.eid, c.pts)
        case MoveToken =>
            ani.moveToken (c.eid, c.pts)
        case MoveToken2Node =>
            ani.moveToken2Node (c.eid, c.from_eid)
        case MoveTokens2Node =>
            ani.moveTokens2Node (c.color, c.from_eid, c.to_eid, c.pts)
        case MoveToken2Edge =>
            ani.moveToken2Edge (c.eid, c.from_eid, 10.0)   // FIX: 10.0?
        case ScaleNode =>
            ani.scaleNode (c.eid, c.pts)
        case ScaleToken =>
            ani.scaleToken (c.eid, c.pts)
        case ScaleTokensAt =>
            ani.scaleTokensAt (c.color, c.from_eid, c.to_eid, c.pts)
        case SetPaintNode =>
            ani.setPaintNode (c.eid, c.color)
        case SetPaintEdge =>
            ani.setPaintEdge (c.eid, c.color)
        case SetPaintToken =>
            ani.setPaintToken (c.eid, c.color)
        case TimeDilation =>
            ani.timeDilation (c.pts)
        case _ =>
            flaw ("invokeCommand", "the " + c.action + " is an unrecognized action")
        } // match
    } // invokeCommand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Repeatedly execute animation commands, sleep and repaint.
     */
    def run ()
    {
        var cmd: AnimateCommand = null
        var when  = 0.0
        var delay = 0l

        println ("DgAnimator.run: start animation at time " + clock)

        breakable { while (clock < stopTime) {

            //:: Get the next animation command from the shared queue.

            if (cmdQ.isEmpty ()) {
                println ("DgAnimator.run: command queue is empty")
                break
            } // if

            cmd   = cmdQ.poll ()
            when  = cmd.time
            delay = round ((when - clock) * aniRatio * ani.timeDilationFactor)

            //:: sleep for the given number (delay) of milliseconds

            Thread.sleep (delay)

            //:: set the animation clock and invoke the animation command

            clock = when
            invokeCommand (cmd)

            //:: Repaint the canvas.

            repaint ()
        }} // while

        println ("DgAnimator.run: end animation at time " + clock)
    } // run

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start the animation by staring the animation thread.
     *  @param tStart  the animation start time
     *  @param tStop   the animation stop time
     */
    def animate (tStart: Double, tStop: Double)
    {
        clock    = tStart
        stopTime = tStop
        new Thread (this).start ()
    } // startAnimation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the animation command queue.
     */
    def getCommandQueue = cmdQ

} // DgAnimator class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DgAnimatorTest` object is used to test the `DgAnimator` class.
 */
object DgAnimatorTest extends App
{
    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Sample method for loading the shared command queue.
     * Ordinarily these commands would come from some simulation engine.
     * @param cq  the animation command queue
     */
    private def loadCommandQueue (cq: ConcurrentLinkedQueue [AnimateCommand])
    {
       //:: Place the nodes into graph.

       cq.add (AnimateCommand (CreateNode, 1, Ellipse (),   "node1", false, yellow, Array (100.0, 110.0, 30.0, 30.0), 0))
       cq.add (AnimateCommand (CreateNode, 2, Ellipse (),   "node2", false, yellow, Array (100.0, 290.0, 30.0, 30.0), 0))
       cq.add (AnimateCommand (CreateNode, 3, Rectangle (), "node3", true,  gold,   Array (300.0, 185.0, 30.0, 60.0), 1000))
       cq.add (AnimateCommand (CreateNode, 4, Ellipse (),   "node4", false, silver, Array (500.0, 110.0, 30.0, 30.0), 2000))
       cq.add (AnimateCommand (CreateNode, 5, Ellipse (),   "node5", false, silver, Array (500.0, 290.0, 30.0, 30.0), 2000))
       cq.add (AnimateCommand (CreateNode, 6, Rectangle (), "node6", true,  gold,   Array (300.0,  35.0, 30.0, 60.0), 3000))
       cq.add (AnimateCommand (CreateNode, 7, Rectangle (), "node7", true,  gold,   Array (300.0, 335.0, 30.0, 60.0), 3000))

       //:: Place the edges into graph.

       cq.add (AnimateCommand (CreateEdge, 8,  QCurve (), "edge1", true, lightyellow, null, 4000, 1, 3))
       cq.add (AnimateCommand (CreateEdge, 9,  QCurve (), "edge2", true, lightyellow, null, 4000, 2, 3))
       cq.add (AnimateCommand (CreateEdge, 10, QCurve (), "edge3", true, lightyellow, null, 5000, 3, 4))
       cq.add (AnimateCommand (CreateEdge, 11, QCurve (), "edge4", true, lightyellow, null, 5000, 3, 5))
       cq.add (AnimateCommand (CreateEdge, 12, QCurve (), "edge5", true, lightyellow, null, 6000, 4, 6))
       cq.add (AnimateCommand (CreateEdge, 13, QCurve (), "edge6", true, lightyellow, null, 6000, 5, 7))
       cq.add (AnimateCommand (CreateEdge, 14, QCurve (), "edge7", true, lightyellow, null, 7000, 6, 1))
       cq.add (AnimateCommand (CreateEdge, 15, QCurve (), "edge8", true, lightyellow, null, 7000, 7, 2))

       //:: Place the tokens into graph.

       cq.add (AnimateCommand (CreateToken, 16, Ellipse (), "token1", false, blue, null, 8000, 1))
       cq.add (AnimateCommand (CreateToken, 17, Ellipse (), "token2", false, cyan, null, 8000, 2))

       //:: Move the tokens around graph.

       for (i <- 0 to 10) {
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 12000 + 10000 * i, 3))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 12000 + 10000 * i, 3))
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 13000 + 10000 * i, 4))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 13000 + 10000 * i, 5))
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 17000 + 10000 * i, 6))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 17000 + 10000 * i, 7))
           cq.add (AnimateCommand (MoveToken2Node, 16, null, null, false, null, null, 18000 + 10000 * i, 1))
           cq.add (AnimateCommand (MoveToken2Node, 17, null, null, false, null, null, 18000 + 10000 * i, 2))
       } // for
    } // loadCommandQueue

    println ("Run DgAnimatorTest")
    val dga = new DgAnimator ("DgAnimator")
    loadCommandQueue (dga.getCommandQueue)
    dga.animate (0, 100000)

} // DgAnimatorTest object

