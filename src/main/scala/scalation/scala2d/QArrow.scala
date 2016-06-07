
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Jan  5 16:14:38 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import scala.math.{atan, cos, Pi, sin}

import scalation.scala2d.Colors._
import scalation.scala2d.QCurve.calcControlPoint
import scalation.scala2d.Shapes.{Dimension, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QArrow` class uses Java's `Path2D` class to create a quad curve with an
 *  arrowhead on the far end.  The main curve is defined by points 'p1' and 'p2'
 *  along with a control point 'pc'.  Points 'p3' and 'p4' are the corners of the
 *  triangular arrowhead.
 *  @param p1   the starting point for the curve/arc
 *  @param pc   the control point for the curve/arc
 *  @param p2   the ending point for the curve/arc
 *  @param len  the length of the arrowhead on the curve/arc
 */
case class QArrow (var p1:  R2  = R2 (0.0, 0.0),
                   var pc:  R2  = R2 (0.0, 0.0),
                   var p2:  R2  = R2 (0.0, 0.0),
                   len: Int = 10)
     extends java.awt.geom.Path2D.Double //with CurvilinearShape
{
    {
        val deltaX = p2.x - pc.x
        val slope  = (p2.y - pc.y) / deltaX                           // slope of curve at p2
        val a1_2 = if (slope == Double.PositiveInfinity) Pi / 2.0      // angle of line pc to p2
              else if (slope == Double.NegativeInfinity) 3.0 * Pi / 2.0
              else if (deltaX < 0.0) Pi + atan (slope)
                 else atan (slope)
        val a2_3 = a1_2 - 5.0 * Pi / 6.0                               // angle of line p2 to p3
        val a3_4 = a1_2 + Pi / 2.0                                     // angle of line p3 to p4
        val p3   = R2 (p2.x + len * cos (a2_3), p2.y + len * sin (a2_3))
        val p4   = R2 (p3.x + len * cos (a3_4), p3.y + len * sin (a3_4))
        moveTo (p1.x, p1.y)
        quadTo (pc.x, pc.y, p2.x, p2.y)
        lineTo (p3.x, p3.y)
        lineTo (p4.x, p4.y)
        lineTo (p2.x, p2.y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a `QArrow` (quad arc) where bend indicates the distance to the
     *  control point.
     *  @param p1    the starting point for the curve/arc
     *  @param p2    the ending point for the curve/arc
     *  @param bend  the bend or curvature  (1. => line length)
     */
    def this (p1: R2, p2: R2, bend: Double)
    {
         this (p1, calcControlPoint (p1, p2, bend), p2)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the x-coordinate of the center of the main line/curve.
     */
    def getCenterX (): Double =
    {
        if (pc.x > 0.0) (p1.x + 2.0 * pc.x + p2.x) / 4.0
        else            (p1.x + p2.x) / 2.0
    } // getCenterX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the y-coordinate of the center of the main line/curve.
     */
    def getCenterY (): Double =
    {
        if (pc.y > 0.0) (p1.y + 2.0 * pc.y + p2.y) / 4.0
        else            (p1.y + p2.y) / 2.0
    } // getCenterY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set (or reset) the location for the `QArrow` as a line.
     *  @param _p1   the starting point
     *  @param _p2   the ending point
     */
    def setLine (_p1: R2, _p2: R2)
    {
        p1 = _p1; p2 = _p2
        val pc = calcControlPoint (p1, p2, 0.0)     // use 0 for the bend
        setLine (p1, pc, p2)
    } // setLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set (or reset) the location for the `QArrow` as a curve using bend
     *  to compute the control point.
     *  @param _p1   the starting point
     *  @param _p2   the ending point
     *  @param bend  the bend or curvature (1. => line-length)
     */
    def setLine (_p1: R2, _p2: R2, bend: Double)
    {
        p1 = _p1; p2 = _p2
        pc = calcControlPoint (p1, p2, bend)
        setLine (p1, pc, p2)
    } // setLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set (or reset) the location for the `QArrow` as a curve using an explicitly
     *  given control point.
     *  @param _p1  the starting point
     *  @param _pc  the control point
     *  @param _p2  the ending point
     */
    def setLine (_p1: R2, _pc: R2, _p2: R2)
    {
        p1 = _p1; pc = _pc; p2 = _p2
        val deltaX = p2.x - pc.x
        val slope  = (p2.y - pc.y) / deltaX                            // slope of curve at p2
        val a1_2 = if (slope == Double.PositiveInfinity) Pi / 2.0      // angle of line pc to p2
              else if (slope == Double.NegativeInfinity) 3.0 * Pi / 2.0
              else if (deltaX < 0.0) Pi + atan (slope)
                 else atan (slope)
        val a2_3 = a1_2 - 5.0 * Pi / 6.0                               // angle of line p2 to p3
        val a3_4 = a1_2 + Pi / 2.0                                     // angle of line p3 to p4
        val p3   = R2 (p2.x + len * cos (a2_3), p2.y + len * sin (a2_3))
        val p4   = R2 (p3.x + len * cos (a3_4), p3.y + len * sin (a3_4))
        moveTo (p1.x, p1.y)
        quadTo (pc.x, pc.y, p2.x, p2.y)
        lineTo (p3.x, p3.y)
        lineTo (p4.x, p4.y)
        lineTo (p2.x, p2.y)
    } // setLine

} // QArrow class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QArrowTest` object is used to test the `QArrow` class.
 */
object QArrowTest extends App
{
    private val arc1 = new QArrow (R2 (200, 200), R2 (300, 200), .25)
    private val arc2 = new QArrow (R2 (200, 200), R2 (300, 300), .25)
    private val arc3 = new QArrow (R2 (200, 200), R2 (200, 300), .25)
    private val arc4 = new QArrow ()
    private val arc5 = new QArrow (R2 (200, 200), R2 (150, 220), R2 (100, 200))
    private val arc6 = new QArrow (R2 (200, 200), R2 (180, 170), R2 (100, 100))
    private val arc7 = new QArrow (R2 (200, 200), R2 (220, 150), R2 (200, 100))
    private val arc8 = new QArrow (R2 (200, 200), R2 (250, 170), R2 (300, 100))

    class Canvas extends Panel
    {
        setBackground (white)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the components into the canvas (drawing panel).
         *  @param gr  low-resolution graphics environment
         */
        override def paintComponent (gr: Graphics)
        {
            super.paintComponent (gr)
            val g2d = gr.asInstanceOf [Graphics2D]            // use hi-resolution
            g2d.setPaint (red)
            g2d.draw (arc1)
            g2d.setPaint (orange)
            g2d.draw (arc2)
            g2d.setPaint (yellow)
            g2d.draw (arc3)
            g2d.setPaint (yellowgreen)
            arc4.setLine (R2 (200, 200), R2 (100, 300), .25)
            g2d.draw (arc4)
            g2d.setPaint (green)
            g2d.draw (arc5)
            g2d.setPaint (cyan)
            g2d.draw (arc6)
            g2d.setPaint (blue)
            g2d.draw (arc7)
            g2d.setPaint (violet)
            g2d.draw (arc8)
        } // paintComponent

     } // Canvas class

    // Put the drawing canvas in the visualization frame

    new VizFrame ("QArrowTest", new Canvas (), 600, 600)

} // QArrowTest object

