
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue Jan  5 16:14:38 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import scala.math.{atan, cos, Pi, sin}

import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{Dimension, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Arrow` class uses Java's Path2D class to create a line with an arrowhead
 *  on the far end.  The main line is defined by points 'p1' and 'p2'.  Points 'p3'
 *  and 'p4' are the corners of the triangular arrowhead.
 *  @param p1   the starting point for the line/arrow
 *  @param p2   the ending point for the line/arrow
 *  @param len  the length of the arrowhead on the line/arrow
 */
case class Arrow (var p1:  R2  = R2 (0.0, 0.0),
                  var p2:  R2  = R2 (0.0, 0.0),
                  var len: Int = 10)
     extends java.awt.geom.Path2D.Double //with CurvilinearShape
{
    {
        val deltaX = p2.x - p1.x
        val slope  = (p2.y - p1.y) / deltaX                            // slope of the main line
        val a1_2 = if (slope == Double.PositiveInfinity) Pi / 2.0      // angle of line p1 to p2
              else if (slope == Double.NegativeInfinity) 3.0 * Pi / 2.0
              else if (deltaX < 0.0) Pi + atan (slope)
                 else atan (slope)
        val a2_3 = a1_2 - 5.0 * Pi / 6.0                               // angle of line p2 to p3
        val a3_4 = a1_2 + Pi / 2.0                                     // angle of line p3 to p4
        val p3   = R2 (p2.x + len * cos (a2_3), p2.y + len * sin (a2_3))
        val p4   = R2 (p3.x + len * cos (a3_4), p3.y + len * sin (a3_4))
        moveTo (p1.x, p1.y)
        lineTo (p2.x, p2.y)
        lineTo (p3.x, p3.y)
        lineTo (p4.x, p4.y)
        lineTo (p2.x, p2.y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the x-coordinate of the center of the main line.
     */
    def getCenterX (): Double = (p1.x + p2.x) / 2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the y-coordinate of the center of the main line.
     */
    def getCenterY (): Double = (p1.y + p2.y) / 2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set (or reset) the location for the Arrow as a line.
     *  @param _p1  the starting point
     *  @param _p2  the ending point
     */
    def setLine (_p1: R2, _p2: R2)
    {
        p1 = _p1; p2 = _p2
        val deltaX = p2.x - p1.x
        val slope  = (p2.y - p1.y) / deltaX                           // slope of the main line
        val a1_2 = if (slope == Double.PositiveInfinity) Pi / 2.0     // angle of line p1 to p2
                   else if (slope == Double.NegativeInfinity) 3.0 * Pi / 2.0
                   else if (deltaX < 0.0) Pi + atan (slope)
                   else atan (slope)
        val a2_3 = a1_2 - 5.0 * Pi / 6.0                              // angle of line p2 to p3
        val a3_4 = a1_2 + Pi / 2.0                                    // angle of line p3 to p4
        val p3   = R2 (p2.x + len * cos (a2_3), p2.y + len * sin (a2_3))
        val p4   = R2 (p3.x + len * cos (a3_4), p3.y + len * sin (a3_4))
        moveTo (p1.x, p1.y)
        lineTo (p2.x, p2.y)
        lineTo (p3.x, p3.y)
        lineTo (p4.x, p4.y)
        lineTo (p2.x, p2.y)
    } // setLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set (or reset) the location for the Arrow as a line. The bend parameter
     *  is ignored for this class, since arrows are straight.
     *  @param _p1   the starting point
     *  @param _p2   the ending point
     *  @param bend  the bend or curvature (0. => straight line)
     */
    def setLine (_p1: R2, _p2: R2, bend: Double)
    {
        p1 = _p1; p2 = _p2
        setLine (p1, p2)
    } // setLine
    
} // Arrow class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ArrowTest` object is used to test the `Arrow` class.
 */
object ArrowTest extends App
{
    private val arrow1 = new Arrow (R2 (200, 200), R2 (300, 200))
    private val arrow2 = new Arrow (R2 (200, 200), R2 (300, 300))
    private val arrow3 = new Arrow (R2 (200, 200), R2 (200, 300))
    private val arrow4 = new Arrow (R2 (200, 200), R2 (100, 300))
    private val arrow5 = new Arrow (R2 (200, 200), R2 (100, 200))
    private val arrow6 = new Arrow (R2 (200, 200), R2 (100, 100))
    private val arrow7 = new Arrow (R2 (200, 200), R2 (200, 100))
    private val arrow8 = new Arrow (R2 (200, 200), R2 (300, 100))

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
            g2d.draw (arrow1)
            g2d.setPaint (orange)
            g2d.draw (arrow2)
            g2d.setPaint (yellow)
            g2d.draw (arrow3)
            g2d.setPaint (yellowgreen)
            g2d.draw (arrow4)
            g2d.setPaint (green)
            g2d.draw (arrow5)
            g2d.setPaint (cyan)
            g2d.draw (arrow6)
            g2d.setPaint (blue)
            g2d.draw (arrow7)
            g2d.setPaint (violet)
            g2d.draw (arrow8)
        } // paintComponent

     } // Canvas class

    // Put the drawing canvas in the visualization frame

    new VizFrame ("ArrowTest", new Canvas (), 600, 600)

} // ArrowTest object

