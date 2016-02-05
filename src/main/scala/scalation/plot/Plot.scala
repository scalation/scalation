
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.plot

import math.{ceil, floor, min, pow, round}

import scalation.linalgebra.VectoD
import scalation.scala2d.{Panel, VizFrame}
import scalation.scala2d.{Ellipse, Line}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{BasicStroke, Dimension, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Plot` class takes 'x' and 'y' vectors of data values and plots the '(x, y)'
 *  data points.  Optionally, a 'z' vector may be plotted with 'y'.  Note, axes are
 *  determined by the 'x' and 'y' vectors only.  For more vertical vectors use PlotM.
 *  @param x       the x vector of data values (horizontal)
 *  @param y       the y vector of data values (primary vertical)
 *  @param z       the z vector of data values (secondary vertical) to compare with y
 *  @param _title  the title of the plot
 */
class Plot (x: VectoD, y: VectoD, z: VectoD = null, _title: String = "Plot y vs. x")
      extends VizFrame (_title, null)
{
    getContentPane ().add (new Canvas (x, y, z, getW, getH))
    setVisible (true)

} // Plot class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FramelessPlot` class is used for embedded applications.
 *  @param x       the x vector of data values (horizontal)
 *  @param y       the y vector of data values (primary vertical)
 *  @param z       the z vector of data values (secondary vertical) to compare with y
 *  @param width   the width
 *  @param height  the height
 */
class FramelessPlot (x: VectoD, y: VectoD, z: VectoD = null, var width: Int = 640, var height: Int = 480)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Dynamically create and return a drawing canvas.
     */
    def canvas: Canvas = new Canvas (x, y, z, width, height)

} // FramelessPlot class
 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Canvas` class provides a canvas on which to draw the plot.
 *  @param x       the x vector of data values (horizontal)
 *  @param y       the y vector of data values (primary vertical)
 *  @param z       the z vector of data values (secondary vertical) to compare with y
 *  @param width   the width
 *  @param height  the height
 */
class Canvas (x: VectoD, y: VectoD, z: VectoD, width: Int, height: Int)
      extends Panel
{
    private val EPSILON   = 1E-9
    private val frameW    = width
    private val frameH    = height
    private val offset    = 50
    private val baseX     = offset
    private val baseY     = frameH - offset
    private val stepsX    = 10
    private val stepsY    = 10
    private val minX      = floor (x.min ())
    private val maxX      = ceil (x.max () + EPSILON)
    private val minY      = floor (y.min ())
    private val maxY      = ceil (y.max () + EPSILON)
    private val deltaX    = maxX - minX
    private val deltaY    = maxY - minY
    private val diameter  = 4
    private val dot       = Ellipse ()
    private val axis      = Line ()

    setBackground (white)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Paint the canvas by plotting the data points.
     *  @param gr  low-res graphics environment
     */
    override def paintComponent (gr: Graphics)
    {
        super.paintComponent (gr)
        val g2d = gr.asInstanceOf [Graphics2D]            // use hi-res

        var x_pos = 0
        var y_pos = 0
        var step  = 0.0

        //:: Draw the axes

        g2d.setPaint (black)
        g2d.setStroke (new BasicStroke (2.0f))
        axis.setLine (baseX - 1, baseY + 1, baseX + 10 + frameW - 2 * offset, baseY + 1)
        g2d.draw (axis)
        axis.setLine (baseX - 1, offset - 10, baseX - 1, baseY + 1)
        g2d.draw (axis)

        //:: Draw the labels on the axes

        y_pos = baseY + 15
        step  = deltaX / stepsX.asInstanceOf [Double]       // for x-axis
        for (j <- 0 to stepsX) {
            val x_val = clip (minX + j * step)
            x_pos = offset - 8 + j * (frameW - 2 * offset) / stepsX
            g2d.drawString (x_val, x_pos, y_pos)
        } // for

        x_pos = baseX - 30
        step  = deltaY / stepsY.asInstanceOf [Double]       // for y-axis
        for (j <- 0 to stepsY) {
            val y_val = clip (maxY - j * step)
            y_pos = offset + 2 + j * (frameH - 2 * offset) / stepsY
            g2d.drawString (y_val, x_pos, y_pos)
        } // for

        //:: Draw the dots for the data points being plotted

        for (i <- 0 until y.dim) {
            val xx = round ((x(i) - minX) * (frameW - 2 * offset).asInstanceOf [Double])
            x_pos = (xx / deltaX).asInstanceOf [Int] + offset
            val yy = round ((maxY - y(i)) * (frameH - 2 * offset).asInstanceOf [Double])
            y_pos = (yy / deltaY).asInstanceOf [Int] + offset
            dot.setFrame (x_pos, y_pos, diameter, diameter)         // x, y, w, h
            g2d.setPaint (black)
            g2d.fill (dot)
        } // for

        if (z != null) {
            for (i <- 0 until min (y.dim, z.dim)) {
                val xx = round ((x(i) - minX) * (frameW - 2 * offset).asInstanceOf [Double])
                x_pos = (xx / deltaX).asInstanceOf [Int] + offset
                val yy = round ((maxY - z(i)) * (frameH - 2 * offset).asInstanceOf [Double])
                y_pos = (yy / deltaY).asInstanceOf [Int] + offset
                dot.setFrame (x_pos, y_pos, diameter, diameter)         // x, z, w, h
                g2d.setPaint (red)
                g2d.fill (dot)
            } // for
        } // if
    } // paintComponent

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert value to string and cut out the first four characters.
     *  @param x  the value to convert and cut
     */
    def clip (x: Double): String =
    {
        val s = x.toString 
        s.substring (0, min (s.length, 4))
    } // clip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a Plot vectors to a string.
     */
    override def toString = "Plot (y = " + y + " vs. x = " + x + ")"

} // Canvas class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PlotTest` object is used to test the `Plot` class.
 *  > run-main scalation.plot.PlotTest
 */
object PlotTest extends App
{
    import scalation.linalgebra.VectorD
/*
    val x = VectorD (0.0, 1.0, 2.0, 3.0,  4.0,  5.0,  6.0, 7.0, 8.0, 9.0, 10.0)
    val y = VectorD (0.0, 1.0, 4.0, 9.0, 16.0, 25.0, 16.0, 9.0, 4.0, 1.0,  0.0)
*/
    val x = new VectorD (100)
    val y = new VectorD (100)
    for (i <- 0 until 100) { x(i) = i / 10.0; y(i) = pow (x(i) - 5, 2) }
    val plot = new Plot (x, y)
    println ("plot = " + plot)

} // PlotTest object

