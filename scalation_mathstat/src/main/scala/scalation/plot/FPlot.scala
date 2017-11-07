
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.4
 *  @date    Mon Oct 31 18:05:06 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.plot

import scala.math.{ceil, floor, min, pow, round}

import scalation.linalgebra.{VectoD, VectoI}
import scalation.math.FunctionS2S
import scalation.scala2d.{Panel, VizFrame}
import scalation.scala2d.{Ellipse, Line}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{BasicStroke, Dimension, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FPlot` class takes 'x' and 'y' vectors of data values and plots the '(x, y)'
 *  data points.  Optionally, a 'z' vector may be plotted with 'y'.  Note, axes are
 *  determined by the 'x' and 'y' vectors only.  For more vertical vectors use `PlotM`.
 *  @param x1      the x vector of data values (horizontal)
 *  @param y1      the y vector of data values (primary vertical)
 *  @param x2      the x vector of functional values (horizontal)
 *  @param y2      the z function of data values (secondary vertical) to compare with y
 *  @param _title  the title of the plot
 */
class FPlot (x1: VectoD, y1: VectoD, x2: VectoD, y2: FunctionS2S, _title: String = "Plot y vs. x")
    extends VizFrame (_title, null)
{
    getContentPane ().add (new FCanvas (x1, y1, x2, y2, getW, getH))
    setVisible (true)

} // FPlot class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FCanvas` class provides a canvas on which to draw the plot.
 *  @param x1      the x vector of data values (horizontal)
 *  @param y1      the y vector of data values (primary vertical)
 *  @param x2      the x vector of functional values (horizontal)
 *  @param y2      the z function of data values (secondary vertical) to compare with y
 *  @param width   the width
 *  @param height  the height
 */
class FCanvas (x1: VectoD, y1: VectoD, x2: VectoD, y2: FunctionS2S, width: Int, height: Int)
    extends Panel
{
    private val EPSILON  = 1E-9
    private val frameW   = width
    private val frameH   = height
    private val offset   = 50
    private val baseX    = offset
    private val baseY    = frameH - offset
    private val stepsX   = 10
    private val stepsY   = 10
    private val minX     = floor (x1.min ())
    private val maxX     = ceil (x1.max () + EPSILON)
    private val minY     = floor (y1.min ())
    private val maxY     = ceil (y1.max () + EPSILON)
    private val deltaX   = maxX - minX
    private val deltaY   = maxY - minY
    private val diameter = 4
    private val dot      = Ellipse ()
    private val axis     = Line ()

    setBackground (white)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Paint the canvas by plotting the data points.
     *  @param gr  low-resolution graphics environment
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

        for (i <- 0 until x1.dim) {
            val xx = round ((x1(i) - minX) * (frameW - 2 * offset).asInstanceOf [Double])
            x_pos = (xx / deltaX).asInstanceOf [Int] + offset
            val yy = round ((maxY - y1(i)) * (frameH - 2 * offset).asInstanceOf [Double])
            y_pos = (yy / deltaY).asInstanceOf [Int] + offset
            dot.setFrame (x_pos, y_pos, diameter, diameter)         // x, y, w, h
            g2d.setPaint (black)
            g2d.fill (dot)
        } // for

        var x_pos_prev = 0
        var y_pos_prev = 0

        for (i <- 0 until x2.dim) {
            val xx = round ((x2(i) - minX) * (frameW - 2 * offset).asInstanceOf [Double])
            x_pos = (xx / deltaX).asInstanceOf [Int] + offset
            val yy = round ((maxY - y2(x2(i))) * (frameH - 2 * offset).asInstanceOf [Double])
            y_pos = (yy / deltaY).asInstanceOf [Int] + offset
            if (x1.contains(x2(i))) {
                dot.setFrame(x_pos - diameter/2, y_pos - diameter/2, diameter, diameter) // x, y, w, h
            } else {
                dot.setFrame(x_pos - diameter/4, y_pos - diameter/4, diameter/2, diameter/2) // x, y, w, h
            } // if
            g2d.setPaint (red)
            g2d.fill (dot)
            if (i > 0) {
                g2d.setStroke (new BasicStroke (1.0f))
                g2d.drawLine (x_pos_prev, y_pos_prev, x_pos, y_pos)
            } // if
            x_pos_prev = x_pos
            y_pos_prev = y_pos
        } // for

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
    /** Convert 'this' object to a string.
     */
    override def toString = "Plot (y = " + y1 + " vs. x = " + x1 + ")"

} // FCanvas class

