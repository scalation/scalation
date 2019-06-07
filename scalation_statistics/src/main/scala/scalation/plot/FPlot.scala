
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.6
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.plot

import scala.collection.immutable.NumericRange
import scala.math.{ceil, floor, min, pow, round}

import scalation.math.FunctionS2S
import scalation.linalgebra.{VectorD, MatrixD}
import scalation.scala2d.{Panel, VizFrame}
import scalation.scala2d.{Ellipse, Line}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{BasicStroke, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FPlot` class takes 'x' and 'y' vectors of data values and plots the '(x, y)'
 *  data points.  Optionally, a 'z' vector may be plotted with 'y'.  Note, axes are
 *  determined by the 'x' and 'y' vectors only.  For more vertical vectors use `PlotM`.
 *  @param x       the horizontal data values
 *  @param fs      a sequence of scalar-to-scalar functions to plot
 *  @param _title  the title of the plot
 *  @param lines   flag for generating a line plot
 */
class FPlot (x: NumericRange [BigDecimal], fs: Seq [FunctionS2S], _title: String = "FPlot", lines: Boolean = false)
      extends VizFrame (_title, null)
{
    def this (x: NumericRange [BigDecimal], y: FunctionS2S, _title: String, lines: Boolean) =
    {
        this (x, Seq(y), _title, lines)
    } // aux. constructor

    getContentPane ().add (new FCanvas (x, fs, getW, getH, lines))
    setVisible (true)
} // FPlot class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FCanvas` class provides a canvas on which to draw the plot.
 *  @param x       the x vector of data values (horizontal)
 *  @param fs      a sequence of scalar-to-scalar functions to plot
 *  @param width   the width
 *  @param height  the height
 *  @param lines   flag for generating a line plot
 */
class FCanvas (x: NumericRange [BigDecimal], fs: Seq [FunctionS2S], width: Int, height: Int, lines: Boolean = false)
      extends Panel
{
    private val _x: VectorD = VectorD (for (i <- x) yield i.toDouble)
    private val _y: MatrixD = MatrixD (for (f <- fs) yield VectorD (for (j <- x) yield f(j.toDouble)), false)

    private val EPSILON   = 1E-9
    private val frameW    = width
    private val frameH    = height
    private val offset    = 50
    private val baseX     = offset
    private val baseY     = frameH - offset
    private val stepsX    = 10
    private val stepsY    = 10
    private val minX      = floor (_x.min ())
    private val maxX      = ceil (_x.max () + EPSILON)
    private val minY      = floor (_y.min ())
    private val maxY      = ceil (_y.max ()) + EPSILON
    private val deltaX    = maxX - minX
    private val deltaY    = maxY - minY
    private val diameter  = 1
    private val r         = diameter / 2
    private val dot       = Ellipse ()
    private val axis      = Line ()

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

        var px_pos = 0 // previous x
        var py_pos = 0 // previous y

        for (i <- _y.range1) {
            val color = randomColor (i)
            g2d.setPaint (color)

            for (j <- _y.range2) {
                val xx = round ((_x(j) - minX) * (frameW - 2 * offset).asInstanceOf [Double])
                x_pos = (xx / deltaX).asInstanceOf [Int] + offset
                val yy = round ((maxY - _y(i, j)) * (frameH - 2 * offset).asInstanceOf [Double])
                y_pos = (yy / deltaY).asInstanceOf [Int] + offset
                dot.setFrame (x_pos, y_pos, diameter, diameter)         // x, y, w, h

                // g2d.setPaint (black)
                g2d.fill (dot)

                // connect with lines
                if (j != 0 && lines) {
                    g2d.setStroke (new BasicStroke (1.0f))
                    g2d.drawLine (px_pos+r, py_pos+r, x_pos+r, y_pos+r)
                } // if

                px_pos = x_pos // update previous x
                py_pos = y_pos // update previous y

            } // for

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
    /** Convert a Plot vectors to a string.
     */
    override def toString = "FPlot (y = " + _y + " vs. x = " + _x + ")"

} // FCanvas class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FPlotTest` object is used to test the `FPlot` class.
 *  > runMain scalation.plot.FPlotTest
 */
object FPlotTest extends App
{
    def f1 (x: Double) = 10.0 * math.sin (2.0 * x) + 50.0
    def f2 (x: Double) = pow (x, 2)
    def f3 (x: Double) = x + 50
    def f4 (x: Double) = 10.0 * math.cos (2.0 * x) + 50.0    
    val hRange = BigDecimal (-1.0) to BigDecimal (10.0) by BigDecimal (0.1)
    val plot = new FPlot (hRange, Seq(f1, f2, f3, f4), lines = true)
    println ("plot = " + plot)

} // FPlotTest object

