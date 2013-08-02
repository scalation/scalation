
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.plot

import math.{ceil, floor, min, pow, round}
import swing.{MainFrame, Panel}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.scala2d.{Ellipse, Line}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{BasicStroke, Dimension, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The PlotM class takes an x vector and a y matrix of data values and plots the
 *  (x, y_i) data points for each row y_i of the matrix.
 *  @param x       the x vector of data values (horizontal)
 *  @param y       the y matrix of data values where y(i) is the i-th vector (vertical)
 *  @param _title  the title of the plot
 */
class PlotM (x: VectorD, y: MatrixD, var label: Array [String] = null,
            _title: String = "PlotM y_i vs. x for each i")
      extends MainFrame
{
    private val EPSILON   = 1E-9
    private val frameSize = new Dimension (700, 700)
    private val frameW    = (round (frameSize.getWidth ())).asInstanceOf [Int]
    private val frameH    = (round (frameSize.getHeight ())).asInstanceOf [Int]
    private val offset    = 70
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

    if (label == null) label = defaultLabels

    println ("x-axis: minX = " + minX + " maxX = " + maxX)
    println ("y-axis: minY = " + minY + " maxY = " + maxY)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The PlotM class takes an x vector and a y array of vectors of data values
     *  and plots the (x, y_i) data points for each row y_i of the matrix.
     *  @param x       the x vector of data values (horizontal)
     *  @param y-i     the y array of vectors of data values (vertical)
     *  @param _title  the title of the plot
    def this (x: VectorD, y: Array [VectorD])
    {
        this (x, new MatrixD (y))
    } // constructor
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default labels for y-vector.
     */
    def defaultLabels: Array [String] =
    {
        val l = new Array [String] (y.dim1)
        for (i <- 0 until y.dim1) l(i) = "Vector" + i
        l
    } // defaultLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a canvas on which to draw the plot.
     */
    val canvas = new Panel
    {
        background    = white
        preferredSize = frameSize

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Paint the canvas by plotting the data points.
         *  @param g2d  the high resolution 2D Graphics context 
         */
        override def paintComponent (g2d: Graphics2D)
        {
            super.paintComponent (g2d)
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

            //:: Draw the color keys below the x-axis

            g2d.drawString ("Key:", offset, frameH - 20)

            //:: Draw the dots for the data points being plotted

            for (i <- 0 until y.dim1) {
                val y_i = y(i)
                val color = randomColor (i)
                g2d.setPaint (color)
                g2d.drawString (label(i), offset * (i + 2), frameH - 20)

                for (j <- 0 until x.dim) {
                    val xx = round ((x(j) - minX) * (frameW - 2 * offset).asInstanceOf [Double])
                    x_pos = (xx / deltaX).asInstanceOf [Int] + offset
                    val yy = round ((maxY - y_i(j)) * (frameH - 2 * offset).asInstanceOf [Double])
                    y_pos = (yy / deltaY).asInstanceOf [Int] + offset
                    dot.setFrame (x_pos, y_pos, diameter, diameter)         // x, y, w, h
                    g2d.fill (dot)
                } // for
            } // for
        } // paintComponent

    } // canvas Panel

    {
        title    = _title
        contents = canvas
        visible  = true
    } // primary constructor

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

} // PlotM class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the PlotM class.
 */
object PlotMTest extends App
{
    val x = new VectorD (200)
    val y = new MatrixD (5, 200)
    for (i <- 0 until 200) {
        x(i)    = (i - 100) / 10.0
        y(0, i) = 10.0 * x(i)
        y(1, i) = pow (x(i), 2)
        y(2, i) = .1 * pow (x(i), 3)
        y(3, i) = .01 * pow (x(i), 4)
        y(4, i) = .001 * pow (x(i), 5)
    } // for
    val plot = new PlotM (x, y, Array ("Linear", "Quadratic", "Cubic", "Quartic", "Quintic"))
    println ("plot = " + plot)

} // PlotMTest object

