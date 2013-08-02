
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Oct 17 16:01:39 EDT 2011
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.plot

import collection.mutable.ArrayBuffer
import math.{ceil, floor, min, pow, round}
import swing.{MainFrame, Panel}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.calculus.Calculus.FunctionV2S
import scalation.scala2d.{Ellipse, Line, Rectangle}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{BasicStroke, Dimension, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Contour class takes a function f and diplays color-coded values for f(x, y)
 *  over a two dimensional grid defined the lower (lb) and upper (ub) bounds.
 *  An optional path is included that can be used to show, for example, the
 *  search path taken by an optimzer (e.g., a Conjugate Gradient NLP solver).
 *  @param f       the function whose color-coded contour plot is sought
 *  @param lb      the lower bounds on the plotting domain
 *  @param ub      the upper bounds on the plotting domain
 *  @param path    the points on a path (e.g., a search path)
 *  @param deltaF  estimate of the range of possible functional values (if < 0, will be computed)
 *  @param lbF     the lower bound on the functional value
 *  @param _title  the title of the plot
 */
class Contour (f: FunctionV2S, lb: VectorD, ub: VectorD, path: ArrayBuffer [VectorD] = null,
               private var deltaF: Double = -1.0, private var lbF: Double = 0.0,
               _title: String = "Contour plot of f(x, y)")
      extends MainFrame
{
    private val EPSILON   = 1E-9
    private val _1_3      = 1.0 / 3.0
    private val _2_3      = 2.0 / 3.0
    private val frameSize = new Dimension (600, 600)
    private val frameW    = (round (frameSize.getWidth ())).asInstanceOf [Int]
    private val frameH    = (round (frameSize.getHeight ())).asInstanceOf [Int]
    private val offset    = 50
    private val baseX     = offset
    private val baseY     = frameH - offset
    private val stepsX    = 10
    private val stepsY    = 10
    private val minX      = floor (lb(0))
    private val maxX      = ceil (ub(0))
    private val minY      = floor (lb(1))
    private val maxY      = ceil (ub(1))
    private val deltaX    = maxX - minX
    private val deltaY    = maxY - minY
    private val width     = 9
    private val diameter  = 6
    private val square    = Rectangle ()
    private val dot       = Ellipse ()
    private val axis      = Line ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a canvas on which to draw the contour plot.
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

            //:: Draw squares for the color-coded values of the points of the function being plotted

            for (x <- lb(0) to ub(0) by deltaX/50.0; y <- lb(1) to ub(1) by deltaY/50.0) {
                val vec  = VectorD (x, y)
                val frac = (f(vec) - lbF) / deltaF         // fractional way from lower to upper bound

                val rgb = if (frac > _2_3)                 // Red-Green-Blue (RGB) tuple values
                              ( ((frac-_2_3) * 765).toInt, ((1-frac) * 765).toInt, 0 )
                          else if (frac > _1_3)
                              ( 0, ((frac-_1_3) * 765).toInt, ((_2_3-frac) * 765).toInt )
                          else
                              ( ((_1_3-frac) * 400).toInt, 0, ((frac) * 765).toInt )
                println ("frac = " + frac + ", rgb = " + rgb)
                val color = new Color (rgb._1, rgb._2, rgb._3)

                val xx    = round ((x - lb(0)) * (frameW - 2 * offset).asInstanceOf [Double])
                x_pos     = (xx / deltaX).asInstanceOf [Int] + offset
                val yy    = round ((ub(1) - y) * (frameH - 2 * offset).asInstanceOf [Double])
                y_pos     = (yy / deltaY).asInstanceOf [Int] + offset - diameter
                square.setFrame (x_pos, y_pos, width, width)         // x, y, w, h
                g2d.setPaint (color)
                g2d.fill (square)
            } // for 

            //:: Draw the dots for the points on a search path, if given

            if (path != null) {
                for (p <- path) {
                    val xx    = round ((p(0) - lb(0)) * (frameW - 2 * offset).asInstanceOf [Double])
                    x_pos     = (xx / deltaX).asInstanceOf [Int] + offset
                    val yy    = round ((ub(1) - p(1)) * (frameH - 2 * offset).asInstanceOf [Double])
                    y_pos     = (yy / deltaY).asInstanceOf [Int] + offset - diameter
                    dot.setFrame (x_pos, y_pos, diameter, diameter)      // x, y, w, h
                    g2d.setPaint (yellow)
                    g2d.fill (dot)
                } // for
            } // if
        } // paintComponent

        
    } // canvas Panel

    {
        if (deltaF < 0.0) resetBounds ()
        title    = _title
        contents = canvas
        visible  = true
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the bounds on the functional values of f.  If the caller fails to
     *  provide an estimate for deltaF, this method should be called.
     */
    def resetBounds ()
    {
        var minF = Double.PositiveInfinity
        var maxF = Double.NegativeInfinity
        for (x <- lb(0) to ub(0) by deltaX/50.0; y <- lb(1) to ub(1) by deltaY/50.0) {
            val vec   = VectorD (x, y)
            val f_vec = f(vec)
            if (f_vec < minF) minF = f_vec
            if (f_vec > maxF) maxF = f_vec
        } // for
        lbF    = minF              // lower bounds on functional values for f
        deltaF = maxF - minF       // range of functional values for f
    } // resetBounds

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
    /** Convert basic Contour information to a string.
     */
    override def toString = "Contour (lb = " + lb + ", f(lb) = " + f(lb) +
                                   ", ub = " + ub + ", f(ub) = " + f(ub) + ")"

} // Contour class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Contour class.
 */
object ContourTest extends App
{
    def f(x: VectorD): Double = (x(0)/2.0 - 3.0) * (x(0)/2.0 - 3.0) + (x(1)/3.0 - 2.0) * (x(1)/3.0 - 2.0)
    val lb     = VectorD (0.0, 0.0)
    val ub     = VectorD (10.0, 10.0)
    val deltaF = 18.0
    val path   = ArrayBuffer (VectorD (0.0, 0.0), VectorD (3.0, 2.0), VectorD (6.0, 6.0))
    val plot   = new Contour (f, lb, ub, path)
    println ("plot = " + plot)

} // ContourTest object

