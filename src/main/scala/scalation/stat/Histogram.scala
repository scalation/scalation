
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Nov 2 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.stat

import scala.math.{ceil, floor, min, round}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.random.{Normal, Uniform}
import scalation.scala2d.{Panel, VizFrame}
import scalation.scala2d.{Line, Rectangle}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{BasicStroke, Dimension, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Histogram` class takes a vector of values, counts the number of values
 *  in each of several intervals and displays the counts vertically in a
 *  histogram.
 *  @param value         the vector of values (want several per interval)
 *  @param numIntervals  the number of intervals (typically 5 to 100)
 *  @param _title        title of the histogram
 */
class Histogram (value: VectorD, numIntervals: Int, _title: String = "Histogram")
      extends VizFrame (_title, null)
{
    /** Create a drawing canvas
     */
    val canvas = new Canvas (getW, getH, value, numIntervals)

    getContentPane ().add (canvas)
    setVisible (true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a Histogram to a string.
     */
    override def toString = canvas.toString

} // Histogram class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FramelessHistogram` class should be used in embedded applications.
 *  @param frameW        the width
 *  @param frameH        the height
 *  @param value         the vector of values (want several per interval)
 *  @param numIntervals  the number of intervals (typically 5 to 100)
 *  @param _title        title of the histogram
 */
class FramelessHistogram (frameW: Int, frameH: Int, value: VectorD, numIntervals: Int,
                         _title: String = "Histogram")
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Dynamically create and return a drawing canvas.
     */
    def canvas: Canvas = new Canvas (frameW, frameH, value, numIntervals)

} // FramelessHistogram class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Create a canvas on which to draw the histogram.
 *  @param frameW        the width
 *  @param frameH        the height
 *  @param value         the vector of values (want several per interval)
 *  @param numIntervals  the number of intervals (typically 5 to 100)
 */
class Canvas (frameW: Int, frameH: Int, value: VectorD, numIntervals: Int)
      extends Panel
{
    private val EPSILON       = 1E-9
    private val offset        = 50
    private val baseX         = offset
    private val baseY         = frameH - offset
    private val minValue      = floor (value.min ())
    private val maxValue      = ceil (value.max () + EPSILON)
    private val intervalWidth = (maxValue - minValue) / numIntervals.toDouble  // + EPSILON
    private val histogram     = computeHistogram ()
    private val maxHistogram  = histogram.max ()
    private val c             = computeCoordinates ()
    private val bar           = Rectangle ()
    private val axis          = Line ()

    setBackground (white)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Paint the canvas by drawing the rectangles (vertical bars) making up
     *  the histogram.
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
        step  = (maxValue - minValue) / 10.0
        for (j <- 0 to 10) {
            val x_val = clip (minValue + j * step)
            x_pos = offset - 8 + j * (frameW - 2 * offset) / 10 
            g2d.drawString (x_val, x_pos, y_pos)
        } // for

        x_pos = baseX - 30
        for (j <- 0 to 20) {
            val y_val = clip ((20 - j) * maxHistogram / 20)
            y_pos = offset + 2 + j * (frameH - 2 * offset) / 20 
            g2d.drawString (y_val, x_pos, y_pos)
        } // for

        //:: Draw the bars making up the histogram

        for (p <- c) {
            bar.setFrame (p(0), p(1), p(2), p(3))    // x, y, w, h
            g2d.setPaint (blue)
            g2d.fill (bar)
            g2d.setPaint (black)
            g2d.draw (bar)
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
    /** Compute the counts for each interval in the histogram.
     */
    def computeHistogram (): VectorD =
    {
        val h = new VectorD (numIntervals)
        for (x <- value) h((floor ((x - minValue) / intervalWidth)).toInt) += 1
        h
    } // computeHistogram

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute coordinates (x, y, w, h) for each of bars used to draw the histogram.
     */
    def computeCoordinates (): MatrixD =
    {
        val c      = new MatrixD (numIntervals, 4)
        val w      = (frameW - 2 * offset) / numIntervals.toDouble
        val scale  = (baseY - offset) / maxHistogram

        for (i <- 0 until numIntervals) {
            val h = histogram(i) * scale
            c(i) = VectorD (baseX + i * w, baseY.toDouble - h, w, h)
        } // for
        c
    } // computeCoordinates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a Histogram to a string.
     */
    override def toString = histogram.toString

} // Canvas class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HistogramTest` object is used to test the `Histogram` class.
 *  As 'k' increases, the sum of Uniform approaches Normal.
 *  > run-main scalation.stat.HistogramTest
 */
object HistogramTest extends App
{
    val intervals  = 100
    val samples    = 40000
    val k          = 2               // number of Uniform RV to add up

    val uniformRV   = Uniform (0, 1)
    val uniformDist = new VectorD (samples)
    for (i <- 0 until samples) {
        var sum = 0.0
        for (j <- 0 until k) sum += uniformRV.gen
        uniformDist(i) = sum
    } // for
   
    val h1 = new Histogram (uniformDist, intervals, "Histogram for Sum of Uniform")
    println ("histogram = " + h1)

    val normalRV   = Normal (0, 1)
    val normalDist = new VectorD (samples)
    for (i <- 0 until samples) normalDist(i) = normalRV.gen

    val h2 = new Histogram (normalDist, intervals, "Histogram for Normal")
    println ("histogram = " + h2)

//  val h3 = new Histogram (VectorD (0.0, 2.0, 3.0, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 8.0, 9.0),
//                          5, "Simple Histogram")
//  println ("histogram = " + h3)

} // HistogramTest object

