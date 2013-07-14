
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Mon Sep 21 15:05:06 EDT 2009
 * @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import actors.Actor
import math.{abs, pow, sqrt}
import swing.{MainFrame, Panel}

import scalation.scala2d.Colors._
import scalation.scala2d.QCurveCalc.computeControlPoint
import scalation.scala2d.Shapes.{Dimension, Graphics2D}
import scalation.util.Error

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The QCurve class enhances the QuadCurve.Double class (from the java.awt.geom
 * package) by allowing entities to move along such quadratic curves as well as
 * lines.  Although the curve could be developed as a quadratic function where
 * y = ax^2 + bx + c.  The following quadratic bezier formulation is used:
 * p(t) = (x(t), y(t)) = [(1-t)^2 * p1] + [2 * (1-t) * t * pc] + [t^2 * p2].
 * @param p1        the starting point for the quad curve
 * @param pc        the control point for the quad curve
 * @param p2        the ending point for the quad curve
 * @param straight  whether the quad curve is straight (i.e., a line)
 */
case class QCurve (var p1:       R2      = R2 (0., 0.),
                   var pc:       R2      = R2 (0., 0.),
                   var p2:       R2      = R2 (0., 0.),
                   var straight: Boolean = true)
     extends java.awt.geom.QuadCurve2D.Double (p1.x, p1.y, pc.x, pc.y, p2.x, p2.y)
     with CurvilinearShape with Error
{
    /** Trajectory parameter t ranges from 0. to 1. (indicates how far along the curve)
     */
    private var t = 0.

    /** Number of discrete steps to take along trajectory
     */
    private var steps = 200

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Construct a straight line (degenerate quad curve).
     * @param p1  the starting point
     * @param p2  the ending point
     */
    def this (p1: R2, p2: R2)
    {
        this (p1, computeControlPoint (p1, p2), p2, true)
    } // constructor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Construct a quad curve where bend indicates the distance to the control
     * point.
     * @param p1    the starting point
     * @param p2    the ending point
     * @param bend  the bend or curvature (1. => line length)
     */
    def this (p1: R2, p2: R2, bend: Double)
    {
        this (p1, computeControlPoint (p1, p2, bend), p2, false) 
    } // constructor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Construct a quad curve using an explicitly given control point.
     * @param p1  the starting point
     * @param pc  the control point
     * @param p2  the ending point
     */
    def this (p1: R2, pc: R2, p2: R2)
    {
        this (p1, pc, p2, false) 
    } // constructor

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the x-coordinate of the center of the line/curve.
     */
    def getCenterX (): Double =
    {
        if (pc.x > 0.) (p1.x + 2. * pc.x + p2.x) / 4.
        else           (p1.x + p2.x) / 2.
    } // getCenterX

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the y-coordinate of the center of the line/curve.
     */
    def getCenterY (): Double =
    {
        if (pc.y > 0.) (p1.y + 2. * pc.y + p2.y) / 4.
        else           (p1.y + p2.y) / 2.
    } // getCenterY

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the location for the QCurve as a line.
     * @param _p1  the starting point
     * @param _p2  the ending point
     */
    def setLine (_p1: R2, _p2: R2)
    {
        p1 = _p1; p2 = _p2
        pc = computeControlPoint (p1, p2)         // middle, on line => line
        super.setCurve (p1, pc, p2)
    } // setLine

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the location for the QCurve as a curve using bend
     * to compute the control point.
     * @param _p1   the starting point
     * @param _p2   the ending point
     * @param bend  the bend or curvature (1. => line-length)
     */
    def setLine (_p1: R2, _p2: R2, bend: Double)
    {
        p1 = _p1; p2 = _p2
        pc = computeControlPoint (p1, p2, bend)   // off line => curve
        straight = false
        super.setCurve (p1, pc, p2)
    } // setLine

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set (or reset) the location for the QCurve as a curve using an
     * explicitly given control point.
     * @param _p1  the starting point
     * @param _pc  the control point
     * @param _p2  the ending point
     */
    override def setLine (_p1: R2, _pc: R2, _p2: R2)
    {
        p1 = _p1; pc = _pc; p2 = _p2
        straight = false
        super.setCurve (p1, pc, p2)
    } // setLine

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the first/start point of the quad curve.
     */
    def getFirst: R2 = p1

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the first/start point of the quad curve, adjusted from top-left to
     * center coordinates.
     * @param width   the width of object traversing the curve
     * @param height  the height of object traversing the curve
     */
    def getFirst (width: Double, height: Double): R2 = 
    {
        R2 (p1.x + width / 2., p1.y + height / 2.)
    } // getFirst

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the control point of the quad curve.
     */
    def getControl: R2 = pc
 
    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the last/end point of the quad curve.
     */  
    def getLast: R2 = p2

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Get the last/end point of the quad curve, adjusted from top-left to
     * center coordinates.
     * @param width   the width of object traversing the curve
     * @param height  the height of object traversing the curve
     */
    def getLast (width: Double, height: Double): R2 = 
    {
        R2 (p2.x + width / 2., p2.y + height / 2.)
    } // getLast

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Are (x, y) and (xe, ye) essentially the same?
     */
    def isSame (x: Double, y: Double, xe: Double, ye: Double, step: Double): Boolean =
    {
        (xe - x) * (xe - x) + (ye - y) * (ye -y) < step * step
    } // isSame

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Given a value for the trajectory parameter t (in [0., 1.]) calculate
     * the point on the curve using the Quadratic Bezier equation.
     * See http://en.wikipedia.org/wiki/Bézier_curve#Quadratic_curves
     */
    def eval (): R2 =
    {
       R2 (pow (1.-t, 2) * p1.x + 2. * (1.-t) * t * pc.x + pow (t, 2) * p2.x,
           pow (1.-t, 2) * p1.y + 2. * (1.-t) * t * pc.y + pow (t, 2) * p2.y)
    } // eval

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Return the next point on the quad curve (one step beyond current point).
     * Return null if t > 1. (i.e., past end point).
     */
    def next (): R2 =
    {
        var q: R2 = null                        // the next point along the curve
        if (t > 1.) {
            t = 0.                              // reset trajectory
        } else {
            q = eval ()                         // calculate the new point
        } // if
        t += 1. / steps.asInstanceOf [Double]   // increment trajectory parameter
        // println ("QCurve.next: q = " + q)
        q
    } // next

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Return the next point on the quad curve (one step beyond current point)
     * and adjust from top-left to center coordinates for the object traversing
     * the curve based on its width and height.
     * Return null if t > 1. (i.e., past end point).
     * @param width   the width of object traversing the curve
     * @param height  the height of object traversing the curve
     */
    override def next (width: Double, height: Double): R2 =
    {
        val q = next ()
        if (q != null) R2 (q.x - width / 2., q.y - height / 2.) else null
    } // next

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Set the number of steps for tokens to take as move along the quad curve.
     * @param steps  the number of steps to take along the quad curve
     */
    def setSteps (_steps: Int)
    {
        steps = _steps
    } // setSteps

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Show the start, control and end points of the the QCurve.
     */
    override def toString: String =
    {
        "QCurve ( " + p1 + " , " + pc + " , " + p2 + " )"
    } // toString

} // QCurve class

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object provides formulas used by the QCurve class.
 */
object QCurveCalc
{
    /** Tolerance for comparing real numbers
     */
    private val EPSILON = 1E-7

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the slope of the line defined by points p1 and p2.
     * Note: if deltaX is 0, the method returns infinity.
     * @param p1  the starting point
     * @param p2  the ending point
     */
    def slope (p1: R2, p2: R2): Double =
    {
        (p2.y - p1.y) / (p2.x - p1.x)
    } // slope

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the distance or the length of the line connecting points p1
     * and p2.
     * @param p1  the starting point
     * @param p2  the ending point
     */
    def distance (p1: R2, p2: R2): Double =
    {
        sqrt (pow (p2.x - p1.x, 2) + pow (p2.y - p1.y, 2))
    } // slope

    /**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     * Compute the location (x, y) of the control point.  It is positioned
     * orthogonally to the mid point of the line connecting p1 and p2 at a
     * distance dist, where dist = bend * || p2 - p1 ||.  A bend of 0. gives
     * a straight line, while 2./-2. gives a huge bend up-right/down-left.
     * @param p1    the starting point
     * @param p2    the ending point
     * @param bend  the bend or curvature 
     */
    def computeControlPoint (p1: R2, p2: R2, bend: Double = 0.): R2 =
    {
        val mid = R2 ((p1.x + p2.x) / 2., (p1.y + p2.y) / 2.)
        if (abs (bend) < EPSILON) {
            mid
        } else {
            val m    = slope (p1, p2)
            val dist = bend * distance (p1, p2)
            if (m.isInfinity) {		
                R2 (mid.x + dist, mid.y)
            } else {
                R2 (mid.x + dist * m / sqrt (1. + pow (m, 2)), mid.y - dist / sqrt (1. + pow (m, 2)))
            } // if
        } // if
    } // computeControlPoint

} // QCurveCalc object

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The QCurveTest object tests QCurve's (quad curves).
 */
object QCurveTest extends App
{
    private val line1 = new QCurve (R2 (200, 200), R2 (400, 200))
    private val line2 = new QCurve (R2 (200, 200), R2 (200, 400))
    private val line3 = new QCurve (R2 (200, 200), R2 (400, 400))

    private val curve1 = new QCurve (R2 (200, 200), R2 (400, 200), 1.)
    private val curve2 = new QCurve (R2 (200, 200), R2 (200, 400), 1.)
    private val curve3 = new QCurve (R2 (200, 200), R2 (400, 400), 1.)

    private val curve4 = new QCurve (R2 (200, 200), R2 (400, 200), -2.)
    private val curve5 = new QCurve (R2 (200, 200), R2 (200, 400), -2.)
    private val curve6 = new QCurve (R2 (200, 200), R2 (400, 400), -2.)

    private val canvas = new Panel
    {
        background    = white
        preferredSize = new Dimension (600, 600)

        override def paintComponent (g2d: Graphics2D)
        {
            super.paintComponent (g2d)
            g2d.setPaint (red)
            g2d.draw (line1)
            g2d.draw (curve1)
            g2d.draw (curve4)
            g2d.setPaint (blue)
            g2d.draw (line2)
            g2d.draw (curve2)
            g2d.draw (curve5)
            g2d.setPaint (purple)
            g2d.draw (line3)
            g2d.draw (curve3)
            g2d.draw (curve6)
        } // paintComponent

    } // canvas Panel

    private def top = new MainFrame
    {
        title    = "QCurveTest"
        contents = canvas
        visible  = true
    } // top MainFrame

    println ("Run QCurveTest")
    top

} // QCurveTest object

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * The QCurveTest2 object tests traversals of QCurve's (quad curves).
 */
object QCurveTest2 extends App
{
    private class QCurveAnimator extends MainFrame with Actor
    {
        val curve = Array (new QCurve (R2 (100, 200), R2 (500, 200)),
                           new QCurve (R2 (100, 200), R2 (500, 200), .5),
                           new QCurve (R2 (100, 200), R2 (500, 200), -.5))
        val ball  = Ellipse ()

        val canvas = new Panel
        {
            background    = white
            preferredSize = new Dimension (600, 600)

            override def paintComponent (g2d: Graphics2D)
            {
                super.paintComponent (g2d)
                g2d.setPaint (red)           // R in RGB order
                g2d.draw (curve(0))
                g2d.setPaint (green)         // G in RGB order
                g2d.draw (curve(1))
                g2d.setPaint (blue)          // B in RGB order
                g2d.draw (curve(2))
                g2d.setPaint (purple)
                g2d.fill (ball)
            } // paintComponent

        } // canvas Panel

        def act ()
        {
             val size    = 10.
             var loc: R2 = null
             
             for (i <- 0 until curve.length) {
                 println ("Move ball along RGB curve " + i)
                 loc = curve(i).next (size, size)
                 while (loc != null) {
                     Thread.sleep (50)
                     ball.setFrame (loc.x, loc.y, size, size)
                     repaint ()
                     loc = curve(i).next (size, size)
                 } // while
             } // for
        } // act

        title    = "QCurveTest2"
        contents = canvas
        visible  = true
        start ()

    } // QCurveAnimator class

    println ("Run QCurveTest2")
    new QCurveAnimator ()

} // QCurveTest2 object

