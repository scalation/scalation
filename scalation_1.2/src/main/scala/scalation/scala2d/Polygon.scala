
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Oct 22 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file). 
 */

package scalation.scala2d

import scala.math.{cos, Pi, sin}

import scalation.scala2d.Colors._
import scalation.scala2d.Constants._
import scalation.scala2d.Shapes.{Dimension, Graphics, Graphics2D}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Polygon` class enhances the `Path2D.Double` class (from the `java.awt.geom`
 *  package) by adding a constructor for building a polygon given its vertices.
 *  @param vertex  the n >= 3 corner points of the polygon
 */
case class Polygon (vertex: Array [R2])
     extends java.awt.geom.Path2D.Double
{
    {
        val n = vertex.length
        if (n < 3) println ("Polygon.constructor", "need at least 3 vertices to make a polygon")
        moveTo (vertex(0).x, vertex(0).y)
        for (i <- 1 until n) lineTo (vertex(i).x, vertex(i).y)
        lineTo (vertex(0).x, vertex(0).y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the top-left coordinates of the polygons bounding box.
     */
    def getTopLeft: R2 =
    {
        val bounds = getBounds2D ()
        R2 (bounds.getX (), bounds.getY ())
    } // getTopLeft

} // Polygon class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Triangle class enhances the Path2D.Double class (from the `java.awt.geom`
 *  package) by adding a constructor for building a triangle given its vertices.
 *  @param vertex  the three corner points of the triangle
 */
case class Triangle (vertex: Array [R2])
     extends java.awt.geom.Path2D.Double
{
    {
        val n = vertex.length
        if (n != 3) println ("Triangle.constructor", "need exactly 3 vertices to make a triangle")
        moveTo (vertex(0).x, vertex(0).y)
        for (i <- 1 until n) lineTo (vertex(i).x, vertex(i).y)
        lineTo (vertex(0).x, vertex(0).y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a Right Isosceles Triangle.
     *  @param topLeft  the top left point for the triangle (min x and y coordinates)
     *  @param side     the length of the two sides emanating from top-left
     */
    def this (topLeft: R2, side: Double)
    {
        this (Array [R2] (topLeft,
                          R2 (topLeft.x + side, topLeft.y),
                          R2 (topLeft.x, topLeft.y + side)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a Right Triangle.
     *  @param topLeft  the top left point for the triangle (min x and y coordinates)
     *  @param side1    the width of the triangle (change in x)
     *  @param side2    the height of the triangle (change in y)
     */
    def this (topLeft: R2, side1: Double, side2: Double)
    {
        this (Array [R2] (topLeft,
                          R2 (topLeft.x + side1, topLeft.y),
                          R2 (topLeft.x, topLeft.y + side2)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the top-left coordinates of the polygons bounding box.
     */
    def getTopLeft: R2 =
    {
        val bounds = getBounds2D ()
        R2 (bounds.getX (), bounds.getY ())
    } // getTopLeft

} // Triangle class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Quad` class enhances the `Path2D.Double` class (from the `java.awt.geom`
 *  package) by adding a constructor for building a quadrilateral given its vertices.
 *  @param vertex  the four corner points of the quadrilateral
 */
case class Quad (vertex: Array [R2])
     extends java.awt.geom.Path2D.Double
{
    {
        val n = vertex.length
        if (n != 4) println ("Quad.constructor", "need exactly 4 vertices to make a quad")
        moveTo (vertex(0).x, vertex(0).y)
        for (i <- 1 until n) lineTo (vertex(i).x, vertex(i).y)
        lineTo (vertex(0).x, vertex(0).y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a square.
     *  @param topLeft  the top left point for the square (min x and y coordinates)
     *  @param side     the length of each side in the square
     */
    def this (topLeft: R2, side: Double)
    {
        this (Array [R2] (topLeft,
                          R2 (topLeft.x + side, topLeft.y),
                          R2 (topLeft.x + side, topLeft.y + side),
                          R2 (topLeft.x, topLeft.y + side)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a parallelogram (rectangle if shift is 0).
     *  @param topLeft  the top left point for the parallelogram (min x and y coordinates).
     *  @param side1    the width of the parallelogram (change in x)
     *  @param side2    the height of the parallelogram (change in y)
     *  @param shift    the x-shift between top and bottom sides 
     */
    def this (topLeft: R2, side1: Double, side2: Double, shift: Double = 0.0)
    {
        this (Array [R2] (topLeft,
                          R2 (topLeft.x + side1, topLeft.y),
                          R2 (topLeft.x + side1 + shift, topLeft.y +side2),
                          R2 (topLeft.x + shift, topLeft.y + side2)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the top-left coordinates of the polygons bounding box.
     */
    def getTopLeft: R2 =
    {
        val bounds = getBounds2D ()
        R2 (bounds.getX (), bounds.getY ())
    } // getTopLeft

} // Quad class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Pentagon` class enhances the `Path2D.Double` class (from the `java.awt.geom`
 *  package) by adding a constructor for building a pentagon given its vertices.
 *  @param vertex  the five corner points of the pentagon
 */
case class Pentagon (vertex: Array [R2])
     extends java.awt.geom.Path2D.Double
{
    {
        val n = vertex.length
        if (n != 5) println ("Pentagon.constructor", "need exactly 5 vertices to make a pentagon")
        moveTo (vertex(0).x, vertex(0).y)
        for (i <- 1 until n) lineTo (vertex(i).x, vertex(i).y)
        lineTo (vertex(0).x, vertex(0).y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a equilateral `Pentagon`.
     *  @param topLeft  the top left point for the pentagon (min x and y coordinates)
     *  @param side     the length of each side in the pentagon
     */
    def this (topLeft: R2, side: Double)
    {
        this (Array [R2] (R2 (topLeft.x + cos72 * side, topLeft.y),
                          R2 (topLeft.x + (1.0 + cos72) * side, topLeft.y),
                          R2 (topLeft.x + (1.0 + 2.0 * cos72) * side, topLeft.y + sin72 * side),
                          R2 (topLeft.x + (0.5 + cos72) * side, topLeft.y + (1.0 + 2.0 * cos72) * side),
                          R2 (topLeft.x, topLeft.y + sin72 * side)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the top-left coordinates of the polygons bounding box.
     */
    def getTopLeft: R2 =
    {
        val bounds = getBounds2D ()
        R2 (bounds.getX (), bounds.getY ())
    } // getTopLeft

} // Pentagon class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Hexagon` class enhances the `Path2D.Double` class (from the `java.awt.geom`
 *  package) by adding a constructor for building a hexagon given its vertices.
 *  @param vertex  the six corner points of the hexagon
 */
case class Hexagon (vertex: Array [R2])
     extends java.awt.geom.Path2D.Double
{
    {
        val n = vertex.length
        if (n != 6) println ("Hexagon.constructor", "need exactly 6 vertices to make a hexagon")
        moveTo (vertex(0).x, vertex(0).y)
        for (i <- 1 until n) lineTo (vertex(i).x, vertex(i).y)
        lineTo (vertex(0).x, vertex(0).y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a equilateral `Hexagon`.
     *  @param topLeft  the top left point for the hexagon (min x and y coordinates)
     *  @param side     the length of each side in the hexagon
     */
    def this (topLeft: R2, side: Double)
    {
        this (Array [R2] (topLeft,
                          R2 (topLeft.x + side, topLeft.y),
                          R2 (topLeft.x + (1.0 + cos60) * side, topLeft.y + sin60 * side),
                          R2 (topLeft.x + side, topLeft.y + 2.0 * sin60 * side),
                          R2 (topLeft.x, topLeft.y + 2.0 * sin60 * side),
                          R2 (topLeft.x - cos60 * side, topLeft.y + sin60 * side)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the top-left coordinates of the polygons bounding box.
     */
    def getTopLeft: R2 =
    {
        val bounds = getBounds2D ()
        R2 (bounds.getX (), bounds.getY ())
    } // getTopLeft

} // Hexagon class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Octagon` class enhances the `Path2D.Double` class (from the `java.awt.geom`
 *  package) by adding a constructor for building an octagon given its vertices.
 *  @param vertex  the eight corner points of the octagon
 */
case class Octagon (vertex: Array [R2])
     extends java.awt.geom.Path2D.Double
{
    {
        val n = vertex.length
        if (n != 8) println ("Octagon.constructor", "need exactly 8 vertices to make an octagon")
        moveTo (vertex(0).x, vertex(0).y)
        for (i <- 1 until n) lineTo (vertex(i).x, vertex(i).y)
        lineTo (vertex(0).x, vertex(0).y)
    } // primary constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a equi-lateral `Octagon`.
     *  @param topLeft  the top left point for the octagon (min x and y coordinates)
     *  @param side     the length of each side in the octagon
     */
    def this (topLeft: R2, side: Double)
    {
        this (Array [R2] (R2 (topLeft.x + cos45 * side, topLeft.y),
                          R2 (topLeft.x + (1.0 + cos45) * side, topLeft.y),
                          R2 (topLeft.x + (1.0 + 2.0 * cos45) * side, topLeft.y + cos45 * side),
                          R2 (topLeft.x + (1.0 + 2.0 * cos45) * side, topLeft.y + (1.0 + cos45) * side),
                          R2 (topLeft.x + (1.0 + cos45) * side, topLeft.y + (1.0 + 2.0 * cos45) * side),
                          R2 (topLeft.x + cos45 * side, topLeft.y + (1.0 + 2.0 * cos45) * side),
                          R2 (topLeft.x, topLeft.y + (1.0 + cos45) * side),
                          R2 (topLeft.x, topLeft.y + cos45 * side)))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the top-left coordinates of the polygons bounding box.
     */
    def getTopLeft: R2 =
    {
        val bounds = getBounds2D ()
        R2 (bounds.getX (), bounds.getY ())
    } // getTopLeft

} // Octagon class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Useful constants.
 */
object Constants
{
    val cos45 = cos (Pi / 4.0)     // same value for sin45
    val cos60 = cos (Pi / 3.0)
    val sin60 = sin (Pi / 3.0)
    val cos72 = cos (2.0 * Pi / 5.0)
    val sin72 = sin (2.0 * Pi / 5.0)

} // Constants object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolygonTest` object tests the `Polygon`, `Triangle`, `Quad`, `Hexagon`
 *  and Octagon classes.
 */
object PolygonTest extends App
{
    private val dot        = Ellipse ()
    private val triangle   = new Triangle (R2 (100, 100), 100, 150)
    private val triangleXY = triangle.getTopLeft
    private val square     = new Quad     (R2 (400, 100), 150)
    private val squareXY   = square.getTopLeft
    private val parogram   = new Quad     (R2 (100, 350), 150, 100, 25)
    private val parogramXY = parogram.getTopLeft
    private val pentagon   = new Pentagon (R2 (400, 350), 90)
    private val pentagonXY = pentagon.getTopLeft
    private val hexagon    = new Hexagon  (R2 (100, 600), 80)
    private val hexagonXY  = hexagon.getTopLeft
    private val octagon    = new Octagon  (R2 (400, 600), 70)
    private val octagonXY  = octagon.getTopLeft

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
            g2d.setPaint (red);     g2d.fill (triangle)
            g2d.setPaint (black);   dot.setFrame (triangleXY.x, triangleXY.y, 5, 5); g2d.fill (dot)
            g2d.setPaint (green);   g2d.fill (square)
            g2d.setPaint (black);   dot.setFrame (squareXY.x, squareXY.y, 5, 5); g2d.fill (dot)
            g2d.setPaint (blue);    g2d.fill (parogram)
            g2d.setPaint (black);   dot.setFrame (parogramXY.x, parogramXY.y, 5, 5); g2d.fill (dot)
            g2d.setPaint (yellow);  g2d.fill (pentagon)
            g2d.setPaint (black);   dot.setFrame (pentagonXY.x, pentagonXY.y, 5, 5); g2d.fill (dot)
            g2d.setPaint (cyan);    g2d.fill (hexagon)
            g2d.setPaint (black);   dot.setFrame (hexagonXY.x, hexagonXY.y, 5, 5); g2d.fill (dot)
            g2d.setPaint (magenta); g2d.fill (octagon)
            g2d.setPaint (black);   dot.setFrame (octagonXY.x, octagonXY.y, 5, 5); g2d.fill (dot)
        } // paintComponent

    } // Canvas class

    // Put the drawing canvas in the visualization frame

    new VizFrame ("PolygonTest", new Canvas (), 700, 900)

} // PolygonTest object

