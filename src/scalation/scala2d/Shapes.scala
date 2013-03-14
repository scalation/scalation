
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Jan 10 17:08:32 EST 2010
 *  @see     LICENSE (MIT style license file).
 *  -----------------------------------------------------------------------------
 *  The scala2d package defines/redefines Java2D shapes (see java.awt and
 *  java.awt.geom).  The shapes are divided into five groups based on their
 *  dimensionality as well as their base type (class/trait):
 *  Basic:
 *      BasicStroke
 *      Dimension
 *      Shape
 *      Graphics2D
 *      RectangularShape
 *  0D:
 *      R2 redefines java.awt.geom.Point2D.Double
 *  1D:
 *      CurvilinearShape is introduced and the following subtypes are defined
 *      Line    extends Line2D
 *      QCurve  extends QuadCurve
 *      Arrow   Line with an arrowhead
 *      QArrow  QuadCurve with an arrowhead
 *  1-2D:
 *      Path redefines java.awt.geom.Path2D.Double with subtypes
 *      Polygon
 *      Triangle
 *      Quad
 *      Pentagon
 *      Hexagon
 *      Octagon
 *  2D:
 *      RectangularShape redefines java.awt.geom.RectangularShape with subtypes
 *      Arc
 *      Ellipse
 *      Rectangle
 *      RoundRectangle
 *  Note:
 *      CubicCurve is currently not supported
 */

package scalation.scala2d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Scala type aliases for basic Java2D types.
 */
object Shapes
{
    type BasicStroke      = java.awt.BasicStroke
    type Dimension        = java.awt.Dimension
    type Graphics2D       = java.awt.Graphics2D
    type RectangularShape = java.awt.geom.RectangularShape
    type Shape            = java.awt.Shape
} // Shapes


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience case class for Point2D.
 */
case class R2 (xx: Double, yy: Double) extends java.awt.geom.Point2D.Double (xx, yy)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience case class for Path2D.  Its subtypes (case class Polygon, etc.)
 * are defined in other files in the scala2d package.
 */
case class Path () extends java.awt.geom.Path2D.Double


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience case class for Arc2D (a subclass of RectangularShape).
 */
case class Arc () extends java.awt.geom.Arc2D.Double


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience case class for Ellipse2D (a subclass of RectangularShape).
 */
case class Ellipse () extends java.awt.geom.Ellipse2D.Double


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience case class for Rectangle2D (a subclass of RectangularShape).
 */
case class Rectangle () extends java.awt.geom.Rectangle2D.Double


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience case class for RoundRectangle2D (a subclass of RectangularShape).
 */
case class RoundRectangle () extends java.awt.geom.RoundRectangle2D.Double

