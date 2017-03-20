
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Tue May 13 16:18:42 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *  -----------------------------------------------------------------------------
 *  The scala2d package defines/redefines Java2D shapes (see `java.awt` and
 *  `java.awt.geom`).  The shapes are divided into five groups based on their
 *  dimensionality as well as their base type (class/trait):
 *  Basic:
 *      `BasicStroke`
 *      `Dimension`
 *      `Shape`
 *      `Graphics`
 *      `Graphics2D`
 *      `RectangularShape`
 *  0D:
 *      `R2 redefines `java.awt.geom.Point2D.Double`
 *  1D:
 *      //`CurvilinearShape` is introduced and the following subtypes are defined
 *      `Line`    extends `Line2D`
 *      `QCurve`  extends `QuadCurve`
 *      `Arrow`   `Line` with an arrowhead
 *      `QArrow`  `QuadCurve` with an arrowhead
 *  1-2D:
 *      `Path redefines `java.awt.geom.Path2D.Double` with subtypes
 *      `Polygon`
 *      `Triangle`
 *      `Quad`
 *      `Pentagon`
 *      `Hexagon`
 *      `Octagon`
 *  2D:
 *      `RectangularShape redefines `java.awt.geom.RectangularShape` with subtypes
 *      `Arc`
 *      `Ellipse`
 *      `Rectangle`
 *      `RoundRectangle`
 *  Note:
 *      `CubicCurve` is currently not supported
 */

package scalation.scala2d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Shapes` object Scala provides type aliases for basic Java2D types.
 */
object Shapes
{
    type BasicStroke      = java.awt.BasicStroke
    type Dimension        = java.awt.Dimension
    type Graphics         = java.awt.Graphics
    type Graphics2D       = java.awt.Graphics2D
    type RectangularShape = java.awt.geom.RectangularShape
    type Shape            = java.awt.Shape
} // Shapes


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Arc` is a convenience case class for `Arc2D` (a subclass of `RectangularShape`).
 */
case class Arc () extends java.awt.geom.Arc2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Ellipse` is a convenience case class for Ellipse2D (a subclass of `RectangularShape`).
 */
case class Ellipse () extends java.awt.geom.Ellipse2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Line` is a convenience case class for `Line2D` (a subclass of `RectangularShape`).
 */
case class Line (var p1:  R2 = R2 (0.0, 0.0),
                 var p2:  R2 = R2 (0.0, 0.0))
     extends java.awt.geom.Line2D.Double (p1, p2)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Path` is a convenience case class for `Path2D.`  Its subtypes (case class `Polygon`,
    etc.) are defined in other files in the `scala2d` package.
 */
case class Path () extends java.awt.geom.Path2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `Rectangle` is a convenience case class for `Rectangle2D `(a subclass of
 *  `RectangularShape`).
 */
case class Rectangle () extends java.awt.geom.Rectangle2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `RoundRectangle` is a convenience case class for `RoundRectangle2D` (a subclass of
 *  `RectangularShape`).
 */
case class RoundRectangle () extends java.awt.geom.RoundRectangle2D.Double

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `R2` is a convenience case class for `Point2D`.
 */
case class R2 (xx: Double, yy: Double) extends java.awt.geom.Point2D.Double (xx, yy)

