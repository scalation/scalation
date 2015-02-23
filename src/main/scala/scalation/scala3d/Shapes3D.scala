
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Jason Kaine
 *  @version 1.1
 *  @date    Sat Dec 20 13:06:20 EST 2014
 *  @see     LICENSE (MIT style license file).
 *  -----------------------------------------------------------------------------
 *  The scala3d package defines/redefines javafx shapes.  The shapes are divided into
 *  five groups based on their dimensionality as well as their base type (class/trait):
 *  Basic:
 *      Geometry
 *      RectangularShape
 *  3D:
 *      Cylinder
 *      MeshView
 *      Point3D
 *
 *  Note:
 *      CubicCurve is currently not supported
 */

package scalation.scala3d

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Shapes` object Scala provides type aliases for basic Java2D types.
 */
object Shapes3D
{
    type Scene             = javafx.scene.Scene
    type GraphicsContext   = javafx.scene.canvas.GraphicsContext
    type Canvas            = javafx.scene.canvas.Canvas
    type Shape3D           = javafx.scene.shape.Shape3D

} // Shapes

case class Box (width: Double, height: Double, depth: Double) extends javafx.scene.shape.Box (width, height,depth)

case class Cylinder (radius: Double, height: Double) extends javafx.scene.shape.Cylinder (radius, height)

case class MeshView () extends javafx.scene.shape.MeshView

case class Sphere (radius: Double) extends javafx.scene.shape.Sphere (radius)

case class Point3D (xx: Double, yy: Double, zz: Double) extends javafx.geometry.Point3D (xx, yy, zz)


