
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat May 16 13:51:17 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     https://docs.oracle.com/javase/8/javafx/api/javafx/geometry/Point3D.html
 */

package scalation.scala3d

import javafx.application.Application
import javafx.event.{Event, EventHandler, EventTarget}
import javafx.geometry.Point3D
import javafx.scene.{Group, Scene}
import javafx.scene.input.MouseEvent
import javafx.scene.paint.Color
import javafx.scene.transform.{Rotate, Scale, Translate}
import javafx.scene.shape._
import javafx.css.Styleable

import math.{acos, toDegrees}

import Point3D_O.{Y_AXIS, ZERO}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Line3D` class ...
 */
class Line3D (p1: Point3D, p2: Point3D)
      extends Cylinder(1.0, p1.distance (p2))
{
    private val DEBUG = true

    if (DEBUG) println (s"Line3D ($p1, $p2)")
    val midPt = p2.midpoint(p1)
    val translateMidpoint = new Translate (midPt.getX (), midPt.getY (), midPt.getZ ())
    val axis = p2.subtract (p1).crossProduct (Y_AXIS)
    val angle = acos (p2.subtract (p1).normalize ().dotProduct (Y_AXIS))
    val rotation  = new Rotate(- toDegrees (angle), axis)
    getTransforms ().add (translateMidpoint)
    getTransforms ().add (rotation)

} // Line3D class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Line3Dend` class ...
 */
class Line3Dend (p1: Point3D, p2: Point3D)
      extends Line3D (p1, p2)
{
    val sphere1 = new Sphere (4.0)
    val sphere2 = new Sphere (4.0)
        
    sphere1.getTransforms ().add (new Translate (p1.getX (), p1.getY (), p1.getZ ()))
    sphere2.getTransforms ().add (new Translate (p2.getX (), p2.getY (),p2. getZ ()))
        
    def getSpheres () = (sphere1, sphere2) 

} // Line3Dend class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Line3DHelper` class ...
 */
case class Line3DHelper () extends javafx.application.Application ()
{
    val root = new Group()
    val camera = PerspectiveCamera ()
    var anchorX: Double = _
    var anchorY: Double = _
    var anchorAngle: Double = _

    def mkEventHandler[E <: Event](f: E => Unit) = new EventHandler[E] { def handle(e: E) = f(e) }

    override def start (primaryStage: javafx.stage.Stage)
    {
        println ("Line3D.start")
//      camera.buildCamera ()
        val p1 = Point3D.ZERO
        val p2 = new Point3D (50, -100, 50)

        val line = new Line3Dend (p1, p2)
        
        val s = line.getSpheres ()
        val group1 = new Group (s._1)
        val group2 = new Group (s._2)
        val group3 = new Group (line)
        root.getChildren ().addAll (group1, group2, group3)
        try { 
            val scene = new Scene (root, 1024, 768, true)
            scene.setOnMousePressed (mkEventHandler ((event: MouseEvent) =>
            {
                anchorX = event.getSceneX ()
                anchorY = event.getSceneY ()
                anchorAngle = camera.getRotate ()
            }))
            scene.setOnMouseDragged (mkEventHandler ((event: MouseEvent) =>
            {
                camera.setRotate (anchorAngle + anchorX - event.getSceneX ())
            }))
            scene.setFill (javafx.scene.paint.Color.YELLOW)
            primaryStage.setTitle ("scala3d sample")
            primaryStage.setScene (scene)
            primaryStage.show ()
            scene.setCamera (camera)
        } catch {
            case e: Exception => e.printStackTrace ()
        } // try
    } // start
    
} // Line3DHelper class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Line3DTest` object is used to test the `Line3D` class.
 *  > runMain scalation.scala3d.Line3DTest
 */
object Line3DTest
{
    def main (args: Array [String])
    {
        javafx.application.Application.launch(classOf [Line3DHelper], args: _*)
    } // main

} // Line3DTest object

