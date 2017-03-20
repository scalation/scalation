
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun May 17 15:07:22 EDT 2015
 *  @see     LICENSE (MIT style license file)
 *           Also see Oracle Copyright below
 *
 *  Translated to Scala from MoleculeSampleApp
 *  @see https://docs.oracle.com/javase/8/javafx/graphics-tutorial/sampleapp3d.htm
 *  @see https://docs.oracle.com/javase/8/javafx/graphics-tutorial/sampleapp3d-code.htm
 */

/*
 * Copyright (c) 2013, 2014 Oracle and/or its affiliates.
 * All rights reserved. Use is subject to license terms.
 *
 * This file is available and licensed under the following license:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *  - Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the distribution.
 *  - Neither the name of Oracle nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES LOSS OF USE,
 * DATA, OR PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package scalation.scala3d

import javafx.application.Application
import javafx.event.EventHandler
import javafx.scene._
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.paint.Color
import javafx.scene.shape.{Box, Cylinder, Sphere}
import javafx.scene.transform.Rotate
import javafx.stage.Stage

import PerspectiveCamera._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MoleculeSampleApp` class illustrates the creation of 3D objects
 *  (a representation of a Hydrogen molecule) as well as how to rotate them using
 *  the mouse.  It support the following keyboard commands as well:
 *  'V' toggles the visibility of the Hydrogen molecule,
 *  'X' toggles the visibility of the coordinate axes,
 *  'Z' restores original locations.
 *
 *  @author cmcastil  (author of Java version)
 */
class MoleculeSampleApp extends Application
{
    val root          = new Group ()
    val axisGroup     = new Xform ()
    val moleculeGroup = new Xform ()
    val world         = new Xform ()
    val cameraXform   = new Xform ()
    val cameraXform2  = new Xform ()
    val cameraXform3  = new Xform ()
    val camera        = PerspectiveCamera ()

    private val AXIS_LENGTH             = 250.0
    private val HYDROGEN_ANGLE          = 104.5
    private val CONTROL_MULTIPLIER      = 0.1
    private val SHIFT_MULTIPLIER        = 10.0
    private val MOUSE_SPEED             = 0.1
    private val ROTATION_SPEED          = 2.0
    private val TRACK_SPEED             = 0.3
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the coordinate axes (x, y and z).
     */
    private def buildAxes ()
    {
        println ("buildAxes ()")
        val redMaterial   = PhongMaterial (Color.DARKRED, Color.RED)
        val greenMaterial = PhongMaterial (Color.DARKGREEN, Color.GREEN)
        val blueMaterial  = PhongMaterial (Color.DARKBLUE, Color.BLUE)

        val xAxis = new Box (AXIS_LENGTH, 1, 1)
        val yAxis = new Box (1, AXIS_LENGTH, 1)
        val zAxis = new Box (1, 1, AXIS_LENGTH)

        xAxis.setMaterial (redMaterial)
        yAxis.setMaterial (greenMaterial)
        zAxis.setMaterial (blueMaterial)

        axisGroup.getChildren ().addAll (xAxis, yAxis, zAxis)
        axisGroup.setVisible (false)
        world.getChildren ().addAll (axisGroup)
    } // buildAxes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Handle mouse events for moving the molecule/axes.
     *  @param scene  the current scene
     *  @param root   the root node of the scene
     */
    private def handleMouse (scene: Scene, root: Node)
    {
        var mPos = (0.0, 0.0)                       // current mouse position
        var mOld = (0.0, 0.0)                       // prior/old mouse position
        var mDel = (0.0, 0.0)                       // distance/delta mPos - mOld

        scene.setOnMousePressed (new EventHandler [MouseEvent] () {
            override def handle (me: MouseEvent)
            {
                mPos = (me.getSceneX (), me.getSceneY ())
                mOld = mPos
            } // handle
        })
        scene.setOnMouseDragged (new EventHandler [MouseEvent] () {
            @Override
            override def handle (me: MouseEvent)
            {
                mOld = mPos
                mPos = (me.getSceneX (), me.getSceneY ())
                mDel = (mPos._1 - mOld._1, mPos._2 - mOld._2)
                
                var modifier = 1.0
                
                if (me.isControlDown ()) {
                    modifier = CONTROL_MULTIPLIER
                } // if
                if (me.isShiftDown ()) {
                    modifier = SHIFT_MULTIPLIER
                } // if
                if (me.isPrimaryButtonDown ()) {
                    cameraXform.getRy.setAngle (cameraXform.getRy.getAngle () - mDel._1 * MOUSE_SPEED * modifier * ROTATION_SPEED)  
                    cameraXform.getRx.setAngle (cameraXform.getRx.getAngle () + mDel._2 * MOUSE_SPEED * modifier * ROTATION_SPEED)  
                } else if (me.isSecondaryButtonDown ()) {
                    val z    = camera.getTranslateZ ()
                    val newZ = z + mDel._1 * MOUSE_SPEED * modifier
                    camera.setTranslateZ (newZ)
                } else if (me.isMiddleButtonDown ()) {
                    cameraXform2.t.setX (cameraXform2.t.getX () + mDel._1 * MOUSE_SPEED * modifier * TRACK_SPEED)  
                    cameraXform2.t.setY (cameraXform2.t.getY () + mDel._2 * MOUSE_SPEED * modifier * TRACK_SPEED)  
                } // if
            } // handle
        })
    } // handleMouse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Handle keyboard events for visibility/restoring molecule/axes.
     *  @param scene  the current scene
     *  @param root   the root node of the scene
     */
    private def handleKeyboard (scene: Scene, root: Node)
    {
        scene.setOnKeyPressed (new EventHandler [KeyEvent] () {
            override def handle (event: KeyEvent)
            {
                event.getCode match {
                case KeyCode.Z => cameraXform2.t.setX (0.0)
                                  cameraXform2.t.setY (0.0)
                                  camera.setTranslateZ (CAMERA_INITIAL_DISTANCE)
                                  cameraXform.getRy.setAngle (CAMERA_INITIAL_Y_ANGLE)
                                  cameraXform.getRx.setAngle (CAMERA_INITIAL_X_ANGLE)
                case KeyCode.X => axisGroup.setVisible (! axisGroup.isVisible ())
                case KeyCode.V => moleculeGroup.setVisible (! moleculeGroup.isVisible ())
                case _         => println ("handleKeyboard: unrecognized key code")
                } // match
            } // handle
        })
    } // scene.setOnKeyPressed
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the Hydrogen molecule using three spheres and two narrow cylinders.
     *  The molecule component hierarchy is as follows:
     *
     *  [*] moleculeXform
     *      [*] oxygenXform
     *          [*] oxygenSphere
     *      [*] hydrogen1SideXform
     *          [*] hydrogen1Xform
     *              [*] hydrogen1Sphere
     *          [*] bond1Cylinder
     *      [*] hydrogen2SideXform
     *          [*] hydrogen2Xform
     *              [*] hydrogen2Sphere
     *          [*] bond2Cylinder
     */
    private def buildMolecule ()
    {
        val redMaterial   = PhongMaterial (Color.DARKRED, Color.RED)
        val whiteMaterial = PhongMaterial (Color.WHITE, Color.LIGHTBLUE)
        val greyMaterial  = PhongMaterial (Color.DARKGREY, Color.GREY)

        val moleculeXform      = new Xform ()
        val oxygenXform        = new Xform ()
        val hydrogen1SideXform = new Xform ()
        val hydrogen1Xform     = new Xform ()
        val hydrogen2SideXform = new Xform ()
        val hydrogen2Xform     = new Xform ()

        val oxygenSphere = new Sphere (40.0)
        oxygenSphere.setMaterial (redMaterial)

        val hydrogen1Sphere = new Sphere (30.0)
        hydrogen1Sphere.setMaterial (whiteMaterial)
        hydrogen1Sphere.setTranslateX (0.0)

        val hydrogen2Sphere = new Sphere (30.0)
        hydrogen2Sphere.setMaterial (whiteMaterial)
        hydrogen2Sphere.setTranslateZ (0.0)

        val bond1Cylinder = new Cylinder (5, 100)
        bond1Cylinder.setMaterial (greyMaterial)
        bond1Cylinder.setTranslateX (50.0)
        bond1Cylinder.setRotationAxis (Rotate.Z_AXIS)
        bond1Cylinder.setRotate (90.0)

        val bond2Cylinder = new Cylinder (5, 100)
        bond2Cylinder.setMaterial (greyMaterial)
        bond2Cylinder.setTranslateX (50.0)
        bond2Cylinder.setRotationAxis (Rotate.Z_AXIS)
        bond2Cylinder.setRotate (90.0)

        moleculeXform.getChildren ().add (oxygenXform)
        moleculeXform.getChildren ().add (hydrogen1SideXform)
        moleculeXform.getChildren ().add (hydrogen2SideXform)
        oxygenXform.getChildren ().add (oxygenSphere)
        hydrogen1SideXform.getChildren ().add (hydrogen1Xform)
        hydrogen2SideXform.getChildren ().add (hydrogen2Xform)
        hydrogen1Xform.getChildren ().add (hydrogen1Sphere)
        hydrogen2Xform.getChildren ().add (hydrogen2Sphere)
        hydrogen1SideXform.getChildren ().add (bond1Cylinder)
        hydrogen2SideXform.getChildren ().add (bond2Cylinder)

        hydrogen1Xform.setTx (100.0)
        hydrogen2Xform.setTx (100.0)
        hydrogen2SideXform.setRy (HYDROGEN_ANGLE)

        moleculeGroup.getChildren ().add (moleculeXform)

        world.getChildren ().addAll (moleculeGroup)
    } // buildMolecule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Start the javafx application.
     *  @param  the primary stage
     */
    override def start (primaryStage: Stage)
    {
        // setUserAgentStylesheet (STYLESHEET_MODENA)
        println ("start ()")

        root.getChildren ().add (world)
        root.setDepthTest (DepthTest.ENABLE)

        // buildScene ()
        buildCamera (camera, cameraXform, cameraXform2, cameraXform3, root)
        buildAxes ()
        buildMolecule ()

        val scene = new Scene (root, 1024, 768, true)
        scene.setFill (Color.GREY)
        handleKeyboard (scene, world)
        handleMouse (scene, world)

        primaryStage.setTitle ("Molecule Sample Application")
        primaryStage.setScene (scene)
        primaryStage.show ()

        scene.setCamera (camera)
    } // start

} // MoleculeSampleApp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MoleculeSampleApp` object is used to launch the javafx application.
 */
object MoleculeSampleApp
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The main method serves as a fallback in case the application can not be
     *  launched through deployment artifacts, e.g., in IDEs with limited javafx
     *  support.  NetBeans ignores main.
     *  @param args  the command line arguments
     */
    def main (args: Array [String])
    {
//      launch (args)       Java style launch
        javafx.application.Application.launch (classOf [MoleculeSampleApp], args: _*)
    } // main

} // MoleculeSampleApp object

