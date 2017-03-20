
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun May 17 15:07:22 EDT 2015
 *  @see     LICENSE (MIT style license file)
 */

package scalation.scala3d

import javafx._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerspectiveCamera` object provides factory methods for building 
 *  cameras.
 */
object PerspectiveCamera
{
    val CAMERA_INITIAL_DISTANCE = -450
    val CAMERA_INITIAL_X_ANGLE  = 70.0
    val CAMERA_INITIAL_Y_ANGLE  = 320.0

    private val CAMERA_NEAR_CLIP = 0.1
    private val CAMERA_FAR_CLIP  = 10000.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a camera to be used for viewing a scene.
     */
    def apply () =
    {
        new scene.PerspectiveCamera (true)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a camera to be used for viewing a scene.
     */
    def buildCamera (camera: scene.PerspectiveCamera, cameraXform: Xform, cameraXform2: Xform,
                     cameraXform3: Xform, root: scene.Group)
    {
        println ("buildCamera ()")
        root.getChildren ().add (cameraXform)
        cameraXform.getChildren ().add (cameraXform2)
        cameraXform2.getChildren().add (cameraXform3)
        cameraXform3.getChildren().add (camera)
        cameraXform3.setRz (180.0)

        camera.setNearClip (CAMERA_NEAR_CLIP)
        camera.setFarClip (CAMERA_FAR_CLIP)
        camera.setTranslateZ (CAMERA_INITIAL_DISTANCE)
        cameraXform.getRy.setAngle (CAMERA_INITIAL_Y_ANGLE)
        cameraXform.getRx.setAngle (CAMERA_INITIAL_X_ANGLE)
    } // buildCamera

} // PerspectiveCamera object

