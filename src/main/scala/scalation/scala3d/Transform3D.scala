
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Jason Kaine
 *  @version 1.1
 *  @date    Sat Dec 20 13:06:20 EST 201
 *  @see     LICENSE (MIT style license file).
 */

package scalation.scala3d

import javafx.scene.transform.Translate

import scalation.scala3d.Shapes3D.Shape3D

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Transform3D` trait provides a simple technique for transforming
*  (translation, scaling and rotation) rectangular shapes.
*/
trait Transform3D
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move/translate the shape to location (x, y).
     *  @param shape  the shape/object to move
     *  @param x      the x-coordinate
     *  @param y      the y-coordinate
     */
    def move (shape: Shape3D, x: Double, y: Double, z: Double)
    {
        shape.setTranslateX (x)
        shape.setTranslateY (y)
        shape.setTranslateZ (z)
        //shape.getTransforms ().add(new Translate (x, y, z))
    } // move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move/translate the shape to location (x, y, z).
     *  @param shape  the shape/object to move
     *  @param p      the point (x, y, z)-coordinates
     */
    def move (shape: Shape3D, p: Array [Double])
    {
        if (p.length != 3) {
            println ("Transform.move", "p array must be of size 3")
        } else {
            //shape.getTransforms ().clear ()
            shape.setTranslateX (p(0))
            shape.setTranslateY (p(1))
            shape.setTranslateZ (p(2))
            //shape.getTransforms ().add (new Translate (p(0), p(1), p(2)))
        } // if
    } // move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale/resize the shape to the new width and height parameters.
     *  @param shape  the shape/object to scale (change size)
     *  @param w      the width
     *  @param h      the height
     *  @param d      the depth
     */
    def scale (shape: Shape3D, w: Double, h: Double, d: Double)
    {
        shape.setScaleX (shape.getScaleX () * w)
        shape.setScaleY (shape.getScaleY () * h)
        shape.setScaleZ (shape.getScaleZ () * d)
    } // scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale/resize the shape to the new width and height parameters.
     *  @param shape  the shape/object to scale (change size)
     *  @param p      the point (w, h, d) parameters
     */
    def scale (shape: Shape3D, p: Array [Double])
    {
        if (p.length != 3) {
            println ("Trandform.scale", "p array must be of size 3")
        } else {
            shape.setScaleX (shape.getScaleX () * p(0))
            shape.setScaleY (shape.getScaleY () * p(1))
            shape.setScaleZ (shape.getScaleZ () * p(2))
        } // if
    } // scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate the shape by theta radians.
     *  @param shape  the shape/object to rotate
     *  @param theta   the rotation angle in radians
     */
    def rotate (shape: Box, theta: Double)
    {
        // FIX
    } // rotate

} // Transform trait

