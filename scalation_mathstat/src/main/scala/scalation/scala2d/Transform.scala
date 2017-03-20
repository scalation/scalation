
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Jan 10 17:08:32 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.scala2d

import scalation.scala2d.Shapes.RectangularShape

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Transform` trait provides a simple technique for transforming
 *  (translation, scaling and rotation) rectangular shapes.
 */
trait Transform
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move/translate the shape to location (x, y).
     *  @param shape  the shape/object to move
     *  @param x      the x-coordinate
     *  @param y      the y-coordinate
     */
    def move (shape: RectangularShape, x: Double, y: Double) 
    {
        shape.setFrame (x, y, shape.getWidth, shape.getHeight)
    } // move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move/translate the shape to location (x, y).
     *  @param shape  the shape/object to move
     *  @param p      the point (x, y)-coordinates
     */
    def move (shape: RectangularShape, p: Array [Double]) 
    {
        if (p.length != 2) {
            println ("Transform.move", "p array must be of size 2")
        } else {
            shape.setFrame (p(0), p(1), shape.getWidth, shape.getHeight)
        } // if
    } // move

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale/resize the shape to the new width and height parameters.
     *  @param shape  the shape/object to scale (change size)
     *  @param w      the width
     *  @param h      the height
     */
    def scale (shape: RectangularShape, w: Double, h: Double) 
    {
        shape.setFrame (shape.getX, shape.getY, w, h)
    } // scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale/resize the shape to the new width and height parameters.
     *  @param shape  the shape/object to scale (change size)
     *  @param p      the point (w, h) parameters
     */
    def scale (shape: RectangularShape, p: Array [Double]) 
    {
        if (p.length != 2) {
            println ("Transform.scale", "p array must be of size 2")
        } else {
            shape.setFrame (shape.getX, shape.getY, p(0), p(1))
        } // if
    } // scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate the shape by theta radians.
     *  @param shape  the shape/object to rotate
     *  @param theta   the rotation angle in radians
     */
    def rotate (shape: RectangularShape, theta: Double) 
    {
        // FIX
    } // scale

} // Transform trait

