
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sat May 16 13:51:17 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     https://docs.oracle.com/javase/8/javafx/api/javafx/geometry/Point3D.html
 */

package scalation.scala3d

import javafx._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Point3D` object supplies factory methods for `Point3D`.
 */
object Point3D
{
    /** The zero vector/point (0.0, 0.0, 0.0).
     */
    val ZERO = geometry.Point3D.ZERO

    /** The unit vector pointing along the x-axis.
     */
    val X_AXIS = new geometry.Point3D (1.0, 0.0, 0.0)

    /** The unit vector pointing along the y-axis.
     */
    val Y_AXIS = new geometry.Point3D (0.0, 1.0, 0.0)

    /** The unit vector pointing along the z-axis.
     */
    val Z_AXIS = new geometry.Point3D (0.0, 0.0, 1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a `Point3D` object from x, y. z coordinates.
     *  @param x  the x coordinate
     *  @param y  the y coordinate
     *  @param z  the z coordinate
     */
    def apply (x: Double, y: Double, z: Double) =
    {
        new geometry.Point3D (x, y, z)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a `Point3D` object from an array.
     *  @param arr  the array giving coordinates for the point (uses first 3 values)
     */
    def apply (arr: Array [Double]) =
    {
        if (arr.length < 3) println ("ERROR - Point3D.apply: requires at least 3 values in array")
        new geometry.Point3D (arr(0), arr(1), arr(2))
    } // apply

} // Point3D object

