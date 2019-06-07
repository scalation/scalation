
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug  8 14:33:21 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.spatial

import scalation.linalgebra.{VectoD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector3D` class stores and operates on Numeric Vectors of base type `Double`
 *  and having 3 coordinates (x, y, z).
 *  @param v3  the 1D array used to store vector elements
 */
class Vector3D (v3: Array [Double] = null)
      extends VectorD (3, v3)
{
    if (v == null) {
        v = Array.ofDim [Double] (3)
    } else if (v.length != 3) {
        flaw ("constructor", s"the array length ${v.length} != 3")
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'x' coordinate.
     */
    def x: Double = v(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'y' coordinate.
     */
    def y: Double = v(1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'z' coordinate.
     */
    def z: Double = v(2)

} // Vector3D class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector3D` companion object provides factory methods for creating vectors.
 */
object Vector3D
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Vector3D` from the two coordinates.
     *  @param x  the first coordinate
     *  @param y  the second coordinate
     */
    def apply (x: Double, y: Double, z: Double): Vector3D =
    {
        val v = new Vector3D ()
        v(0) = x; v(1) = y; v(2) = z
        v
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Vector3D` from `VectoD` object.
     *  @param x  the VectoD object
     */
    def apply (x: VectoD): Vector3D =
    {
        val v = new Vector3D ()
        v(0) = x(0); v(1) = x(1); v(2) = x(2)
        v
    } // apply


} // Vector3D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector3DTest` object is used to test the `Vector3D` class.
 *  > runMain scalation.spatial.Vector3DTest
 */
object Vector3DTest extends App
{
    val u = new Vector3D ()
    val v = new Vector3D (Array (1.0, 2.0, 3.0))
    val w = Vector3D (2.0, 4.0, 6.0)

    println ("u     = " + u)
    println ("v     = " + v)
    println ("w     = " + w)
    println ("v + w = " + (v + w))

} // Vector3DTest object

