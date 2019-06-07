
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug  8 14:33:21 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.spatial

import scalation.linalgebra.{VectoD, VectorD}
import scalation.util.banner

import LatLong2UTM.latLong2UTMxy

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector2D` class stores and operates on Numeric Vectors of base type `Double`
 *  and having 2 coordinates (x, y).
 *  @param v2  the 1D array used to store vector elements
 */
class Vector2D (v2: Array [Double] = null)
      extends VectorD (2, v2)
{
    if (v == null) {
        v = Array.ofDim [Double] (2)
    } else if (v.length != 2) {
        flaw ("constructor", s"the array length ${v.length} != 2")
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'x' coordinate.
     */
    def x: Double = v(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'y' coordinate.
     */
    def y: Double = v(1)

} // Vector2D class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector2D` companion object provides factory methods for creating vectors.
 */
object Vector2D
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Vector2D` from the two coordinates.
     *  @param x  the first coordinate
     *  @param y  the second coordinate
     */
    def apply (x: Double, y: Double): Vector2D = new Vector2D (Array (x, y))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Vector2D` from the two coordinates.
     *  @param xy  the (first, second) coordinates
     */
    def apply (xy: (Double, Double)): Vector2D = new Vector2D (Array (xy._1, xy._2))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Vector2D` from a `VectoD` object.
     *  @param x  the VectoD object
     */
    def apply (x: VectoD): Vector2D = new Vector2D (Array (x(0), x(1)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert latitude-longitude to (x, y) coordinates (only valid within one zone).
     *  @see en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
     *  @param latLong  the Latitude-Longitude coordinates
     */
    def fromLatLong (latLong: LatitudeLongitude): Vector2D =
    {
        Vector2D (latLong2UTMxy (latLong))
    } // fromLatLong

} // Vector2D object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector2DTest` object is used to test the `Vector2D` class.
 *  It tests basic opertions.
 *  > runMain scalation.spatial.Vector2DTest
 */
object Vector2DTest extends App
{
    val u = new Vector2D ()
    val v = new Vector2D (Array (1.0, 2.0))
    val w = Vector2D (2.0, 4.0)

    println ("u     = " + u)
    println ("v     = " + v)
    println ("w     = " + w)
    println ("v + w = " + (v + w))

} // Vector2DTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vector2DTest2` object is used to test the `Vector2D` class.
 *  It tests the conversion of `LatitudeLongitude` to local (x, y) coordinates
 *  > runMain scalation.spatial.Vector2DTest2
 */
object Vector2DTest2 extends App
{
    import Vector2D.fromLatLong
    import Earth.meters2Miles

    val locs = Array (LatitudeLongitude (33.9519, 83.3576),      // 0. Athens, GA
                      LatitudeLongitude (33.7490, 84.3880),      // 1. Atlanta, GA
                      LatitudeLongitude (34.2979, 83.8241),      // 2. Gaineville, GA
                      LatitudeLongitude (33.5957, 83.4679),      // 3. Madison, GA
                      LatitudeLongitude (33.4735, 82.0105),      // 4. Augusta, GS
                      LatitudeLongitude (32.8407, 83.6324),      // 5. Macon, GA
                      LatitudeLongitude (33.0801, 83.2321))      // 6. Milledgeville, GA

     banner (s"locations near Athens, GA")
     for (loc <- locs) {
         val xy = fromLatLong (loc)
         println (s"local (x, y) for loc = (${xy.x * meters2Miles}, ${xy.y * meters2Miles})")
     } // for

} // Vector2DTest2 object

