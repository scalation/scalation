
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug  8 14:33:21 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.spatial

import scalation.columnar_db.Relation
import scalation.linalgebra.{MatriD, MatrixD, VectorD}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matrix3D` class stores and operates on Numeric Matrices of base type `Double`
 *  and having 3 coordinates (x, y, z).
 *  @param d1  the first/row dimension (second dimension fixed at 3)
 *  @param v3  the 1D array used to store vector elements
 */
class Matrix3D (d1: Int,
      protected var v3: Array [Array [Double]] = null)
      extends MatrixD (d1, 3, v3)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'x' vector (first column).
     */
    def x: VectorD = col(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'y' vector (second column).
     */
    def y: VectorD = col(1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'z' vector (third column).
     */
    def z: VectorD = col(2)

} // Matrix3D class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matrix3D` companion object provides factory methods for creating vectors.
 */
object Matrix3D extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Matrix3D` from the three coordinates (one row, three columns).
     *  @param x  the first coordinate
     *  @param y  the second coordinate
     *  @param z  the third coordinate
     */
    def apply (x: Double, y: Double, z: Double): Matrix3D =
    {
        val v = new Matrix3D (1)
        v(0, 0) = x; v(0, 1) = y; v(0, 2) = z
        v
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Matrix3D` from a three-column `MatriD`.
     *  FIX - try to avoid casting
     *  @param a  the `MatriD` object
     */
    def apply (a: MatriD): Matrix3D =
    {
        if (a.dim2 != 3) { flaw ("apply", "the `MatriD` must have three columns"); null }
        else new Matrix3D (a.dim1, a.asInstanceOf [MatrixD]())
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Matrix3D` from the three columns in the given 'table'.
     *  FIX - make more general than `Relation`
     *  @param colPos1  the first column position
     *  @param colPos2  the second column position
     *  @param colPos3  the third column position
     */
    def apply (table: Relation, colPos1: Int, colPos2: Int, colPos3: Int): Matrix3D =
    {
        Matrix3D (table.toMatriD (Seq (colPos1, colPos2, colPos3)))
    } // apply

} // Matrix3D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matrix3DTest` object is used to test the `Matrix3D` class.
 *  > runMain scalation.spatial.Matrix3DTest
 */
object Matrix3DTest extends App
{
    val u = new Matrix3D (2)
    val v = new Matrix3D (2, Array (Array (1.0, 2.0, 3.0),
                                    Array (4.0, 5.0, 6.0)))
    val a = new MatrixD ((2, 3), 2.0,  4.0,  6.0,
                                 8.0, 10.0, 12.0)
    val w = Matrix3D (a)

    println ("u     = " + u)
    println ("v     = " + v)
    println ("a     = " + a)
    println ("w     = " + w)
    println ("v + w = " + (v + w))

} // Matrix3DTest object

