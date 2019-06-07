
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
/** The `Matrix2D` class stores and operates on Numeric Matrices of base type `Double`
 *  and having 2 coordinates (x, y).
 *  @param d1  the first/row dimension (second dimension fixed at 2)
 *  @param v2  the 1D array used to store vector elements
 */
class Matrix2D (d1: Int,
      protected var v2: Array [Array [Double]] = null)
      extends MatrixD (d1, 2, v2)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'x' vector (first column).
     */
    def x: VectorD = col(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'y' vector (second column).
     */
    def y: VectorD = col(1)

} // Matrix2D class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matrix2D` companion object provides factory methods for creating vectors.
 */
object Matrix2D extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Matrix2D` from the three coordinates (one row, three columns).
     *  @param x  the first coordinate
     *  @param y  the second coordinate
     */
    def apply (x: Double, y: Double): Matrix2D =
    {
        val v = new Matrix2D (1)
        v(0, 0) = x; v(0, 1) = y
        v
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Matrix2D` from a three-column `MatriD`.
     *  FIX - try to avoid casting
     *  @param a  the `MatriD` object
     */
    def apply (a: MatriD): Matrix2D =
    {
        if (a.dim2 != 2) { flaw ("apply", "the `MatriD` must have three columns"); null }
        else new Matrix2D (a.dim1, a.asInstanceOf [MatrixD]())
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Matrix2D` from the three columns in the given 'table'.
     *  FIX - make more general than `Relation`
     *  @param colPos1  the first column position
     *  @param colPos2  the second column position
     */
    def apply (table: Relation, colPos1: Int, colPos2: Int): Matrix2D =
    {
        Matrix2D (table.toMatriD (Seq (colPos1, colPos2)))
    } // apply

} // Matrix2D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matrix2DTest` object is used to test the `Matrix2D` class.
 *  > runMain scalation.spatial.Matrix2DTest
 */
object Matrix2DTest extends App
{
    val u = new Matrix2D (2)
    val v = new Matrix2D (2, Array (Array (1.0, 2.0),
                                    Array (4.0, 5.0)))
    val a = new MatrixD ((2, 2), 2.0,  4.0,
                                 8.0, 10.0)
    val w = Matrix2D (a)

    println ("u     = " + u)
    println ("v     = " + v)
    println ("a     = " + a)
    println ("w     = " + w)
    println ("v + w = " + (v + w))

} // Matrix2DTest object

