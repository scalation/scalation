
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.2
 *  @date    Tue Sep 17 17:12:07 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Matrix Computation, 4th ed.  
 */

package scalation.linalgebra

import java.lang.Math.copySign

import math.{abs, sqrt}

import scalation.linalgebra.MatrixD.eye
import scalation.math._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Givens` objects has methods for determinng values 'c = cos(theta)' and
 *  's = sin(theta) for Givens rotation matrices as well as methods for applying
 *  Givens rotations.
 */
object Givens
{
    type CosSin = Tuple2 [Double, Double]    // type for (cosine, sine)

    private val DEBUG = false                // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create the values for a Givens 2-by-2 rotation matrix.  Given scalars
     *  'y' and 'z', efficiently compute 'c = cos(theta)' and 's = sin(theta)'
     *  that can be used to form the rotation matrix.
     *  @see http://www.netlib.org/lapack/lawnspdf/lawn150.pdf
     *
     *  | y  z | |  c  s |  =  | hypot(y, z)  0 |
     *           | -s  c |
     *
     *  | c -s | | y |  =  | hypot(y, z) |
     *  | s  c | | z |     |      0      |
     *
     *  @param y  the first scalar
     *  @param z  the second scalar
     */
    def givens (y: Double, z: Double): CosSin =
    {
        if (z =~ 0.0) {
            val c = copySign (1.0, y)
            val s = 0.0
            (c, 0.0)
        } else if (y =~ 0.0) {
            val c = 0.0
            val s = -copySign (1.0, z)
            (0, s)
        } else if (abs (z) > abs (y)) {
            val t = y / z
            val s = -copySign (1.0 / sqrt (1.0 + t * t), z)
            (-s * t, s)
        } else {
            val t = z / y
            val c = copySign (1.0 / sqrt (1.0 + t * t), y)
            (c, -c * t)
        } // if
    } // givens

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Efficiently perform a Givens row update: 'a = g (i, k, theta).t * a'.
     *  The update just affects two row.  FIX
     *  @param a   the matrix to update
     *  @param i   the first row ??
     *  @param k   the second row ??
     *  @param cs  the (cosine, sine) of theta
     *  @see Section 5.1.9 in Matrix Computation.
     */
    def givensRowUpdate (a: MatrixD, i: Int, k: Int, cs: CosSin)
    {
        val (c, s) = cs
        for (j <- 0 until a.dim2) {
            val t1 = a(i,j)
            val t2 = a(k,j)
            a(i,j) = c * t1 - s * t2
            a(k,j) = s * t1 + c * t2
        } // for
    } // givensRowUpdate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Efficiently perform a Givens column update: a = a * g (i, k, theta).
     *  The update just affects two columns.  FIX
     *  @param a   the matrix to update
     *  @param i   the first column ??
     *  @param k   the second column ??
     *  @param cs  the (cosine, sine) of theta
     *  @see Section 5.1.9 in Matrix Computation.
     */
    def givensColUpdate (a: MatrixD, i: Int, k: Int, cs: CosSin)
    {
        val (c, s) = cs
        for (j <- 0 until a.dim1) {
            val t1 = a(j, i)
            val t2 = a(j, k)
            a(j, i) = c * t1 - s * t2
            a(j, k) = s * t1 + c * t2
        } // for
    } // givensColUpdate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a Givens rotation matrix with angle 'theta = atan(s/c)'.
     *  A matrix is post-multiplied by the Given matrix to clear element (i, k).
     *  The 2-by-2 rotation is embedded in an identity matrix of dimension 'n'.
     *  @param i   the first diagonal position  (i, i)
     *  @param k   the second diagonal position (k, k)
     *  @param n   the dimension of the resulting rotation matrix
     *  @param cs  the (cosine, sine) of theta
     */
    def givensRo (i: Int, k: Int, n: Int, cs: CosSin): MatrixD =
    {
        val b = eye (n)
        b(i, i) =  cs._1; b(i, k) =  cs._2         // |  c   s  |
        b(k, i) = -cs._2; b(k, k) =  cs._1         // | -s   c  |
        if (DEBUG) println ("givensRo: b = " + b)
        b
    }  // givensRo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a transposed Givens rotation matrix with angle 'theta = atan(s/c)'.
     *  A matrix is pre-multiplied by the Given matrix to clear element (k, i).
     *  The 2-by-2 rotation is embedded in an identity matrix of dimension 'n'.
     *  @param i   the first diagonal position  (i, i)
     *  @param k   the second diagonal position (k, k)
     *  @param n   the dimension of the resulting rotation matrix
     *  @param cs  the (cosine, sine) of theta
     */
    def givensRoT (i: Int, k: Int, n: Int, cs: CosSin): MatrixD =
    {
        val b = eye (n)
        b(i, i) = cs._1; b(i, k) = -cs._2           // |  c  -s  |
        b(k, i) = cs._2; b(k, k) =  cs._1           // |  s   c  |
        if (DEBUG) println ("givensRoT: b = " + b)
        b
    }  // givensRoT

} // Givens object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GivensTest` object tests the `Givens` object by using two rotations to
 *  turn matrix 'a' into an upper triangular matrix, clearing positions (1, 0)
 *  and (2, 1).  A third rotation clears position (0, 1) but also puts a non-zero
 *  value at (1, 0).
 *  @see http://en.wikipedia.org/wiki/Givens_rotation
 *  > run-main scalation.linalgebra.GivensTest
 */
object GivensTest extends App
{
    import Givens._

    var a = new MatrixD ((3, 3), 6.0, 5.0, 0.0,
                                 5.0, 1.0, 4.0,
                                 0.0, 4.0, 3.0)
    println ("a = " + a)

    val cs1 = givens (a(0, 0), a(1, 0))    // rotation 1 takes (1, 0) = 5 -> 0 
    val g1  = givensRoT (0, 1, 3, cs1)     // use givensRoT below the diagonal
    a = g1 * a                             // pre-multiply by Givens matrix g1
    println ("(c1, s1) = " + cs1)
    println ("rotation 1: a = " + a)

    val cs2 = givens (a(1, 1), a(2, 1))    // rotation 2 takes (2, 1) = 4 -> 0 
    val g2  = givensRoT (1, 2, 3, cs2)
    a = g2 * a                             // pre-multiply by Givens matrix g2
    println ("(c2, s2) = " + cs2)
    println ("rotation 2: a = " + a)

    val cs3 = givens (a(0, 0), a(0, 1))    // rotation 3 takes (0, 1) = 4.48 -> 0 
    val g3  = givensRo (0, 1, 3, cs3)      // use givensRo above the diagonal
    a = a * g3                             // post-multiply by Givens matrix g3
    println ("(c3, s3) = " + cs3)
    println ("rotation 3: a = " + a)

} // GivensTest object

