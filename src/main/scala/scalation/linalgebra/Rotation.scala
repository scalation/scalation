
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Wed May 28 16:06:12 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see fortranwiki.org/fortran/show/svd
 *
 *  Code translated from LAPACK Fortran code
 */

package scalation.linalgebra

import math.{abs, log, max, sqrt}

import scalation.math.DoubleWithExp._
import scalation.math.ExtremeD.{EPSILON, MIN_NORMAL}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Rotation` class is a data structure for holding the results of rotating
 *  by angle a.
 *  @param cs  the cos (a)
 *  @param sn  the sin (a)
 *  @param r   the nonzero element of the rotated vector
 */
case class Rotation (cs: Double, sn: Double, r: Double)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Rotation` object provides methods for rotating in a plane.
 *  <p>
 *      [  cs  sn  ]  .  [ f ]  =  [ r ]   where cs^2 + sn^2 = 1
 *      [ -sn  cs  ]     [ g ]     [ 0 ]
 *  <p>
 */
object Rotation
{
    private val EXPO    = (log (MIN_NORMAL / EPSILON) / log (2.0) / 2.0).toInt
    private val SAF_MN2 = 2.0 ~^ EXPO
    private val SAF_MX2 = 1.0 / SAF_MN2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate vectors 'x' and 'y' using cosine 'cs' and sine 'sn'.
     *
     *  @see BLAS SUBROUTINE DROT (N, DX, INCX, DY, INCY, C, S)
     *  restriction:  INCX = INCY = 1
     *
     *  @param n     the number of elements involved in rotation
     *  @param x     the first vector
     *  @param y     the second vector
     *  @param cs    the cosine for the rotation
     *  @param sn    the sine for the rotation
     */
    def rot (n: Int, x: VectorD, y: VectorD, cs: Double, sn: Double)
    {
        if (n <= 0) { println ("rot: failed - n = " + n); return }
        for (i <- 0 until n) {
            val t = cs * x(i) + sn * y(i)
            y(i)  = cs * y(i) - sn * x(i) 
            x(i)  = t
        } // for
    } // rot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate column vectors with indices 'jx' and 'jy' using cosine 'cs' and sine 'sn'.
     *
     *  @see BLAS SUBROUTINE DROT (N, DX, INCX, DY, INCY, C, S)
     *  restriction:  INCX = INCY = 1
     *
     *  @param n     the number of elements involved in rotation
     *  @param a     the matrix containing the columns to be rotated 
     *  @param jx    the index for the first column vector
     *  @param jy    the index for the second column vector
     *  @param cs    the cosine for the rotation
     *  @param sn    the sine for the rotation
     */
    def rotCol (n: Int, a: Matrix, jx: Int, jy: Int, cs: Double, sn: Double)
    {
        if (n <= 0) { println ("rotCol: failed - n = " + n); return }
        for (i <- 0 until n) {
            val t = cs * a(i, jx) + sn * a(i, jy)
            a(i, jy)  = cs * a(i, jy) - sn * a(i, jx) 
            a(i, jx)  = t
        } // for
    } // rotCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate vector '[f, g]' to vector '[r, 0]' to make the second element 0.
     *
     *  @see LAPACK SUBROUTINE DLARTG (F, G, CS, SN, R)
     *
     *  @param f  the first element of the vector to be rotated
     *  @param g  the second element of the vector to be rotated
     */
    def rotate (f: Double, g: Double): Rotation =
    {
        if (g == 0) return Rotation (1.0, 0.0, f)
        if (f == 0) return Rotation (0.0, 0.0, g)

        var f1    = f                           // working copy of f
        var g1    = g                           // working copy of g
        var cs    = 0.0                         // cosine
        var sn    = 0.0                         // sine
        var r     = 0.0                         // nonzero element
        var scale = abs (f1) max abs (g1)       // max absolute value
       
        if (scale >= SAF_MX2) {
            rott (SAF_MN2, SAF_MX2)
        } else if (scale <= SAF_MN2) {
            rott (SAF_MX2, SAF_MN2)
        } else {
            r  = sqrt (f1*f1 + g1*g1)
            cs = f1 / r
            sn = g1 / r
        } // if

        def rott (saf_a: Double, saf_b: Double)
        {
            var count = 0
            do {
                count += 1
                f1    *= saf_a
                g1    *= saf_a
                scale  = abs (f1) max abs (g1)
            } while (scale <= saf_b)
            r  = sqrt (f1*f1 + g1*g1)
            cs = f1 / r
            sn = g1 / r
            for (i <- 1 to count) r *= saf_b
        } // rott

        if (abs (f) > abs (g) && cs < 0.0) Rotation (-cs, -sn, -r)
        else                               Rotation (cs, sn, r)
    } // rotate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate the vector 'x to vector 'y' to make 'y(1) = 0'.
     *  @param x  the vector to be rotated
     */
    def rotate (x: VectorD): Rotation = rotate (x(0), x(1))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the cosine and sine for a rotation, form the rotation matrix.
     *  @param q  the results of a rotation 
     */
    def formMatrix (q: Rotation): MatrixD = new MatrixD ((2, 2), q.cs, q.sn,
                                                                -q.sn, q.cs)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rotate the vectors in matrix 'a'.
     *
     * @see LAPACK SUBROUTINE DLASR (SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA)
     * restriction:  PIVOT = 'v'
     *
     * @param left      whether to apply rotation from the left (true) or right (false)
     * @param forward   whether to loop in the forward (true) or backward (false) direction
     * @param m         the relevant number of rows
     * @param n         the relevant number of columns
     * @param cs        the array of cosines
     * @param ics       the index offset for the array of cosines
     * @param sn        the array of sines
     * @param isn       the index offset for the array of sines
     * @param a         the matrix to rotate
     */
    def rotateV (left: Boolean, forward: Boolean, m: Int, n: Int,
                 cs: Array [Double], ics: Int, sn: Array [Double], isn: Int, a: MatrixD)
    {
        if (left) {                               // left side => form P * A

             val (k1, k2, k3) = if (forward) (0, m-2, 1) else (m-2, 0, -1)
             for (j <- k1 to k2 by k3) {
                 val ctemp = cs(j + ics)
                 val stemp = sn(j + isn)
                 if (ctemp != 1.0 || stemp != 0.0) {
                     for (i <- 0 until n) {
                         val temp = a(j+1, i)
                         a(j+1, i) = ctemp * temp - stemp * a(j, i)
                         a(j, i)   = stemp * temp + ctemp * a(j, i)
                     } // for
                 } // if
             } // for

        } else {                                  // right side => form A * P.t

             val (k1, k2, k3) = if (forward) (0, n-2, 1) else (n-2, 0, -1)
             for (j <- k1 to k2 by k3) {
                 val ctemp = cs(j + ics)
                 val stemp = sn(j + isn)
                 if (ctemp != 1.0 || stemp != 0.0) {
                     for (i <- 0 until m) {
                         val temp = a(i, j+1)
                         a(i, j+1) = ctemp * temp - stemp * a(i, j)
                         a(i, j)   = stemp * temp + ctemp * a(i, j)
                     } // for
                 } // if
             } // for

        } // if
    } // rotateV

} // Rotation object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RotationTest` object is used to test the `Rotation` object,
 */
object RotationTest extends App
{
    import Rotation._

    val x = VectorD (1.0, 2.0)
    val q = rotate (x)
    val a = formMatrix (q)

    println ("x          = " + x)
    println ("rotate (x) = " + q)
    println ("a * x      = " + a * x)

} // RotationTest

