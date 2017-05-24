
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Khalid Jahangeer
 *  @version 1.3
 *  @date    Wed May 28 16:06:12 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.math.{abs, sqrt}
import scalation.math.ExtremeD.TOL
import scalation.util.{banner, sline}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDDecomp` trait specifies the major methods for Singular Value
 *  Decomposition implementations:
 *------------------------------------------------------------------------------
 *  SVD  - Golub-Kahan-Reinsch Algorithm translated from Algol code
 *  SVD2 - Compute 'a.t * a', 'a * a.t' and use `Eigenvalue` and `Eigenvector`
 *  SVD3 - Implicit Zero-Shift 'QR' Algorithm
 *  SVD4 - Golub-Kahan-Reinsch Algorithm coded from psuedo-code in Matrix Computations
 *  The last three are still under development.
 */
trait SVDecomp
{
    /** Factor type contains 'u, s, v' which are the left orthogonal matrix, the diagonal
     *  matrix/vector containing singular values and the right orthogonal matrix.
     */
    type FactorType     = (MatriD, VectoD, MatriD)
    type FactorTypeFull = (MatriD, MatriD, MatriD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor/deflate the matrix by iteratively turning elements not in the main
     *  diagonal to zero.  Then return the vector of singular values (i.e., the main
     *  diagonal), along with the left and right singular matrices.
     */
    def factor (): FactorType = ???                          // return (u, s, v)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reorder the singular values to be in non-increasing order.  Must swap
     *  singular vectors in lock step with singular values.  To minimize the
     *  number of swaps, selection sort is used.
     *  @param ft  the factored matrix (u, s, v)
     */
    def reorder (ft: FactorType)
    {
        val n = ft._2.dim
        for (i <- 0 until n) {
            val j = ft._2.argmax (i, n)             // index of largest element in s(i:n)
            if (i != j) {
                ft._2.swap (i, j)                   // s diagonal matrix
                ft._1.swapCol (i, j)                // u left orthogonal matrix
                ft._3.swapCol (i, j)                // v right orthogonal matrix
            } // if
        } // for
    } // reorder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Flip negative singular values to positive and set singular values close
     *  to zero to zero.
     *  @param u  the left orthongonal matrix
     *  @param s  the vector of singular values
     */
    def flip (u: MatriD, s: VectoD)
    {
        for (i <- s.indices) {
            if (abs (s(i)) < TOL) s(i) = 0.0                   // zero out
            if (s(i) < 0.0) { u(i) *= -1.0; s(i) *= -1.0 }     // flip sign
        } //for
    } // flip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Flip negative main diagonal elements in the singular vectors to positive.
     *  @param u  the left orthongonal matrix
     *  @param v  the right orthongonal matrix
     */
    def flip (u: MatriD, v: MatriD)
    {
        for (j <- u.range2 if u(j, j) < 0.0) u.setCol(j, u.col(j) * -1.0)
        for (j <- v.range2 if v(j, j) < 0.0) v.setCol(j, v.col(j) * -1.0)
    } // flip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'a^t*a*x = b' using `SVD`.
     *  @param b  the constant vector
     */
    def solve (b: VectoD): VectoD = ???

} // SVDecomp trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecomp` object provides several test matrices as well as methods
 *  for determining rank and test the SVD factorizations.
 */
object SVDecomp extends SVDecomp
{
    val a1 = new MatrixD ((2, 2), 1.00,  2.00,                             // original matrix
                                  0.00,  2.00)                             // 2 by 2, bidiagonal

    val a2 = new MatrixD ((3, 2), 3.0, -1.0,                               // original matrix
                                  1.0,  3.0,                               // 3 by 2
                                  1.0,  1.0)

    val a3 = new MatrixD ((3, 3), 1.0, 1.0, 0.0,                           // original matrix
                                  0.0, 2.0, 2.0,                           // 3 by 3, bidiagonal
                                  0.0, 0.0, 3.0)

    val a4 = new MatrixD ((3, 3), 0.0,     1.0, 1.0,                       // original matrix
                                  sqrt(2), 2.0, 0.0,                       // 3 by 3
                                  0.0,     1.0, 1.0)

    val a5 = new MatrixD ((4, 4), 0.9501, 0.8913, 0.8214, 0.9218,          // original matrix
                                  0.2311, 0.7621, 0.4447, 0.7382,          // 4 by 4
                                  0.6068, 0.4565, 0.6154, 0.1763,
                                  0.4860, 0.0185, 0.7919, 0.4057)

    val a6 = new MatrixD ((3, 2), 4, 5,                                   // original matrix
                                  6, 7,                                   // 3 by 2
                                  9, 8)

    val a7 = new MatrixD ((5, 3), 0.44444444,  0.3333333, -1.3333333,     // original matrix
                                  0.41111111, -0.3166667, -0.3333333,     // 5 by 3
                                 -0.18888889,  0.4833333, -0.3333333,
                                 -0.03333333, -0.6500000,  1.0000000,
                                 -0.63333333,  0.1500000,  1.0000000)

     val a8 = new MatrixD ((4, 4), 1.0, 2.0, 3.0, 4.0,                    // original matrix
                                   4.0, 3.0, 2.0, 1.0,                    // 4 by 4
                                   5.0, 6.0, 7.0, 8.0,
                                   8.0, 7.0, 6.0, 5.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the rank of a matrix by counting the number of non-zero singular
     *  values.  The implementation assumes zero singular values are last in the vector.
     *  @param s  the vector of singular values for the matrix whose rank is sought
     */
    def rank (s: VectorD): Int =
    {
        var i = 0
        while (i < s.dim && s(i) != 0.0) i += 1
        i
    } // rank

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the SVD Factorization algorithm on matrix 'a' by factoring the matrix
     *  into a left matrix u, a vector s, and a right matrix v.  Then multiply back
     *  to recover the original matrix 'u ** s * v.t'.
     *  @param a      the orginal matrix
     *  @param u_s_v  the given matrix a factored into three components
     *  @param name   the name of the test case
     */
    def test (a: MatriD, u_s_v: FactorType, name: String)
    {
        banner (name)
        println (s"factor matrix a = $a")
        println (sline () + s"into (u, s, v) = $u_s_v")
        val (u, s, v) = u_s_v
        val prod = u ** s * v.t                                  // compute the product
        println (sline () + s"check: u ** s * v.t = $prod")      // should equal the original a matrix
        println (s"prod - a = ${prod - a}")
        println (sline ())
        assert (prod == a)
    } // test

} // SVDecomp object

import SVDecomp._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest
 */
object SVDecompTest extends App
{
     val (u, s, v) = (new SVD (a1)).factor ()
     test (a1, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a1)).factor ()
     test (a1, (u2, s2, v2), "SVD2Test")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest2` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest2
 */
object SVDecompTest2 extends App
{
     val (u, s, v) = (new SVD (a2)).factor ()
     test (a2, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a2)).factor ()
     test (a2, (u2, s2, v2), "SVD2Test")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest3` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest3
 */
object SVDecompTest3 extends App
{
     val (u, s, v) = (new SVD (a3)).factor ()
     test (a3, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a3)).factor ()
     test (a3, (u2, s2, v2), "SVD2Test")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest4` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest4
 */
object SVDecompTest4 extends App
{
     val (u, s, v) = (new SVD (a4)).factor ()
     test (a4, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a4)).factor ()
     test (a4, (u2, s2, v2), "SVD2Test")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest5` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest5
 */
object SVDecompTest5 extends App
{
     val (u, s, v) = (new SVD (a5)).factor ()
     test (a5, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a5)).factor ()
     test (a5, (u2, s2, v2), "SVD2Test")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest6` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest6
 */
object SVDecompTest6 extends App
{
     val (u, s, v) = (new SVD (a6)).factor ()
     test (a6, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a6)).factor ()
     test (a6, (u2, s2, v2), "SVD2Test")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest7` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest7
 */
object SVDecompTest7 extends App
{
     val (u, s, v) = (new SVD (a7)).factor ()
     test (a7, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a7)).factor ()
     test (a7, (u2, s2, v2), "SVDTest")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest7


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest8` object tests the equality of the `SVD` and `SVD2` classes.
 *  > run-main scalation.linalgebra.SVDecompTest8
 */
object SVDecompTest8 extends App
{
     val (u, s, v) = (new SVD (a8)).factor ()
     test (a8, (u, s, v), "SVDTest")

     val (u2, s2, v2) = (new SVD2 (a8)).factor ()
     test (a8, (u2, s2, v2), "SVDTest")

     assert ((u, s, v) == (u2, s2, v2))

} // SVDecompTest68

