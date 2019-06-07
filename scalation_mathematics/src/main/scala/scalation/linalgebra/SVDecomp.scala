
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Khalid Jahangeer
 *  @version 1.6
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
      extends Factorization
{
    /** Factor type contains 'u, s, v' which are the left orthogonal matrix, the diagonal
     *  matrix/vector containing singular values and the right orthogonal matrix.
     */
    type FactorType     = (MatriD, VectoD, MatriD)
    type FactorTypeFull = (MatriD, MatriD, MatriD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor/deflate the matrix by iteratively turning elements not in the main
     *  diagonal to zero.
     */
    def factor (): SVDecomp = { factor123 (); null }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the two factored matrices.
     */
    def factors: (MatriD, MatriD) =
    {
        throw new UnsupportedOperationException ("SVDecomp has three, not two factors")
    } // factors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor/deflate the matrix by iteratively turning elements not in the main
     *  diagonal to zero.  Then return the vector of singular values (i.e., the main
     *  diagonal), along with the left and right singular matrices.
     */
    def factor123 (): FactorType = ???                          // return (u, s, v)

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the condition number of 'this' matrix, i.e., the ratio of the
     *  largest singular value to the smallest.  Note, if not of full rank, it
     *  will be infinity.
     */
    def conditionNum: Double = { val s = factor123 ()._2; s(0) / s(s.dim -1) }

} // SVDecomp trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecomp` object provides several test matrices as well as methods
 *  for making full representations, reducing dimensionality, determining rank
 *  and testing SVD factorizations.
 */
object SVDecomp extends SVDecomp
{
    val a1 = new MatrixD ((2, 2), 1.00,  2.00,                           // original matrix
                                  0.00,  2.00)                           // 2 by 2, bidiagonal

    val a2 = new MatrixD ((3, 2), 3.0, -1.0,                             // original matrix
                                  1.0,  3.0,                             // 3 by 2
                                  1.0,  1.0)

    val a3 = new MatrixD ((3, 3), 1.0, 1.0, 0.0,                         // original matrix
                                  0.0, 2.0, 2.0,                         // 3 by 3, bidiagonal
                                  0.0, 0.0, 3.0)

    val a4 = new MatrixD ((3, 3), 0.0,     1.0, 1.0,                     // original matrix
                                  sqrt(2), 2.0, 0.0,                     // 3 by 3
                                  0.0,     1.0, 1.0)

    val a5 = new MatrixD ((4, 4), 0.9501, 0.8913, 0.8214, 0.9218,        // original matrix
                                  0.2311, 0.7621, 0.4447, 0.7382,        // 4 by 4
                                  0.6068, 0.4565, 0.6154, 0.1763,
                                  0.4860, 0.0185, 0.7919, 0.4057)

    val a6 = new MatrixD ((3, 2), 4, 5,                                  // original matrix
                                  6, 7,                                  // 3 by 2
                                  9, 8)

    val a7 = new MatrixD ((5, 3), 0.44444444,  0.3333333, -1.3333333,    // original matrix
                                  0.41111111, -0.3166667, -0.3333333,    // 5 by 3
                                 -0.18888889,  0.4833333, -0.3333333,
                                 -0.03333333, -0.6500000,  1.0000000,
                                 -0.63333333,  0.1500000,  1.0000000)

     val a8 = new MatrixD ((4, 4), 1.0, 2.0, 3.0, 4.0,                   // original matrix
                                   4.0, 3.0, 2.0, 1.0,                   // 4 by 4
                                   5.0, 6.0, 7.0, 8.0,
                                   8.0, 7.0, 6.0, 5.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert an SVD factoring to its full representation, returning the result as
     *  three matrices.
     *  @param u_s_v  the 3-way factorization
     */
    def factorFull (u_s_v: FactorType): FactorTypeFull =
    {
        val s  = u_s_v._2.dim
        val ss = new MatrixD (s); ss.setDiag (s)                 // turn vector into diagonal matrix
        (u_s_v._1, ss, u_s_v._3)
    } // factorFull

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reduce the dimensionality of the 'u', 's' and 'v' matrices from 'n' to 'k'.
     *  If 'k = rank', there is no loss of information; when 'k < rank', multiplying
     *  the three matrices results in an approximation (little is lost so long as
     *  the singular values set to zero (i.e., clipped) are small).
     *  @param u_s_v  the 3-way factorization
     *  @param k      the desired dimensionality
     */
    def reduce (u_s_v: FactorType, k: Int): FactorType =
    {
        (u_s_v._1.sliceCol (0, k), u_s_v._2.slice (0, k), u_s_v._3.sliceCol (0, k))
    } // reduce

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
    def test (a: MatriD, svd: SVDecomp, name: String)
    {
        banner (name)
        println (s"factor matrix a = $a")
        val (u, s, v) = svd.factor123 ()                         // factor matrix a
        println (sline () + s"into (u, s, v) = ${(u, s, v)}")
        val prod = u ** s * v.t                                  // compute the product
        println (sline () + s"check: u ** s * v.t = $prod")      // should equal the original a matrix
        println (s"prod - a = ${prod - a}")                      // difference should be close to 0
        println (sline ())
        assert (prod == a)
        println (sline ())
    } // test

} // SVDecomp object

import SVDecomp._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest
 */
object SVDecompTest extends App
{
     test (a1, new SVD (a1),  "SVDTest")
     test (a1, new SVD2 (a1), "SVD2Test")

} // SVDecompTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest2` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest2
 */
object SVDecompTest2 extends App
{
     test (a2, new SVD (a2),  "SVDTest")
     test (a2, new SVD2 (a2), "SVD2Test")

} // SVDecompTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest3` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest3
 */
object SVDecompTest3 extends App
{
     test (a3, new SVD (a3),  "SVDTest")
     test (a3, new SVD2 (a3), "SVD2Test")

} // SVDecompTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest4` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest4
 */
object SVDecompTest4 extends App
{
     test (a4, new SVD (a4),  "SVDTest")
     test (a4, new SVD2 (a4), "SVD2Test")

} // SVDecompTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest5` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest5
 */
object SVDecompTest5 extends App
{
     test (a5, new SVD (a5),  "SVDTest")
     test (a5, new SVD2 (a5), "SVD2Test")

} // SVDecompTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest6` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest6
 */
object SVDecompTest6 extends App
{
     test (a6, new SVD (a6),  "SVDTest")
     test (a6, new SVD2 (a6), "SVD2Test")

} // SVDecompTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest7` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest7
 */
object SVDecompTest7 extends App
{
     test (a7, new SVD (a7),  "SVDTest")
     test (a7, new SVD2 (a7), "SVD2Test")

} // SVDecompTest7


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDecompTest8` object tests the equality of the `SVD` and `SVD2` classes.
 *  > runMain scalation.linalgebra.SVDecompTest8
 */
object SVDecompTest8 extends App
{
     test (a8, new SVD (a8),  "SVDTest")
     test (a8, new SVD2 (a8), "SVD2Test")

} // SVDecompTest8

