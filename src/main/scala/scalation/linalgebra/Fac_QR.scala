
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.2
 *  @date    Sun Jul 24 13:39:21 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.math.min

import scalation.util.Error

import MatrixD.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR` abstarct class provides base methods to factor an 'm-by-n' matrix 'aa'
 *  into the product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'aa = q * r'.
 *------------------------------------------------------------------------------
 *  @param aa     the matrix to be factor into q and r
 *  @param needQ  flag indicating whether a full q matrix is needed
 */
abstract class Fac_QR [MatT <: MatriD] (aa: MatT, needQ: Boolean = true)
         extends Factorization with Error
{
    protected val m = aa.dim1                            // the number of rows in matrix aa
    protected val n = aa.dim2                            // the number of columns in matrix aa
    protected val p = min (m, n)                         // the smallest dimension
    protected val r = aa.zero (n, n)                     // the right upper triangular r matrix
    protected val q = if (needQ) eye (m, n) else null    // the orthogonal q matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return both the orthogonal 'q' matrix and the right upper triangular 'r' matrix.
     */
    def factors: (MatriD, MatriD) = (q, r)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute values for the full 'q' matrix.
     */
    def computeQ ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'aa*x = b' using the QR Factorization 'aa = q*r' via
     *  'r*x = q.t * b'.  Requires calculating 'q' matrix first.
     *  @param  y the constant vector
     */
    def solve (b: VectoD): VectoD = r.bsolve (q.t * b) 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using QR Factorization
     *  'q*r*x = 0'.  Gives a basis of dimension 'n - rank' for the nullspace
     *  @param rank  the rank of the matrix (number of linearly independent row vectors)
     */
    def nullspace (rank: Int): MatriD

} // Fac_QR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QRTest` object is used to test the `Fac_QR` classes.
 *  @see www.ee.ucla.edu/~vandenbe/103/lectures/qr.pdf
 *  @see www.math.usm.edu/lambers/mat610/sum10/lecture9.pdf
 *  FIX: the 'nullspaceV' function need to be fixed.
 *  > run-main scalation.linalgebra.Fac_QRTest
 */
object Fac_QRTest extends App
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the correctness of the QR Factorization.
     *  @param a   the matrix to factor
     *  @param qr  the QR Factorization class/algorithm to use
     *  @param nm  the name of test case/matrix
     */
    def test (nm: String, a: MatrixD, qr: Fac_QR [MatrixD])
    {
        println ("-" * 60)
        val (q, r) = qr.factor12 ()                      // (q orthogonal, r upper triangular)
        val prod   = q * r                               // product of q * r
        val r_est  = r.rank (true)                       // estimated rank
        val ns     = qr.nullspace (r_est)                // ns is a basis for the nullscpace

        println (nm + "   = " + a)                        // original matrix
        println ("q    = " + q)                          // orthogonal matrix
        println ("r    = " + r)                          // right upper triangular matrix
        println ("r_est  = " + r_est)                    // rank
        println ("q*r  = " + prod)                       // product q * r
        println ("eq   = " + (a == prod))                // check that q * r  = a
        println ("ns   = " + ns)                         // nullspace
        println ("a*ns = " + (a * ns))                   // check that a * ns = 0
    } // test

    val a1 = new MatrixD ((4, 3), 9.0,  0.0, 26.0,
                                 12.0,  0.0, -7.0,
                                  0.0,  4.0,  4.0,
                                  0.0, -3.0, -3.0)

    val a2 = new MatrixD ((2, 2), 2.0,  1.0,
                                 -4.0, -2.0)

    val a3 = new MatrixD ((3, 3), 0.0,  1.0,  1.0,
                                 -5.0, -2.0, -2.0,
                                 -5.0, -2.0, -2.0)

    val a4 = new MatrixD ((4, 4), -1.0, 1.0, 2.0,  4.0,
                                   2.0, 0.0, 1.0, -7.0,
                                   2.0, 0.0, 1.0, -7.0,
                                   2.0, 0.0, 1.0, -7.0)

    val a5 = new MatrixD ((5, 3), 0.8147, 0.0975, 0.1576,
                                  0.9058, 0.2785, 0.9706,
                                  0.1270, 0.5469, 0.9572,
                                  0.9134, 0.9575, 0.4854,
                                  0.6324, 0.9649, 0.8003)

//  since m < n, use Fac_LQ instead
//  val a6 = new MatrixD ((2, 4), 1.0, 2.0, 3.0, 4.0,
//                                5.0, 6.0, 7.0, 8.0)

    println ("*" * 60)
    println ("Fac_QRTest: Fac_QR_H")
    test ("a1", a1, new Fac_QR_H (a1))
    test ("a2", a2, new Fac_QR_H (a2))
    test ("a3", a3, new Fac_QR_H (a3))
    test ("a4", a4, new Fac_QR_H (a4))
    test ("a5", a5, new Fac_QR_H (a5))

    println ("*" * 60)
    println ("Fac_QRTest: Fac_QR_H2")
    test ("a1", a1, new Fac_QR_H2 (a1))
    test ("a2", a2, new Fac_QR_H2 (a2))
    test ("a3", a3, new Fac_QR_H2 (a3))
    test ("a4", a4, new Fac_QR_H2 (a4))
    test ("a5", a5, new Fac_QR_H2 (a5))

    println ("*" * 60)
    println ("Fac_QRTest: Fac_QR_H3")
    test ("a1", a1, new Fac_QR_H3 (a1))
    test ("a2", a2, new Fac_QR_H3 (a2))
    test ("a3", a3, new Fac_QR_H3 (a3))
    test ("a4", a4, new Fac_QR_H3 (a4))
    test ("a5", a5, new Fac_QR_H3 (a5))

    println ("*" * 60)
    println ("Fac_QRTest: Fac_QR_RR")
    test ("a1", a1, new Fac_QR_RR (a1))
    test ("a2", a2, new Fac_QR_RR (a2))
    test ("a3", a3, new Fac_QR_RR (a3))
    test ("a4", a4, new Fac_QR_RR (a4))
    test ("a5", a5, new Fac_QR_RR (a5))

    println ("*" * 60)
    println ("Fac_QRTest: Fac_QR_MGS")
    test ("a1", a1, new Fac_QR_MGS (a1))
    test ("a2", a2, new Fac_QR_MGS (a2))
    test ("a3", a3, new Fac_QR_MGS (a3))
    test ("a4", a4, new Fac_QR_MGS (a4))
    test ("a5", a5, new Fac_QR_MGS (a5))

} // Fac_QRTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QRTest2` object is used to test the correctness of the 'solve' method
 *  in the `Fac_QR` classes.
 *  > run-main scalation.linalgebra.Fac_QRTest2
 */
object Fac_QRTest2 extends App
{
    // 20 data points:      Constant      x_1     x_2    x_3      x_4
    //                                    Age  Weight    Dur   Stress
    val x = new MatrixD ((20, 5), 1.0,   47.0,   85.4,   5.1,    33.0,
                                  1.0,   49.0,   94.2,   3.8,    14.0,
                                  1.0,   49.0,   95.3,   8.2,    10.0,
                                  1.0,   50.0,   94.7,   5.8,    99.0,
                                  1.0,   51.0,   89.4,   7.0,    95.0,
                                  1.0,   48.0,   99.5,   9.3,    10.0,
                                  1.0,   49.0,   99.8,   2.5,    42.0,
                                  1.0,   47.0,   90.9,   6.2,     8.0,
                                  1.0,   49.0,   89.2,   7.1,    62.0,
                                  1.0,   48.0,   92.7,   5.6,    35.0,
                                  1.0,   47.0,   94.4,   5.3,    90.0,
                                  1.0,   49.0,   94.1,   5.6,    21.0,
                                  1.0,   50.0,   91.6,  10.2,    47.0,
                                  1.0,   45.0,   87.1,   5.6,    80.0,
                                  1.0,   52.0,  101.3,  10.0,    98.0,
                                  1.0,   46.0,   94.5,   7.4,    95.0,
                                  1.0,   46.0,   87.0,   3.6,    18.0,
                                  1.0,   46.0,   94.5,   4.3,    12.0,
                                  1.0,   48.0,   90.5,   9.0,    99.0,
                                  1.0,   56.0,   95.7,   7.0,    99.0)
    //  response BP
    val y = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
                     114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    val qr = new Fac_QR_H (x)
    qr.factor ()
    println ("b1 = " + qr.solve (y))                       // compute the b vector by using 'solve' of 'Fac_QR_H'

    val qr2 =  new Fac_QR_H2 (x)
    qr2.factor ()
    println ("b2 = " + qr2.solve (y))                      // compute the b vector by using 'solve' of 'Fac_QR_H2'

    val qr3 =  new Fac_QR_H3 (x)
    qr3.factor ()
    println ("b3 = " + qr3.solve (y))                      // compute the b vector by using 'solve' of 'Fac_QR_H3'

} // Fac_QRTest2 object

