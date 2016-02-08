
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.2
 *  @date    Sun Jan 20 13:01:21 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.util.Error

import MatrixD.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR` class provides methods to factor an 'm-by-n' matrix 'a' into the
 *  product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.  It uses Householder orthogonalization.
 *  @see 5.1 and 5.2 in Matrix Computations
 *  @see QRDecomposition.java in Jama
 *  @see http://www.stat.wisc.edu/~larget/math496/qr.html
 *  @param a  the matrix to be factor into q and r
 */
class Fac_QR (a: MatrixD)
      extends Factorization with Error
{
    private val m  = a.dim1                   // the number of rows in matrix a
    private val n  = a.dim2                   // the number of columns in matrix a
    private val aa = new MatrixD (a)          // a copy of matrix a
    private val q  = eye(m, n)                // the orthogonal q matrix
    private val r  = new MatrixD (n, n)       // the right upper triangular r matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the first factor, i.e., orthogonal 'q' matrix.
     */
    def factor1 (): MatrixD = { if (raw) factor (); q }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the second factor, i.e., the right upper triangular 'r' matrix.
     */
    def factor2 (): MatrixD = { if (raw) factor (); r }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of two matrices, 'a = q * r', returning
     *  both the orthogonal 'q' matrix and the right upper triangular 'r' matrix.
     *  This algorithm uses Householder orthogonalization.
     *  @see 5.1 and 5.2 in Matrix Computations
     *  @see QRDecomposition.java in Jama
     */
    def factor (): Tuple2 [MatrixD, MatrixD] = 
    {
        for (k <- 0 until n) {                                  // for each column k
            var _norm = aa.col(k, k).norm                       // norm of the kth column
            if (! (_norm =~ 0.0)) {
                if (aa(k, k) < 0.0) _norm = -_norm              // make k-th Householder vector
                for (i <- k until m) aa(i, k) /= _norm
                aa(k, k) += 1.0
                for (j <- k + 1 until n) {                      // transform remaining columns
                    var s = 0.0
                    for (i <- k until m) s +=  aa(i, k) * aa(i, j)
                    s = -s / aa(k, k)
                    for (i <- k until m) aa(i, j) += s * aa(i, k)
                } // for
            } // if
            r(k, k) = -_norm
        } // for
        raw = false                                             // factoring completed
   
        for (i <- 0 until n; j <- i + 1 until n) r(i, j) = aa(i, j)

        for (k <- n-1 to 0 by -1; j <- k until n) {             // form the q matrix
            if (! (aa(k, k) =~ 0.0)) {
                var s = 0.0
                for (i <- k until m) s += q(i, j) * aa(i, k)
                s = -s / aa(k, k)
                for (i <- k until m) q(i, j) += s * aa(i, k)
            } // if
         } // for
         (q, r)
    } // factor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'a*x = b' using the QR Factorization 'a = q*r' via
     *  'r*x = q.t * b'.
     *  @param  b the constant vector
     */
    def solve (b: VectoD): VectoD = backSub (r, q.t * b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward substitution to solve for 'x' in 'r*x = b'.
     *  @param  r  the right upper triangular matrix
     *  @param  b  the constant vector
     */
    def backSub (r: MatrixD, b: VectorD): VectorD =
    {
        val x = new VectorD (n)                   // vector to solve for
        for (k <- n-1 to 0 by -1) {               // solve for x in r*x = b
            x(k) = (b(k) - (r(k) dot x)) / r(k, k)
        } // for
        x
    } // backSub

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using QR Factorization
     *  'q*r*x = 0'.  Gives a basis of dimension 'n' - rank for the nullspace
     *  @param rank  the rank of the matrix (number of linearly independent row vectors)
     *  FIX: should work, but it does not
     */
    def nullspace (rank: Int): MatrixD = 
    {
        flaw ("nullspace", "method has bugs - so do not use")
        (new Fac_QR (a.t)).factor1 ().slice (0, n, rank, n)       // last n - rank columns
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using QR Factorization
     *  'q*r*x = 0'.  Gives only one vector in the nullspace.
     */
    def nullspaceV: VectorD =
    {
        val x = new VectorD (n); x(n-1) = 1.0        // vector to solve for
        val b = new VectorD (n)                      // new rhs as -r_i,n-1          
        for (i <- 0 until n) b(i) = -r(i, n-1)
        val rr = r.slice (0, n, 0, n-1)              // drop last column
        for (k <- n-2 to 0 by -1) {                  // solve for x in rr*x = b
            x(k) = (b(k) - (rr(k) dot x)) / rr(k, k)
        } // for
        x
    } // nullspaceV

} // Fac_QR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QRTest` object is used to test the `Fac_QR` class.
 *  @see http://www.ee.ucla.edu/~vandenbe/103/lectures/qr.pdf
 *  > run-main scalation.linalgebra.Fac_QRTest
 */
object Fac_QRTest extends App
{
    def test (a: MatrixD)
    {
        val qr = new Fac_QR (a)               // for factoring a into q * r
        val (q, r) = qr.factor ()             // (q orthogonal, r upper triangular)
        val ns = qr.nullspaceV                // ns is a point in the nullscpace
    
        println ("--------------------------------------------------------")
        println ("a    = " + a)
        println ("q    = " + q)
        println ("r    = " + r)
        println ("ns   = " + ns)
        println ("q*r  = " + (q*r))       // check that q*r  = a
        println ("a*ns = " + (a*ns))      // check that a*ns = 0
    } // test

    val a1 = new MatrixD ((4, 3), 9.0,  0.0, 26.0,
                                 12.0,  0.0, -7.0,
                                  0.0,  4.0,  4.0,
                                  0.0, -3.0, -3.0)

    val a2 = new MatrixD ((2, 2), 2.0,  1.0,
                                 -4.0, -2.0)

    val a3 = new MatrixD ((3, 3), 2.0,  1.0,  1.0,
                                 -5.0, -2.0, -2.0,
                                 -5.0, -2.0, -2.0)

    val a4 = new MatrixD ((4, 4), -1.0, 1.0, 2.0,  4.0,
                                   2.0, 0.0, 1.0, -7.0,
                                   2.0, 0.0, 1.0, -7.0,
                                   2.0, 0.0, 1.0, -7.0)

    test (a1)
    test (a2)
    test (a3)
    test (a4)

} // Fac_QRTest object

