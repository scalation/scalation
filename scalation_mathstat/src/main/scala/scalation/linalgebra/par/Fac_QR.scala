
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Feb  2 16:56:03 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.par

import scalation.linalgebra.{Factorization, MatriD, VectoD}
import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR` class provides methods to factor an 'm-by-n' matrix 'a' into the
 *  product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.   It uses Gram-Schmidt orthogonalization.
 *  Note, orthogonal means that 'q.t * q = I'.
 *  This version uses parallel processing to speed up execution.
 *  @see http://www.stat.wisc.edu/~larget/math496/qr.html
 *  @see http://en.wikipedia.org/wiki/Gram–Schmidt_process
 *       (stabilized Gram–Schmidt orthonormalization)
 *  @param a  the matrix to be factor into q and r
 */
class Fac_QR (a: MatrixD)
      extends Factorization with Error
{
    private val m = a.dim1                   // the number of rows in matrix a
    private val n = a.dim2                   // the number of columns in matrix a
    private val q = new MatrixD (a)          // the orthogonal q matrix
    private val r = new MatrixD (n, n)       // the right upper triangular r matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of two matrices, 'a = q * r', returning
     *  both the orthogonal 'q' matrix and the right upper triangular 'r' matrix.
     */
    def factor ()
    {
        if (factored) return

        for (j <- 0 until n) {                // for each column j
            val _norm = q.col(j).norm         // norm of the jth column
            r(j, j) = _norm

            if (! (_norm =~ 0.0)) {
                for (i <- 0 until m) q(i, j) /= _norm
                for (k <- j + 1 until n) {
                    r(j, k) = q.col(j) dot q.col(k)
                    for (i <- 0 until m) q(i, k) -= q(i, j) * r(j, k)
                } // for
             } // if

         } // for

         factored = true
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return both the orthogonal 'q' matrix and the right upper triangular 'r' matrix.
     */
    def factors: (MatriD, MatriD) = (q, r)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'a*x = b' using the QR Factorization 'a = q*r' via
     *  'r*x = q.t * b'.
     *  @param  b the constant vector
     */
    def solve (b: VectoD): VectoD = r.bsolve (q.t * b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using QR Factorization
     *  'q*r*x = 0'.  Gives a basis of dimension 'n' - rank for the nullspace
     *  @param rank  the rank of the matrix (number of linearly independent row vectors)
     *  FIX: should work, but it does not
     */
    def nullspace (rank: Int): MatrixD = 
    {
        flaw ("nullspace", "method has bugs - so do not use")
        (new Fac_QR (a.t)).factor1.asInstanceOf [MatrixD].slice (0, n, rank, n)       // last n - rank columns
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
 */
object Fac_QRTest extends App
{
     def test (a: MatrixD)
     {
         val qr = new Fac_QR (a)               // for factoring a into q * r
         val (q, r) = qr.factor12 ()           // (q orthogonal, r upper triangular)
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

     val a4 = new MatrixD ((2, 4), -1.0, 1.0, 2.0,  4.0,
                                    2.0, 0.0, 1.0, -7.0)

     val a5 = new MatrixD ((4, 4), -1.0, 1.0, 2.0,  4.0,
                                    2.0, 0.0, 1.0, -7.0,
                                    2.0, 0.0, 1.0, -7.0,
                                    2.0, 0.0, 1.0, -7.0)

     test (a1)
     test (a2)
     test (a3)
     test (a4)
     test (a5)

} // Fac_QRTest object

