
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Jan 20 13:01:21 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.math.ExtremeD.TOL

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR_MGS` class provides methods to factor an 'm-by-n' matrix 'a' into the
 *  product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.  It uses Modified Gram-Schmidt (MGS) orthogonalization.
 *  Note, orthogonal means that 'q.t * q = I'.
 *  @see http://www.stat.wisc.edu/~larget/math496/qr.html
 *  @see http://en.wikipedia.org/wiki/Gram–Schmidt_process
 *       (stabilized Gram–Schmidt orthonormalization)
 *  @param a  the matrix to be factor into q and r
 */
class Fac_QR_MGS (a: MatrixD)
      extends Fac_QR (a, true)
{
    override val q = new MatrixD (a)             // the orthogonal q matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of two matrices, 'a = q * r', returning
     *  both the orthogonal 'q' matrix and the right upper triangular 'r' matrix.
     *  This algorithm uses Modified Gram-Schmidt 'MGS' orthogonalization.
     *  @see Algorithm 5.2.6 in Matrix Computations.
     */
    def factor ()
    {
        if (factored) return

        for (j <- 0 until n) {                   // for each column j
            val _norm = q.col(j).norm            // norm of the jth column
            r(j, j) = _norm

            if (! (_norm =~ 0.0)) {
                for (i <- 0 until m) q(i, j) /= _norm
                for (k <- j + 1 until n) {
                    r(j, k) = q.col(j) dot q.col(k)
                    for (i <- 0 until m) q(i, k) -= q(i, j) * r(j, k)
                } // for
             } // if

         } // for
         r.clean (TOL)                           // comment out to avoid cleaning r matrix

         factored = true
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the full orthogonal matrix 'q'.  No implementation needed, since
     *  it is automatically computed.
     */
    def computeQ () {}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using 'QR' Factorization
     *  'q*r*x = 0'.  Gives a basis of dimension 'n - rank' for the nullspace
     *  @param rank  the rank of the matrix (number of linearly independent column vectors)
     */
    def nullspace (rank: Int): MatriD = 
    {
        val aat = a.t                                          // transpose of a
        val ns = if (aat.dim1 < aat.dim2) {
            val qq = (new Fac_LQ (aat)).factor2 ()             // using LQ
            qq.slice (rank, qq.dim1).t                         // last n - rank rows, transpose
        } else {
            val qq = (new Fac_QR_MGS (aat)).factor1 ()         // using QR
            qq.slice (0, aat.dim1, rank, aat.dim2)             // last n - rank columns
        } // if
        if (ns.dim2 > 0) ns else new MatrixD (aat.dim1, 0)
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using 'QR' Factorization
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

} // Fac_QR_MGS class

