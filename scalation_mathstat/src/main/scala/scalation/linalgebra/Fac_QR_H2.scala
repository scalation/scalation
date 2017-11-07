
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.4
 *  @date    Sat Jul 30 22:53:47 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.math.ExtremeD.TOL

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR_H2` class provides methods to factor an 'm-by-n' matrix 'aa' into
 *  the product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.  It uses Householder orthogonalization.
 *  @see 5.1 and 5.2 in Matrix Computations
 *  @see QRDecomposition.java in Jama
 *  @see www.stat.wisc.edu/~larget/math496/qr.html
 *  @see math.stackexchange.com/questions/678843/householder-qr-factorization-for-m-by-n-matrix-both-m-n-and-mn
 *------------------------------------------------------------------------------
 *  This implementation replaces matrix operations in `Fac_QR_H3` with low-level
 *  operations for greater efficiency.  Also, calculates Householder vectors differently.
 *  Caveat: for m < n use `Fac_LQ`.
 *------------------------------------------------------------------------------
 *  @param aa     the matrix to be factor into q and r
 *  @param needQ  flag indicating whether a full q matrix is needed
 */
class Fac_QR_H2 [MatT <: MatriD] (aa: MatT, needQ: Boolean = true)
      extends Fac_QR (aa, needQ)
{
    private val a = aa.copy ()               // copy of matrix aa

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of two matrices, 'a = q * r', returning
     *  both the orthogonal 'q' matrix and the right upper triangular 'r' matrix.
     *  This algorithm uses Householder orthogonalization.
     *  @see 5.1 and 5.2 in Matrix Computations
     *  @see QRDecomposition.java in Jama
     */
    def factor ()
    {
        if (factored) return

        for (k <- 0 until p) {                                  // for each column k in a
            val a_k = a.col (k, k)                              // A(k:m, k) column vector
            var _norm = a_k.norm                                // norm of this column
            if (_norm !=~ 0.0) {
                if (a_k(0) < 0.0) _norm = -_norm                // make kth Householder vector
                for (i <- k until m) a(i, k) /= _norm
                a(k, k) += 1.0
                for (j <- k + 1 until n) {                      // transform remaining columns
                    var sum = 0.0
                    for (i <- k until m) sum += a(i, k) * a(i, j)
                    sum /= - a(k, k)
                    for (i <- k until m) a(i, j) += sum * a(i, k)
                } // for
            } // if
            r(k, k) = -_norm                                    // set r's diagonal element r_kk
        } // for
        if (needQ) computeQ ()
        for (j <- 0 until p; i <- 0 until j) r(i, j) = a(i, j)  // fill in rest of r matrix
        r.clean (TOL)                                           // comment out to avoid cleaning r matrix

        factored = true
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the full 'q' orthogonal matrix based on updated values in 'a'.
     */
    def computeQ ()
    {
        for (k <- p-1 to 0 by -1) {
            if (a(k, k) !=~ 0.0) {
                for (j <- k until n) {
                    var sum = 0.0
                    for (i <- k until m) sum += q(i, j) * a(i, k)
                    sum /= - a(k, k)
                    for (i <- k until m) q(i, j) += sum * a(i, k)
                } // for
            } // if
        } // for
    } // computeQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using 'QR' Factorization
     *  'q*r*x = 0'.  Gives a basis of dimension 'n - rank' for the nullspace
     *  @param rank  the rank of the matrix (number of linearly independent column vectors)
     */
    def nullspace (rank: Int): MatriD = 
    {
        val aat = aa.t                                         // transpose of aa
        val ns = if (aat.dim1 < aat.dim2) {
            val qq = (new Fac_LQ (aat)).factor2 ()             // using LQ
            qq.slice (rank, qq.dim1).t                         // last n - rank rows, transpose 
        } else {
            val qq = (new Fac_QR_H2 (aat)).factor1 ()          // using QR
            qq.slice (0, aat.dim1, rank, aat.dim2)             // last n - rank columns 
        } // if
        if (ns.dim2 > 0) ns else new MatrixD (aat.dim1, 0)
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a: { x | a*x = 0 }' using 'QR' Factorization
     *  'q*r*x = 0'.  Gives only one vector in the nullspace.
     *  @param x  a vector with the correct dimension
     */
    def nullspaceV (x: VectoD): VectoD =
    {
        x(n-1) = 1.0                                 // vector to solve for
        val b = x.zero (n)                           // new rhs as -r_i,n-1          
        for (i <- 0 until n) b(i) = -r(i, n-1)
        val rr = r.slice (0, n, 0, n-1)              // drop last column
        for (k <- n-2 to 0 by -1) {                  // solve for x in rr*x = b
            x(k) = (b(k) - (rr(k) dot x)) / rr(k, k)
        } // for
        x
    } // nullspaceV

} // Fac_QR_H2 class

