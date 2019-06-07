
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.6
 *  @date    Sat Jul 30 22:53:47 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.math.ExtremeD.TOL

import MatrixD.{eye, outer}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR_H3` class provides methods to factor an 'm-by-n' matrix 'a' into the
 *  product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.  It uses  Householder orthogonalization.
 *  Note, orthogonal means that 'q.t * q = I'.
 *  @see 5.1 and 5.2 in Matrix Computations
 *  @see QRDecomposition.java in Jama
 *  @see www.stat.wisc.edu/~larget/math496/qr.html
 *------------------------------------------------------------------------------
 *  This implementation is the easiest to understandard, but the least efficient.
 *  Caveat: for m < n use `Fac_LQ`.
 *  FIX: change 'aa: MatrixD' to 'aa: MatriD', requires 'times_ip_pre' in trait
 *------------------------------------------------------------------------------
 *  @param a      the matrix to be factor into q and r
 *  @param needQ  whether the full q matrix is need, e.g., Regression does not
 */
class Fac_QR_H3 (aa: MatrixD, needQ: Boolean = true)
      extends Fac_QR (aa, needQ)
{
    private val a = aa.copy ()                 // copy of matrix aa 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Householder vector 'v' computed from vector 'x'.
     *  @param x  the given vector for calculating the Householder vector
     *  @param α  the sign adjusted norm of x
     */
    def houseV (x: VectoD, α: Double): VectoD = x + (0, α)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Householder reflection matrix 'h' computed from Householder vector 'v'.
     *  @param v  the Householder vector
     */
    def hReflect (v: VectoD) = eye (v.dim) - outer (v, v) * (2.0 / v.normSq)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of two matrices 'a = q * r', where
     *  'q' is an orthogonal matrix and 'r' is a right upper triangular matrix.
     */
    def factor (): Fac_QR_H3 =                                  // FIX - add MatT parameter
    {
        if (factored) return this

        for (k <- 0 until p) {                                  // for each column k in a
            val a_k = a.col (k, k)                              // A(k:m, k) column vector
            var _norm = a_k.norm                                // norm of this column
            if (_norm !=~ 0.0) {
                if (a_k(0) < 0.0) _norm = - _norm               // adjust sign
                val v = houseV (a_k, _norm)                     // kth Householder vector
                val h = hReflect (v)                            // kth Householder reflection matrix
                a.times_ip_pre (h, k)                           // pre-multiply a and h starting at a_kk
                for (i <- k until m) a(i, k) = v(i-k)           // store Householder vector in bottom part of a
            } // if
            r(k, k) = - _norm                                   // set r's diagonal element r_kk
        } // for
        if (needQ) computeQ ()
        for (j <- 0 until p; i <- 0 until j) r(i, j) = a(i, j)  // fill in rest of r matrix
        r.clean (TOL)                                           // comment out to avoid cleaning r matrix

        factored = true
        this
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
                    sum *= - 2.0 / a.col (k, k).normSq
                    for (i <- k until m) q(i, j) += sum * a(i, k)
                } // for
            } // if
        } // for
    } // computeQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the nullspace of matrix 'a':  basis { x | a*x = 0 }' using 'QR'
     *  Factorization 'q*r*x = 0'.  Gives a basis of dimension 'n - rank' for the
     *  nullspace.
     *  @param rank  the rank of the matrix (number of linearly independent column vectors)
     */
    def nullspace (rank: Int): MatriD = 
    {
        val aat = aa.t                                         // transpose of aa
        val ns = if (aat.dim1 < aat.dim2) {
            val qq = (new Fac_LQ (aat)).factor2 ()             // using LQ
            qq.slice (rank, qq.dim1).t                         // last n - rank rows, transpose    
        } else {
            val qq = (new Fac_QR_H3 (aat)).factor1 ()          // using QR
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
        for (i <- 0 until n) b(i) = -a(i, n-1)
        val rr = a.slice (0, n, 0, n-1)              // drop last column
        for (k <- n-2 to 0 by -1) {                  // solve for x in rr*x = b
            x(k) = (b(k) - (rr(k) dot x)) / rr(k, k)
        } // for
        x
    } // nullspaceV

} // Fac_QR_H3 class

