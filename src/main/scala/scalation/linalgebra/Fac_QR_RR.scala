
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.2
 *  @date    Sun Jul 24 13:39:21 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.math.ExtremeD.TOL

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR_RR` class provides methods to factor an 'm-by-n' matrix 'a' into the
 *  product of two matrices:
 *  <p>
 *      'q' - an 'm-by-n' orthogonal matrix and
 *      'r' - an 'n-by-n' right upper triangular matrix
 *  <p>
 *  such that 'a = q * r'.   It uses uses Householder orthogonalization.
 *  @see 5.1 and 5.2 in Matrix Computations
 *  @see QRDecomposition.java in Jama
 *  @see www.stat.wisc.edu/~larget/math496/qr.html
 *-------------------------------------------------------------------------------
 *  This implementation extends `Fac_QR_H` and adds column pivoting for greater
 *  robustness and resonably accurate rank determination (Rank Revealing QR).
 *  Caveat: for m < n use `Fac_LQ`.
 *-------------------------------------------------------------------------------
 *  @param aa     the matrix to be factor into q and r
 *  @param needQ  flag indicating whether a full q matrix is needed
 */
class Fac_QR_RR [MatT <: MatriD] (aa: MatT, needQ: Boolean = true)
      extends Fac_QR_H (aa, needQ)
{
    private var _rank = 0                                          // the rank of matrix aa
    private val piv   = VectorI.range (0, n)                       // vector storing the column pivots

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform QR Factorization and result in a single matrix which contains Householder vectors
     *  of each columns in lower triangular of 'aa' matrix. 
     *  This Rank Revealing algorithm uses Householder orthogonalization and pivoting.
     *  @see 5.1 and 5.2 in Matrix Computations
     *  @see QRDecomposition.java in Jama
     */
    override def factor ()
    {
        if (factored) return

        val c = VectorD (for (j <- at.range1) yield at(j).normSq)  // length^2 of a's columns, at's rows

        var c_m = c.max ()                                         // maximum column length^2
        while (_rank < n && c_m > TOL) {                           // stop when max < TOL (tolerance for zero)
            val k_m = c.indexOf (c_m)                              // index of column with max length^2
            piv.swap (k_m, _rank)                                  // swap pivot column to max
            if (k_m != _rank) {
                at.swap (k_m, _rank)                               // swap rows in at (columns in a)
                c.swap (k_m, _rank)                                // swap column lengths
            } // if
            colHouse (_rank)                                       // perform kth factoring step                  
            for (j <- _rank+1 until n) c(j) -= at(j, _rank) ~^ 2
            _rank += 1
            c_m = if (_rank < n) c.slice (_rank).max () else 0.0
        } // while

        for (j <- 1 until p) {                                     // fill in rest of r matrix
            val at_j = at.v(j)
            for (i <- 0 until j) r(i, j) = at_j(i)
        } // for
        if (needQ) computeQ ()
        r.clean (TOL)                                              // comment out to avoid cleaning r matrix

        factored = true
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the rank (number of independent columns) in matrix 'aa'.
     */
    def rank: Int = _rank

} // Fac_QR_RR class

