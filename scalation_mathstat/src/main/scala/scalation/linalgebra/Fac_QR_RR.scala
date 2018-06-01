
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhaochong Liu
 *  @version 1.5
 *  @date    Sat Jul 30 22:53:47 EDT 2016
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
      extends Fac_QR_H (aa, needQ) with Pivoting
{
    private var _rank = 0                                          // the rank of matrix aa
    private val _piv  = VectorI.range (0, n)                       // the vector storing the column pivots

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the pivot vector.
     */
    def piv: VectorI = _piv

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform QR Factorization and result in a single matrix which contains Householder vectors
     *  of each columns in lower triangular of 'aa' matrix. 
     *  This Rank Revealing algorithm uses Householder orthogonalization and pivoting.
     *  @see 5.1 and 5.2 in Matrix Computations
     *  @see QRDecomposition.java in Jama
     */
    override def factor (): Fac_QR_RR [MatT] =
    {
        if (factored) return this

        val c = VectorD (for (j <- at.range1) yield at(j).normSq)  // length^2 of a's columns, at's rows

        var c_m = c.max ()                                         // maximum column length^2
        while (_rank < n && c_m > TOL) {                           // stop when max < TOL (tolerance for zero)
            val k_m = c.indexOf (c_m)                              // index of column with max length^2
            _piv.swap (k_m, _rank)                                 // swap pivot column to max
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
        this
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the rank (number of independent columns) in matrix 'aa'.
     */
    def rank: Int = _rank

} // Fac_QR_RR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_QR_RRTest` object is used to test the `Fac_QR_RR` classes.
 *  @see www.ee.ucla.edu/~vandenbe/103/lectures/qr.pdf
 *  @see www.math.usm.edu/lambers/mat610/sum10/lecture9.pdf
 *  FIX: the 'nullspaceV' function need to be fixed.
 *  > runMain scalation.linalgebra.Fac_QR_RRTest
 */
object Fac_QR_RRTest extends App
{
    import Fac_QR._

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the correctness of the QR_RR Factorization.
     *  @param a   the matrix to factor
     *  @param qr  the QR Factorization class/algorithm to use
     *  @param nm  the name of test case/matrix
     */
    def test (nm: String, a: MatrixD, qr: Fac_QR_RR [MatrixD])
    {
        println ("-" * 60)
        val (q, r) = qr.factor12 ()                      // (q orthogonal, r upper triangular)
        val prod   = q * r                               // product of q * r
        val r_est  = qr.rank                             // estimated rank
        val ns     = qr.nullspace (r_est)                // ns is a basis for the nullscpace
        val ar     = qr.reorderCols (a, qr.piv)          // a matrix with columns reordered

        println (nm + "    = " + a)                      // original matrix
        println ("q     = " + q)                         // orthogonal matrix
        println ("r     = " + r)                         // right upper triangular matrix
        println ("r_est = " + r_est)                     // rank
        println ("q*r   = " + prod)                      // product q * r
        println ("eq    = " + (a == prod))               // check that q * r == a
        println ("ar    = " + ar)                        // original matrix reordered
        println ("eq    = " + (ar == prod))              // check that q * r == ar
        println ("ns    = " + ns)                        // nullspace
        println ("a*ns  = " + (a * ns))                  // check that a * ns = 0
    } // test

    println ("*" * 60)
    println ("Fac_QRTest: Fac_QR_RR")
    test ("a1", a1, new Fac_QR_RR (a1))
    test ("a2", a2, new Fac_QR_RR (a2))
    test ("a3", a3, new Fac_QR_RR (a3))
    test ("a4", a4, new Fac_QR_RR (a4))
    test ("a5", a5, new Fac_QR_RR (a5))

} // Fac_QR_RRTest

