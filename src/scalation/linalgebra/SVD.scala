
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Feb 14 14:06:00 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www2.cs.cas.cz/mweb/download/publi/Ples2006.pdf
 *  @see www.math.iit.edu/~fass/477577_Chapter_12.pdf
 *  @see Handbook of Linear Algrbra, Chapter 45
 */

// U N D E R   D E V E L O P M E N T  - to be merged with SVDecomp.scala

package scalation.linalgebra

import math.signum

import scalation.linalgebra.Householder.house
import scalation.linalgebra.MatrixD.{eye, outer}
import scalation.math.Basic.oneIf
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to compute the Singlar Value Decomposition (SVD) of
 *  matrix 'a', i.e., decompose matrix 'a' into 'u * b * v.t'.
 *  @param a  the m-by-n matrix to decompose (requires m >= n)
 */
class SVD (a: MatrixD)
      extends Error
{
    private val m = a.dim1         // number of rows
    private val n = a.dim2         // number of columns

    if (n > m) flaw ("constructor", "SVD requires m >= n")

    private val u = eye (m)        // initialize left unitary matrix to m-by-m identity matrix
    private val v = eye (n)        // initialize right unitary matrix to n-by-n identity matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the Householder Bidiagonalization Algorithm to compute unitary
     *  matrices u and v such that u.t * a * v = b where matrix b is bidiagonal.
     *  The b matrix will only have non-zero elements on its main diagonal and
     *  its super diagonal (the diagonal above the main).
     *  This implementation computes b in-place in matrix a.
     *  @see Matrix Computations:  Algorithm 5.4.2 Householder Bidiagonalization
     */
    def bidiagonalization (): Tuple3 [MatrixD, MatrixD, MatrixD] =
    {
        for (j <- 0 until n) {
            val (vu, bu) = house (a(j until m, j))                   // Householder (vector vu, scalar bu)
            val hu = eye (m-j) - outer (vu, vu * bu)                 // Householder matrix hu
            a(j until m, j until n) = hu * a(j until m, j until n)   // zero column j below main diagonal
//          for (i <- j+1 until m) a(i, j) = vu(i-j)                 // save key part of Householder vector vu
            u *= hu.diag (j)                                         // multiply u by Householder matrix hu

            if (j < n-2) {
                val (vv, bv) = house (a(j, j+1 until n))                     // Householder (vector vv, scalar bv)
                val hv = eye (n-j-1) - outer (vv, vv * bv)                   // Householder matrix hv
                a(j until m, j+1 until n) = a(j until m, j+1 until n) * hv   // zero row j past super diagonal
//              for (k <- j+2 until n) a(j, k) = vv(k-j-1)                   // save key part of Householder vector vv
                v *= hv.diag (j+1)                                           // multiply v by Householder matrix hv
            } // if
        } // for

        for (i <- 0 until m; j <- 0 until n if j != i && j != i+1) a(i, j) = 0.   // clean matrix off diagonals

        (u, a, v)                           // return (u, b as a, v)
    } // bidiagonalization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the Golub-Kahan Bidiagonalization Algorithm to compute unitary matrices
     *  u and v such that u.t * a * v = b where matrix b is bidiagonal (nonzero
     *  elements on the main diagonal and the diagonal above it).  Solve a * v = u * b
     *  by computing column vectors u_k and v_k and diagonals c and d.  Use transposes
     *  of u and v since row access is more efficient than column access.
     *  Caveat: assumes bidiagonals elements are non-negative and need to add
     *  re-orthogonalization steps.
     *  @see web.eecs.utk.edu/~dongarra/etemplates/node198.html
     */
    def bidiagonalization2 (): Tuple2 [VectorD, VectorD] =
    {
        var c  = new VectorD (n-1)                   // above main diagonal (beta)
        var d  = new VectorD (n)                     // main diagonal (alpha)
        var ut = new MatrixD (n, m)                  // transpose of u 
        var vt = new MatrixD (n, m); vt(0, 0) = 1.   // transpose of v
        val at = a.t                                 // transpose of matrix a

        for (k <- 0 until n) {
            ut(k)  = a * vt(k); if (k > 0) ut(k) -= ut(k-1) * c(k-1)
            d(k)   = ut(k).norm
            ut(k) /= d(k)

            val k1 = k+1
            if (k1 < n) {
                vt(k1)  = at * ut(k) - vt(k) * d(k)
                c(k)    = vt(k1).norm
                vt(k1) /= c(k)
            } // if
        } // for

        println ("ut = " + ut)
        println ("vt = " + vt)

        (c, d)
    } // bidiagonalization2

} // SVD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SVD class.
 *  bidiagonalization answer = [ (21.8, -.613), (12.8, 2.24, 0.) ]
 */
object SVDTest extends App
{
    val a = new MatrixD ((4, 3), 1.,  2.,  3.,
                                 4.,  5.,  6.,
                                 7.,  8.,  9.,
                                10., 11., 12.)
    println ("a = " + a)
    val svd = new SVD (a)
    val (u, b, v) = svd.bidiagonalization ()
    println ("u     = " + u)
    println ("b     = " + b)    
    println ("v     = " + v)
    println ("ubv.t = " + u * b * v.t)    // should equal the original a

} // SVDTest object

