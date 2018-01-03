
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Jan 22 15:18:51 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Matrix Computations:  Algorithm 5.4.2 Householder Bidiagonalization
 *
 *  This version translated from the following Algol code:
 *  @see people.duke.edu/~hpgavin/SystemID/References/Golub+Reinsch-NM-1970.pdf
 */

package scalation.linalgebra

import math.{abs, sqrt}

import scalation.linalgebra.Householder.house
import scalation.math.ExtremeD.TOL
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bidiagonal` class is used to create a bidiagonal matrix from matrix 'a'.
 *  It uses the Householder Bidiagonalization Algorithm to compute orthogonal
 *  matrices 'u' and 'v' such that 
 *  <p>
 *      u.t * a * v = b
 *      a  =  u * b * v.t
 *  <p>
 *  where matrix 'b' is bidiagonal, i.e., it will only have non-zero elements on
 *  its main diagonal and its super-diagonal (the diagonal above the main).
 *  <p>
 *      u is an m-by-n matrix
 *      b is an n-by-n matrix (bidiagonal - FIX: use `BidMatrixD`)
 *      v is an n-by-n matrix
 *  <p>
 *------------------------------------------------------------------------------
 *  @param a  the m-by-n matrix to bidiagonalize (requires m >= n)
 */
class Bidiagonal [MatT <: MatriD] (a: MatT) 
      extends Error
{
    private val m = a.dim1                    // the number of rows in matrix a
    private val n = a.dim2                    // the number of columns in matrix a

    if (n > m) flaw ("constructor", "Bidiagonal requires m >= n")

    private val u  = a.copy ()                // initialize left orthogonal matrix to m-by-n matrix
    private val v  = new MatrixD (n, n)       // initialize right orthogonal matrix to n-by-n matrix
    private val e  = new VectorD (n)          // super-diagonal for b
    private val q  = new VectorD (n)          // main diagonal for b
    private val b  = new MatrixD (n, n)       // n-by-n matrix from e and q diagonals
    private var bm = 0.0                      // maximum column magnitude from the bidiagonal matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum column magnitude from bidiagonal matrix 'b'.
     */
    def bmax: Double = bm

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the super-diagonal 'e' and main diagonal 'q' as vectors.
     */
    def e_q: (VectorD, VectorD) = (e, q)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sliced dot product.  Take the dot product of two row/column
     *  vectors starting from the 'from' parameter.
     *  @param v1    the first vector
     *  @param v2    the second vector
     *  @param from  the offset from which to compute the sliced dot product
     */
    def sdot (v1: VectoD, v2: VectoD, from: Int = 0): Double =
    {
        var sum = 0.0
        for (i <- from until v1.dim) sum += v1(i) * v2(i)
        sum
    } // sdot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Bidiagonalize matrix 'a' using the Householder Bidiagonalization Algorithm
     *  to compute orthogonal matrices 'u' and 'v' such that 'u.t * a * v = b'
     *  where matrix 'b' is bidiagonal.
     */
    def bidiagonalize (): (MatriD, MatriD, MatriD) =
    {
        var f, g = 0.0                                  // typically [ f  g ]
        var h    = 0.0                                  //           [ 0  h ]
        for (i <- 0 until n) {

            var l = i + 1                               // set control index l
            e(i) = g                                    // assign ith super-diagonal element
            var s = sdot (u.col(i), u.col(i), i)        // u(i:m,i) dot u(i:m,i)

            if (s < TOL) g = 0.0
            else {
                f = u(i, i)
                g = if (f < 0.0) sqrt (s) else -sqrt (s)
                h = f * g - s; u(i, i) = f - g
                for (j <- l until n) {
                    s = sdot (u.col(i), u.col(j), i)
                    f = s / h
                    for (k <- i until m) u(k, j) += f * u(k, i)
                } // for
            } // if

            q(i) = g                                     // assign ith main diagonal element
            s = sdot (u(i), u(i), l)                     // u(i,l:n) dot u(i,l:n)

            if (s < TOL) g = 0.0
            else {
                f = u(i, i+1)
                g = if (f < 0) sqrt (s) else -sqrt(s)
                h = f * g - s; u(i, i+1) = f - g
                for (j <- l until n) e(j) = u(i, j) / h
                for (j <- l until m) {
                    s = sdot (u(i), u(j), l)
                    for (k <- l until n) u(j, k) += s * e(k)
                } // for
            } // if

            val y = abs (q(i)) + abs (e(i)); if (y > bm) bm = y
        } // for
 
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Accumulate right-hand side transformations to create 'v' matrix.
         */
        def transformRHS ()
        {
            var l = n - 1
            for (i <- n-1 to 0 by -1) {
                if (g != 0.0) {
                    h = u(i, i+1) * g
                    for (j <- l until n) v(j, i) = u(i, j) / h
                    for (j <- l until n) {
                        val s = sdot (u(i), v.col(j), l)
                        for (k <- l until n) v(k, j) += s * v(k, i)
                    } // for
                } // if
                for (j <- l until n) { v(i, j) = 0.0; v(j, i) = 0.0 }
                v(i, i) = 1.0
                g = e(i)
                l = i
            } // for
        } // transformRHS

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Accumulate left-hand side transformations to update 'u' matrix.
         */
        def transformLHS ()
        {
            for (i <- n-1 to 0 by -1) {
                val l = i + 1
                g = q(i)
                for (j <- l until n) u(i, j) = 0.0
                if (g != 0.0) {
                    h = u(i, i) * g
                    for (j <- l until n) {
                        val s = sdot (u.col(i), u.col(j), l)
                        f = s / h
                        for (k <- i until m) u(k, j) += f * u(k, i)
                    } // for
                    for (j <- i until m) u(j, i) /= g
                } else {
                    for (j <- i until m) u(j, i) = 0.0
                } // if
                u(i, i) += 1.0
            } // for
        } // transformLHS

        transformRHS ()                         // transform the RHS to finalize matrix v
        transformLHS ()                         // transform the LHS to finalize matrix u
        b.setDiag (e.slice (1, n), 1)           // put the super-diagonal into matrix b
        b.setDiag (q)                           // put the main diagonal into matrix b
        (u, b, v)                               // return the three matrices
    } // bidiagonalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test whether the product of factorization equals the orginal matrix.
     */
    def test ()
    {
        val prod = u * b * v.t                  // compute the product of the three matrices

        println (s"u    = $u")
        println (s"b    = $b")
        println (s"v    = $v")
        println (s"prod = $prod")
        assert (a == prod)
    } // test

} // Bidiagonal class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BidiagonalTest` object is used to test the `Bidiagonal` class.
 *  bidiagonalization answer = [ (21.8, -.613), (12.8, 2.24, 0.) ]
 *  @see books.google.com/books?isbn=0801854148  (p. 252)
 *  > runMain scalation.linalgebra.BidiagonalTest
 */
object BidiagonalTest extends App
{
    val a = new MatrixD ((4, 3), 1.0,  2.0,  3.0,     // orginal matrix
                                 4.0,  5.0,  6.0,
                                 7.0,  8.0,  9.0,
                                10.0, 11.0, 12.0)
    println ("a = " + a)

    val bid = new Bidiagonal (a)                      // Householder bidiagonalization
    bid.bidiagonalize ()                              // bidiagonalize a
    bid.test ()                                       // test the product u * b * v.t

} // BidiagonalTest object

