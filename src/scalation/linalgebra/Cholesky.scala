
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Sameer Gaherwar, John Miller
 *  @version 1.0
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import math.sqrt

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object provides a method to factor symmetric positive definite matrices
 *  a into l * l.t.
 *  @param a  the symmetric positive definite matrix to factor
 */
class Cholesky (a: MatrixD)
      extends Error
{
    if ( ! a.isSquare)    flaw ("cholesky", "the matrix must be square")
    if ( ! a.isSymmetric) flaw ("cholesky", "the matrix must be symmetric")

    private val l = new MatrixD (a.dim1, a.dim2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the lower triangular submatrix l from the Cholesky decomposition
     *  a = l * l.t where l.t is the transpose.  It uses the Cholesky–Banachiewicz
     *  algorithm.
     *  @see introcs.cs.princeton.edu/java/95linear
     */
    def cholesky (): MatrixD =
    {
        for (i <- 0 until a.dim1; j <- 0 to i) {
            val diff = a(i, j) - (l(i) dot l(j))
            l(i, j)  = if (i == j) sqrt (diff) else  diff / l(j, j)
        } // for
        l
    } // cholesky

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Cholesky decomposition to solve a system of equations: a * x = b,
     *  returning the solution x using forward and backward substitution.
     *  @param b  the constant vector
     */
    def solve (b: VectorD): VectorD =
    {
        val y = new VectorD (l.dim2)
        for (k <- 0 until y.dim) {                   // solve for y in l*y = b
            y(k) = (b(k) - (l(k) dot y)) / l(k, k)
        } // for

        val x = new VectorD (l.dim2)
        for (k <- x.dim - 1 to 0 by -1) {            // solve for x in l.t*x = y
            x(k) = (y(k) - (l.col(k) dot x)) / l(k, k)
        } // for
        x
    } // solve

} // Cholesky object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Cholesky class.
 *  @see ece.uwaterloo.ca/~dwharder/NumericalAnalysis/04LinearAlgebra/cholesky
 */
object CholeskyTest extends App
{
    val a = new MatrixD ((4, 4), 4.0,  0.4,   0.8, -0.2,
                                 0.4,  1.04, -0.12, 0.28,
                                 0.8, -0.12,  9.2,  1.4,
                                -0.2,  0.28,  1.4,  4.35)
    val b = new VectorD (-0.2, -0.32, 13.52, 14.17)

    val c = new Cholesky (a)
    println ("a = " + a)
    println ("cholesky = " + c.cholesky ())
    println ("solve    = " + c.solve (b))

} // CholeskyTest object

