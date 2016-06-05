
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Sameer Gaherwar, John Miller
 *  @version 1.2
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.math.sqrt

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_Cholesky` class provides methods to factor an 'n-by-n' symmetric,
 *  positive definite matrix 'a' into the product of two matrices:
 *  <p>
 *      'l'   - an 'n-by-n' left lower triangular matrix
 *      'l.t' - an 'n-by-n' right upper triangular matrix - transpose of 'l'
 *  <p>
 *  such that 'a = l * l.t'.
 *  @param a  the symmetric, positive definite matrix to be factor
 */
class Fac_Cholesky [MatT <: MatriD] (a: MatT)
      extends Factorization with Error
{
    private val n = a.dim1                    // the matrix is n-by-n
    private val l = a.zero (n, n)             // for factored lower triangular matrix

    if (! a.isSquare)    flaw ("constructor", "matrix a must be square")
    if (! a.isSymmetric) flaw ("constructor", "matrix a must be symmetric")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of 'l' and 'l.t', returning the lower
     *  triangular matrix 'l' from the Cholesky Factorization 'a = l * l.t'
     *  where 'l.t' is the transpose.  It uses the Cholesky–Banachiewicz algorithm.
     *  @see introcs.cs.princeton.edu/java/95linear
     */
    def factor1 (): MatriD =
    {
        for (i <- 0 until n; j <- 0 to i) {
            val diff = a(i, j) - (l(i) dot l(j))
            l(i, j)  = if (i == j) sqrt (diff) else  diff / l(j, j)
        } // for
        raw = false                                      // factoring completed
        l
    } // factor1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of 'l' and 'l.t', returning both.
     */
    def factor (): Tuple2 [MatriD, MatriD] = { if (raw) factor1 (); (l, l.t) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of 'l' and 'l.t', returning l.t.
     */
    def factor2 (): MatriD =  { if (raw) factor1 (); l.t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the lower triangular matrix 'l' from the Cholesky Factorization to
     *  solve a system of equations 'a * x = b'. Return the solution x using
     *  forward and backward substitution.
     *  @param b  the constant vector
     */
    def solve (b: VectoD): VectoD =
    {
        val y = new VectorD (n)
        for (k <- 0 until n) {                        // solve for y in l*y = b
            y(k) = (b(k) - (l(k) dot y)) / l(k, k)
        } // for

        val x = new VectorD (n)
        for (k <- n-1 to 0 by -1) {                   // solve for x in l.t*x = y
            x(k) = (y(k) - (l.col(k) dot x)) / l(k, k)
        } // for
        x
    } // solve

} // Fac_Cholesky class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_CholeskyTest` object is used to test the `Fac_Cholesky` class.
 *  @see ece.uwaterloo.ca/~dwharder/NumericalAnalysis/04LinearAlgebra/cholesky
 *  > run-main scalation.linalgebra.Fac_CholeskyTest
 */
object Fac_CholeskyTest extends App
{
    val a = new MatrixD ((4, 4), 4.0,  0.4,   0.8, -0.2,
                                 0.4,  1.04, -0.12, 0.28,
                                 0.8, -0.12,  9.2,  1.4,
                                -0.2,  0.28,  1.4,  4.35)
    val b = VectorD (-0.2, -0.32, 13.52, 14.17)

    val chol = new Fac_Cholesky (a)
    println ("a = " + a)
    println ("factor1  = " + chol.factor1 ())
    println ("solve    = " + chol.solve (b))

    val lu = a.lud
    println ("lu solve = " + a.solve (lu, b))

} // Fac_CholeskyTest object

