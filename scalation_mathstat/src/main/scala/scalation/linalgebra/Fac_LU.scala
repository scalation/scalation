
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Mustafa Nural, John Miller
 *  @version 1.3
 *  @date    Fri May 26 14:32:21 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  LU Factorization for square (n-by-n) and rectangular (m-by-n, m > n) matrices.
 *  Uses partial (row) pivoting that uses largest possible pivot for each column.
 *
 *  Code adapted from Jama LUDecomposition.java
 *  @see github.com/fiji/Jama/blob/master/src/main/java/Jama/LUDecomposition.java
 */

package scalation.linalgebra

import scala.collection.mutable.Set
import scala.math.{abs, min, signum}

import scalation.util.{banner, Error}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LU` class provides methods to factor an 'm-by-n' matrix into its
 *  lower and upper triangular products:
 *  <p>
 *      A  = LU   when partial pivoting is not needed
 *      PA = LU   where P is the permutation matrix
 *      A  = QLU  where Q = P.inverse
 *  <p>
 *  where 'a' is the given matrix, 'l' is an 'm-by-n' lower triangular matrix, and
 *  'u' is an 'n-by-n' upper triangular matrix.  The permutation matrix is represented
 *  by the 'piv' vector.  Once factored, can be used to solve a system of linear
 *  equations.
 *  <p>
 *     Solve for x in Ax = b: Ax = QLUx = b => LUx = Pb using steps (1) and (2)
 *     (1) Solve Ly = Pb  using forward substitution for y
 *     (2) Solve Ux = y   using backward substitution for x
 *  <p>
 *  @param a  the given m-by-n rectangular matrix
 */
class Fac_LU [MatT <: MatriD] (a: MatT)
    extends Factorization with Error
{
    private val DEBUG  = true                              // debug flag
    private val (m, n) = (a.dim1, a.dim2)                  // (# rows, # columns) in matrix a

    if (m < n) flaw ("constructor", "requires m >= n")

    private val l          = new MatrixD (a)               // copy of matrix a to be factored
                                                           // initally holds both l and u, proper l after split
    private var u: MatrixD = null                          // the right upper triangular matrix
    private var pivsign    = 1                             // initial value for pivot sign (used in det method)
    private val piv        = VectorI.range (0, m)          // the initial values for pivots

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of 'l' and 'u'.
     */
    def factor () =
    {
        for (j <- l.range2) {                              // for each column j
            val col_j = l.col(j)()                         // array (copied from lu) holding column j

            for (i <- l.range1) {                          // for each row i
                val row_i = l()(i)                         // array (in l) holding row i
                val kmax  = min (i, j)
                var sum   = 0.0                            // compute dot product truncated at kmax
                for (k <- 0 until kmax) sum += row_i(k) * col_j(k)
                col_j(i) -= sum                            // decrement by dot product
                row_i(j)  = col_j(i)                       // row i col j = col j row i
            } // for

            var p = j                                      // find pivot for column j
            for (i <- j+1 until m; if abs (col_j(i)) > abs (col_j(p))) p = i

            if (p != j) {
                if (DEBUG) println (s"factor: swap rows $j and $p")
                l.swap (p, j)                              // swap rows j and p
                piv.swap (p, j)                            // also swap in pivot vector
                pivsign = -pivsign
            } // if

            if (l(j, j) != 0.0) {                         // compute multipliers for l
                for (i <- j+1 until m) l(i, j) /= l(j, j)
            } // if
        } // for

        factored = true
        split ()                                          // split l into l proper and u
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split 'l' into lower and upper triangular matrices by placing the upper
     *  portion in 'u' and clearing, so 'l' is properly lower triangular.
     */
    def  split ()
    {
        u = l.upperT                                      // extract upper triangle including main diagonal
        for (i <- 0 until n; j <- i until n) {            // set main diagonal to one and
            l(i, j) = if (i == j) 1.0 else 0.0            // clear entries above main diagonal
        } // for
    } // split

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'l' and 'u' matrices.
     */
    def factors: (MatriD, MatriD) = (l, u)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Permute matrix 'c', equivalent to 'qc', i.e., multiplying by the inverse
     *  of the permutation matrix 'p'.
     *  @param c  the matrix to permute
     */
    def permute (c: MatriD)
    {
        val swapped = Set [Int] ()
        for (j <- 0 until n) if (j != piv(j) && ! (swapped contains j)) {
            val pj = piv(j)
            if (DEBUG) println (s"permute: swap rows $j and $pj")
            swapped += pj
            c.swap (j, pj)
        } // for
    } // permute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Permute vector 'd', equivalent to 'pd', i.e., multiplying by the
     *  permutation matrix 'p'.
     *  @param d  the vector to permute
     */
    def permute (d: VectoD): VectoD =
    {
        val swapped = Set [Int] ()
        for (j <- 0 until n) if (j != piv(j) && ! (swapped contains j)) {
            val pj = piv(j)
            if (DEBUG) println (s"permute: swap elements $j and $pj")
            swapped += pj
            d.swap (j, pj)
        } // for
        d
    } // permute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' via 'l*y = b' and 'u*x = y'.
     *  Return the solution vector 'x'.
     *  @param b  the constant vector
     */
    def solve (b: VectoD): VectoD =
    {
        if (m != n) throw new IllegalArgumentException ("solve: requires a square matrix")

        val bb = b.copy                              // make a copy to perserve b
        permute (bb)                                 // permute according to piv
        val y = new VectorD (l.dim2)                 // forward substitution
        for (k <- 0 until y.dim) {                   // solve for y in l*y = bb
            val l_k = l(k)
            var sum = 0.0
            for (j <- 0 until k) sum += l_k(j) * y(j)
            y(k) = bb(k) - sum
        } // for
        if (DEBUG) println (s"solve: y = $y")
        bsolve (y)
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' using back substitution in the equation 'u*x = y' where
     *  matrix 'u' is upper triangular.
     *  @param y  the constant vector
     */
    def bsolve (y: VectoD): VectorD =
    {
        val x = new VectorD (u.dim2)                 // vector to solve for
        for (k <- x.dim - 1 to 0 by -1) {            // solve for x in u*x = y
            val u_k = u(k)
            var sum = 0.0
            for (j <- k + 1 until u.dim2) sum += u_k(j) * x(j)
            x(k) = (y(k) - sum) / u(k, k)
        } // for
        if (DEBUG) println (s"bsolve: x = $x")
        x
    } // bsolve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the inverse of matrix 'a' from the LU Factorization.
     */
    def inverse: MatriD =
    {
        if (m != n) throw new IllegalArgumentException ("inverse: matrix must be square");

        val inv = MatrixD.eye (n)                    // inverse matrix - starts as identity
        for (j <- a.range2) inv.setCol (j, solve (inv.col (j)))
        inv
    } // inverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of matrix 'a'.  The value of the determinant
     *  indicates, among other things, whether there is a unique solution to a
     *  system of linear equations (a nonzero determinant).
     */
    def det: Double =
    {
        if (m != n) throw new IllegalArgumentException ("det: matrix must be square");

        var dt = pivsign.toDouble
        for (j <- l.range2) dt *= u(j, j)
        dt
    } // det

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the rank (number of independent columns) of 'm-by-n' matrix 'a', by
     *  counting the number of non-zero diagonal elements in 'u'.
     *  If 'rank < n', then 'a' is singular.
     *  @see en.wikipedia.org/wiki/Rank_%28linear_algebra%29
     *  @see `Fac_QR_RR` or `SVD`
     */
    def rank: Int = n - u.getDiag ().countZero

} // Fac_LU class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LU` companion object provides functions related to LU Factorization.
 */
object Fac_LU extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' in an over determined system of
     *  linear equation using least squares.  Return the solution vector 'x'.
     *  @see people.csail.mit.edu/bkph/articles/Pseudo_Inverse.pdf
     *  @param a  the matrix A holding the coefficients of the equations
     *  @param b  the constant vector
     */
    def solveOver (a: MatriD, b: VectoD): VectoD =
    {
        val at = a.t
        val lu = new Fac_LU (at * a)
        lu.factor ()
        lu.solve (at * b)
    } // solveOver

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' in an under determined system of
     *  linear equation by finding the smallest solution.  Return the solution
     *  vector 'x'.
     *  @see people.csail.mit.edu/bkph/articles/Pseudo_Inverse.pdf
     *  @param a  the matrix A holding the coefficients of the equations
     *  @param b  the constant vector
     */
    def solveUnder (a: MatriD, b: VectoD): VectoD =
    {
        val at = a.t
        val lu = new Fac_LU (a * at)
        lu.factor ()
        at * lu.solve (b)
    } // solveUnder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve a system of linear equations 'a*x = b'.
     *  @param a   the matrix A holding the coefficients of the equations
     *  @param lu  LU Factorization of matrix A
     *  @param b   the constant vector
     */
    def solve (a: MatriD, lu: Fac_LU [MatriD], b: VectoD): VectoD =
    {
        val (m, n) = (a.dim1, a.dim2)
        if (m == n)     lu.solve (b)         // square
        else if (m > n) solveOver (a, b)     // more equations than variables
        else            solveUnder (a, b)    // fewer equations then variables
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute an estimate of the L1 norm of 'this' matrix, i.e., maximum absolute
     *  column sum.  It uses an adapted version of Hager's algorithm.
     *  @author Michael Cotterell
     *  @see Algorithm 4.1 in HIGHAM1998
     *  @see Higham, N.J. "Fortran Codes for Estimating the One-Norm of a
     *       Real or Complex Matrix, with Applications to Condition Estimation."
     *       ACM Trans. Math. Soft., 14, 1988, pp. 381-396.
     *  @see www.maths.manchester.ac.uk/~higham/narep/narep135.pdf
     *  @param a     the matrix A whose norm is sought
     *  @param a_lu  LU Factorization of matrix A
     *  @param inv   whether or not to compute for inverse (default true)
     */
    def norm1est (a: MatriD, a_lu: Fac_LU [MatriD], inv: Boolean = true): Double =
    {
        if (! a.isSquare) flaw ("norm1est", "the matrix must be square")

        if (! a_lu.factored) a_lu.factor ()                       // factor matrix a
        val at    = a.t
        val at_lu = new Fac_LU (a); at_lu.factor ()                  // factor the transpose matrix a

        val e  = a(0); e.set (1.0 / a.dim2)
        var v  = if (inv) a_lu.solve (e) else a * e
        if (a.dim2 == 1) return v.norm

        var γ    = v.norm1
        var ξ    = v.map (signum (_))
        var x    = if (inv) at_lu.solve (ξ) else at * ξ
        var k    = 2
        var done = false
        val ITER = 5

        while (! done) {
            val j = x.abs.argmax ()
            for (k <- e.range) e(k) = if (k == j) 1.0 else 0.0    // one in jth position
            v     = if (inv) a_lu.solve (e) else a.col (j)
            val g = γ
            γ     = v.norm1

            if (v.map (signum (_)) == ξ || γ <= g) {
                for (i <- x.range) x(i) = (if (i % 2 == 0) -1.0 else 1.0) * (1.0 + ((i - 1.0) / (x.dim - 1)))
                x = if (inv) a_lu.solve (x) else a * x
                if ((2.0 * x.norm1) / (3.0 * x.dim) > γ) { v = x; γ = (2.0 * x.norm1) / (3.0 * x.dim) }
            } // if

            ξ  = v.map (signum (_))
            x  = if (inv) at_lu.solve (ξ) else at * ξ
            k += 1
            if (x.norm1 == x(j) || k > ITER) done = true
        } // while
        γ
    } // norm1est

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute an estimate of the L1 norm of 'this' matrix, i.e., maximum absolute
     *  column sum.  It uses an adapted version of Hager's algorithm.
     *  @param a     the matrix A whose norm is sought
     *  @param inv   whether or not to compute for inverse (default true)
     */
    def norm1est (a: MatriD, inv: Boolean): Double =
    {
        norm1est (a, new Fac_LU (a), inv)
    } // norm1est

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the condition number of matrix 'a', which equals
     *  <p>
     *      ||a|| ||b||  where b = a.inverse
     *  <p>
     *  Requires 'a' to be a square matrix.
     *  For rectangular matrices, @see `SVDecomp`.
     *  @param a     the matrix whose condition number is sought
     *  @param a_lu  LU Factorization of matrix A
     */
    def conditionNum (a: MatriD, a_lu: Fac_LU [MatriD]): Double =
    {
        val nrm1 = a.norm1                            // norm of matrix a
        val nrm2 = a_lu.inverse.norm1                 // norm of a^-1
        nrm1 * nrm2
    } // conditionNum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the condition number of matrix 'a', which equals
     *  <p>
     *      ||a|| ||b||  where b = a.inverse
     *  <p>
     *  Requires 'a' to be a square matrix.
     *  For rectangular matrices, @see `SVDecomp`.
     *  @param a     the matrix whose condition number is sought
     *  @param a_lu  LU Factorization of matrix A
     */
    def conditionNum2 (a: MatriD, a_lu: Fac_LU [MatriD]): Double =
    {
        val nrm1 = a.norm1                            // norm of matrix a
        val nrm2 = norm1est (a, a_lu)                 // estimate of norm of a^-1
        nrm1 * nrm2
    } // conditionNum2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the LU Factorization of matrix 'a' into 'l' and 'u' and its usage
     *  in solving a system of linear equations.
     *  @param a  the matrix A to be factored
     *  @param b  the constant vector in Ax = b
     */
    def test (a: MatriD, b: VectoD)
    {
        println (s"a = $a")
        println (s"b = $b")
        val n = a.dim2

        banner ("Factor A into L and U using LU Factorization")

        val lu = new Fac_LU (a)
        lu.factor ()                                       // factor matrix A into L and U
        val (l, u) = lu.factors
        println (s"(l, u) = ($l, $u)")
        println (s"rank = ${lu.rank}")                     // rank of matrix A

        if (a.isSquare) {
            println (s"cond = ${conditionNum (a, lu)}")    // condition number of matrix A
            println (s"con2 = ${conditionNum2 (a, lu)}")   // condition number of matrix A

            banner ("Solve for x in Ax = b using LUx = Pb")

            val x = lu.solve (b)                           // solve for x
            val ax = a * x                                 // compute Ax
            println (s"x   = $x")
            println (s"a*x = $ax")
            println (s"b   = $b")
            println (s"a*x - b = ${ax - b}")
            assert (ax == b)                               // ensure Ax = Pb

            banner ("Verify that A * A^-1 = I")

            val ai  = lu.inverse                           // inverse of A
            val aai = a * ai                               // multiply A and A^-1
            println (s"ai = $ai")
            println (s"ai - a.inverse = ${ai - a.inverse}")
            println (s"aai = $aai")
            assert (aai == MatrixD.eye (n))                // ensure A * A^-1 = I
        } // if

        banner ("Verfify that A = QLU")

        val qlu = l * u                                    // multiply L and U
        lu.permute (qlu)                                   // permute l * u -> QLU
        println (s"q*l*u = $qlu")
        println (s"a - q*l*u = ${a - qlu}")
        assert (a == qlu)                                  // ensure A = QLU
    } // test

} // Fac_LU object

import Fac_LU.test


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LUTest` object is used to test the `Fac_LU` class.
 *  Test a Square Matrix.
 *  > run-main scalation.linalgebra.Fac_LUTest
 */
object Fac_LUTest extends App
{
    val a = new MatrixD ((4, 4), 16.0,  0.4,  0.8, -0.2,
                                  1.0,  1.0,  2.0, -0.12,          // (1, 1) 3.0 -> 1.0 for pivoting
                                  0.28, 1.0, 24.8, -0.12,
                                  0.28, 1.4,  4.35, 1.0)

    val b = VectorD (-0.2, -0.32, 13.52, 14.17)

    test (a, b)

} // Fac_LUTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LUTest2` object is used to test the `Fac_LU` class.
 *  Test a Rectangular Matrix.
 *  > run-main scalation.linalgebra.Fac_LUTest2
 */
object Fac_LUTest2 extends App
{
    val a = new MatrixD ((5, 4), 16.0,  0.4,  0.8,  -0.2,
                                  1.0,  3.0,  2.0,  -0.12,
                                  0.28, 1.0, 24.8,  -0.12,
                                  9.2,  1.4,  1.0, -10.2,
                                  0.28, 1.4,  4.35,  1.0)

    val b = VectorD (-0.2, -0.32, 13.52, 14.17, 35)

    test (a, b)

} // Fac_LUTest2 object

