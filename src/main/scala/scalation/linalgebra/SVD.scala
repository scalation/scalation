
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.2
 *  @date    Mon Mar  2 16:18:29 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Matrix Computation, 4th ed. 
 *  @see www2.cs.cas.cz/mweb/download/publi/Ples2006.pdf
 *  @see www.math.iit.edu/~fass/477577_Chapter_12.pdf
 *  @see Handbook of Linear Algrbra, Chapter 45
 *  @see cs.fit.edu/~dmitra/SciComp/11Spr/SVD-Presentation-Updated2.ppt
 *  @see http://www.cs.utexas.edu/users/inderjit/public_papers/HLA_SVD.pdf
 */

package scalation.linalgebra

import math.abs

import scalation.linalgebra.Givens.{givens, givensRo, givensRoT, givensColUpdate, givensRowUpdate}
import scalation.linalgebra.MatrixD.eye
import scalation.math.double_exp
import scalation.math.ExtremeD.EPSILON
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD` class is used to compute the Singlar Value Decomposition (SVD) of
 *  matrix 'aa' using the Golub-Kahan-Reinsch Algorithm.
 *  Decompose matrix 'aa' into the product of three matrices:
 *  <p>
 *      aa = uu * a * vv.t
 *  <p>
 *  where 'uu' is a matrix of orthogonal eigenvectors of 'aa * aa.t'
 *        (LEFT SINGULAR VECTORS)
 *        'vv' is a matrix of orthogonal eigenvectors of 'aa.t * aa'
 *        (RIGHT SINGULAR VECTORS) and
 *        'a' is a diagonal matrix of square roots of eigenvalues of 'aa.t * aa' &' aa * aa.t'
 *        (SINGULAR VALUES).
 *  @param aa  the m-by-n matrix to deflate/decompose (algorithm requires m >= n)
 */
class SVD (aa: MatrixD)
      extends SVDecomp with Error
{
    private val DEBUG      = false                // debug flag
    private val m          = aa.dim1              // number of rows
    private val n          = aa.dim2              // number of columns

    if (n > m) flaw ("constructor", "SVD requires m >= n")

    private var a           = new MatrixD (aa)    // work on modifiable copy of aa (will hold singular values)
    private var uu: MatrixD = null                // left orthogonal matrix  uu = u_1 * ... u_k
    private var s:  VectorD = null                // vector of singular values (main diagonal of a after deflation)
    private var vv: MatrixD = null                // right orthogonal matrix vv = v_1 * ... v_k

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'aa' into the product of a matrix of left singular vectors 'uu',
     *  a vector of singular values 's' and a matrix of right singular vectors 'vv'
     *  such that 'aa = uu ** s * vv.t'.
     */
    def factor (): Tuple3 [MatrixD, VectorD, MatrixD] =
    {
        if (! a.isBidiagonal) {
            val bid = new Bidiagonal (a)
            val (u, b, v) = bid.bidiagonalize ()  // turn a into a bidiagonal matrix
            uu = u; a = b; vv = v
        } else {
            uu = eye (m); vv = eye (n)
        } // if
        if (DEBUG) println ("factor: bidiagonal a = " + a)
        deflate ()                                // deflate the superdiagonal
        s = a.getDiag ()                          // get the singular values from matrix a
        reorder ()                                // reorder so largest singular values come first
        (uu, s, vv)
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate matrix 'aa' and decompose it using a Singular Value Decomposition
     *  (SVD) algorithm.
     *  Deflate matrix 'aa' forming a diagonal matrix consisting of singular
     *  values and return the singular values in vector 's'.  Also return the
     *  singular vector matrices 'uu' and 'vv'.
     *  @see Matrix Computation: Algorithm 8.6.2 SVD Algorithm.
     */
    private def deflate () 
    {
        var p  = 0                          // # zero elements in left end of superdiagonal
        var q  = 0                          // # zero elements in right end of superdiagonal
 
        while (true) {
            for (i <- 0 until n-1) {
                if (abs (a(i, i+1)) < EPSILON * (abs (a(i, i)) + abs (a(i+1, i+1)))) a(i, i+1) = 0.0
            } // for

            val (p, q) = findMiddle ()
            if (q >= n-1) return            // return since no non-zero elements remain in superdiagonal
            val k = findZero (p, n-q)

            if (k >= 0) {
                if (DEBUG) println ("deflate: found zero on diagonal at " + k)
                // use Givens rotation to make superdiagonal element a(k, k+1) = 0.0
                val cs = givens (a(k-1, k+1), a(k, k+1))
                val u  = givensRoT (k-1, k, n, cs)                    // left orthogonal matrix u_k^t
                a = u * a                                             // zero element with Givens rotations
            } else {
                diagonStep (p, q)
            } // if

            if (DEBUG) println ("deflate: a = " + a)
        } // while
    } // deflate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take one step in converting the bidiagonal matrix 'a' to a diagonal matrix.
     *  That is, reduce the middle run of nonzero super-diagonal elements by one.
     *  @see Matrix Computation: Algorithm 8.6.1 Golub-Kahan Step.
     *  @param p  the size of the head of the super-diagonal
     *  @param q  the size of the tail of the super-diagonal
     */
    private def diagonStep (p: Int, q: Int)
    {
        import SVD.trailing
        import Eigen_2by2.eigenvalues

        val tt = trailing (a(p until n-q, p until n-q))                  // trailing 2-by-2 submatrix of a.t * a
        val l  = eigenvalues (tt)                                        // the eigenvalues of the submatrix
        if (DEBUG) println ("diagonStep: tt = " + tt + "\ndiagonStep: l = " + l)

        val td = tt(1, 1)                                                // last diagonal element in a.t * a
        val mu = if (abs (td - l(0)) <= abs (td - l(1))) l(0) else l(1)  // pick closest eigenvalue
        var y  = a(p, p) * a(p, p) - mu
        var z  = a(p, p) * a(p, p+1)
        if (DEBUG) println ("diagonStep: (mu, y, z) = " + (mu, y, z))

        for (k <- p until n-1-q) {

            // Givens rotation 1: k, k+1, theta1 (c1, s1); zero right
            val cs1 = givens (y, z)                                // compute rotation cosine and sine
            givensColUpdate (a, k, k+1, cs1)                       // rotate to clear an element in a
            val v = givensRo (k, k+1, n, cs1)                      // right orthogonal matrix v_k
            vv = vv * v                                            // update vv 

            if (DEBUG) {
                println ("diagonStep (" + k + "): rotation 1: (c1, s1) = " + cs1)
                println ("diagonStep (" + k + "): rotation 1: v = " + v)
                println ("diagonStep (" + k + "): rotation 1: a = " + a)
            } // if

            y = a(k, k); z = a(k+1, k)
            if (DEBUG) println ("diagonStep: (y, z) = " + (y, z))

            // Givens rotation 2: k, k+1, theta2 (c2, s2); zero down
            val cs2 = givens (y, z)                                // compute rotation cosine and sine
            givensRowUpdate (a, k, k+1, cs2)                       // rotate to clear an element in a
            val u = givensRo (k, k+1, m, cs2)                      // left orthogonal matrix u_k^t
            uu = uu * u                                            // update uu

            if (DEBUG) {
                println ("diagonStep (" + k + "): rotation 2: (c2, s2) = " + cs2)
                println ("diagonStep (" + k + "): rotation 2: u = " + u)
                println ("diagonStep (" + k + "): rotation 2: a = " + a)
            } // if

            if (k < n-q-2) {
                y = a(k, k+1); z = a(k, k+2)
                if (DEBUG) println ("diagonStep: (y, z) = " + (y, z))
            } // if

        } // for
    } // diagonStep

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find/return the index of the first diagonal entry in 'a' from 'j' until 'k'
     *  that is zero; otherwise -1 (not found).
     *  @param j  strart the search here
     *  @param k  end the search here
     */
    private def findZero (j: Int, k: Int): Int =
    {
        for (i <- j until k if a(i, i) =~ 0.0) return i
        -1
    } // findZero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the run of nonzero elements in the middle of the super-diagonal
     *  of matrix 'a' such that the tail super-diagonal contains only zeros.
     *  Return p the size of the head and q the size of the tail.
     */
    private def findMiddle (): Tuple2 [Int, Int] =
    {
        var i = n - 1
        while (i >= 1 && a(i-1, i) =~ 0.0) i -= 1
        val q = n - 1 - i
        while (i >= 1 && ! (a(i-1, i) =~ 0.0)) i -= 1
        val p = i
        if (DEBUG) println ("findMiddle: (p, q) = " + (p, q)) 
        (p, q)
    } // findMiddle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reorder the singular values to be in non-increasing order.  Must swap
     *  singular vectors in lock step with singular values.  To minimize the
     *  number of swaps, selection sort is used.
     */
    private def reorder ()
    {
        for (i <- 0 until n) {
            val j = s(i until n).argmax ()
            if (i != j) {
                s.swap (i, j)
                uu.swapCol (i, j)
                vv.swapCol (i, j)
            } // if  
        } // for  
    } // reorder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for `x` in `a^t*a*x = b` using `SVD`.
     *  @param  b the constant vector
     */
    def solve (b: VectorD): VectorD =
    {
        val (u, d, vt) = factor ()                   // factor using SVD
        val alpha = u.t * b                          // principle component regression
        vt ** d.recip * alpha                        // estimate coefficients
    } // solve

} // SVD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD` companion object.
 */
object SVD
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trailing 2-by-2 submatrix of 'b.t * b' without multiplying
     *  the full matrices.
     *  @param b  the given bidiagonal matrix
     */
    def trailing (b: MatrixD): MatrixD =
    {
//      println ("trailing: b = " + b)
        val n3  = b.dim2 - 1
        val n2  = n3 - 1
        val n1  = n2 - 1
        val b12 = if (n1 < 0) 0.0 else b(n1, n2)
        val b22 = b(n2, n2)
        val b23 = b(n2, n3)
        val b33 = b(n3, n3)
        new MatrixD ((2, 2), b12*b12 + b22*b22,  b22*b23,
                             b22*b23,  b23*b23 + b33*b33) 
    } // trailing

} // SVD object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest` object is used to test the `SVD` class starting with a matrix that
 *  is already bidiagonalized and gives eigenvalues of 28, 18 for the first step.
 *  @see http://ocw.mit.edu/ans7870/18/18.06/javademo/SVD/
 *  > run-main scalation.linalgebra.SVDTest
 */
object SVDTest extends App
{
    val bb = new MatrixD ((2, 2), 1.00,  2.00,
                                  0.00,  2.00)

    val u_ = new MatrixD ((2, 2), 0.75, -0.66,
                                  0.66,  0.75)

    val b_ = new MatrixD ((2, 2), 2.92,  0.00,
                                  0.00,  0.68)

    val v_ = new MatrixD ((2, 2), 0.26, -0.97,
                                  0.97,  0.26)

    println ("svd: (u_, b_, v_) = " + (u_, b_, v_))  // answer from Web page
    println ("u_b_v_.t = " + u_ * b_ * v_.t)         // should equal the original bb
    
/*
    val bb = new MatrixD ((3, 3), 3.0, 5.0, 0.0,     // original bidiagonal matrix
                                  0.0, 1.0, 4.0,
                                  0.0, 0.0, 2.0)
*/
    println ("----------------------------------------")
    println ("SVDTest")
    println ("----------------------------------------")
    println ("bb = " + bb)
    println ("----------------------------------------")

    val svd = new SVD (bb)                            // Singular Value Decomposition
    val (u, s, v) = svd.factor ()                     // factor bb
    println ("svd.factor: (u, s, v) = " + (u, s, v))

    println ("u ** s * v.t = " + (u ** s * v.t))       // should equal the original bb

} // SVDTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest2` is used to test the `SVD` class.
 *  Answer: singular values = (3.82983, 1.91368, 0.81866)
 */
object SVDTest2 extends App
{
    import MatrixD.eye

    val bb = new MatrixD ((3, 3), 1.0, 1.0, 0.0,
                                  0.0, 2.0, 2.0,
                                  0.0, 0.0, 3.0)

    println ("----------------------------------------")
    println ("SVDTest2")
    println ("----------------------------------------")
    println ("bb = " + bb)
    println ("----------------------------------------")

    val svd = new SVD (bb)                            // Singular Value Decomposition
    val (u, s, v) = svd.factor ()                     // factor bb
    println ("svd.factor: (u, s, v) = " + (u, s, v))

    println ("u ** s * v.t = " + (u ** s * v.t))      // should equal the original bb

} // SVDTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest3` object is used to test the `SVD` class starting with a general
 *  matrix.
 *  @see www.mathstat.uottawa.ca/~phofstra/MAT2342/SVDproblems.pdf
 */
object SVDTest3 extends App
{
/*
    val a = new MatrixD ((3, 2), 1.0, 2.0,           // original matrix
                                 2.0, 2.0,
                                 2.0, 1.0)
*/
    val a = new MatrixD ((4, 4), 0.9501, 0.8913, 0.8214, 0.9218,
                                 0.2311, 0.7621, 0.4447, 0.7382,
                                 0.6068, 0.4565, 0.6154, 0.1763,
                                 0.4860, 0.0185, 0.7919, 0.4057)
/*
    val a = new MatrixD ((4, 3), 1.0,  2.0,  3.0,
                                 4.0,  5.0,  6.0,
                                 7.0,  8.0,  9.0,
                                10.0, 11.0, 12.0)
*/
    println ("a = " + a)

    val bid = new Bidiagonal (a)                         // Householder Bidiagonalization
    val (uu, bb, vv) = bid.bidiagonalize ()              // bidiagonalize a
    println ("bid.bidiagonalize: (uu, bb, vv) = " + (uu, bb, vv))

    val svd = new SVD (bb)                               // Singular Value Decomposition
    val (u, s, v) = svd.factor ()                        // factor bb
    println ("svd.factor: (u, s, v) = " + (u, s, v))
    println ("u ** s * v.t = " + (u ** s * v.t))         // should equal the original a

} // SVDTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest4` object is used to test the `SVD` companion object.
 */
object SVDTest4 extends App
{
    import SVD.trailing

    val b = new MatrixD ((4, 4), 1.0, 5.0, 0.0, 0.0,    // the bidiagonal matrix
                                 0.0, 2.0, 6.0, 0.0,
                                 0.0, 0.0, 3.0, 7.0,
                                 0.0, 0.0, 0.0, 4.0)
    val n = b.dim2
    println ("b = " + b)
    println ("trailing b.t * b = " + trailing (b))
    println ("check: " + (b.t * b)(n-2 to n, n-2 to n))

} // SVDTest4 object

