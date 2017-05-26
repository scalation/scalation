
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.3
 *  @date    Mon Mar  2 16:18:29 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Matrix Computation, 4th ed. 
 *  @see www2.cs.cas.cz/mweb/download/publi/Ples2006.pdf
 *  @see www.math.iit.edu/~fass/477577_Chapter_12.pdf
 *  @see Handbook of Linear Algrbra, Chapter 45
 *  @see cs.fit.edu/~dmitra/SciComp/11Spr/SVD-Presentation-Updated2.ppt
 *  @see www.cs.utexas.edu/users/inderjit/public_papers/HLA_SVD.pdf
 *  @see people.duke.edu/~hpgavin/SystemID/References/Golub+Reinsch-NM-1970.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.linalgebra

import scala.math.abs

import scalation.linalgebra.Givens.{givens, givensRo, givensRoT, givensColUpdate, givensRowUpdate}
import scalation.linalgebra.MatrixD.eye
import scalation.math.double_exp
import scalation.math.ExtremeD.EPSILON
import scalation.util.{banner, Error, sline}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4` class is used to compute the Singular Value Decomposition 'SVD' of
 *  matrix 'aa' using the Golub-Kahan-Reinsch Algorithm.
 *  Factor/decompose matrix 'aa' into the product of three matrices:
 *  <p>
 *      aa = uu * a * vv.t
 *  <p>
 *  where 'uu' is a matrix of orthogonal eigenvectors of 'aa * aa.t'
 *        (LEFT SINGULAR VECTORS)
 *        'vv' is a matrix of orthogonal eigenvectors of 'aa.t * aa'
 *        (RIGHT SINGULAR VECTORS) and
 *        'a' is a diagonal matrix of square roots of eigenvalues of 'aa.t * aa' & 'aa * aa.t'
 *        (SINGULAR VALUES).
 *  FIX: need to reorder so singular values are in decreasing order.
 *  FIX: make the singular values positive
 *------------------------------------------------------------------------------
 *  @param aa  the m-by-n matrix to deflate/decompose (algorithm requires m >= n)
 */
class SVD4 (aa: MatrixD)
      extends SVDecomp with Error
{
    private val DEBUG = false                     // debug flag
    private val m     = aa.dim1                   // number of rows
    private val n     = aa.dim2                   // number of columns

    if (n > m) flaw ("constructor", "SVD4 implementation requires m >= n")

    private var a           = aa.copy             // work on modifiable copy of aa (will hold singular values)
    private var uu: MatrixD = null                // left orthogonal matrix  uu = u_1 * ... u_k
    private var s:  VectorD = null                // vector of singular values (main diagonal of a after deflation)
    private var vv: MatrixD = null                // right orthogonal matrix vv = v_1 * ... v_k

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of a matrix of left singular vectors 'uu',
     *  a vector of singular values 's' and a matrix of right singular vectors 'vv'
     *  such that 'a = uu ** s * vv.t'.
     */
    override def factor123 (): FactorType =
    {
        if (! a.isBidiagonal) {
            val bid = new Bidiagonal2 (a)
            val (u, b, v) = bid.bidiagonalize ()  // turn a into a bidiagonal matrix
            uu = u; a = b; vv = v
        } else {
            // uu = eye (m); vv = eye (n)
            uu = eye (m, n); vv = eye (n)
        } // if
        if (DEBUG) println ("factor: bidiagonal a = " + a)
        deflate ()                                // deflate the superdiagonal
        s = a.getDiag ()                          // get the singular values from matrix a
        reorder ()                                // reorder so largest singular values come first
        (uu, s, vv)
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate matrix 'a' forming a diagonal matrix consisting of singular
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
        import SVD4.trailing
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
     *  @param j  start the search here
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
    def solve (b: VectorD): VectoD =
    {
        val (u, d, vt) = factor123 ()                // factor using SVD4
        val alpha = u.t * b                          // principle component regression
        vt ** d.recip * alpha                        // estimate coefficients
    } // solve

} // SVD4 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4` companion object.
 */
object SVD4
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the SVD4 Factorization algorithm on matrix 'a' by factoring the matrix
     *  into a left matrix u, a vector s, and a right matrix v.  Then multiply back
     *  to recover the original matrix.
     *  @param a     the given matrix to factor
     *  @param name  the name of the test case
     */
    def test (a: MatrixD, name: String)
    {
        banner (name)
        println ("original matrix a = " + a)

        val svd       = new SVD4 (a)                             // Singular Value Decomposition object
        val (u, s, v) = svd.factor123 ()                         // factor matrix a
        println (sline () + "svd.factor: (u, s, v) = " + (u, s, v))
        val prod = u ** s * v.t                                  // compute the product
        println (sline () + "check: u ** s * v.t = " + prod)     // should equal the original a matrix
        println ("a - prod = " + (a - prod))
        assert (prod == a)
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the SVD4 Factorization algorithm on a bidiagonalization of matrix 'a', factoring
     *  it into a left matrix 'uu', bidiagonal matrix 'bb', and right matrix 'vv'.
     *  Then multiply back to recover the original matrix.
     *  @param a     the given matrix to bidiagonalize and then factor
     *  @param name  the name of the test case
     */
    def testBid (aa: MatrixD, name: String)
    {
        val a   = aa.copy                                        // make a copy of aa
        val bid = new Bidiagonal2 (a)                            // Householder Bidiagonalization
        val (uu, bb, vv) = bid.bidiagonalize ()                  // bidiagonalize a
        println (sline () + "bid.bidiagonalize: (uu, bb, vv) = " + (uu, bb, vv))
        test (bb, name)
    } // testBid

} // SVD4 object

import SVD4.{test, testBid}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test` object is used to test the `SVD4` class starting with a matrix that
 *  is already in bidiagonal form and gives eigenvalues of 28, 18 for the first step.
 *  @see ocw.mit.edu/ans7870/18/18.06/javademo/SVD/
 *  > run-main scalation.linalgebra.SVD4Test
 */
object SVD4Test extends App
{
    val a  = new MatrixD ((2, 2), 1.00,  2.00,                // original matrix
                                  0.00,  2.00)                // 2 by 2, bidiagonal

    val u_ = new MatrixD ((2, 2), 0.75, -0.66,
                                  0.66,  0.75)

    val b_ = new MatrixD ((2, 2), 2.92,  0.00,
                                  0.00,  0.68)

    val v_ = new MatrixD ((2, 2), 0.26, -0.97,
                                  0.97,  0.26)

    println ("svd: (u_, b_, v_) = " + (u_, b_, v_))           // answer from Web page
    println ("u_b_v_.t = " + u_ * b_ * v_.t)                  // should equal the original a
    
    test (a, "SVD4Test")

} // SVD4Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test2` is used to test the `SVD4` class.
 *  Answer: singular values = (3.82983, 1.91368, 0.81866)
 *  > run-main scalation.linalgebra.SVD4Test2
 */
object SVD4Test2 extends App
{
    val bb = new MatrixD ((3, 3), 1.0, 1.0, 0.0,              // original matrix
                                  0.0, 2.0, 2.0,              // 3 by 3, bidiagonal
                                  0.0, 0.0, 3.0)

    test (bb, "SVD4Test2")

} // SVD4Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test3` object is used to test the `SVD4` class starting with general
 *  (i.e., not bidiagonalized) matrices.  All probelems for which m >= n from
 *  the following Webspage are tried.
 *  @see www.mathstat.uottawa.ca/~phofstra/MAT2342/SVDproblems.pdf
 *  @see mysite.science.uottawa.ca/phofstra/MAT2342/SVDproblems.pdf
 *  > run-main scalation.linalgebra.SVD4Test3
 */
object SVD4Test3 extends App
{
    import scala.math.sqrt

    val a2 = new MatrixD ((2, 2), 1.0, 2.0,                   // original matrix, problem 2
                                  2.0, 1.0)                   // 2 by 2

    testBid (a2, "SVD4Test3_2b")                               // test the bidiagonalized matrix
    test (a2, "SVD4Test3_2")                                   // test the original matrix

    val a3 = new MatrixD ((3, 3), 0.0,     1.0, 1.0,          // original matrix, problem 3
                                  sqrt(2), 2.0, 0.0,          // 3 by 3
                                  0.0,     1.0, 1.0)

//  testBid (a3, "SVD4Test3_3b")                               // test the bidiagonalized matrix - FIX - fails
    test (a3, "SVD4Test3_3")                                   // test the original matrix

} // SVD4Test3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test4` object is used to test the `SVD4` class starting with a general
 *  matrix.
 *  > run-main scalation.linalgebra.SVD4Test4
 */
object SVD4Test4 extends App
{
    val a = new MatrixD ((4, 4), 0.9501, 0.8913, 0.8214, 0.9218,     // original matrix
                                 0.2311, 0.7621, 0.4447, 0.7382,     // 4 by 4
                                 0.6068, 0.4565, 0.6154, 0.1763,
                                 0.4860, 0.0185, 0.7919, 0.4057)

//  testBid (a, "SVD4Test34")                                  // test the bidiagonalized matrix
    test (a, "SVD4Test4")                                      // test the original matrix

} // SVD4Test4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test5` is used to test the `SVD4` class on a problem where the matrix
 *  is not a square matrix.
 *  > run-main scalation.linalgebra.SVD4Test5
 */
object SVD4Test5 extends App
{
    val a = new MatrixD ((3, 2), 4, 5,    // original matrix
                                 6, 7,    // 3 by 2
                                 9, 8)


//  testBid (a, "SVD4Test5b")                                  // test the bidiagonalized matrix
    test (a, "SVD4Test5")                                      // test the original matrix

} // SVD4Test5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test5` is used to test the `SVD4` class on a larger test problem.
 *  @see www.maths.manchester.ac.uk/~peterf/MATH48062/math48062%20Calculating%20and%20using%20the%20svd%20of%20a%20matrix.pdf
 *  FIX: this example does not work, in the sense that is does not converge to 'TOL'.
 *  > run-main scalation.linalgebra.SVD4Test6
 */
object SVD4Test6 extends App
{
    val a = new MatrixD ((5, 3), 0.44444444,  0.3333333, -1.3333333,    // original matrix
                                 0.41111111, -0.3166667, -0.3333333,    // 5 by 3
                                -0.18888889,  0.4833333, -0.3333333,
                                -0.03333333, -0.6500000,  1.0000000,
                                -0.63333333,  0.1500000,  1.0000000)

//  testBid (a, "SVD4Test6b")                                  // test the bidiagonalized matrix
    test (a, "SVD4Test6")                                      // test the original matrix

} // SVD4Test6 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD4Test6` object is used to test the `SVD4` companion object's computation
 *  of trailing submatrices.
 *  > run-main scalation.linalgebra.SVD4Test7
 */
object SVD4Test7 extends App
{
    import SVD4.trailing

    val b = new MatrixD ((4, 4), 1.0, 5.0, 0.0, 0.0,          // bidiagonal matrix
                                 0.0, 2.0, 6.0, 0.0,          // 4 by 4
                                 0.0, 0.0, 3.0, 7.0,
                                 0.0, 0.0, 0.0, 4.0)
    val n = b.dim2
    println ("b = " + b)
    println ("trailing b.t * b = " + trailing (b))
    println ("check: " + (b.t * b)(n-2 to n, n-2 to n))

} // SVD4Test7 object

