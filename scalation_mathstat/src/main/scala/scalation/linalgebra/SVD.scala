
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Khalid Jahangeer, John Miller
 *  @version 1.3
 *  @date    Sun Mar 19 15:16:20 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Matrix Computations:  Algorithm 8.6.1 Golub-Kahan SVD Step Algorithm
 *  @see Matrix Computations:  Algorithm 8.6.2 SVD Algorithm
 *
 *  This version translated from the following Algol code:
 *  @see people.duke.edu/~hpgavin/SystemID/References/Golub+Reinsch-NM-1970.pdf
 */

package scalation.linalgebra

import scala.math.{abs, hypot}

import scalation.math.ExtremeD.EPSILON
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD` class is used to compute the Singular Value Decomposition 'SVD' of
 *  matrix 'a' using the Golub-Kahan-Reinsch Algorithm.
 *  Factor/decompose matrix 'a' into the product of three matrices:
 *  <p>
 *      a = u * q * v.t
 *  where
 *     u is an m-by-n matrix of
 *         orthogonal eigenvectors of 'a * a.t' (LEFT SINGULAR VECTORS)
 *     q is an n-by-n diagonal matrix of square roots of
 *         eigenvalues of 'a.t * a' & 'a * a.t' (SINGULAR VALUES)
 *     v is an n-by-n matrix of
 *         orthogonal eigenvectors of 'a.t * a' (RIGHT SINGULAR VECTORS)
 *  <p>
 * The SVD algorithm implemented is based on plane rotations.  It involves transforming
 * the input matrix to its Bidiagonal form using the Householder transformation procedure.
 * The Bidiagonal matrix is then used to obtain singular values using the QR algorithm.
 *------------------------------------------------------------------------------
 *  @param a  the m-by-n matrix to factor/decompose (requires m >= n)
 */
class SVD [MatT <: MatriD] (a: MatT)
    extends SVDecomp with Error
{
    private val DEBUG          = false               // debug flag
    private val MAX_ITER       = 100                 // maximum number of iterations
    private val m              = a.dim1              // number of rows
    private val n              = a.dim2              // number of columns

    if (n > m) flaw ("constructor", "SVD implementation requires m >= n")

    private var l              = 0                   // lower index vs. k for zeroing out super-diagonal elements
    private var f, g           = 0.0                 // typcally [ f g ]
    private var h              = 0.0                 //          [ 0 h ]
    private var bmx            = 0.0                 // maximum column magnitude in the bidiagonal matrix
    private var test_fconverge = true                // whether singular values have reached converging magnitude
    private var eps            = EPSILON             // adjustable small value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' into the product of a matrix of left singular vectors 'u',
     *  a vector of singular values 'q' and a matrix of right singular vectors 'v'
     *  such that 'a = u ** q * v.t'.
     */
    override def factor123 (): FactorType =
    {
        val bid       = new Bidiagonal (a)                // class for making bidiagonal matrices 
        val (u, b, v) = bid.bidiagonalize ()              // factor a into a bidiagonal matrix b 
        val (e, q)    = bid.e_q                           // get b's super-diagonal e and main diagonal q
        bmx           = bid.bmax                          // largest column magnitude in b
        eps          *= bmx                               // adjust eps based on bmx
        var c, s      = 0.0                               // cosine, sine for rotations
        var y         = 0.0                               // holds values from q and u
        var z         = 0.0                               // holds values from q, v and normalization of (f, h)

        for (k <- n-1 to 0 by -1) iterate (k)             // diagonalization of the bidiagonal form

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Iterate until the super-diagonal element 'e(k)' is (near) zero.
         *  @param k  the upper index (decremented by the loop above)
         */
        def iterate (k : Int)
        {
            for (iter <- 0 until MAX_ITER) {
                testFSplitting (k, e, q)
                if (DEBUG) println (s"iterate: l = $l \t test_fconverge = $test_fconverge")

                if (! test_fconverge) cancellation (l, k)

                if (testFConvergence (l, k)) {
                    println (s"iterate: for k = $k, converged after ${iter+1} iterations")
                    if (DEBUG) println (s"iterate: e = $e \nq = $q")
                    return
                } // if

                shiftFromBottom (k, e, q)
                qrTransform (l, k)
            } // for
        } // iterate

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Test whether the lower index 'l' has caught the upper index 'k'.
         *  @param l  the lower index
         *  @param k  the upper index
         */
        def testFConvergence (l : Int, k : Int): Boolean =
        {
            if (DEBUG) println (s"testFConvergence ($l, $k)")
            z = q(k)
            if (l == k) {                                 // convergence indicated by l equaling k
                if (z < 0.0) {                            // make sure singular value is non-negative
                    q(k) = -z
                    for (j <- 0 until n) v(j, k) = -v(j, k)
                } //if
                true
            } // if
            else false
        } // testFConvergence

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Cancellation (set to zero) of super-diagonal element e(l) if l > 0.
         *  Requires test_fconverge to be false
         *  @param l  the lower index
         *  @param k  the upper index
         */
        def cancellation (l : Int, k : Int)
        {
            c = 0.0; s = 1.0                              // set cosine, sine
            for (j <- l to k) {                           // each column l to k
                f     = s * e(j)                          // sine * e(j)
                e(j) *= c                                 // cosine * e(j)

                if (abs (f) <= eps) return                // f near zero => return & test f convergence

                g = q(j); h = hypot (f, g)                // hypotenuse/norm
                q(j) = h
                c = g / h; s = -f / h                     // reset cosine, sine
                rotateU (l-1, j)                          // rotation for columns l-1 and j of u
            } // for
        } // cancellation

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Perform next QR transformation.
         *  @param l  the lower index
         *  @param k  the upper index
         */
        def qrTransform (l : Int, k : Int)
        {
            c = 1.0; s = 1.0                              // set cosine, sine
            for (j <- l+1 to k) {                         // each column l+1 to k
                g = e(j); h = s * g; g *= c               // compute g, h
                y = q(j); z = hypot (f, h)                // hypotenuse/norm
                e(j-1) = z                                // update e

                c = f / z; s = h / z                      // reset cosine, sine
                f =  bmx * c + g * s                      // compute f
                g = -bmx * s + g * c; h = y * s           // compute g, h
                y *= c
                rotateV (j-1, j)                          // update v

                z = hypot (f, h)                          // hypotenuse/norm
                q(j-1) = z                                // update q

                c   = f / z; s = h / z                    // reset cosine, sine
                f   =  c * g + s * y                      // compute f
                bmx = -s * g + c * y
                rotateU (j-1, j)                          // update u
            } // for
            e(l) = 0.0
            e(k) = f
            q(k) = bmx
        } // qrTransform

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Perform rotation for columns 'j1' and 'j2' in the 'u' matrix.
         *  @param j1  the first column involved 
         *  @param j2  the second column involved 
         */
        def rotateU (j1: Int, j2: Int)
        {
            for (i <- 0 until m) {             // each row of u
                y = u(i, j1)                   // changes to y and z affect outer scope
                z = u(i, j2)
                u(i, j1) =  y * c + z * s
                u(i, j2) = -y * s + z * c
            } // for
        } // rotateU

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Perform rotation for columns 'j1' and 'j2' in the 'v' matrix.
         *  @param j1  the first column involved 
         *  @param j2  the second column involved 
         */
        def rotateV (j1: Int, j2: Int)
        {
            for (i <- 0 until n) {             // each row of v
                bmx = v(i, j1)                 // changes to y and z affect outer scope
                z   = v(i, j2)
                v(i, j1) =  bmx * c + z * s
                v(i, j2) = -bmx * s + z * c
            } // for
        } // rotateV

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         /*  Shift from bottom 2x2 minor.
          *  @param k  the upper index
          *  @param e  the super-diagonal
          *  @param q  the main diagonal
          */
        def shiftFromBottom (k : Int, e: VectoD, q: VectoD)
        {
            bmx = q(l)
            y   = q(k-1)
            g   = e(k-1); h = e(k)
            f   = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y)
            g   = hypot (f, 1.0)
            f   = if (f < 0) ((bmx - z) * (bmx + z) + h * (y / (f - g) - h)) / bmx
            else             ((bmx - z) * (bmx + z) + h * (y / (f + g) - h)) / bmx

            if (DEBUG) println (s"shiftFromBottom: f = $f \t g = $g")
        } // shiftFromBottom

        // final part for factor method
        flip (u, q)                            // convert singluar values to positive, if any, adjusting u accordingly
        reorder ((u, q, v))                    // reorder so largest singular values come first
        (u, q, v)                              // return left matrix, singular vector and right matrix
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test super-diagonal element 'e(l)' and main diagonal element 'q(l-1)'
     *  to set the lower index 'l'.
     *  @param k  the upper index
     *  @param e  the super-diagonal
     *  @param q  the main diagonal
     */
    def testFSplitting (k : Int, e: VectoD, q: VectoD)
    {
        for (ll <- k to 0 by -1) {
            l = ll                               // make global index l track loop variable ll
            test_fconverge = false
            if (abs (e(ll))   <= eps) { println (s"e(ll) = ${e(ll)}"); test_fconverge = true; return }
            if (abs (q(ll-1)) <= eps) return
        } // for
    } // testFSplitting

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'a^t*a*x = b' using `SVD`.
     *  @param b  the constant vector
     */
    override def solve (b: VectoD): VectoD =
    {
        val (u, d, vt) = factor123 ()                // factor using SVD
        val alpha = u.t * b                          // principle component regression
        vt ** d.recip * alpha                        // estimate coefficients
    } // solve

} // SVD class

import SVDecomp._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest` is used to test the `SVD` class.
 *  > run-main scalation.linalgebra.SVDTest
 */
object SVDTest extends App
{
    test (a1, new SVD (a1), "SVDTest")

} // SVDTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest2` object is used to test the `SVD` class.
 *  @see www.ling.ohio-state.edu/~kbaker/pubs/Singular_Value_Decomposition_Tutorial.pdf
 *  > run-main scalation.linalgebra.SVDTest2
 */
object SVDTest2 extends App
{
    test (a2, new SVD (a2), "SVDTest2")

} // SVDTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest3` object is used to test the `SVD` class.
 *  > run-main scalation.linalgebra.SVDTest3
 */
object SVDTest3 extends App
{
    test (a3, new SVD (a3), "SVDTest3")

} // SVDTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest4` object is used to test the `SVD` class.
 *  > run-main scalation.linalgebra.SVDTest4
 */
object SVDTest4 extends App
{
    test (a4, new SVD (a4), "SVDTest4")

} // SVDTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest5` object is used to test the `SVD` class.
 *  > run-main scalation.linalgebra.SVDTest5
 */
object SVDTest5 extends App
{
    test (a5, new SVD (a5), "SVDTest5")

} // SVDTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest6` object is used to test the `SVD` class.
 *  > run-main scalation.linalgebra.SVDTest6
 */
object SVDTest6 extends App
{
    test (a6, new SVD (a6), "SVDTest6")

} // SVDTest6 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest7` object is used to test the `SVD` class.
 *  This test case currently fails as ScalaTion does not consider 9.18964e-09
 *  to be approximately zero.
 *  > run-main scalation.linalgebra.SVDTest7
*/
object SVDTest7 extends App
{
    test (a7, new SVD (a7), "SVDTest7")

} // SVDTest7 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDTest8` object is used to test the `SVD` class.
 *  > run-main scalation.linalgebra.SVDTest7
 */
object SVDTest8 extends App
{
    test (a8, new SVD (a8), "SVDTest8")

} // SVDTest8 object

