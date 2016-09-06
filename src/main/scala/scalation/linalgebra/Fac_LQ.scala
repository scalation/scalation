
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Jul 30 22:53:47 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LQ` class provides methods to factor an 'm-by-n' matrix 'aa' into the
 *  product of two matrices, when m < n.
 *  <p>
 *      'l' - an 'm-by-m' left lower triangular matrix
 *      'q' - an 'm-by-n' orthogonal matrix and
 *  <p>
 *  such that 'aa = l * q'.
 *  Note, orthogonal means that 'q.t * q = I'.
 *  @param aa  the matrix to be factor into l and q
 */
class Fac_LQ (aa: MatriD)
      extends Factorization with Error
{
    private val art = aa.t                        // transpose of aa
    private var l: MatriD = null                  // the left lower triangular l matrix
    private var q: MatriD = null                  // the orthogonal q matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'art' into the product of two matrices, 'art = qt * rt'.
     *  Then compute 'r' and 'q'.
     *  @see http://math.stackexchange.com/questions/1640695/rq-decomposition
     */
    def factor ()
    {
        val (qt, lt) = new Fac_QR_H (art).factor12 ()     // change `Fac-QR_H` class as needed
        l = lt.t
        q = qt.t
    } // factor

    def factors: (MatriD, MatriD) = (l, q)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in 'aa*x = b' using the 'QR' Factorization 'aa = l*q' via
     *  'x = q.t * l.inv * b'.
     *  FIX: need method that does not require calling 'inverse'
     *  @param  b the constant vector
     */
    def solve (b: VectoD): VectoD = q.t * l.inverse * b

} // Fac_QR_LQ class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_LQTest` object is used to test the `Fac_LQ` class.
 *  > run-main scalation.linalgebra.Fac_LQTest
 */
object Fac_LQTest extends App
{
    def test (a: MatrixD, b: VectorD)
    {
        val lq = new Fac_LQ (a)                // for factoring a into q * r
        val (l, q) = lq.factor12 ()            // (l left lower triangular, q orthogonal)
   
        println ("--------------------------------------------------------")
        println ("a   = " + a)                 // original matrix
        println ("l   = " + l)                 // left matrix
        println ("q   = " + q)                 // orthogonal matrix
        println ("l*q = " + l*q)               // check that l*r = a
        val x = lq.solve (b)
        println ("x   = " + x)                 // solve for x in a*x = b
        println ("b   = " + b)                 // rhs vector
        println ("a*x = " + a*x)               // check that a*x = b
    } // test

    val a1 = new MatrixD ((2, 4), 1.0, 2.0, 3.0, 4.0,
                                  5.0, 6.0, 7.0, 8.0)

    val b1 = VectorD (10, 12)
    test (a1, b1)

} // Fac_LQTest object

