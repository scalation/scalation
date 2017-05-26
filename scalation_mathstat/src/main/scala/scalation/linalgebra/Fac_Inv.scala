
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Peng Hao, John Miller
 *  @version 1.3
 *  @date    Fri May 26 14:32:21 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.math.sqrt

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_Inv` class provides methods to factor an 'n-by-n' identity matrix 'I'
 *  into the product of two matrices 'a' and 'a^-1'
 *  <p>
 *      a * a^-1 = I
 *  <p>
 *  where 'a' is the given matrix and 'a^-1' is its inverse.
 *  @param a  the given n-by-n square matrix
 */
class Fac_Inv [MatT <: MatriD] (a: MatT)
      extends Factorization with Error
{
    private val n = a.dim1                     // the a matrix is n-by-n
    private var ai: MatriD = null              // the inverse of a

    if (! a.isSquare) flaw ("constructor", "matrix a must be square")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'I' into the product of 'a' and 'a^-1' by computing the
     *  inverse of 'a'.
     */
    def factor () = { ai = a.inverse }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return both the matrices 'a' and its inverse 'a^-1'.
     */
    def factors: (MatriD, MatriD) = (a, ai)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use the inverse matrix 'ai' to solve a system of equations 'a * x = b'.
     *  Return the solution vector 'x'.
     *  @param b  the constant vector
     */
    def solve (b: VectoD): VectoD = ai * b

} // Fac_Inv class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fac_InvTest` object is used to test the `Fac_Inv` class.
 *  > run-main scalation.linalgebra.Fac_InvTest
 */
object Fac_InvTest extends App
{
    val a = new MatrixD ((4, 4), 4.0,  0.4,   0.8, -0.2,
                                 0.4,  1.04, -0.12, 0.28,
                                 0.8, -0.12,  9.2,  1.4,
                                -0.2,  0.28,  1.4,  4.35)
    val b = VectorD (-0.2, -0.32, 13.52, 14.17)

    val inv = new Fac_Inv (a)
    println ("a = " + a)
    println ("factor = " + inv.factor ())
    println ("solve  = " + inv.solve (b))

    val lu = a.lud
    println ("lu solve = " + a.solve (lu, b))

} // Fac_InvTest object

