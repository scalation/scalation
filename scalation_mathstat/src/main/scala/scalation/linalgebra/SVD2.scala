
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Khalid Jahangeer
 *  @version 1.4
 *  @date    Thu Jan 17 13:12:42 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm
 *  @see     http://www.cs.utexas.edu/users/inderjit/public_papers/HLA_SVD.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.linalgebra

import scala.math.sqrt

import MatrixD.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2` class performs Single Value Decomposition 'SVD' using the `Eigen`
 *  class.  For a direct, more robust algorithm that is less sensitive to round-off errors,
 *  @see the `SVD` class.
 *  @param a  the matrix to be factored/decomposed
 */
class SVD2 (a: MatrixD)
      extends SVDecomp
{
    private val DEBUG      = true               // debug flag
    private val (m, n)     = (a.dim1, a.dim2)   // number of rows, columns in matrix a
    private val s_vec      = true               // provide option for s to be a vector (true) or a matrix (false)
//  private var s: VectorD = null               // vector of singular values (main diagonal)
    private var notflat    = true               // whether matrix a is already factored/deflated

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor matrix 'a' forming a diagonal matrix consisting of singular values
     *  and return the singular values in vector 's' along with left and right
     *  singular matrices, 'u' and 'v'.
     */
    override def factor123 (): FactorType =
    {
        if (notflat) deflate () else (eye(m), a.getDiag (), eye(n))    // (u, s, v)
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate matrix 'a' and decompose it into 'u * s * v.t', where 'u's columns
     *  are the eigenvectors of 'a * a.t' and 'v's columns are the eigenvectors
     *  of 'a.t * a'.
     */
    def deflate (): FactorType =
    {
        val aat = a * a.t                                         // a times a transpose
        if (DEBUG) println ("aat = " + aat)
        val u_e = (new Eigenvalue (aat)).getE ()                  // aat's eigenvalues
        val u   = (new Eigenvector (aat, u_e)).getV.normalizeU    // u matrix

        val ata = a.t * a                                         // a transpose times a
        if (DEBUG) println ("ata = " + ata)
        val v_e = (new Eigenvalue (ata)).getE ()                  // ata's eigenvalues
        val v   = (new Eigenvector (ata, v_e)).getV.normalizeU    // v matrix

        val s = if (m < n) VectorD (for (j <- 0 until m) yield sqrt (u_e(j)))
                else       VectorD (for (j <- 0 until n) yield sqrt (v_e(j)))

        flip (u, s)

        if (DEBUG) {
            println ("u_e = " + u_e)
            println ("u   = " + u)
            println ("v_e = " + v_e)
            println ("v   = " + v)
            println ("s   = " + s)
        } // if
    
        notflat = false
        (u, s, v)
    } // deflate

} // SVD2 class

import SVDecomp._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test` object is used to test the `SVD2` class.
 *  @see www.ling.ohio-state.edu/~kbaker/pubs/Singular_Value_Decomposition_Tutorial.pdf
 *  > run-main scalation.linalgebra.SVD2Test
 */
object SVD2Test extends App
{
    test (a1, new SVD2 (a1), "SVD2Test")

} // SVD2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test2` object is used to test the `SVD2` class.
 *  @see www.ling.ohio-state.edu/~kbaker/pubs/Singular_Value_Decomposition_Tutorial.pdf
 *  > run-main scalation.linalgebra.SVD2Test2
 */
object SVD2Test2 extends App
{
    test (a2, new SVD2 (a2), "SVD2Test2")

} // SVD2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test3` object is used to test the `SVD2` class.
 *  > run-main scalation.linalgebra.SVD2Test3
 */
object SVD2Test3 extends App
{
    test (a3, new SVD2 (a3), "SVD2Test3")

} // SVD2Test3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test4` object is used to test the `SVD2` class.
 *  > run-main scalation.linalgebra.SVD2Test4
 */
object SVD2Test4 extends App
{
    test (a4, new SVD2 (a4), "SVD2Test4")

} // SVD2Test4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test5` object is used to test the `SVD2` class.
 *  > run-main scalation.linalgebra.SVD2Test5
 */
object SVD2Test5 extends App
{
    test (a5, new SVD2 (a5), "SVD2Test5")

} // SVD2Test5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test6` object is used to test the `SVD2` class.
 *  > run-main scalation.linalgebra.SVD2Test6
 */
object SVD2Test6 extends App
{
    test (a6, new SVD2 (a6), "SVD2Test6")

} // SVD2Test6 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test7` object is used to test the `SVD2` class.
 *  > run-main scalation.linalgebra.SVD2Test7
 */
object SVD2Test7 extends App
{
    test (a7, new SVD2 (a7), "SVD2Test7")

} // SVD2Test7 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test8` object is used to test the `SVD2` class.
 *  > run-main scalation.linalgebra.SVD2Test8
 */
object SVD2Test8 extends App
{
    test (a8, new SVD2 (a8), "SVD2Test8")

} // SVD2Test8 object

