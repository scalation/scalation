
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu Jan 17 13:12:42 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm
 *  @see     http://www.cs.utexas.edu/users/inderjit/public_papers/HLA_SVD.pdf
 */

package scalation.linalgebra

import math.sqrt

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2` class performs Single Value Decompositions (SVDs) using the `Eigen`
 *  class.  For a direct, more robust algorithm that is less sensitive to round-off errors,
 *  @see the `SVD` class.
 *  @param a  the matrix to be deflated/decomposed
 */
class SVD2 (a: MatrixD)
      extends SVDecomp
{
    private val DEBUG   = true                  // debug flag
    private val m       = a.dim1                // the number of row in matrix A
    private val n       = a.dim2                // the number of columns in matrix A
    private val s       = new MatrixD (m, n)    // matrix of singular values

    private var notflat = true                  // whether matrix A is already deflated

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate matrix 'a' forming a diagonal matrix consisting of singular
     *  values and return the singular values in a vector.
     */
    def deflate (): VectorD =
    {
        if (notflat) deflateV ()
        s.getDiag ()
    } // deflate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate matrix 'a' and decompose it into 'u * s * v.t', where 'u's columns
     *  are the eigenvectors of 'a * a.t' and 'v's columns are the eigenvectors
     *  of 'a.t * a'.
     */
    def deflateV (): Tuple3 [MatrixD, MatrixD, MatrixD] =
    {
        val aat = a * a.t                                 // a times a transpose
        println ("aat = " + aat)
        val u_e = (new Eigenvalue (aat)).getE             // aat's eigenvalues
        val u   = (new Eigenvector (aat, u_e)).getV       // u matrix

        val ata = a.t * a                                 // a transpose times a
        println ("ata = " + ata)
        val v_e = (new Eigenvalue (ata)).getE             // ata's eigenvalues
        val vt  = (new Eigenvector (ata, v_e)).getV.t     // v matrix

        if (m < n) {
           for (i <- 0 until m) s(i, i) = sqrt (u_e(i))
        } else {
           for (i <- 0 until n) s(i, i) = sqrt (v_e(i))
        } // if

        if (DEBUG) {
            println ("u_e = " + u_e)
            println ("u   = " + u)
            println ("v_e = " + v_e)
            println ("vt  = " + vt)
            println ("s   = " + s)
        } // if
    
        notflat = false
        (u, s, vt)
    } // deflateV

} // SVD2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test` object is used to test the `SVD2` class.
 *  @see www.ling.ohio-state.edu/~kbaker/pubs/Singular_Value_Decomposition_Tutorial.pdf
 */
object SVD2Test extends App
{
    val a = new MatrixD ((2, 3), 3.0, 1.0, 1.0,            // 2-by-3 matrix
                                -1.0, 3.0, 1.0)

    println ("----------------------------------------")
    println ("a = " + a)
    val svd = new SVD2 (a)

    println ("----------------------------------------")
    println ("Test SVD2")
    println ("----------------------------------------")
    val res = svd.deflateV ()
    println ("u * s * v.t = " + res._1 * res._2 * res._3)

} // SVD2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVD2Test2` object is used to test the `SVD2` class.
 */
object SVD2Test2 extends App
{
    val a = new MatrixD ((3, 3), 1.0, 1.0, 0.0,             // 3-by-3 matrix
                                 0.0, 2.0, 2.0,
                                 0.0, 0.0, 3.0)

    println ("----------------------------------------")
    println ("a = " + a)
    val svd = new SVD2 (a)

    println ("----------------------------------------")
    println ("Test SVD")
    println ("----------------------------------------")
    val res = svd.deflateV ()
    println ("u * s * v.t = " + res._1 * res._2 * res._3)

} // SVD2Test2 object

