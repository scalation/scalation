
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Jan 17 13:12:42 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://web.mit.edu/be.400/www/SVD/Singular_Value_Decomposition.htm
 *  @see     http://www.cs.utexas.edu/users/inderjit/public_papers/HLA_SVD.pdf
 */

package scalation.linalgebra

import math.sqrt

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class performs Single Value Decompositions (SVDs).
 *  @param a  the matrix to be decomposed
 */
class SVDecomp (a: MatrixD)
{
    private val DEBUG = true         // debug flag
    private val m     = a.dim1       // the number of row in matrix a
    private val n     = a.dim2       // the number of columns in matrix a

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose matrix a into u * s * v.t, where u's columns are the eigenvectors
     *  of a * a.t and v's columns are the eigenvectors of a.t * a.
     *  u is a 
     */
    def decompose (): Tuple3 [MatrixD, MatrixD, MatrixD] =
    {
        val aat = a * a.t                                 // a times a transpose
        println ("aat = " + aat)
        val u_e = (new Eigenvalue (aat)).getE             // aat's eigenvalues
        val u   = (new Eigenvector (aat, u_e)).getV       // u matrix

        val ata = a.t * a                                 // a transpose times a
        println ("ata = " + ata)
        val v_e = (new Eigenvalue (ata)).getE             // ata's eigenvalues
        val vt  = (new Eigenvector (ata, v_e)).getV.t     // v matrix

        val s = new MatrixD (m, n)                        // singular values matrix
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
    
        (u, s, vt)
    } // decompose

} // SVDecomp


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SVDecomp class.
 *  @see http://www.ling.ohio-state.edu/~kbaker/pubs/Singular_Value_Decomposition_Tutorial.pdf
 */
object SVDecompTest extends App
{
    val a = new MatrixD ((2, 3), 3.0, 1.0, 1.0,            // 2-by-3 matrix
                                -1.0, 3.0, 1.0)
    val svd = new SVDecomp (a)
    val res = svd.decompose ()
    println ("a = " + a)
    println ("u * s * v.t = " + res._1 * res._2 * res._3)

} // SVDecompTest object

