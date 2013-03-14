
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Jan 12 14:50:04 EST 2012
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.predict.ws/H_principle/SvanteHarald.htm
 *  @see     http://folk.uio.no/henninri/pca_module/pca_nipals.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra.MatrixD.outer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class computes the Principal Components for a data matrix x.
 *  @param x           the mean centered data matrix
 *  @param isMeanZero  whether the data matrix is mean zero
 */
class PrincipalComponents (x: MatrixD, isMeanZero: Boolean = true)
{
    private val EPSILON  = 1.E-8    // a small threshold value
    private val MAX_ITER = 10       // the maximum number of iterations
  
    if (! isMeanZero) {
//     for (i <- 0 until x.dim1) x(i) = x(i) - x(i).mean
    } // if

    private var e = x              // the e residuals matrix, x with PC's subtracted

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the Principal Components for mean-zero data matrix x.
     *  @param nPC  the number of Principal Components (PCs) to find
     */
    def findPCs (nPC: Int)
    {
        println ("findPCs: find the Principal Components for data matrix x")
        println ("x = " + x)
        for (i <- 1 to nPC) println ("tp (" + i + ") = " + solve (i))
        println ("residuals e = " + e)
    } // findPCs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the i-th Principal Component PC_i return the score and loading
     *  vectors t and p.
     *  @param i  the i-th Principal Component PC_i
     */
    def solve (i: Int): Tuple2 [VectorD, VectorD] =
    {
        var t  = x.col (i)          // the scores for PC_i, initialized to i-th column of x
        var p  = new VectorD (1)    // the loadings for PC_i
        var tt = new VectorD (1)    // saved value for t (the old t)
        var dt = new VectorD (1)    // the difference between t and tt

        breakable { for (k <- 1 to MAX_ITER) {
            p  = (e * t) / t.normSq           // project x onto t to find the corresponding loading p
            p  = p / p.norm                   // normalise loading vector p to length 1
            tt = t                            // save the old value of t
            t  = (e * p) / p.normSq           // project x onto p to find corresponding score vector t
            dt = t - tt                       // calculate dt as the difference between previous and current scores
            if (i > 1 && dt.norm > EPSILON) break  // check for the convergence
        }} // for
        e = e - outer (t, p)                   // remove the estimated PC_i component from e
        (t, p)
    } // solve

} // PrincipalComponents class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the PrincipalComponents class.
 */
object PrincipalComponentsTest
{
    val x = new MatrixD ((2, 2), 1, -1,       // the data matrix
                                -2,  2)
    val pca = new PrincipalComponents (x)     // perform Principal Component Analysis (PCA)
    pca.findPCs (2)                           // find 2 Principal Components

} // PrincipalComponentsTest

