
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  Many of the algorithms used are from:
 *    Averill M. Law and W. David Kelton
 *    Simulation Modeling and Analysis, 2nd Edition
 *    McGraw-Hill, Inc., NY, 1991.
 */

package scalation.random

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates Normal (Gaussian) random variate vector.
 *  This continuous RV models normally distributed data.  FIX
 *  @see 
 *  @param mu      the mean vector
 *  @param cov     the covariance matrix
 *  @param stream  the random number stream
 */
abstract case class NormalVec (mu: VectorD, cov: MatrixD, stream: Int = 0)
     extends Variate (stream)
{
    val normal = Normal ()    // generator for standard normal distribution

    def vgen: VectorD =
    {
        val z = new VectorD (mu.dim)
        for (i <- 0 until mu.dim) z(i) = normal.gen
        val t = cov         // cov.cholesky
        t * z + mu
    } // vgen

} // NormalVec

