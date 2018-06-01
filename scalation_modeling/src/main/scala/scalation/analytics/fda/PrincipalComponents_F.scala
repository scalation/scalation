
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Wed Oct 19 20:39:06 EDT 2016
 *  @see     LICENSE (MIT style license file).

 *  @see www.predict.ws/H_principle/SvanteHarald.htm
 *  @see folk.uio.no/henninri/pca_module/pca_nipals.pdf
 *  @see Functional Data Analysis, Second Edition, Chapter 8
 *  @see http://link.springer.com/book/10.1007%2Fb98888
 */

package scalation.analytics.fda

import scalation.analytics.{PrincipalComponents, Reducer}
import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.Functions

import StatFunction.toMatrix

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrincipalComponents_F` class performs the Principal Component Analysis 'PCA'
 *  on data matrix 'x'.  It can be used to reduce the dimensionality of the data.
 *  First find the Principal Components 'PC's by calling 'findPCs' and then call
 *  'reduce' to reduce the data (i.e., reduce matrix 'x' to a lower dimensionality
 *  matrix).
 *  @param xa  the array of functions
 *  @param t   the vector of time points
 */
class PrincipalComponents_F (xa: Functions, t: VectorD)
      extends Reducer
{
    val x   = toMatrix (xa, t)
    val pca = new PrincipalComponents (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the Principal Components/Features, the eigenvectors with the 'k'
     *  highest eigenvalues.
     *  @param k  the number of Principal Components 'PC's to find
     */
    def findPCs (k: Int): MatriD = pca.findPCs (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply the zero mean data matrix by the feature matrix to reduce
     *  dimensionality.
     */
    def reduce (): MatriD = pca.reduce ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Approximately recover the original data by multiplying the reduced matrix
     *  by the inverse (via transpose) of the feature matrix and then adding back
     *  the means.
     */
    def recover: MatriD = pca.recover

} // PrincipalComponents_F class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrincipalComponents_FTest` object is used to test the `PrincipalComponents_F` class.
 *  @see www.ce.yildiz.edu.tr/personal/songul/file/1097/principal_components.pdf
 *  > runMain scalation.analytics.PrincipalComponents_FTest
 */
object PrincipalComponents_FTest extends App
{
 
    // FIX - need test case

} // PrincipalComponents_FTest

