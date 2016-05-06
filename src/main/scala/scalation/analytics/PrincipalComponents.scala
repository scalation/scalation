
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Mar 25 14:58:13 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.predict.ws/H_principle/SvanteHarald.htm
 *  @see     http://folk.uio.no/henninri/pca_module/pca_nipals.pdf
 */

package scalation.analytics

import util.control.Breaks.{breakable, break}

import scalation.linalgebra.{Eigenvalue, Eigenvector}
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra.MatrixD.outer
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrincipalComponents` class performs the Principal Component Analysis 'PCA'
 *  on data matrix 'x'.  It can be used to reduce the dimensionality of the data.
 *  First find the Principal Components 'PC's by calling 'findPCs' and then call
 *  'reduce' to reduce the data (i.e., reduce matrix 'x' to a lower dimensionality
 *  matrix).
 *  @param x  the data matrix to reduce, stored column-wise
 */
class PrincipalComponents (x: MatrixD)
      extends Reducer with Error
{
    if (! x.isRectangular) flaw ("constructor", "x matrix must be rectangular")

    private val MAX_ITER = 200                               // the maximum number of iterations
    private val EPSILON  = 1E-8                              // a value close to zero
    private val M        = x.dim1                            // the number of data points (rows)
    private val MR       = M.toDouble                        // M as a real number
    private val N        = x.dim2                            // the number of variables (columns)
    private val mu       = meanCenter ()                     // the mean vector
    private val cov      = computeCov ()                     // the covariance matrix
    private val eigenVal = (new Eigenvalue (cov)).getE       // the eigenvalues of cov
    private val eigenVec = computeEigenVectors (eigenVal)    // the eigenvectors for cov

    private var featureMat: MatrixD = null                   // the feature matrix
    private var reducedMat: MatrixD = null                   // the reduced matrix

    println ("mu = " + mu)
    println ("zero-meaned x = " + x)
    println ("cov = " + cov)
    println ("eigenVal = " + eigenVal)
    println ("eigenVec = " + eigenVec)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center the data about the means (i.e., subtract the means) and return
     *  the mean vector (i.e., the mean for each variable/dimension).
     */
    def meanCenter (): VectorD =
    {
        val mu  = new VectorD (N)                              // the mean vector
        for (j <- 0 until N) mu(j) = x.col(j).sum / MR         // compute means
        for (i <- 0 until M; j <- 0 until N) x(i, j) -= mu(j)  // subtract the means
        mu
    } // meanCenter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assuming mean centered data, compute the covariance matrix.
     */
    def computeCov (): MatrixD =
    {
        val cov = new MatrixD (N, N)                           // the covariance matrix
        for (i <- 0 until N; j <- i until N) {
            cov(i, j) = (x.col(i) dot x.col(j)) / (MR - 1.0)
            if (i != j) cov(j, i) = cov(i, j)
        } // for
        cov
    } // computeCov

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unit eigenvectors for the covariance matrix.
     *  @param eVal  the vector of eigenvalues for the covariance matrix
     */
    def computeEigenVectors (eVal: VectorD): MatrixD =
    {
        val eigenVec = (new Eigenvector (cov, eigenVal)).getV                 // the eigenvectors for cov
        for (j <- 0 until N) eigenVec.setCol (j, eigenVec.col(j).normalizeU)  // want unit vectors
        eigenVec
    } // computeEigenVectors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the Principal Components/Features, the eigenvectors with the 'k'
     *  highest eigenvalues.
     *  @param k  the number of Principal Components 'PC's to find
     */
    def findPCs (k: Int): MatrixD =
    {
        featureMat = eigenVec(0 until N, 0 until k)     // store in feature matrix
        featureMat
    } // findPCs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply the zero mean data matrix by the feature matrix to reduce
     *  dimensionality.
     */
    def reduce (): MatrixD =
    {
        reducedMat = x * featureMat                     // store in reduced matrix
        reducedMat
    } // reduce

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Approximately recover the original data by multiplying the reduced matrix
     *  by the inverse (via transpose) of the feature matrix and then adding back
     *  the means.
     */
    def recover (): MatrixD = reducedMat * featureMat.t + mu

} // PrincipalComponents class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrincipalComponentsTest` object is used to test the `PrincipalComponents` class.
 *  @see http://www.ce.yildiz.edu.tr/personal/songul/file/1097/principal_components.pdf
 */
object PrincipalComponentsTest extends App
{
    val x = new MatrixD ((10, 2), 2.5, 2.4,         // the data matrix
                                  0.5, 0.7,
                                  2.2, 2.9,
                                  1.9, 2.2,
                                  3.1, 3.0,
                                  2.3, 2.7,
                                  2.0, 1.6,
                                  1.0, 1.1,
                                  1.5, 1.6,
                                  1.1, 0.9)

    val pca = new PrincipalComponents (x)           // perform Principal Component Analysis (PCA)
    println ("pc          = " + pca.findPCs (1))     // find 1 Principal Components
    println ("reduced   x = " + pca.reduce)          // the reduce data (lower dimensionality)
    println ("recovered x = " + pca.recover)         // the approximately recovered data

} // PrincipalComponentsTest


//------------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the i-th Principal Component 'PC_i' return the score and loading
     *  vectors t and p.
     *  @param i  the i-th Principal Component 'PC_i'
     */
//  def solve (i: Int): Tuple2 [VectorD, VectorD] =
//  {
//      var t  = x.col (i)          // the scores for PC_i, initialized to i-th column of x
//      var p  = new VectorD (1)    // the loadings for PC_i
//      var tt = new VectorD (1)    // saved value for t (the old t)
//      var dt = new VectorD (1)    // the difference between t and tt
//
//      breakable { for (k <- 1 to MAX_ITER) {
//          p  = (e * t) / t.normSq           // project x onto t to find the corresponding loading p
//          p  = p / p.norm                   // normalise loading vector p to length 1
//          tt = t                            // save the old value of t
//          t  = (e * p) / p.normSq           // project x onto p to find corresponding score vector t
//          dt = t - tt                       // calculate dt as the difference between previous and current scores
//          if (i > 1 && dt.norm > EPSILON) break  // check for the convergence
//      }} // for
//      e = e - outer (t, p)                   // remove the estimated PC_i component from e
//      (t, p)
//  } // solve

