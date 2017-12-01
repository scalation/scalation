
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**  @author  Khalid Jahangeer
  *  @version 1.4
  *  @date    Wed June 08 16:06:12 EDT 2017
  *  @see     LICENSE (MIT style license file).
  */

package scalation.linalgebra

import scala.math.{abs, round, sqrt}

import scalation.math.double_exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDImputed` class is used to predict the missing values of an input matrix
 *  by employing the concept of column mean imputation and then applying
 *  Singular Value Decomposition to factor the matrix.  Once the factors are obtained
 *  the missing value in the matrix is obtained as the dot product of 'p' and 'q', where
 *  <p>
 *      p = u * sqrt(s)            left orthogonal matrix * Singular Values Vector
 *      q = sqrt(s) * v.t          singular values vector * transpose of right orthogonal matrix
 *      predict (i, j) = p dot q
 *  <p>
 *------------------------------------------------------------------------------
 *  @param a  the input data matrix
 *  @param k  the number of factors
 */
class SVDImputed (a: MatrixD, k: Int)
      extends SVDecomp
{
    var p: MatriD = null                                             // p matrix 
    var q: MatriD = null                                             // q matrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row normalized version of 'this' matrix.  For all values that are
     *  not 0 replace with self - row mean (mean calculation doesnot include 0s).
     *  @param ia  the column mean inputed matrix
     */
    def normalize (ia: MatrixD): MatrixD =
    {
        var norm_a = new MatrixD (ia.dim1, ia.dim2)
        for (i <- ia.range1; j <- ia.range2) {
            val a_i_nz = a(i).filter (_ !=~ 0.0)
            val temp = if (a_i_nz.sum !=~ 0.0) a_i_nz.mean else 0.0
            if (ia(i, j) !=~ 0.0) norm_a(i, j) = ia(i, j) - temp
        } // for
        norm_a
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a row denormalized version of 'this' matrix and return the denormalized
     *  value of the row.
     *  @param i  the row id
     */
    def denormalize (i: Int): Double =
    {
        if (a(i).filter (_ !=~ 0.0).sum !=~ 0.0) a(i).filter (_ !=~ 0.0).mean
        else 0.0
    } //denormalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model to myield the 'p' and 'a' matices using the dataset.
     *  @see Application of Dimensionality Reduction in Recommender System -- A Case Study
     *  @see www.dtic.mil/get-tr-doc/pdf?AD=ADA439541
     *  @param factors  the u, s and v from SVD factorization
     */
    def train (factors: FactorType)
    {
        val (u, s, v) = SVDecomp.reduce (factors, k)                 // dimensionality reduction
        val s_root    = new MatrixD (k, k)
        s_root.setDiag (s.map (sqrt (_)))                            // square roots of singular values
        val (uu, vv)  = if (a.dim1 < a.dim2) (v, u) else (u, v)      // in case m < n the SVD of a.t would have been computed
        p = uu * s_root
        q = s_root * vv.t
    } // training

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the final value for a missing value in the matrix.
     *  @see Application of Dimensionality Reduction in Recommender System -- A Case Study
     *  @see www.dtic.mil/get-tr-doc/pdf?AD=ADA439541
     *  @oaram i  the row id
     *  @oaram j  the column id
     */
    def predict (i: Int, j: Int): Double = (p(i) dot q.col(j)) + denormalize (i)

} // SVDImputed class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDImputed` companion object is used to perform imputation.
 */
object SVDImputed
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Impute values for missing values in the 'b' matrix using Column Mean Imputation.
     *  @param b  the input data matrix 
     */
    def impute (b: MatrixD): MatrixD =
    {
        val ib = b.copy ()
        val colmeans = colMeans (b)                          // store the colmeans/min to imputed for all 0 value entries in norm
        for (i <- b.range1; j <- b.range2) {
            if (b(i, j) =~ 0.0) ib(i, j) = colmeans (j)      // column mean imputation
        } // for
        ib
    } // impute

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of mean values of each column without including 0's
     *  @param b  the input data matrix 
     */
    def colMeans (b: MatrixD): VectorD =
    {
        val mn = minNZ (b)                                               // minimum value of normalized matrix
        var colmeans = new VectorD (b.dim2)                              // impute column mean for all 0 value entries in norm
        for (j <- b.range2) {
            if (b.col(j).filter (_ != 0.0).size == 0) colmeans(j) = mn   // if a column has all 0's then assign minimum value
            else colmeans(j) = b.col(j).filter (_ !=~ 0.0).mean
        } // for
        colmeans
    } // colMeans

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum positive,  non-zero element of the entire matrix.
     *  @param b  the input data matrix 
     */
    def minNZ (b: MatrixD): Double =
    {
        var mn = b(0, 0)
        for (i <- b.range1; j <- b.range2 if b(i, j) > 0.0) {
            if (b(i, j) < mn) mn = b(i, j)
        } // for
        mn
    } // minNZ

} // SVDImputed object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDImputedTest` object is used to test the `SVDImputed` method.
  *  > run-main scalation.linalgebra.SVDImputedTest
  */
object SVDImputedTest extends App
{
    val a = new MatrixD ((12, 6),  1.0, 0.0, 2.0, 0.0, 0.0, 1.0,          // training data
                                   0.0, 0.0, 4.0, 2.0, 0.0, 0.0,
                                   3.0, 5.0, 0.0, 4.0, 4.0, 3.0,
                                   0.0, 4.0, 1.0, 0.0, 3.0, 0.0,
                                   0.0, 0.0, 2.0, 5.0, 4.0, 3.0,
                                   5.0, 0.0, 0.0, 0.0, 2.0, 0.0,
                                   0.0, 4.0, 3.0, 0.0, 0.0, 0.0,
                                   0.0, 0.0, 0.0, 4.0, 0.0, 2.0,
                                   5.0, 0.0, 4.0, 0.0, 0.0, 0.0,
                                   0.0, 2.0, 3.0, 0.0, 0.0, 0.0,
                                   4.0, 1.0, 5.0, 2.0, 2.0, 4.0,
                                   0.0, 3.0, 0.0, 0.0, 0.0, 0.0)

    val b = new MatrixD ((12, 6),  1.0, 0.0, 2.0, 0.0, 0.0, 1.0,         // testing data
                                   0.0, 0.0, 4.0, 2.0, 0.0, 0.0,
                                   3.0, 5.0, 0.0, 4.0, 4.0, 3.0,
                                   0.0, 4.0, 1.0, 0.0, 3.0, 0.0,
                                   0.0, 0.0, 2.0, 5.0, 4.0, 3.0,
                                   5.0, 0.0, 0.0, 0.0, 2.0, 0.0,
                                   0.0, 4.0, 3.0, 0.0, 0.0, 0.0,
                                   0.0, 0.0, 0.0, 4.0, 0.0, 2.0,
                                   5.0, 0.0, 4.0, 0.0, 0.0, 0.0,
                                   0.0, 2.0, 3.0, 0.0, 0.0, 0.0,
                                   4.0, 1.0, 5.0, 2.0, 2.0, 4.0,
                                   0.0, 3.0, 0.0, 0.0, 5.0, 0.0)
 
    val k = 3

    println (s"a = $a")

    val svdI = new SVDImputed (a, k)
    val ia = SVDImputed.impute (a)
    println (s"ia = $ia")

    val na = svdI.normalize (ia)
    println (s"na = $na")

    val factors = new SVD (ia).factor123 ()
    println (s"factors = $factors")

    svdI.train (factors)
    println (svdI.predict (11, 4))
    
} // SVDImputedTest

