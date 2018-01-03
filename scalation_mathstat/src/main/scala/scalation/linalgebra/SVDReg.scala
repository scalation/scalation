
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**  @author  Khalid Jahangeer
  *  @version 1.4
  *  @date    Wed July 08 16:06:12 EDT 2017
  *  @see     LICENSE (MIT style license file).
  */

package scalation.linalgebra

import scala.math.{abs, round, sqrt}

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDReg` class works on the principle of Gradient Descent for minimizing
 *  the error generated and L2 regularization, while predicting the missing value
 *  in the matrix.  This is obtained by the dot product of 'u(i)' and 'v(j)' vectors:
 *  Dimensionality is reduced from 'n' features to 'k' factors.
 *  <p>
 *      predict (i, j) = u(i) dot v(j)
 *  <p>
 *------------------------------------------------------------------------------
 *  @param a  the input m-by-n data matrix (m instances, n features/variables)
 *  @param k         the number of factors (k <= n)
 */
class SVDReg (a: MatrixD, k: Int)
      extends Error
{
    private val DEBUG      = false                                // debug flag
    private val gamma      = 0.02                                 // regularization term
    private val lambda     = 0.001                                // learning rate
    private val max_epochs = 100                                  // maximum number of iterations of SGD
    private val min_improv = 0.001                                // minimum improvement in the error
    private val m          = a.dim1                               // no of rows
    private val n          = a.dim2                               // no of columns
    private var u          = new MatrixD (m, k)                   // user feature matrix
    private var v          = new MatrixD (n, k)                   // item feature matrix

    if (m < k || n < k) flaw ("constructor", "SVDReg implementation requires k < number of users and items")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of the non-zero mean / k of the initial rating matrix.
     */
    def nz_sqmean: Double =
    {
        var sum     = 0.0
        var count   = 0
        for (i <- a.range1; j <- a.range2){
            if (a(i, j) != 0.0) {
                sum   += a(i, j)
                count += 1
            } // if
        } // for
        sqrt (sum / (count * k))
    } // nz_sqmean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor the the input matrix 'a' to obtain the 'u' and the 'v' matrices.
     */
    def factor
    {
        var nz_mean = nz_sqmean
        u.set (nz_mean)                                                     // set u and v to a very small value intially
        v.set (nz_mean)
        var old_error = Double.MaxValue
        for (i <- 0 until k) {                                              // execute for each factor
            if (DEBUG) println (s"Factor = $i")
            var j = 0
            while (j < max_epochs && old_error > min_improv) {              // run until max iterations or the error < minimum improvement
                val train_err = update (i)                                  // obtain the regularized error for the current factor
                if (abs (old_error - train_err) > min_improv) old_error = train_err
                j += 1
            } // while
        } // for
    } // factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the 'u' and 'v' matrix to minimze sum of squared error and return
     *  the mean sum of squared errors.
     *  @param h  the current column to update
     */
    def update (h: Int): Double =
    {
        var sse   = 0.0                                                      // sum of squared errors
        var count = 0
        for (i <- a.range1; j <- a.range2){
            if (a(i, j) != 0.0) {                                     // calculate error only for ratings present
                val err = a(i, j) - predict (i, j)
                sse   += err * err
                count += 1
                val uu = u(i, h)
                val vv = v(j, h)
                u(i, h) += lambda * (err * vv - gamma * uu)
                v(j, h) += lambda * (err * uu - gamma * vv)
            } // if
        } // for
        sqrt (sse / n)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value for given row and column.
     *  @param i  the row id
     *  @param j  the column id
     */
    def predict (i: Int, j: Int): Double = u(i) dot v(j)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the value of the objective function after the 'u' and 'v' matrices are
     *  generated.
     */
    def calc_objf: Double =
    {
        var obfun = 0.0
        for (i <- a.range1; j <- a.range2){
            if (a(i, j) != 0.0) {
                val dotprod = u(i) dot v(j)
                obfun += (a(i, j) - dotprod) * (a(i, j) - dotprod)
            } // if
        } // for
        obfun
    } //calc_objfn

} // SVDReg class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDRegTest` object is used to test the `SVDReg` method.
 *  @see `analytics.recommender.ModelBasedRecommender` for further testing
 *  > runMain scalation.linalgebra.SVDRegTest
 */
object SVDRegTest extends App
{
    val a = new MatrixD((12, 6),  1.0, 0.0, 2.0, 0.0, 0.0, 1.0,          // training data
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

    val b = new MatrixD((12, 6),  1.0, 0.0, 2.0, 0.0, 0.0, 1.0,        // testing data
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


    val k      = 3
    val svdreg = new SVDReg (a, k)
    svdreg.factor
    println ("Objective Function value is = " + svdreg.calc_objf)
    println ("Predicted value = " + svdreg.predict (11, 4))

} // SVDRegTest object

