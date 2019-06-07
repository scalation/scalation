
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Oct  9 17:40:30 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.round

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.stat.Statistic

import PredictorMat.pullResponse
import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoundRegression` class supports rounded multiple linear regression.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the transformed regression equation
 *  <p>
 *      y  =  round (b dot x) + e  =  round (b_0 + b_1 * x_1 +  b_2 * x_2 ... b_k * x_k) + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector 'b'
 *  @param x          the data/input matrix
 *  @param y          the response/output vector
 *  @param fname_     the feature/variable names
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class RoundRegression (x: MatriD, y: VectoD, fname_ : Strings = null, technique: RegTechnique = QR)
      extends Regression (x, y, fname_, null, technique)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Round the predicted response values 'yp' to their nearest integer values.
     *  @param yp  the unrounded predicted response vector
     */
    def vround (yp: VectoD): VectoI =
    {
        val yq = new VectorI (yp.dim)
        for (i <- yp.range) yq(i) = round (yp(i)).toInt
        yq
    } // vround

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.  Requires overriding to handle
     *  the rounding.
     *  @param xx  the test data/input matrix
     *  @param yy  the test response/output vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): RoundRegression =
    {
        val yp = vround (xx * b).toDouble                   // y predicted for xx (test/full)
        e = yy - yp                                         // residual/error vector
        diagnose (e, yy)                                    // compute diagnostics
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = b dot z
             def ipredict (z: VectoD): Int   = round (b dot z).toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    override def predict (z: MatriD): VectoD  = z * b
             def ipredict (z: MatriD): VectoI = vround (z * b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns) 
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    override def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new RoundRegression (x, y, fname, technique),
                                                 xx, k, rando)
    } // crossVal

} // RoundRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoundRegression` companion object provides a factory method.
 */
object RoundRegression
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RoundRegression` object for an integer response vector.
     *  @param x          the data/input matrix
     *  @param y          the integer response/output vector
     *  @param fname      the feature/variable names
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
    */
    def apply (x: MatriD, y: VectoI, fname: Strings, technique: RegTechnique): RoundRegression =
    {
        new RoundRegression (x, y.toDouble, fname, technique)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RoundRegression` object using a combined matrix.
     *  @param xy         the combined data matrix and response vector
     *  @param fname      the feature/variable names
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
     */
    def apply (xy: MatriD, fname: Strings = null, technique: RegTechnique = QR): RoundRegression =
    {
        val (x, y) = pullResponse (xy)
        new RoundRegression (x, y, fname, technique)
    } // apply

} // RoundRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RoundRegressionTest` object tests `RoundRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  round (b dot x)  =  round (b_0 + b_1*x_1 + b_2*x_2).
 *  <p>
 *  > runMain scalation.analytics.RoundRegressionTest
 */
object RoundRegressionTest extends App
{
    //                            1 x0 x1  y
    val xy = new MatrixD ((9, 4), 1, 0, 0, 1,              // 9-by-4 matrix
                                  1, 0, 1, 0,
                                  1, 0, 2, 0,
                                  1, 1, 0, 1,
                                  1, 1, 1, 0,
                                  1, 1, 2, 0,
                                  1, 2, 0, 1,
                                  1, 2, 1, 0,
                                  1, 2, 2, 1)

    println ("xy = " + xy)
    val (x, y) = pullResponse (xy)

    val rrg = new RoundRegression (x, y)
    rrg.train ().eval ()
    println ("parameter = " + rrg.parameter)
    println ("fitMap    = " + rrg.fitMap)

    val yp  = rrg.predict (x)
    val ypi = rrg.vround (yp)
    for (j <- y.range) println (s"y_j = ${y(j)} \t ypi_j = ${ypi(j)} \t yp_j = ${yp(j)}")

} // RoundRegressionTest object


