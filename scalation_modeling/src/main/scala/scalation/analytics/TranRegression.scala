
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Jan 18 15:06:16 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{exp, log}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.FunctionS2S
import scalation.plot.Plot
import scalation.util.{Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegression` class supports transformed multiple linear regression.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the transformed regression equation
 *  <p>
 *      transform (y)  =  b dot x + e  =  b_0 + b_1 * x_1 +  b_2 * x_2 ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model) and
 *  'transform' is the function (defaults to log) used to transform the response vector 'y'.
 *  Common transforms:  log (y), sqrt (y) when y > 0
 *  More generally, a Box-Cox Transformation may be applied.
 *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.469.7176&rep=rep1&type=pdf
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.
 *  Caveat: this class does not provide transformations on columns of matrix 'x'.
 *  @see www.ams.sunysb.edu/~zhu/ams57213/Team3.pptx
 *  @param x          the design/data matrix
 *  @param y          the response vector
 *  @param transform  the transformation function (defaults to log)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class TranRegression (x: MatrixD, y: VectorD, transform: FunctionS2S = log, technique: RegTechnique = QR)
      extends Predictor with Error
{
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    val yt = y.map (transform)                        // transform the response vector
    val rg = new Regression (x, yt, technique)        // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1, x_2 ... x_k] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD) { rg.train (yy.map (transform)) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  regression equation on 'yt'.
     */
    def train () { rg.train (yt) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    override def fit: VectorD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = rg.fitLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = rg.predict (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): Tuple3 [Int, VectoD, VectorD] = rg.backElim ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectorD = rg.vif

} // TranRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      log (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > run-main scalation.analytics.TranRegressionTest
 */
object TranRegressionTest extends App
{
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x)
    println ("y = " + y)

    val trg   = new TranRegression (x, y)
    trg.train ()
    println ("fit = " + trg.fit)

    val yp = trg.predict (z)
    println ("predict (" + z + ") = " + yp)

} // TranRegressionTest object

