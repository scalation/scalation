
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Feb  2 18:18:15 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.{cos, Pi, sin}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.util.{banner, Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrigRegression` class supports trigonometric regression.  In this case,
 *  't' is expanded to '[1, sin (wt), cos (wt), sin (2wt), cos (2wt), ...]'.
 *  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 sin (wt)  + b_2 cos (wt)  +
 *                                  b_3 sin (2wt) + b_4 cos (2wt) + ... + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y
 *      b  =  fac.solve (.)
 *  <p>
 *  @see link.springer.com/article/10.1023%2FA%3A1022436007242#page-1
 *  @param t          the input vector: t_i expands to x_i
 *  @param y          the response vector
 *  @param k          the maximum multiplier in the trig function (kwt)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class TrigRegression (t: VectoD, y: VectoD, k: Int, technique: RegTechnique = QR)
      extends Predictor with Error
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")
    if (t.dim <= k)     flaw ("constructor", "not enough data points for the given order")

    private val w = (2.0 * Pi) / (t.max() - t.min())           // base displacement angle in radians
    private val x = new MatrixD (t.dim, 1 + 2 * k)             // design matrix built from t
    for (i <- t.range) x(i) = expand (t(i))
    private val rg = new Regression (x, y, technique)          // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the scalar 't' into a vector of powers of 't': 
     *  '[1, sin (wt), cos (wt), sin (2wt), cos (2wt), ...]'.
     *  @param t  the scalar to expand into the vector
     */
    def expand (t: Double): VectoD = 
    {
        val wt = w * t
        val v = new VectorD (1 + 2 * k)
        v(0) = 1.0
        for (j <- 1 to k) {
            v(2*j-1) = sin (j * wt)
            v(2*j)   = cos (j * wt)
        } // for
        v
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [expanded t] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): Regression [MatrixD, VectoD] = rg.train (yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  regression equation using 'y'.
     */
//    def train (): Regression [MatrixD, VectoD] = rg.train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param yy   the response vector
     */ 
    def eval (yy: VectoD = y)
    {
        e = yy - x * b                                         // compute residual/error vector e
        diagnose (yy)                                          // compute diagnostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficients.
     */
    override def coefficient: VectoD = rg.coefficient

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    override def fit: VectoD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = rg.fitLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot expand (z),
     *  e.g., (b_0, b_1, b_2) dot (1, z, z^2).
     *  @param z  the new scalar to predict
     */
    def predict (z: Double): Double = rg.predict (expand (z))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = rg.predict (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    def forwardSel (cols: Set [Int]): (Int, VectoD, VectoD) = rg.forwardSel (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable from
     *  the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    def backwardElim (cols: Set [Int]): (Int, VectoD, VectoD) = rg.backwardElim (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectoD = rg.vif

} // TrigRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrigRegressionTest` object tests `TrigRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1 sin wt + b_2 cos wt + ... b_2k-1 sin kwt + b_2k cos kwt + e
 *  <p>
 *  The data is generated from a noisy cubic function.
 *  > runMain scalation.analytics.TrigRegressionTest
 */
object TrigRegressionTest extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 10000.0)
    val t     = VectorD.range (0, 100)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 100) { val x = (i - 40)/2.0; y(i) = 1000.0 + x + x*x + x*x*x + noise.gen }

    println ("t = " + t)
    println ("y = " + y)

    val harmonics = 8
    val trg   = new TrigRegression (t, y, harmonics)
    trg.train ().eval ()
    println ("coefficient = " + trg.coefficient)
    println ("              " + trg.fitLabels)
    println ("fit         = " + trg.fit)

    val z   = 10.5                                  // predict y for one point
    val yp1 = trg.predict (z)
    println ("predict (" + z + ") = " + yp1)

    banner ("test predictions")
    val yp = t.map (trg.predict (_))
    println (s" y = $y \n yp = $yp")
    new Plot (t, y, yp, "TrigRegression")

} // TrigRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrigRegressionTest2` object tests `TrigRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1 sin wt + b_2 cos wt + ... b_2k-1 sin kwt + b_2k cos kwt + e
 *  <p>
 *  The data is generated from periodic noisy cubic functions.
 *  > runMain scalation.analytics.TrigRegressionTest2
 */
object TrigRegressionTest2 extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 10.0)
    val t     = VectorD.range (0, 200)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 5) {
        for (j <- 0 until 20) { val x = j - 4;  y(40*i+j) = 100.0 + x + x*x + x*x*x + noise.gen }
        for (j <- 0 until 20) { val x = 16 - j; y(40*i+20+j) = 100.0 + x + x*x + x*x*x + noise.gen }
    } // for

    println ("t = " + t)
    println ("y = " + y)

    val harmonics = 16
    val trg   = new TrigRegression (t, y, harmonics)
    trg.train ().eval ()
    println ("coefficient = " + trg.coefficient)
    println ("              " + trg.fitLabels)
    println ("fit         = " + trg.fit)

    val z   = 10.5                                  // predict y for one point
    val yp1 = trg.predict (z)
    println ("predict (" + z + ") = " + yp1)

    banner ("test predictions")
    val yp = t.map (trg.predict (_))
    println (s" y = $y \n yp = $yp")
    new Plot (t, y, yp, "TrigRegression")

} // TrigRegressionTest2 object

