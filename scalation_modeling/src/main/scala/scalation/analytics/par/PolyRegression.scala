
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.par

import scala.collection.mutable.Set

import scalation.analytics.HyperParameter
import scalation.analytics.RegTechnique._
import scalation.linalgebra.{MatriD, VectoD}
import scalation.linalgebra.par.{MatrixD, VectorD}
import scalation.math.double_exp
import scalation.plot.Plot
import scalation.util.{Error, time}


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyRegression` class supports polynomial regression.  In this case,
 *  't' is expanded to [1, t, t^2 ... t^k].  Fit the parameter vector 'b' in the
 *  regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * t +  b_2 * t^2 ... b_k * t^k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.
 *  @see www.ams.sunysb.edu/~zhu/ams57213/Team3.pptx
 *  @param t          the input vector: t_i expands to x_i = [1, t_i, t_i^2, ... t_i^k]
 *  @param y          the response vector
 *  @param ord        the order of the polynomial
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class PolyRegression (t: VectorD, y: VectorD, ord: Int, technique: RegTechnique = QR,
                      raw: Boolean = true)
      extends PredictorVec (t, y, ord)
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")
    if (t.dim <= ord)   flaw ("constructor", s"not enough data points for the given order $ord")

    val x = new MatrixD (t.dim, 1 + ord)                     // design matrix built from t
    for (i <- 0 until t.dim) x(i) = expand (t(i))
    rg = new Regression (x, y, null, null, technique)        // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the scalar 't' into a vector of powers of 't':  [1, t, t^2 ... t^k].
     *  @param t  the scalar to expand into the vector
     */
    def expand (t: Double): VectorD = 
    {
        val v = new VectorD (1 + ord)
        for (j <- 0 to ord) v(j) = t~^j
        v
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  regression equation
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, t, t^2 ... t^k] + e
     *  using the least squares method.
     */
    def train (): Regression = rg.train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, t, t^2 ... t^k] + e
     *  using the least squares method.
     *  @param yy  the new response vector
     */
    override def train (yy: VectoD): Regression = rg.train (yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param xx  the test data/input matrix
     *  @param yy  the test response/output vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): Regression = rg.eval (xx, yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.
     */
    override def fit: VectoD = rg.fit

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
    override def predict (z: VectoD): Double = rg.predict (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    override def forwardSel (cols: Set [Int]): (Int, VectoD, VectoD) = rg.forwardSel (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    override def backwardElim (cols: Set [Int]): (Int, VectoD, VectoD) = rg.backwardElim (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    override def vif: VectorD = rg.vif

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param ord    the order of the expansion (e.g., max degree in PolyRegression)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (ord: Int, k: Int = 10, rando: Boolean = true)
    {
        // FIX
        // crossValidate ((t: VectorD, y: VectorD, ord) => new PolyRegression (t, y, ord), k, rando)
    } // crossVal

} // PolyRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyRegressionTest` object tests `PolyRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*t + b_2*t^2.
 *  <p>
 */
object PolyRegressionTest extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 500.0)
    val t     = VectorD.range (0, 100)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 100) y(i) = 10.0 - 10.0 * i + i~^2 + noise.gen

    println ("t = " + t)
    println ("y = " + y)

    val order = 8
    val prg   = new PolyRegression (t, y, order)
    prg.train ().eval ()
    println ("fit = " + prg.fit)

    val z = 10.5                                  // predict y for one point
    val yp = prg.predict (z)
    println ("predict (" + z + ") = " + yp)

//  val yyp = prg.predict (prg.x)                 // predict y for several points
//  println ("predict ( ) = " + yyp)
//
//  new Plot (t, y, yyp)

} // PolyRegressionTest object

