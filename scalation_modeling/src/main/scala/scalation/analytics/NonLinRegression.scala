
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NonLinRegression` class supports non-linear regression.  In this case,
 *  'x' can be multi-dimensional '[1, x1, ... xk]' and the function 'f' is non-linear
 *  in the parameters 'b'.  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =  f(x, b) + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  'b' by using Non-linear Programming to minimize Sum of Squares Error 'SSE'.
 *  @see www.bsos.umd.edu/socy/alan/stats/socy602_handouts/kut86916_ch13.pdf
 *  @param x       the input/design matrix augmented with a first column of ones
 *  @param y       the response vector
 *  @param f       the non-linear function f(x, b) to fit
 *  @param b_init  the initial guess for the parameter vector b
 */
class NonLinRegression (x: MatrixD, y: VectorD,
                        f: (VectoD, VectoD) => Double,
                        b_init: VectorD)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                      // debug flag
    private val k          = x.dim2 - 1                 // number of variables (k = n-1
    private val m          = x.dim1                     // number of data points (rows in matrix x)
    private val r_df       = (m-1.0) / (m-k-1.0)        // ratio of degrees of freedom
    private var rSquared   = -1.0                       // coefficient of determination (quality of fit)
    private var rBarSq     = -1.0                       // adjusted R-squared
    private var fStat      = -1.0                       // F statistic (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function to compute the Sum of Squares Error 'SSE' for given values for
     *  the parameter vector 'b'.
     *  @param b  the parameter vector - FIX - to `VectoD`
     */
    def sseF (b: VectorD): Double =
    {
        val z = new VectorD (m)                         // create vector z to hold predicted outputs
        for (i <- 0 until m) z(i) = f (x(i), b)         // compute values for z
        val e = y - z                                   // residual/error vector
        e dot e                                         // residual/error sum of squares
    } // sseF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  non-linear regression equation
     *      y = f(x, b)
     *  using the least squares method.
     *  Caveat:  Optimizer may converge to an unsatisfactory local optima.
     *           If the regression can be linearized, use linear regression for
     *           starting solution.
     */
    def train ()
    {
        val bfgs = new QuasiNewton (sseF)                // minimize sse using NLP
        b        = bfgs.solve (b_init)                   // estimate for b from optimizer
        diagnose (y, null)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     *  @param e   the residual/error vector (not used directly)
     */
    def diagnose (yy: VectorD, e: VectorD)
    {
        val sse    = sseF (b.asInstanceOf [VectorD])            // residual/error sum of squares - FIX
        val sst    = (yy dot yy) - yy.sum~^2.0 / m.toDouble     // total sum of squares
        val ssr    = sst - sse                                  // regression sum of squares
        rSquared   = ssr / sst                                  // coefficient of determination
        rBarSq     = 1.0 - (1.0-rSquared) * r_df                // R-bar-squared (adjusted R-squared)
        fStat      = ssr * (m-k-1.0)  / (sse * k)               // F statistic (msr / mse)
        if (DEBUG) println ("sse = " + sse)
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.
     */
    def fit: VectorD = VectorD (rSquared, rBarSq, fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = f(z, b),
     *  i.e.0, (b0, b1) dot (1.0, z1).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = f(z, b)

} // NonLinRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NonLinRegressionTest` object tests the `NonLinRegression` class:
 *  y  =  f(x; b)  =  b0 + exp (b1 * x0).
 *  @see www.bsos.umd.edu/socy/alan/stats/socy602_handouts/kut86916_ch13.pdf
 *  Answers:  sse = 49.45929986243339
 *            fit = (VectorD (58.606566327280426, -0.03958645286504356), 0.9874574894685292)
 *            predict (VectorD (50.0)) = 8.09724678182599
 *  FIX: check this example
 */
object NonLinRegressionTest extends App
{
    import scala.math.exp

    // 5 data points: constant term, x1 coordinate, x2 coordinate
    val x = new MatrixD ((15, 1), 2.0, 5.0, 7.0, 10.0, 14.0, 19.0, 26.0, 31.0, 34.0, 38.0, 45.0, 52.0, 53.0, 60.0, 65.0)
    val y = VectorD (54.0, 50.0, 45.0, 37.0, 35.0, 25.0, 20.0, 16.0, 18.0, 13.0, 8.0, 11.0, 8.0, 4.0, 6.0)

    println ("x = " + x)
    println ("y = " + y)

    def f (x: VectoD, b: VectoD): Double = b(0) * exp (b(1) * x(0))   // non-linear regression function
    val b_init = VectorD (4.04, .038)                                   // initial guess for parameter vector b

    val rg = new NonLinRegression (x, y, f, b_init)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = VectorD (1); z(0) = 50.0             // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

} // NonRegressionTest object

