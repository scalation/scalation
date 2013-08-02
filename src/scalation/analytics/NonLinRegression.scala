
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.exp

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.DoubleWithExp._
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The NonLinRegression class supports non-linear regression.  In this case, x
 *  can be multi-dimensional (x0, ... xk-1) and the function f is non-linear in
 *  the parameters b.  Fit the parameter vector b in the regression equation
 *      y  =  f(x, b) + e
 *  where e represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  b by using Non-linear Programming to minimize Sum of Squares Error (SSE).
 *  @see www.bsos.umd.edu/socy/alan/stats/socy602_handouts/kut86916_ch13.pdf
 *  @param x       the input/design matrix augmented with a first column of ones
 *  @param y       the response vector
 *  @param f       the nonlinear function f(x, b) to fit
 *  @param b_init  the initial guess for the parameter vector b
 */
class NonLinRegression (x: MatrixD, y: VectorD,
                        f: (VectorD, VectorD) => Double,
                        b_init: VectorD)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                // debug flag
    private val n          = x.dim1               // number of data points (rows in matrix x)
    private var b: VectorD = null                 // parameter vector (b0, b1, ... bp)
    private var rSquared   = -1.0                  // coefficient of determination (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function to compute the Sum of Squares Error (SSE) for given values for
     *  the parameter vector b.
     *  @param b  the parameter vector
     */
    def sseF (b: VectorD): Double =
    {
        val yp = new VectorD (n)                   // create y predicted vector
        for (i <- 0 until n) yp(i) = f (x(i), b)   // compute values for yp
        val e  = y - yp                            // residual/error vector
        e dot e                                    // residual/error sum of squares
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
        val bfgs = new QuasiNewton (sseF)          // minimize sse using NLP
        b        = bfgs.solve (b_init)             // estimate for b from optimizer
        val sse  = sseF (b)                        // residual/error sum of squares
        val sst  = (y dot y) - y.sum~^2.0 / n       // total sum of squares
        rSquared = (sst - sse) / sst               // coefficient of determination
        if (DEBUG) println ("sse = " + sse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit rSquared)
     */
    def fit: Tuple2 [VectorD, Double] = (b, rSquared)

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = f(z, b),
     *  i.e., (b0, b1) dot (1.0, z1).
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double = f(z, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = f(z_i, b) for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatrixD): VectorD =
    {
        val zp = new VectorD (z.dim1)
        for (i <- 0 until z.dim1) zp(i) = f(z(i), b)
        zp
    } // predict

} // NonLinRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test NonLinRegression class: y  =  b dot x  =  b0 + b1*x1 + b2*x2.
 *  @see www.bsos.umd.edu/socy/alan/stats/socy602_handouts/kut86916_ch13.pdf
 *  Answers:  sse = 49.45929986243339
 *            fit = (VectorD (58.606566327280426, -0.03958645286504356), 0.9874574894685292)
 *            predict (VectorD (50.0)) = 8.09724678182599
 */
object NonLinRegressionTest extends App
{
    // 5 data points: constant term, x1 coordinate, x2 coordinate
    val x = new MatrixD ((15, 1), 2.0, 5.0, 7.0, 10.0, 14.0, 19.0, 26.0, 31.0, 34.0, 38.0, 45.0, 52.0, 53.0, 60.0, 65.0)
    val y = VectorD (54.0, 50.0, 45.0, 37.0, 35.0, 25.0, 20.0, 16.0, 18.0, 13.0, 8.0, 11.0, 8.0, 4.0, 6.0)

    println ("x = " + x)
    println ("y = " + y)

    def f (x: VectorD, b: VectorD): Double = b(0) * exp (b(1) * x(0))   // non-linear regression function
    val b_init = VectorD (4.04, .038)                                   // initial guess for parameter vector b

    val rg = new NonLinRegression (x, y, f, b_init)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = VectorD (1); z(0) = 50.0              // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

} // NonRegressionTest object

