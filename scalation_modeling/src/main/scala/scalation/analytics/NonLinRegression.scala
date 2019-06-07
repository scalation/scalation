
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.log

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.stat.Statistic
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
 *  @param x       the data/input matrix augmented with a first column of ones
 *  @param y       the response/output vector
 *  @param f       the non-linear function f(x, b) to fit
 *  @param b_init  the initial guess for the parameter vector b
 *  @param fname_  the feature/variable names
 */
class NonLinRegression (x: MatriD, y: VectoD, f: (VectoD, VectoD) => Double,
                        b_init: VectorD, fname_ : Strings = null)            // FIX - should be trait - currently fails
      extends PredictorMat (x, y, fname_)
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG  = false                                    // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function to compute the Sum of Squares Error 'SSE' for given values for
     *  the parameter vector 'b'.
     *  @param b  the parameter vector
     */
    def sseF (b: VectoD): Double =
    {
        val yp = VectorD (for (i <- x.range1) yield f(x(i), b))   // create vector yp of predicted responses
        e = y - yp                                                // residual/error vector
        e dot e                                                   // residual/error sum of squares
    } // sseF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  non-linear regression equation for the response vector 'yy'.
     *  <p>
     *      y = f(x, b)
     *  <p>
     *  using the least squares method.
     *  Caveat:  Optimizer may converge to an unsatisfactory local optima.
     *           If the regression can be linearized, use linear regression for
     *           starting solution.
     *  @param yy  the response vector to work with
     */
    def train (yy: VectoD = y): NonLinRegression =
    {
        val bfgs = new QuasiNewton (sseF)                      // minimize sse using NLP
        b        = bfgs.solve (b_init)                         // estimate for b from optimizer

        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = f(z, b)',
     *  i.e., '(b0, b1) dot (1.0, z1)'.
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = f(z, b)

    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("NonLinRegression does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("NonLinRegression does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new NonLinRegression (x, y, f, b_init, fname),
                                                 xx, k, rando)
    } // crossVal

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
    rg.train ().eval ()
    println ("fit = " + rg.fit)

    val z  = VectorD (1); z(0) = 50.0             // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

} // NonRegressionTest object

