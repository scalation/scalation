
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Sep 24 19:00:23 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` class supports simple linear regression.  In this case,
 *  the vector 'x' consists of the constant one and a single variable 'x_1', i.e.,
 *  (1, x_1).  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  (b_0, b_1) dot (1, x_1) + e  =  b_0 + b_1 * x_1 + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  @param x  the input/design matrix augmented with a first column of ones
 *  @param y  the response vector
 */
class SimpleRegression (x: MatrixD, y: VectorD)
      extends Predictor with Error
{
    if (x.dim2 != 2)     flaw ("constructor", "design matrix must have 2 columns")
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val k        = 1                              // number of variables (k = n-1 where n = 2)
    private val m        = x.dim1.toDouble                // number of data points (rows)
    private val r_df     = (m-1.0) / (m-2.0)              // ratio of degrees of freedom

    private var rBarSq   = -1.0                           // adjusted R-squared
    private var fStat    = -1.0                           // F statistic (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simple regression equation
     *  <p>
     *      y = b dot x + e  = (b_0, b_1) dot (1, x_1) + e
     *  <p>
     *  using the least squares method.
     *  @see www.analyzemath.com/statistics/linear_regression.html
     *  @param yy  the response vector
     */
    def train (yy: VectoD)
    {
        val x1  = x.col(1)                                // get column 1 of x = [(1.0, x1)]
        val sx  = x1.sum                                  // sum of x values
        val sy  = y.sum                                   // sum of y values
        val ssx = x1 dot x1                               // sum of squares x
        val ssy = y dot y                                 // sum of squares y
        val sxy = x1 dot y                                // sum of cross products

        b = new VectorD (2)                               // parameter vector (b_0, b_1)
        b(1) = (m * sxy - sx * sy) / (m * ssx - sx*sx)    // slope
        b(0) = (sy - b(1) * sx) / m                       // intercept

        e = y - x * b                                     // residual/error vector
        diagnose (y)                                      // compute diagnostics
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simple regression equation for the response passed into the class 'y'.
     */
    def train () { train (y) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     */
    override def diagnose (yy: VectoD)
    {
        super.diagnose (yy)
        rBarSq   = 1.0 - (1.0-rSq) * r_df                 // R-bar-squared (adjusted R-squared)
        fStat    = (sst - sse) * (m-2.0) / sse            // F statistic (msr / mse)
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including.
     */
    override def fit: VectorD = super.fit.asInstanceOf [VectorD] ++ VectorD (rBarSq, fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = super.fitLabels ++ Seq ("rBarSq", "fStat")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  i.e.0, (b_0, b_1) dot (1, z_1).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

} // SimpleRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` companion object provides a simple factory method
 *  for building simple regression linear regression models.
 */
object SimpleRegression
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simple Linear Regression model, automatically prepending the
     *  column of ones (form matrix from two column vectors [ 1 x ]).
     *  @param x  the input/design m-by-1 vector
     *  @param y  the response m-vector
     */
    def apply (x: VectorD, y: VectorD): SimpleRegression =
    {
        new SimpleRegression (MatrixD.form_cw (1.0, x), y)
    } // apply

} // SimpleRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest` object to test the `SimpleRegression` class:
 *  <p>
 *      y = b0 + b1 * x
 *  <p>
 *  > run-main scalation.analytics.SimpleRegressionTest
 */
object SimpleRegressionTest extends App
{
    // 4 data points:
    val x = VectorD (1, 2, 3, 4)
    val y = VectorD (1, 3, 3, 4)
//  val y = VectorD (1, 3, 2, 4)

    println ("x = " + x)
    println ("y = " + y)

    val rg = SimpleRegression (x, y)
    rg.train ()

    println ("coefficient = " + rg.coefficient)
    println ("            = " + rg.fitLabels)
    println ("fit         = " + rg.fit)

} // SimpleRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest2` object to test the `SimpleRegression` class:
 *  <p>
 *      y = b dot x = (b_0, b_1) dot (1, x_1).
 *  <p>
 *  @see http://www.analyzemath.com/statistics/linear_regression.html
 *  > run-main scalation.analytics.SimpleRegressionTest2
 */
object SimpleRegressionTest2 extends App
{
    // 5 data points:       constant  x_1
    val x = new MatrixD ((5, 2), 1.0, 0.0,          // x 5-by-2 matrix
                                 1.0, 1.0,
                                 1.0, 2.0,
                                 1.0, 3.0,
                                 1.0, 4.0)
    val y = VectorD (2.0, 3.0, 5.0, 4.0, 6.0)       // y vector

    println ("x = " + x)
    println ("y = " + y)

    val rg = new SimpleRegression (x, y)
    rg.train ()

    println ("coefficient = " + rg.coefficient)
    println ("            = " + rg.fitLabels)
    println ("fit         = " + rg.fit)

    val z  = VectorD (1.0, 5.0)               // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = VectorD (for (i <- x.range1) yield rg.predict (x(i)))    // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)

} // SimpleRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest3` object to test the `SimpleRegression` class:
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1.
 *  <p>
 *  @see http://mathbits.com/mathbits/tisection/Statistics2/linear.htm
 *  > run-main scalation.analytics.SimpleRegressionTest3
 */
object SimpleRegressionTest3 extends App
{
    // 20 data points: just x_1 coordinate
    val x1 = VectorD (  4.0,   9.0,  10.0,  14.0,   4.0,   7.0,  12.0,  22.0,   1.0,   3.0,
                        8.0,  11.0,   5.0,   6.0,  10.0,  11.0,  16.0,  13.0,  13.0,  10.0)
    val y  = VectorD (390.0, 580.0, 650.0, 730.0, 410.0, 530.0, 600.0, 790.0, 350.0, 400.0,
                      590.0, 640.0, 450.0, 520.0, 690.0, 690.0, 770.0, 700.0, 730.0, 640.0)

    println ("x1 = " + x1)
    println ("y  = " + y)

    val x  = MatrixD.form_cw (1.0, x1)       // form matrix x from vector x1

    val rg = new SimpleRegression (x, y)
    rg.train ()

    println ("coefficient = " + rg.coefficient)
    println ("            = " + rg.fitLabels)
    println ("fit         = " + rg.fit)

    val z  = VectorD (1.0, 15.0)             // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = VectorD (for (i <- x.range1) yield rg.predict (x(i)))    // predict y for several points
    println ("predict (" + x + ") = " + yyp)
    
    new Plot (x1, y, yyp)

} // SimpleRegressionTest3 object

