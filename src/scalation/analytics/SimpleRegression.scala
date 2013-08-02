
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Sep 24 19:00:23 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.DoubleWithExp._
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SimpleRegression class supports simple linear regression.  In this case,
 *  x is one-dimensional (1, x1).  Fit the parameter vector b in the regression
 *  equation y = b dot x + e = (b0, b1) dot (1., x1) + e = b0 + b1 * x1 + e
 *  where e represents the residuals (the part not explained by the model).
 *  @param x  the input/design matrix augmented with a first column of ones
 *  @param y  the response vector
 */
class SimpleRegression (x: MatrixD, y: VectorD)
      extends Predictor with Error
{
    if (x.dim2 != 2)     flaw ("constructor", "design matrix must have 2 columns")
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val n = x.dim1.toDouble    // number of data points (rows)
    private val b = new VectorD (2)    // parameter vector (b0, b1)

    private var rSquared = -1.         // coefficient of determination (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simple regression equation
     *      y = b dot x + e  = (b0, b1) dot (1., x1) + e
     *  using the least squares method.
     *  @see http://www.analyzemath.com/statistics/linear_regression.html
     */
    def train ()
    {
        val x1  = x.col(1)                     // get column 1 of x = [(1., x1)]
        val sx  = x1.sum                       // sum of x values
        val sy  = y.sum                        // sum of y values
        val ssx = x1 dot x1                    // sum of squares x
        val ssy = y dot y                      // sum of squares y
        val sxy = x1 dot y                     // sum of cross products

        b(1) = (n * sxy - sx * sy) / (n * ssx - sx~^2.)     // slope
        b(0) = (sy - b(1) * sx) / n                         // intercept

        val e   = y - x * b                    // residual/error vector
        val sse = e dot e                      // residual/error sum of squares
        val sst = ssy - sy~^ 2. / n            // total sum of squares
        rSquared = (sst - sse) / sst           // coefficient of determination
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit rSquared)
     */
    def fit: Tuple2 [VectorD, Double] = (b, rSquared)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  i.e., (b0, b1) dot (1., z1).
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatrixD): VectorD = z * b

} // SimpleRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test SimpleRegression class:  y = b dot x = (b0, b1) dot (1., x1).
 *  @see http://www.analyzemath.com/statistics/linear_regression.html
 */
object SimpleRegressionTest extends App
{
    // 5 data points: constant term, x1 coordinate
    val x = new MatrixD ((5, 2), 1., 0.,           // 5-by-2 matrix
                                 1., 1.,
                                 1., 2.,
                                 1., 3.,
                                 1., 4.)
    val y = VectorD (2., 3., 5., 4., 6.)

    println ("x = " + x)
    println ("y = " + y)

    val rg = new SimpleRegression (x, y)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = VectorD (1., 5.)               // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)

} // SimpleRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test SimpleRegression class:  y = b dot x = b0 + b1*x1.
 *  @see http://mathbits.com/mathbits/tisection/Statistics2/linear.htm
 */
object SimpleRegressionTest2 extends App
{
    // 20 data points: just x1 coordinate
    val x1 = VectorD (  4.,   9.,  10.,  14.,   4.,   7.,  12.,  22.,   1.,   3.,
                        8.,  11.,   5.,   6.,  10.,  11.,  16.,  13.,  13.,  10.)
    val y  = VectorD (390., 580., 650., 730., 410., 530., 600., 790., 350., 400.,
                      590., 640., 450., 520., 690., 690., 770., 700., 730., 640.)

    println ("x1 = " + x1)
    println ("y  = " + y)

    val x  = MatrixD.form_cw (1., x1)       // form matrix x from vector x1
    val rg = new SimpleRegression (x, y)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = VectorD (1., 15.)             // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                // predict y for several points
    println ("predict (" + x + ") = " + yyp)
    
    new Plot (x1, y, yyp)

} // SimpleRegressionTest2 object

