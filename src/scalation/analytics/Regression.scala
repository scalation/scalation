
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Wed Aug 26 18:41:26 EDT 2009, Fri Feb  3 13:19:40 EST 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.DoubleWithExp._
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Regression class supports multiple linear regression.  In this case, x
 *  is multi-dimensional (1, x1, ... xk).  Fit the parameter vector b in the
 *  regression equation y = b dot x + e = b0 + b1 * x1 + ... bk * xk + e
 *  where e represents the residuals (the part not explained by the model).
 *  @param x  the input/design matrix augmented with a first column of ones
 *  @param y  the response vector
 */
class Regression (x: MatrixD, y: VectorD)
      extends Predictor with Error
{
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG = false             // debug flag

    private val n = x.dim1.toDouble       // number of data points (rows)
    private var b: VectorD = null         // parameter vector (b0, b1, ... bk)

    private var rSquared = -1.            // coefficient of determination (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y = b dot x + e  =  (b0, ... bk) dot (1., x1 , ... xk) + e
     *  using the least squares method.
     */
    def train ()
    {
        b = (x.t * x).inverse * x.t * y            // parameter vector (b0, b1, ... bk)

        val e    = y - x * b                       // residual/error vector
        val sse  = e dot e                         // residual/error sum of squares
        val sst  = (y dot y) - y.sum~^2. / n       // total sum of squares
        rSquared = (sst - sse) / sst               // coefficient of determination
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

} // Regression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test Regression class: y = b dot x = b0 + b1*x1 + b2*x2.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 */
object RegressionTest extends App
{
    // 5 data points: constant term, x1 coordinate, x2 coordinate
    val x = new MatrixD ((5, 3), 1., 36.,  66.,               // 5-by-3 matrix
                                 1., 37.,  68.,
                                 1., 47.,  64.,
                                 1., 32.,  53.,
                                 1.,  1., 101.)
    val y = new VectorD (745., 895., 442., 440., 1598.)

    println ("x = " + x)
    println ("y = " + y)

    val rg = new Regression (x, y)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = new VectorD (1., 20., 80.)     // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

} // RegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test Regression class:  y = b dot x = b0 + b1*x1 + b2*x2.
 */
object RegressionTest2 extends App
{
    // 4 data points: constant term, x1 coordinate, x2 coordinate
    val x = new MatrixD ((4, 3), 1., 1., 1.,                  // 4-by-3 matrix
                                 1., 1., 2.,
                                 1., 2., 1.,
                                 1., 2., 2.)
    val y = new VectorD (6., 8., 7., 9.)

    println ("x = " + x)
    println ("y = " + y)

    val rg = new Regression (x, y)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = new VectorD (1., 2., 3.)       // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

} // RegressionTest2 object

