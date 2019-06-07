
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Sep 24 19:00:23 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.stat.Statistic
import scalation.util.{banner, Error}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` class supports simple linear regression.  In this case,
 *  the vector 'x' consists of the constant one and a single variable 'x1', i.e.,
 *  (1, x1).  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  [b0, b1] dot [1, x1] + e  =  b0 + b1 * x1 + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  @param x       the data/input matrix augmented with a first column of ones
 *  @param y       the response/output vector
 *  @param fname_  the feature/variable names
 */
class SimpleRegression (x: MatriD, y: VectoD, fname_ : Strings = null)
      extends PredictorMat (x, y, fname_)
{
    if (x.dim2 != 2) flaw ("constructor", "data matrix must have 2 columns: " + x.dim2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simple regression equation
     *  <p>
     *      y = b dot x + e  = [b0, b1] dot [1, x1] + e
     *  <p>
     *  using the least squares method.
     *  @see www.analyzemath.com/statistics/linear_regression.html
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): SimpleRegression =
    {
        val x1  = x.col(1)                                // get column 1 of x = [1.0, x1]
        val sx  = x1.sum                                  // sum of x values
        val sy  = y.sum                                   // sum of y values
        val ssx = x1 dot x1                               // sum of squares x
        val ssy = y dot y                                 // sum of squares y
        val sxy = x1 dot y                                // sum of cross products

        b = new VectorD (2)                               // parameter vector [b0, b1]
        b(1) = (m * sxy - sx * sy) / (m * ssx - sx*sx)    // slope
        b(0) = (sy - b(1) * sx) / m                       // intercept

        this
    } // train
    
    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("SimpleRegression does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("SimpleRegression does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new SimpleRegression (x, y, fname),
                                                 xx, k, rando)
    } // crossVal

} // SimpleRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegression` companion object provides a simple factory method
 *  for building simple regression linear regression models.
 */
object SimpleRegression extends Error
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simple Linear Regression model from a combined data matrix.
     *  When 'xy.dim2 == 2', a column of all ones will be prepended to the matrix
     *  corresponding to the intercept (form matrix from two column vectors [1 x]).
     *  Take the first two columns for the predictor and the last column for the response.
     *  @see `SimplerRegression` for a model without an intercept parameter
     *  @param xy  the combined data matrix
     */
    def apply (xy: MatriD): SimpleRegression =
    {
        val n = xy.dim2 
        if (n < 2) { flaw ("apply", "the length of the 'xy' matrix must be at least 2"); null }
        else if (n == 2) new SimpleRegression (MatrixD.form_cw (1.0, xy.col(0)), xy.col(n-1))
        else new SimpleRegression (xy.sliceCol (0, 2), xy.col(n-1))
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simple Linear Regression model, automatically prepending the
     *  column of ones (form matrix from two column vectors [1 x]).
     *  @param x  the data/input m-by-1 vector
     *  @param y  the response/output m-vector
     */
    def apply (x: VectoD, y: VectoD): SimpleRegression =
    {
        new SimpleRegression (MatrixD.form_cw (1.0, x), y)
    } // apply

} // SimpleRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest` object to test the `SimpleRegression` class:
 *  <p>
 *      y = b0 + b1 * x
 *  <p>
 *  > runMain scalation.analytics.SimpleRegressionTest
 */
object SimpleRegressionTest extends App
{
    // 4 data points:
    val x = VectorD (1, 2, 3, 4)
    val y = VectorD (1, 3, 3, 4)

    println ("x = " + x)

    val rg = SimpleRegression (x, y)                // automatically prepends a column of ones
    rg.train ().eval ()

    banner ("Test1: Simple Regression Model: y = b_0 + b_1 x + e")
    println ("parameter = " + rg.parameter)         // parameter vector b
    println ("fitMap    = " + rg.fitMap)            // quality of fit
    println ("y         = " + y)                    // actual response vector
    println ("yp        = " + rg.predict ())        // predicted response vector
    println ("error     = " + rg.residual)          // error/residual vector = y - yp 

} // SimpleRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest2` object is used to test the `SimpleRegression` class.
 *  <p>
 *      y = b dot x = [b0, b1] dot [1, x1]
 *  <p>
 *  @see http://www.analyzemath.com/statistics/linear_regression.html
 *  > runMain scalation.analytics.SimpleRegressionTest2
 */
object SimpleRegressionTest2 extends App
{
    // 5 data points:       constant  x1
    val x = new MatrixD ((5, 2), 1.0, 0.0,          // x 5-by-2 matrix
                                 1.0, 1.0,
                                 1.0, 2.0,
                                 1.0, 3.0,
                                 1.0, 4.0)
    val y = VectorD (2.0, 3.0, 5.0, 4.0, 6.0)       // y vector

    println ("x = " + x)

    val rg = new SimpleRegression (x, y)
    rg.train ().eval ()

    banner ("Test2: Simple Regression Model: y = b_0 + b_1 x + e")
    println ("parameter = " + rg.parameter)         // parameter vector b
    println ("fitMap    = " + rg.fitMap)            // quality of fit
    println ("y         = " + y)                    // actual response vector
    val yp = rg.predict ()
    println ("yp        = " + yp)                   // predicted response vector
    println ("error     = " + rg.residual)          // error/residual vector = y - yp 

    val z = VectorD (1.0, 5.0)                      // predict y for new point z
    println (s"predict ($z) = ${rg.predict (z)}")

    new Plot (x.col(1), y, yp, "plot y and yp vs. x")

} // SimpleRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest3` object is used to test the `SimpleRegression` class
 *  <p>
 *      y = b dot x = b0 + b1 * x1
 *  <p>
 *  @see http://mathbits.com/mathbits/tisection/Statistics2/linear.htm
 *  > runMain scalation.analytics.SimpleRegressionTest3
 */
object SimpleRegressionTest3 extends App
{
    // 20 data points: just x1 coordinate
    val x1 = VectorD (  4.0,   9.0,  10.0,  14.0,   4.0,   7.0,  12.0,  22.0,   1.0,   3.0,
                        8.0,  11.0,   5.0,   6.0,  10.0,  11.0,  16.0,  13.0,  13.0,  10.0)
    val y  = VectorD (390.0, 580.0, 650.0, 730.0, 410.0, 530.0, 600.0, 790.0, 350.0, 400.0,
                      590.0, 640.0, 450.0, 520.0, 690.0, 690.0, 770.0, 700.0, 730.0, 640.0)

    println ("x1 = " + x1)

    val x = MatrixD.form_cw (1.0, x1)               // form matrix x from vector x1

    val rg = new SimpleRegression (x, y)
    rg.train ().eval ()

    banner ("Test3: Simple Regression Model: y = b_0 + b_1 x + e")
    println ("parameter = " + rg.parameter)         // parameter vector b
    println ("fitMap    = " + rg.fitMap)            // quality of fit
    println ("y         = " + y)                    // actual response vector
    val yp = rg.predict ()
    println ("yp        = " + yp)                   // predicted response vector
    println ("error     = " + rg.residual)          // error/residual vector = y - yp 

    val z  = VectorD (1.0, 15.0)                    // predict y for new point z
    println (s"predict ($z) = ${rg.predict (z)}")

    new Plot (x1, y, yp, "plot y and yp vs. x1")

} // SimpleRegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRegressionTest4` object is used to test the `SimpleRegression` class
 *  <p>
 *      y = b dot x = b0 + b1 * x1
 *  <p>
 *  This version uses gradient descent to search for the optimal solution for b.
 *  > runMain scalation.analytics.SimpleRegressionTest4
 */
object SimpleRegressionTest4 extends App
{
    // 4 data points:
    val x  = new MatrixD ((4, 2), 1, 1,
                                  1, 2,
                                  1, 3,
                                  1, 4)
    val x1 = x.col(1)
    val y  = VectorD (1, 3, 3, 4)
    val _1 = VectorD.one (x1.dim)

    println ("x = " + x)

    val ITER = 10                                   // number of iterations
    val eta  = 0.02                                 // try different values for the learning rate
    val rg   = new SimpleRegression (x, y)          // create a simple regression model, don't train
    var b    = new VectorD (x.dim2)                 // starting point [0, 0] for parameter vector b

    banner (s"Test4: Simple Regression Model: gradient descent: eta = $eta")
    for (it <- 1 to ITER) {
        val yp = x * b
        val e  = y - yp
        val g  = VectorD (_1 dot e, x1 dot e)
        b     += g * eta
        val sse = e dot e
        println (s"for iteration $it, b = $b, sse = $sse")
    } // for

} // SimpleRegressionTest4 object

