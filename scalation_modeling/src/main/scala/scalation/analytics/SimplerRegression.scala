
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Jan  5 14:03:36 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.stat.Statistic
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimplerRegression` class supports simpler linear regression.
 *  In this case,  the vector 'x' consists of a single variable 'x0'.
 *  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  [b0] dot [x0] + e  =  b0 * x0 + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  The simpler regression model has no intercept parameter, only a slope parameter.
 *  @see `SimpleRegression` for both intercept and slope parameters
 *  @param x       the data/input matrix
 *  @param y       the response/output vector
 *  @param fname_  the feature/variable names
 */
class SimplerRegression (x: MatriD, y: VectoD, fname_ : Strings = null)
      extends PredictorMat (x, y, fname_)
{
    if (x.dim2 != 1) flaw ("constructor", "data matrix must have 1 columns: " + x.dim2)

    override protected val k    = 1                       // number of variables
    //override protected val r_df = (m-1.0) / (m-2.0)     // ratio of degrees of freedom

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  simpler regression equation
     *  <p>
     *      y = b dot x + e  = b0 * x0 + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): SimplerRegression =
    {
        val x0  = x.col(0)                                // get column 0 of x = [x0]
        val ssx = x0 dot x0                               // sum of squares x0
        val sxy = x0 dot y                                // sum of cross products x0, y

        b = new VectorD (1)                               // parameter vector [b0]
        b(0) = sxy / ssx                                  // slope

        this
    } // train

    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("SimplerRegression does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("SimplerRegression does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validatio
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new SimplerRegression (x, y, fname),
                                                 xx, k, rando)
    } // crossVal

} // SimplerRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimplerRegression` companion object provides a simple factory method
 *  for building simple regression linear regression models.
 */
object SimplerRegression extends Error
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simpler Linear Regression model from a combined data matrix.
     *  Take the first column for the predictor and the last column for the response.
     *  @param xy  the combined data matrix
     */
    def apply (xy: MatriD): SimplerRegression =
    {
        val n = xy.dim2
        if (n < 2) { flaw ("apply", "the length of the 'xy' matrix must be at least 2"); null }
        else new SimplerRegression (xy.sliceCol (0, 1), xy.col(n-1))
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simpler Linear Regression model, automatically creating a
     *  a data/input matrix from the vector 'x'.
     *  @param x  the data/input m-by-1 vector
     *  @param y  the response/output m-vector
     */
    def apply (x: VectoD, y: VectoD): SimplerRegression =
    {
        val xx = new MatrixD (x.dim, 1)
        xx.setCol (0, x)
        new SimplerRegression (xx, y)
    } // apply

} // SimplerRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimplerRegressionTest` object is used to test the `SimplerRegression` class.
 *  <p>
 *      y = b0 * x + e
 *  <p>
 *  > runMain scalation.analytics.SimplerRegressionTest
 */
object SimplerRegressionTest extends App
{
    // 4 data points:
    val x = VectorD (1, 2, 3, 4)
    val y = VectorD (1, 3, 3, 4)
//  val y = VectorD (1, 3, 2, 4)

    println ("x = " + x)
    println ("y = " + y)

    val rg = SimplerRegression (x, y)
    rg.train ().eval ()

    println ("parameter = " + rg.parameter)
    println ("fitMap    = " + rg.fitMap)

} // SimplerRegressionTest object


    object SimplerRegression_exer_1 extends App
    {
        val x0  = VectorD (1, 2, 3, 4)
        val y   = VectorD (1, 3, 3, 4)
        val b0  = VectorD.range (0, 200) / 100.0
        val sse = new VectorD (b0.dim)
        for (i <- b0.range) {
            val e  = y - x0 * b0(i)
            sse(i) = e dot e
        } // for
        new Plot (b0, sse)
    } // SimplerRegression_exer_1 object



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimplerRegressionTest2` object is used to test the `SimplerRegression` class.
 *  <p>
 *      y = b dot x + e = [b0] dot [x0] + e
 *  <p>
 *  > runMain scalation.analytics.SimplerRegressionTest2
 */
object SimplerRegressionTest2 extends App
{
    // 5 data points:            x0
    val x = new MatrixD ((5, 1), 0.0,               // x 5-by-1 matrix
                                 1.0,
                                 2.0,
                                 3.0,
                                 4.0)
    val y = VectorD (2.0, 3.0, 5.0, 4.0, 6.0)       // y vector

    println ("x = " + x)
    println ("y = " + y)

    val rg = new SimplerRegression (x, y)
    rg.train ().eval ()

    println ("parameter = " + rg.parameter)
    println ("fitMap    = " + rg.fitMap)

    val z  = VectorD (5.0)                                             // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = VectorD (for (i <- x.range1) yield rg.predict (x(i)))    // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(0), y, yyp)

} // SimplerRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimplerRegressionTest3` object is used to test the `SimplerRegression` class.
 *  <p>
 *      y = b dot x = b0 * x0
 *  <p>
 *  @see http://mathbits.com/mathbits/tisection/Statistics2/linear.htm
 *  > runMain scalation.analytics.SimplerRegressionTest3
 */
object SimplerRegressionTest3 extends App
{
    // 20 data points: just x0 coordinate
    val x0 = VectorD (  4.0,   9.0,  10.0,  14.0,   4.0,   7.0,  12.0,  22.0,   1.0,   3.0,
                        8.0,  11.0,   5.0,   6.0,  10.0,  11.0,  16.0,  13.0,  13.0,  10.0)
    val y  = VectorD (390.0, 580.0, 650.0, 730.0, 410.0, 530.0, 600.0, 790.0, 350.0, 400.0,
                      590.0, 640.0, 450.0, 520.0, 690.0, 690.0, 770.0, 700.0, 730.0, 640.0)

    println ("x0 = " + x0)
    println ("y  = " + y)

    val rg = SimplerRegression (x0, y)
    rg.train ().eval ()

    println ("parameter = " + rg.parameter)
    println ("fitMap    = " + rg.fitMap)

    val z  = VectorD (15.0)                                            // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = VectorD (for (i <- x0.range) yield rg.predict (VectorD (x0(i))))    // predict y for several points
    println ("predict (" + x0 + ") = " + yyp)
    
    new Plot (x0, y, yyp)

} // SimplerRegressionTest3 object

