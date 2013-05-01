
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, QRDecomp, VectorD}
import scalation.math.DoubleWithExp._
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Regression class supports multiple linear regression.  In this case, x
 *  is multi-dimensional (1, x1, ... xk).  Fit the parameter vector b in the
 *  regression equation
 *      y  =  b dot x + e  =  b0 + b1 * x1 + ... bk * xk + e
 *  where e represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *      b  =  x_pinv * y
 *  where x_pinv is the pseudo-inverse.
 *  By default QR Decomposition (more robust) is used to compute x_pinv, with
 *  Gaussian Elimination as an option (set useQR to false).
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x      the input/design matrix augmented with a first column of ones
 *  @param y      the response vector
 *  @param useQR  use QR Decomposition for pseudo-inverse, else Gaussian Elimination
 */
class Regression (x: MatrixD, y: VectorD, useQR: Boolean = true)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                   // debug flag
    private val k          = x.dim2 - 1              // number of variables 
    private val n          = x.dim1.toDouble         // number of data points (rows)
    private val r_df       = (n-1.) / (n-k-1.)       // ratio of degrees of freedom
    private var b: VectorD = null                    // parameter vector (b0, b1, ... bk)
    private var rSquared   = -1.                     // coefficient of determination (quality of fit)
    private var rBarSq     = -1.                     // Adjusted R-squared
    private var fStat      = -1.                     // F statistic (quality of fit)

    private val x_pinv  = if (useQR) {               // pseudo-inverse of x
                              val (q, r) = (new QRDecomp (x)).getQR; r.inverse * q.t
                          } else {
                              (x.t * x).inverse * x.t
                          } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y = b dot x + e  =  (b0, ... bk) dot (1., x1 , ... xk) + e
     *  using the least squares method.
     */
    def train ()
    {
        b        = x_pinv * y                        // parameter vector (b0, b1, ... bk)
        val e    = y - x * b                         // residual/error vector
        val sse  = e dot e                           // residual/error sum of squares
        val sst  = (y dot y) - y.sum~^2. / n         // total sum of squares
        val ssr  = sst - sse                         // regression sum of squares
        rSquared = ssr / sst                         // coefficient of determination (R-squared)
        rBarSq   = 1. - (1.-rSquared) * r_df         // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (n-k-1.)  / (sse * k)       // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      yy = b dot x + e  =  (b0, ... bk) dot (1., x1 , ... xk) + e
     *  using the least squares method.
     *  @param yy  the new response vector
     */
    def train (yy: VectorD)
    {
        b        = x_pinv * yy                       // parameter vector (b0, b1, ... bk)
        val e    = yy - x * b                        // residual/error vector
        val sse  = e dot e                           // residual/error sum of squares
        val sst  = (yy dot yy) - yy.sum~^2. / n      // total sum of squares
        val ssr  = sst - sse                         // regression sum of squares
        rSquared = ssr / sst                         // coefficient of determination
        rBarSq   = 1. - (1.-rSquared) * r_df         // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (n-k-1.)  / (sse * k)       // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit rSquared)
     */
    def fit: Tuple4 [VectorD, Double, Double, Double] = (b, rSquared, rBarSq, fStat)

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): Tuple4 [Int, VectorD, Double, Double] =
    {
        var j_max   = -1                     // index of variable to eliminate
        var b_max: VectorD = null            // parameter values for best solution
        var rSq_max = -1.                    // currently maximizing R squared
        var fS_max  = -1.                    // could optimize on F statistic

        for (j <- 1 to k) {
            val keep = n.toInt               // i-value large enough to not exclude any rows in slice
            val rg_j = new Regression (x.sliceExclude (keep, j), y)       // regress with x_j removed
            rg_j.train ()
            val (b, rSq, fS, rBar) =  rg_j.fit
            if (rSq > rSq_max) { j_max = j; b_max = b; rSq_max = rSq; fS_max = fS}
        } // for
        (j_max, b_max, rSq_max, fS_max)
    } // backElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-colinearity by regressing xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the varaince of xj can be predicted
     *  from the other variables, so xj is a candidate for removal from the model.
     */
    def vif: VectorD =
    {
        val vifV = new VectorD (k)           // VIF vector
        for (j <- 1 to k) {
            val keep = n.toInt               // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                           // x_j is jth column in x
            val rg_j = new Regression (x.sliceExclude (keep, j), x_j)     // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1. / (1. - rg_j.fit._2)                      // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif

} // Regression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test Regression class: y  =  b dot x  =  b0 + b1*x1 + b2*x2.
 *  Test regression and backward elimination.
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

    val z  = new VectorD (1., 20., 80.)         // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                    // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

    println ("backElim = " + rg.backElim ())    // eliminate least predictive variable

} // RegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test Regression class:  y = b dot x = b0 + b1*x1 + b2*x2.
 *  Test regression using QR Decomposition and Gaussian Elimination for computing
 *  the pseudo-inverse.
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

    val rg = new Regression (x, y)           // use QR Decomposition for pseudo-inverse
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = new VectorD (1., 2., 3.)        // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                 // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

    val rg2 = new Regression (x, y, false)   // use Gaussian Elimination for pseudo-inverse
    rg2.train ()
    println ("fit = " + rg2.fit)

} // RegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test the multi-colinearity method in the Regression class.
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 */
object  RegressionTest3 extends App
{
    // 20 data points:     Constant     x1       x2     x3     x4
    //                                 Age   Weight    Dur Stress
    val x = new MatrixD ((20, 5), 1.,   47.,   85.4,   5.1,    33.,
                                  1.,   49.,   94.2,   3.8,    14.,
                                  1.,   49.,   95.3,   8.2,    10.,
                                  1.,   50.,   94.7,   5.8,    99.,
                                  1.,   51.,   89.4,   7.0,    95.,
                                  1.,   48.,   99.5,   9.3,    10.,
                                  1.,   49.,   99.8,   2.5,    42.,
                                  1.,   47.,   90.9,   6.2,     8.,
                                  1.,   49.,   89.2,   7.1,    62.,
                                  1.,   48.,   92.7,   5.6,    35.,
                                  1.,   47.,   94.4,   5.3,    90.,
                                  1.,   49.,   94.1,   5.6,    21.,
                                  1.,   50.,   91.6,  10.2,    47.,
                                  1.,   45.,   87.1,   5.6,    80.,
                                  1.,   52.,  101.3,  10.0,    98.,
                                  1.,   46.,   94.5,   7.4,    95.,
                                  1.,   46.,   87.0,   3.6,    18.,
                                  1.,   46.,   94.5,   4.3,    12.,
                                  1.,   48.,   90.5,   9.0,    99.,
                                  1.,   56.,   95.7,   7.0,    99.)
    //  response BP
    val y = new VectorD (105., 115., 116., 117., 112., 121., 121., 110., 110., 114.,
                         114., 115., 114., 106., 125., 114., 106., 113., 110., 122.)

    val rg = new Regression (x, y)
    rg.train ()

    println ("fit      = " + rg.fit)        // fit model y = b0 + b1*x1 + b2*x2 + b3*x3 + b4*x4
    println ("vif      = " + rg.vif)        // test multi-colinearity (VIF)

} // RegressionTest3 object

