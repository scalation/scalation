
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Jan 31 20:59:02 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

// FIX - currently only works for Inverse

package scalation.analytics

import math.pow

import scalation.linalgebra.{Fac_Cholesky, Fac_QR, MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.util.{Error, time}

import Centering.center
import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegression` class supports multiple linear regression.  In this
 *  case, 'x' is multi-dimensional [x_1, ... x_k].  Both the input matrix 'x' and 
 *  the response vector 'y' are centered (zero mean).  Fit the parameter vector
 *  'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y   [ alternative: b  =  solve (y) ]
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.  Three techniques are provided:
 *  <p>
 *      Fac_QR         // QR Factorization: slower, more stable (default)
 *      Fac_Cholesky   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      Inverse        // Inverse/Gaussian Elimination, classical textbook technique (outdated)
 *  <p>
 *  see http://statweb.stanford.edu/~tibs/ElemStatLearn/
 *  @param x          the centered input/design m-by-n matrix NOT augmented with a first column of ones
 *  @param y          the centered response vector
 *  @param lambda     the shrinkage parameter (0 => OLS) in the penalty term 'lambda * b dot b'
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class RidgeRegression (x: MatrixD, y: VectorD, lambda: Double = 0.1, technique: RegTechnique = Inverse)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG      = true                               // debug flag
    private val k          = x.dim2 - 1                         // number of variables (k = n-1)
    private val m          = x.dim1.toDouble                    // number of data points (rows)
    private val r_df       = (m-1.0) / (m-k-1.0)                // ratio of degrees of freedom
    private var rSquared   = -1.0                               // coefficient of determination (quality of fit)
    private var rBarSq     = -1.0                               // Adjusted R-squared
    private var fStat      = -1.0                               // F statistic (quality of fit)

    private val fac = technique match {                         // select the factorization technique
        case Fac_QR       => new Fac_QR (x)                     // QR Factorization
        case Fac_Cholesky => new Fac_Cholesky (x.t * x)         // Cholesky Factorization
        case _            => null                               // don't factor, use inverse
    } // match

    private val x_pinv = technique match {                      // pseudo-inverse of x
        case Fac_QR       => val (q, r) = fac.factor (); r.inverse * q.t
        case Fac_Cholesky => fac.factor (); null                // don't compute it directly
        case _            => (xtx).inverse * x.t                // classic textbook technique
    } // match

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute x.t * x and add lambda to the diagonal
     */
    def xtx: MatrixD = { val a = x.t * x; for (i <- 0 until a.dim1) a(i, i) += lambda; a }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y  =  b dot x + e  =  [b_1, ... b_k] dot [x_1, ... x_k] + e
     *  using the least squares method.
     */
    def train ()
    {
        b        = if (x_pinv == null) fac.solve (y)
                   else x_pinv * y                              // x parameter vector [b_1, ... b_k]
        val e    = y - x * b                                    // residual/error vector
        val sse  = e dot e                                      // residual/error sum of squares
        val sst  = (y dot y) - pow (y.sum, 2) / m               // total sum of squares
        val ssr  = sst - sse                                    // regression sum of squares
        rSquared = ssr / sst                                    // coefficient of determination (R-squared)
        rBarSq   = 1.0 - (1.0-rSquared) * r_df                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k)                 // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      yy  =  b dot x + e  =  [b_1, ... b_k] dot [x_1, ... x_k] + e
     *  using the least squares method.
     *  @param yy  the new response vector
     */
    def train (yy: VectorD)
    {
        b        = if (x_pinv == null) fac.solve (yy)
                   else x_pinv * yy                             // x parameter vector [b_1, ... b_k]
        val e    = yy - x * b                                   // residual/error vector
        val sse  = e dot e                                      // residual/error sum of squares
        val sst  = (yy dot yy) - pow (yy.sum, 2) / m            // total sum of squares
        val ssr  = sst - sse                                    // regression sum of squares
        rSquared = ssr / sst                                    // coefficient of determination
        rBarSq   = 1.0 - (1.0-rSquared) * r_df                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k)                 // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit including rSquared).
     */
    def fit: VectorD = VectorD (rSquared, rBarSq, fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula below.
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    override def predict (z: MatriD): VectoD = z * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new quality of fit.
     */
    def backElim (): Tuple3 [Int, VectorD, VectorD] =
    {
        var j_max   = -1                              // index of variable to eliminate
        var b_max: VectorD = null                     // parameter values for best solution
        var ft_max = VectorD (3); ft_max.set (-1.0)   // optimize on quality of fit (ft(0) is rSquared)

        for (j <- 1 to k) {
            val keep = m.toInt                        // i-value large enough to not exclude any rows in slice
            val rg_j = new RidgeRegression (x.sliceExclude (keep, j), y)       // regress with x_j removed
            rg_j.train ()
            val b  = rg_j.coefficient
            val ft = rg_j.fit
            if (ft(0) > ft_max(0)) { j_max = j; b_max = b; ft_max = ft }
        } // for
        (j_max, b_max, ft_max)
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
            val keep = m.toInt               // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                           // x_j is jth column in x
            val rg_j = new RidgeRegression (x.sliceExclude (keep, j), x_j)     // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit(0))                        // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif

} // RidgeRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2.
 *  <p>
 *  Test regression and backward elimination.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 */
object RidgeRegressionTest extends App
{
    // 5 data points: x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 2), 36.0,  66.0,               // 5-by-3 matrix
                                 37.0,  68.0,
                                 47.0,  64.0,
                                 32.0,  53.0,
                                  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    // Compute centered (zero mean) versions of x, y and z

    val mu_x = x.mean                 // columnwise mean of x
    val mu_y = y.mean                 // mean of y
    val mu_z = z.mean                 // mean of z
    val x_c  = center (x, mu_x)       // centered x (columnwise)
    val y_c  = y - mu_y               // centered y
    val z_c  = z - mu_z               // centered y

    println ("x_c = " + x_c + "\ny_c = " + y_c + "\nz_c = " + z_c)

    val rrg = new RidgeRegression (x_c, y_c)
    rrg.train ()
    println ("fit = " + rrg.fit)
    val yp = rrg.predict (z_c) + mu_y                     // predict y for one point
    println ("predict (" + z + ") = " + yp)

    val yyp = rrg.predict (x_c) + mu_y                    // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(0), y, yyp)
    new Plot (x.col(1), y, yyp)

    println ("reduced model: fit = " + rrg.backElim ())   // eliminate least predictive variable

} // RidgeRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest2` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  Test regression using QR Decomposition and Gaussian Elimination for computing
 *  the pseudo-inverse.
 */
object RidgeRegressionTest2 extends App
{
    // 4 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((4, 2), 1.0, 1.0,                  // 4-by-3 matrix
                                 1.0, 2.0,
                                 2.0, 1.0,
                                 2.0, 2.0)
    val y = VectorD (6.0, 8.0, 7.0, 9.0)
    val z = VectorD (2.0, 3.0)
    var rrg: RidgeRegression = null

    println ("x = " + x)
    println ("y = " + y)

    println ("-------------------------------------------------")
    println ("Fit the parameter vector b using QR Factorization")
    rrg = new RidgeRegression (x, y)
    rrg.train ()
    println ("fit = " + rrg.fit)
    val yp = rrg.predict (z)                         // predict y for on3 point
    println ("predict (" + z + ") = " + yp)

    val yyp = rrg.predict (x)                        // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

} // RidgeRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest3` object tests the multi-colinearity method in the
 *  `RidgeRegression` class using the following regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 */
object RidgeRegressionTest3 extends App
{
    // 20 data points:             x_1     x_2    x_3      x_4
    //                             Age  Weight    Dur   Stress
    val x = new MatrixD ((20, 4), 47.0,   85.4,   5.1,    33.0,
                                  49.0,   94.2,   3.8,    14.0,
                                  49.0,   95.3,   8.2,    10.0,
                                  50.0,   94.7,   5.8,    99.0,
                                  51.0,   89.4,   7.0,    95.0,
                                  48.0,   99.5,   9.3,    10.0,
                                  49.0,   99.8,   2.5,    42.0,
                                  47.0,   90.9,   6.2,     8.0,
                                  49.0,   89.2,   7.1,    62.0,
                                  48.0,   92.7,   5.6,    35.0,
                                  47.0,   94.4,   5.3,    90.0,
                                  49.0,   94.1,   5.6,    21.0,
                                  50.0,   91.6,  10.2,    47.0,
                                  45.0,   87.1,   5.6,    80.0,
                                  52.0,  101.3,  10.0,    98.0,
                                  46.0,   94.5,   7.4,    95.0,
                                  46.0,   87.0,   3.6,    18.0,
                                  46.0,   94.5,   4.3,    12.0,
                                  48.0,   90.5,   9.0,    99.0,
                                  56.0,   95.7,   7.0,    99.0)
    //  response BP
    val y = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
                     114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

    val rrg = new RidgeRegression (x, y)
    time { rrg.train () }

    println ("fit      = " + rrg.fit)       // fit model y = b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4*x_4
    println ("vif      = " + rrg.vif)       // test multi-colinearity (VIF)

} // RidgeRegressionTest3 object

