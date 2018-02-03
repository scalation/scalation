
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Jan 31 20:59:02 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Ridge Regression using QR
 *  @see math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
 *  Ridge Regression using SVD
 *  @see Hastie, T., Tibshirani, R., & Friedman, J. (2009). The Elements of Statistical Learning
 */

package scalation.analytics

import scala.math.{log, sqrt}

import scalation.linalgebra._
import scalation.minima.GoldenSectionLS
import scalation.plot.Plot
import scalation.util.{banner, Error, time}

import Centering.center
import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegression` class supports multiple linear ridge regression.
 *  In this case, 'x' is multi-dimensional [x_1, ... x_k].  Ridge regression puts
 *  a penalty on the L2 norm of the parameters b to reduce the chance of them taking
 *  on large values that may lead to less robust models.  Both the input matrix 'x'
 *  and the response vector 'y' are centered (zero mean).  Fit the parameter vector
 *  'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the regularized Normal Equations:
 *  <p>
 *      b  =  fac.solve (.)  with regularization  x.t * x + λ * I
 *  <p>
 *  Five factorization techniques are provided:
 *  <p>
 *      'QR'         // QR Factorization: slower, more stable (default)
 *      'Cholesky'   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      'SVD'        // Singular Value Decomposition: slowest, most robust
 *      'LU'         // LU Factorization: similar, but better than inverse
 *      'Inverse'    // Inverse/Gaussian Elimination, classical textbook technique 
 *  <p>
 *  @see statweb.stanford.edu/~tibs/ElemStatLearn/
 *  @param x          the centered input/design m-by-n matrix NOT augmented with a first column of ones
 *  @param y          the centered response m-vector
 *  @param lambda_    the shrinkage parameter (0 => OLS) in the penalty term 'lambda * b dot b'
 *  @param technique  the technique used to solve for b in (x.t*x + lambda*I)*b = x.t*y
 */
class RidgeRegression [MatT <: MatriD, VecT <: VectoD] (x: MatT, y: VecT, lambda_ : Double = 0.1,
                       technique: RegTechnique = Cholesky)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG  = true                                  // debug flag
    private val k      = x.dim2 - 1                            // number of variables (k = n-1)
    private val m      = x.dim1.toDouble                       // number of data points (rows)
    private val df     = (m - k - 1).toInt                     // degrees of freedom
    private val r_df   = (m-1.0) / df                          // ratio of degrees of freedom

    private var rBarSq = -1.0                                  // adjusted R-squared
    private var fStat  = -1.0                                  // F statistic (quality of fit)
    private var aic    = -1.0                                  // Akaike Information Criterion (AIC)
    private var bic    = -1.0                                  // Bayesian Information Criterion (BIC)

    //* Compute x.t * x and add lambda to the diagonal
    private val xtx    = x.t * x
    private val lambda = if (lambda_ <= 0.0) gcv (y) else lambda_
    private var xtx_ : MatT = null.asInstanceOf [MatT]; xtx_λI (lambda)
    private val ey     = MatrixD.eye (x.dim1, x.dim2)          // identity matrix

    type Fac_QR = Fac_QR_H [MatT]                              // change as needed

    // SVD not yet implemented - FIX check implementations
    private val fac: Factorization = technique match {         // select the factorization technique
        case QR       => { val x_ = (x ++ (ey * sqrt (lambda))).asInstanceOf [MatT];
                           new Fac_QR (x_) }                   // QR Factorization
        case Cholesky => new Fac_Cholesky (xtx_)               // Cholesky Factorization
        case _        => new Fac_Inv (xtx_)                    // Inverse Factorization
    } // match
    fac.factor ()                                              // factor the matrix, either X or X.t * X

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute x.t * x + λI.
     *  @param λ  the shrinkage parameter
     */
    def xtx_λI (λ: Double)
    {
        xtx_ = xtx.copy ().asInstanceOf [MatT]
        for (i <- xtx_.range1) xtx_(i, i) += λ
    } // xtx_λI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_1, ... b_k] dot [x_1, ... x_k] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD): RidgeRegression [MatT, VecT] =
    {
        b = technique match {                                  // solve for coefficient vector b
            case QR       => fac.solve (yy ++ new VectorD (y.dim))  // FIX - give formula
            case Cholesky => fac.solve (x.t * yy)                   // L * L.t * b = X.t * yy
            case _        => fac.solve (x.t * yy)                   // b = (X.t * X)^-1 * X.t * yy
        } // match
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation using the least squares method on 'y'.
     */
    def train (): RidgeRegression [MatT, VecT] = train (y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param yy   the response vector
     */
    def eval (yy: VectoD = y)
    {
        e = yy - x * b                                         // compute residual/error vector e
        diagnose (yy)                                          // compute diagnostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal value for the shrinkage parameter 'λ' using Generalized
     *  Cross Validation (GCV).
     *  @param yy the response vector
     */
    def gcv (yy: VectoD): Double =
    {
        def f_sse (λ: Double): Double = 
        {
            xtx_λI (λ)
            train (yy)
            if (sse.isNaN) throw new ArithmeticException ("sse is NaN")
            sse
        } // f_sse

        val gs = new GoldenSectionLS (f_sse _)
        gs.search ()
    } // gcv

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     */
    override protected def diagnose (yy: VectoD)
    {
        super.diagnose (yy)
        rBarSq = 1.0 - (1.0-rSq) * r_df                        // R-bar-squared (adjusted R-squared)
        fStat  = ((sst - sse) * df) / (sse * k)                // F statistic (msr / mse)
        aic    = m * log (sse) - m * log (m) + 2.0 * (k+1)     // Akaike Information Criterion (AIC)
        bic    = aic + (k+1) * (log (m) - 2.0)                 // Bayesian Information Criterion (BIC)
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    override def fit: VectoD = super.fit.asInstanceOf [VectorD] ++ VectorD (rBarSq, fStat, aic, bic)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = super.fitLabels ++ Seq ("rBarSq", "fStat", "aic", "bic")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula below.
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    def forwardSel (cols: Set [Int]): (Int, VectoD, VectoD) =
    {
        // FIX - update implementation
        null
    } // forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable from
     *  the existing model, returning the variable to eliminate, the new parameter
     *  vector, the new quality of fit.  May be called repeatedly.
     *  FIX - update implementation
     *  @param cols  the columns of matrix x to be included in the existing model
     */
    def backwardElim (cols: Set [Int]): (Int, VectoD, VectoD) =
    {
        val ir    =  index_rSq                                  // fit(ir) is rSq
        var j_max = -1                                          // index of variable to eliminate
        var b_max: VectoD = null                                // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabels.size)(-1.0)        // optimize on quality of fit

        for (j <- 1 to k) {
            val keep = m.toInt                                  // i-value large enough to not exclude any rows in slice
            val rg_j = new RidgeRegression (x.sliceExclude (keep, j), y)    // regress with x_j removed
            rg_j.train ().eval ()
            val b  = rg_j.coefficient
            val ft = rg_j.fit
            if (ft(ir) > ft_max(ir)) { j_max = j; b_max = b; ft_max = ft }
        } // for
        (j_max, b_max, ft_max)
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectoD =
    {
        val ir   = index_rSq                                    // fit(ir) is rSq
        val vifV = new VectorD (k)                              // VIF vector
        for (j <- 1 to k) {
            val keep = m.toInt                                  // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                               // x_j is jth column in x
            val rg_j = new RidgeRegression (x.sliceExclude (keep, j), x_j)    // regress with x_j removed
            rg_j.train ().eval ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit(ir))             // store vif for x_1 in vifV(0)
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
    // 5 data points:             x_0    x_1
    val x = new MatrixD ((5, 2), 36.0,  66.0,               // 5-by-2 matrix
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
    val z_c  = z - mu_z               // centered z

    println ("x_c = " + x_c + "\ny_c = " + y_c + "\nz_c = " + z_c)

    val rrg = new RidgeRegression (x_c, y_c)
    rrg.train ().eval ()
    println (rrg.fitLabels)
    println ("fit = " + rrg.fit)
    val yp = rrg.predict (z_c) + mu_y                     // predict y for one point
    println ("predict (" + z + ") = " + yp)

    val cols = Set (0) ++ Array.range (1, x.dim2)
    println ("reduced model: fit = " + rrg.backwardElim (cols))   // eliminate least predictive variable

} // RidgeRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest2` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 */
object RidgeRegressionTest2 extends App
{
    // 4 data points:            x_1  x_2
    val x = new MatrixD ((4, 2), 1.0, 1.0,                  // 4-by-2 matrix
                                 1.0, 2.0,
                                 2.0, 1.0,
                                 2.0, 2.0)
    val y = VectorD (6.0, 8.0, 7.0, 9.0)
    val z = VectorD (2.0, 3.0)

    println ("x = " + x)
    println ("y = " + y)

    banner ("Fit the parameter vector b")
    val rrg = new RidgeRegression (x, y)
    rrg.train ().eval ()
    println ("coefficient = " + rrg.coefficient)
    println ("              " + rrg.fitLabels)
    println ("fit         = " + rrg.fit)

    val yp = rrg.predict (z)                         // predict y for one point
    println ("predict (" + z + ") = " + yp)

} // RidgeRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest3` object tests the multi-collinearity method in the
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
    time { rrg.train ().eval () }

    println ("coefficient = " + rrg.coefficient)
    println ("              " + rrg.fitLabels)
    println ("fit         = " + rrg.fit)           // fit model y = b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4*x_4

    println ("vif         = " + rrg.vif)           // test multi-collinearity (VIF)

} // RidgeRegressionTest3 object

