
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T 

// needs to be FIXed

package scalation.analytics

import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
import scalation.plot.Plot
import scalation.random.CDF.studentTCDF
import scalation.util.{Error, time}
import scalation.util.Unicode.sub

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_WLS` class supports weighted multiple linear regression.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Weighted Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
*      b  =  fac.solve (.)
 *  <p>
 *  Four factorization techniques are provided:
 *  <p>
 *      'QR'         // QR Factorization: slower, more stable (default)
 *      'Cholesky'   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      'SVD'        // Singular Value Decomposition: slowest, most robust
 *      'LU'         // LU Factorization: better than Inverse
 *      'Inverse'    // Inverse/Gaussian Elimination, classical textbook technique
 *  <p>
 *  @see www.markirwin.net/stat149/Lecture/Lecture3.pdf
 *  @param x          the input/design m-by-n matrix augmented with a first column of ones
 *  @param y          the response vector
 *  @param w          the weight vector
 *  @param technique  the technique used to solve for b in x.t*w*x*b = x.t*w*y
 */
class Regression_WLS [MatT <: MatriD, VecT <: VectoD] (x: MatT, y: VecT, private var w: VectoD = null,
                      technique: RegTechnique = Cholesky)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 < x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG  = true                                  // debug flag
    private val k      = x.dim2 - 1                            // number of variables (k = n-1)
    private val m      = x.dim1.toDouble                       // number of data points (rows)
    private val df     = (m - k - 1).toInt                     // degrees of freedom
    private val r_df   = (m-1.0) / (m-k-1.0)                   // ratio of degrees of freedom

    private var rBarSq = -1.0                                  // Adjusted R-squared
    private var fStat  = -1.0                                  // F statistic (quality of fit)
    private var aic    = -1.0                                  // Akaike Information Criterion (AIC)
    private var bic    = -1.0                                  // Bayesian Information Criterion (BIC)

    private var stdErr: VectoD = null                          // standard error of coefficients for each x_j
    private var t: VectoD      = null                          // t statistics for each x_j
    private var p: VectoD      = null                          // p values for each x_j

    if (w == null) {
        val ols_y = new Regression (x, y, technique)           // run OLS on data
        ols_y.train ()
        val e = ols_y.residual                                 // deviations/errors
        val r = e.map ((a: Double) => sqrt (abs (a)))          // root absolute deviations (rad's)

        val ols_r = new Regression (x, r, technique)           // run OLS on rad
        ols_r.train ()
        val rp = ols_r.predict (x)                             // predicted rad
        w      = rp.recip                                      // set weight vector for WLS to reciprocal of rp

        if (DEBUG) {
            println ("b_OLS      = " + ols_y.fit)              // Ordinary Least Squares (OLS)
            println ("residual e = " + e)
            println ("rad r      = " + r)
            println ("rad rp     = " + rp)
            println ("weights    = " + w)
        } // if
    } // if

    private val xtw = new MatrixD (x.t)                        // x.t multiplied by weights
    for (i <- w.range) xtw(i) *= w(i)

    type Fac_QR = Fac_QR_H [MatT]                              // change as needed

    private val fac: Factorization = technique match {         // select the factorization technique
//      case QR       => new Fac_QR (x, false)                 // QR Factorization
        case Cholesky => new Fac_Cholesky (xtw * x)            // Cholesky Factorization
//      case SVD      => new SVD (x)                           // Singular Value Decomposition
        case LU       => new Fac_LU (xtw * x)                  // LU Factorization
        case _        => new Fac_Inv (xtw * x)                 // Inverse Factorization
    } // match
    fac.factor ()                                              // factor the matrix, either X or X.t * X

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector ('b'-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  using the weighted least squares 'WLS' method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD)
    {

        b = technique match {                                  // solve for coefficient vector b
//          case QR       => fac.solve (yy)                    // R * b = Q.t * yy
            case Cholesky => fac.solve (xtw * yy)              // L * L.t * b = X.t * W * yy
//          case SVD      => fac.solve (yy)                    // b = V * Σ^-1 * U.t * yy
            case LU       => fac.solve (xtw * yy)              // b = (X.t * W * X) \ X.t * W * yy
            case _        => fac.solve (xtw * yy)              // b = (X.t * W * X)^-1 * X.t * W * yy
        } // match

        e = yy - x * b                                         // compute residuals/ error vector e
        diagnose (yy)                                          // compute diagonostics
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation for the response passed into the class 'y'.
     */
    def train () { train (y) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     */
    override def diagnose (yy: VectoD)
    {
        super.diagnose (yy)
        rBarSq = 1.0 - (1.0-rSq) * r_df                        // R-bar-squared (adjusted R-squared)
        fStat  = ((sst - sse) * df) / (sse * k)                // F statistic (msr / mse)
        aic    = m * log (sse) - m * log (m) + 2.0 * (k+1)     // Akaike Information Criterion (AIC)
        bic    = aic + (k+1) * (log (m) - 2.0)                 // Bayesian Information Criterion (BIC)

        val facCho = if (technique == Cholesky) fac            // reuse Cholesky factorization
                     else new Fac_Cholesky (x.t * x)           // create a Cholesky factorization
        val l_inv  = facCho.factor1 ().inverse                 // take inverse of l from Cholesky factorization
        val varEst = sse / df                                  // variance estimate
        val varCov = l_inv.t * l_inv * varEst                  // variance-covariance matrix

        stdErr = varCov.getDiag ().map (sqrt (_))                          // standard error of coefficients
        t      = b / stdErr                                                // Student's T statistic
        p      = t.map ((x: Double) => 2.0 * studentTCDF (-abs (x), df))   // p values
    } // diagnose

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    override def fit: VectorD = super.fit.asInstanceOf [VectorD] ++ VectorD (rBarSq, fStat, aic, bic)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = super.fitLabels ++ Seq ("rBarSq", "fStat", "aic", "bic")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): Tuple3 [Int, VectoD, VectorD] =
    {
        var j_max   = -1                              // index of variable to eliminate
        var b_max: VectoD = null                      // parameter values for best solution
        var ft_max = VectorD (3); ft_max.set (-1.0)   // optimize on quality of fit (ft(0) is rSquared)

        for (j <- 1 to k) {
            val keep = m.toInt                        // i-value large enough to not exclude any rows in slice
            val rg_j = new Regression (x.sliceExclude (keep, j), y)       // regress with x_j removed
            rg_j.train ()
            val b  = rg_j.coefficient
            val ft = rg_j.fit
            if (ft(0) > ft_max(0)) { j_max = j; b_max = b; ft_max = ft }
        } // for
        (j_max, b_max, ft_max)
    } // backElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectorD =
    {
        val vifV = new VectorD (k)           // VIF vector
        for (j <- 1 to k) {
            val keep = m.toInt               // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                           // x_j is jth column in x
            val rg_j = new Regression (x.sliceExclude (keep, j), x_j)     // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit(0))                        // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print results and diagnostics for each predictor 'x_j' and the overall
     *  quality of fit.
     */
    def report ()
    {
        println ("Coefficients:")
        println ("        | Estimate   |   StdErr   |  t value | Pr(>|t|)")
        for (j <- 0 until b.dim) {
            println ("%7s | %10.6f | %10.6f | %8.4f | %9.5f".format ("x" + sub(j), b(j), stdErr(j), t(j), p(j)))
        } // for
        println ()
        println ("SSE:             %.4f".format (sse))
        println ("Residual stdErr: %.4f on %d degrees of freedom".format (sqrt (sse/df), k))
        println ("R-Squared:       %.4f, Adjusted rSquared:  %.4f".format (rSq, rBarSq))
        println ("F-Statistic:     %.4f on %d and %d DF".format (fStat, k, df))
        println ("AIC:             %.4f".format (aic))
        println ("BIC:             %.4f".format (bic))
        println ("-" * 80)
    } // report

} // Regression_WLS class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_WLSTest` object tests `Regression_WLS` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  Test regression and backward elimination.
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  > run-main scalation.analytics.Regression_WLSTest
 */
object Regression_WLSTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x)
    println ("y = " + y)

    val rg = new Regression_WLS (x, y)
    rg.train ()
    println ("fit = " + rg.fit)
    val yp = rg.predict (z)                              // predict y for one point
    println ("predict (" + z + ") = " + yp)

    println ("reduced model: fit = " + rg.backElim ())   // eliminate least predictive variable

} // Regression_WLSTest object

