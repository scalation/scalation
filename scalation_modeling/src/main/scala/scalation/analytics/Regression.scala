
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
//import scalation.math.double_exp
import scalation.plot.Plot
import scalation.random.CDF.studentTCDF
import scalation.util.{banner, Error, time}
import scalation.util.Unicode.sub

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegTechnique` object defines the implementation techniques available.
 */
object RegTechnique extends Enumeration
{
    type RegTechnique = Value
    val QR, Cholesky, SVD, LU, Inverse = Value
    val techniques = Array (QR, Cholesky, SVD, LU, Inverse)
    
} // RegTechnique

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in
 *  the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
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
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x          the input/design m-by-n matrix augmented with a first column of ones
 *  @param y          the response vector
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class Regression [MatT <: MatriD, VecT <: VectoD] (x: MatT, y: VecT, technique: RegTechnique = QR)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG  = false                                 // debug flag
    private val k      = x.dim2 - 1                            // number of variables (k = n-1)
    private val m      = x.dim1.toDouble                       // number of data points (rows) as a double
    private val df     = (m - k - 1).toInt                     // degrees of freedom
    private val r_df   = (m - 1.0) / df                        // ratio of degrees of freedom

    private var rBarSq = -1.0                                  // adjusted R-squared
    private var fStat  = -1.0                                  // F statistic (quality of fit)
    private var aic    = -1.0                                  // Akaike Information Criterion (AIC)
    private var bic    = -1.0                                  // Bayesian Information Criterion (BIC)

    private var stdErr: VectoD = null                          // standard error of coefficients for each x_j
    private var t: VectoD      = null                          // t statistics for each x_j
    private var p: VectoD      = null                          // p values for each x_j

    type Fac_QR = Fac_QR_H [MatT]                              // change as needed

    private val fac: Factorization = technique match {         // select the factorization technique
        case QR       => new Fac_QR (x, false)                 // QR Factorization
        case Cholesky => new Fac_Cholesky (x.t * x)            // Cholesky Factorization
        case SVD      => new SVD (x)                           // Singular Value Decomposition
        case LU       => new Fac_LU (x.t * x)                  // LU Factorization
        case _        => new Fac_Inv (x.t * x)                 // Inverse Factorization
    } // match
    fac.factor ()                                              // factor the matrix, either X or X.t * X

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  using the ordinary least squares 'OLS' method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD)
    {
        b = technique match {                                  // solve for coefficient vector b
            case QR       => fac.solve (yy)                    // R * b = Q.t * yy
            case Cholesky => fac.solve (x.t * yy)              // L * L.t * b = X.t * yy
            case SVD      => fac.solve (yy)                    // b = V * Σ^-1 * U.t * yy
            case LU       => fac.solve (x.t * yy)              // b = (X.t * X) \ X.t * yy
            case _        => fac.solve (x.t * yy)              // b = (X.t * X)^-1 * X.t * yy
        } // match

        e = yy - x * b                                         // compute residual/error vector e
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
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot z',
     *  e.g., '(b_0, b_1, b_2) dot (1, z_1, z_2)'.
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot z',
     *  for each row of matrix 'z'.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatT): VectoD = z * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector and the new quality of fit.
     */
    def backElim (): (Int, VectoD, VectorD) =
    {
        val ir    =  2                                               // ft(2) is rSq
        var j_max = -1                                               // index of variable to eliminate
        var b_max =  b                                               // parameter values for best solution
        var ft_max = VectorD.fill (fitLabels.size)(-1.0)             // optimize on quality of fit

        for (j <- 1 to k) {
            val keep = m.toInt                                       // i-value large enough to not exclude any rows in slice
            val rg_j = new Regression (x.sliceExclude (keep, j), y)  // regress with x_j removed
            rg_j.train ()
            val bb = rg_j.coefficient
            val ft = rg_j.fit
            if (ft(ir) > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
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
        val ir   =  2                                           // ft(ir) is rSq
        val vifV = new VectorD (k)                              // VIF vector
        for (j <- 1 to k) {
            val keep = m.toInt                                  // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                          // x_j is jth column in x
            val rg_j = new Regression (x.sliceExclude (keep, j), x_j)    // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit(ir))             // store vif for x_1 in vifV(0)
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

} // Regression class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression` companion object provides a testing method.
 */
object Regression
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test various regression techniques.
     * @param x  the design matrix
     * @param y  the response vector
     * @param z  a vector to predict
     */
    def test (x: MatrixD, y: VectorD, z: VectorD)
    {
        for (tec <- techniques) {
            banner (s"Fit the parameter vector b using $tec")
            val rg = new Regression (x, y, tec)                       // use QR Factorization
            rg.train ()
            println ("b = " + rg.coefficient)
            println ("fit = " + rg.fit)
            rg.report ()

            val yp = rg.predict (z)                          // predict y for on3 point
            println ("predict (" + z + ") = " + yp)

            val yyp = rg.predict (x)                         // predict y for several points
            println ("predict (" + x + ") = " + yyp)

            new Plot (y, yyp, null, tec.toString)
        } // for
    } // test

} // Regression object

import Regression._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest` object tests `Regression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  Test regression and backward elimination.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > run-main scalation.analytics.RegressionTest
 */
object RegressionTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

//  println ("model: y = b_0 + b_1*x_1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    test (x, y, z)

} // RegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest2` object tests `Regression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  Test regression using QR Decomposition and Gaussian Elimination for computing
 *  the pseudo-inverse.
 *  > run-main scalation.analytics.RegressionTest2
 */
object RegressionTest2 extends App
{
    // 4 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((4, 3), 1.0, 1.0, 1.0,                  // 4-by-3 matrix
                                 1.0, 1.0, 2.0,
                                 1.0, 2.0, 1.0,
                                 1.0, 2.0, 2.0)
    val y = VectorD (6.0, 8.0, 7.0, 9.0)
    val z = VectorD (1.0, 2.0, 3.0)
    //var rg: Regression [MatrixD, VectorD] = _

//  println ("model: y = b_0 + b_1*x1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    test (x, y, z)

} // RegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest3` object tests the multi-collinearity method in the
 *  `Regression` class using the following regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 *  > run-main scalation.analytics.RegressionTest3
 */
object RegressionTest3 extends App
{
    // 20 data points:      Constant      x_1     x_2    x_3      x_4
    //                                    Age  Weight    Dur   Stress
    val x = new MatrixD ((20, 5), 1.0,   47.0,   85.4,   5.1,    33.0,
                                  1.0,   49.0,   94.2,   3.8,    14.0,
                                  1.0,   49.0,   95.3,   8.2,    10.0,
                                  1.0,   50.0,   94.7,   5.8,    99.0,
                                  1.0,   51.0,   89.4,   7.0,    95.0,
                                  1.0,   48.0,   99.5,   9.3,    10.0,
                                  1.0,   49.0,   99.8,   2.5,    42.0,
                                  1.0,   47.0,   90.9,   6.2,     8.0,
                                  1.0,   49.0,   89.2,   7.1,    62.0,
                                  1.0,   48.0,   92.7,   5.6,    35.0,
                                  1.0,   47.0,   94.4,   5.3,    90.0,
                                  1.0,   49.0,   94.1,   5.6,    21.0,
                                  1.0,   50.0,   91.6,  10.2,    47.0,
                                  1.0,   45.0,   87.1,   5.6,    80.0,
                                  1.0,   52.0,  101.3,  10.0,    98.0,
                                  1.0,   46.0,   94.5,   7.4,    95.0,
                                  1.0,   46.0,   87.0,   3.6,    18.0,
                                  1.0,   46.0,   94.5,   4.3,    12.0,
                                  1.0,   48.0,   90.5,   9.0,    99.0,
                                  1.0,   56.0,   95.7,   7.0,    99.0)
    //  response BP
    val y = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
                     114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    val z = VectorD(1.0,   46.0,   97.5,   7.0,    95.0)

    test (x, y, z)

} // RegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest4` object tests the multi-collinearity method in the
 *  `Regression` class using the following regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 *  > run-main scalation.analytics.RegressionTest4
 */
object RegressionTest4 extends App
{
    // 20 data points:      Constant      x_1     x_2    x_3      x_4
    //                                    Age  Weight    Dur   Stress
    val x = new MatrixD ((20, 5), 1.0,   47.0,   85.4,   5.1,    33.0,
        1.0,   49.0,   94.2,   3.8,    14.0,
        1.0,   49.0,   95.3,   8.2,    10.0,
        1.0,   50.0,   94.7,   5.8,    99.0,
        1.0,   51.0,   89.4,   7.0,    95.0,
        1.0,   48.0,   99.5,   9.3,    10.0,
        1.0,   49.0,   99.8,   2.5,    42.0,
        1.0,   47.0,   90.9,   6.2,     8.0,
        1.0,   49.0,   89.2,   7.1,    62.0,
        1.0,   48.0,   92.7,   5.6,    35.0,
        1.0,   47.0,   94.4,   5.3,    90.0,
        1.0,   49.0,   94.1,   5.6,    21.0,
        1.0,   50.0,   91.6,  10.2,    47.0,
        1.0,   45.0,   87.1,   5.6,    80.0,
        1.0,   52.0,  101.3,  10.0,    98.0,
        1.0,   46.0,   94.5,   7.4,    95.0,
        1.0,   46.0,   87.0,   3.6,    18.0,
        1.0,   46.0,   94.5,   4.3,    12.0,
        1.0,   48.0,   90.5,   9.0,    99.0,
        1.0,   56.0,   95.7,   7.0,    99.0)
    //  response BP
    val y = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
        114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

    //  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    val z = VectorD(1.0,   46.0,   97.5,   7.0,    95.0)

    val rg = new Regression (x, y)
    rg.train ()
    rg.report ()

    println ("vif      = " + rg.vif)                     // test multi-colinearity (VIF)*/
    println ("reduced model: fit = " + rg.backElim ())   // eliminate least predictive variable

} // RegressionTest4 object
