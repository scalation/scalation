
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 1.4
 *  @date    Tue Apr 18 14:24:14 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
import scalation.math.double_exp
import scalation.minima._
import scalation.plot.Plot
import scalation.random.CDF.studentTCDF
import scalation.util.{Error, time}
import scalation.util.Unicode.sub

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in
 *  the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x   the input/design m-by-n matrix
 *  @param y   the response vector
 *  @param λ0  the initial value for the regularization weight
 */
class LassoRegression [MatT <: MatriD, VecT <: VectoD] (x: MatT, y: VecT, λ0: Double = 0.01)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG    = false                                // debug flag
    private val k        = x.dim2 - 1                           // number of variables (k = n-1)
    private val m        = x.dim1.toDouble                      // number of data points (rows)
    private val df       = (m - k - 1).toInt                    // degrees of freedom
    private val r_df     = (m-1.0) / df                         // ratio of degrees of freedom

    private var rBarSq   = -1.0                                 // adjusted R-squared
    private var fStat    = -1.0                                 // F statistic (quality of fit)
    private var aic      = -1.0                                 // Akaike Information Criterion (AIC)
    private var bic      = -1.0                                 // Bayesian Information Criterion (BIC)

    private var stdErr: VectoD = null                           // standard error of coefficients for each x_j
    private var t: VectoD      = null                           // t statistics for each x_j
    private var p: VectoD      = null                           // p values for each x_j

    private var λ = λ0                                          // weight to put on regularization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squares error + λ * sum of the magnitude  of coefficients.
     *  This is the objective function to be minimized.
     *  @param yy  the response vector
     *  @param b   the vector of coefficients/parameters
     */
    def f (yy: VectoD)(b: VectoD): Double =
    {
        e = yy - x * b                                          // calculate the residuals/error
        e dot e + λ * b.norm1 
    } // f

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  regularized by the sum of magnitudes of the coefficients.
     *  @see pdfs.semanticscholar.org/969f/077a3a56105a926a3b0c67077a57f3da3ddf.pdf
     *  @see `scalation.minima.LassoAdmm`
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): LassoRegression [MatT, VecT] =
    {
//      val g       = f(yy) _
//      val optimer = new CoordinateDescent (g)                 // Coordinate Descent optimizer
//      val optimer = new GradientDescent (g)                   // Gradient Descent optimizer
//      val optimer = new ConjugateGradient (g)                 // Conjugate Gradient optimizer
//      val optimer = new QuasiNewton (g)                       // Quasi-Newton optimizer
//      val optimer = new NelderMeadSimplex (g, x.dim2)         // Nelder-Mead optimizer
//      val b0 = new VectorD (k+1)                              // initial guess for coefficient vector
//      b = optimer.solve (b0, 0.5)                             // find an optimal solution for coefficients
        b = LassoAdmm.solve (x.asInstanceOf [MatrixD], y, λ)
        this
    } // train

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation for the response passed into the class 'y'.
     */
//    def train (): LassoRegression [MatT, VecT] = train (y)

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
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     */
    override protected def diagnose (yy: VectoD)
    {
        super.diagnose (yy)
        rBarSq   = 1.0 - (1.0-rSq) * r_df                        // R-bar-squared (adjusted R-squared)
        fStat    = ((sst - sse) * df) / (sse * k)                // F statistic (msr / mse)
        aic      = m * log (sse) - m * log (m) + 2.0 * (k+1)     // Akaike Information Criterion (AIC)
        bic      = aic + (k+1) * (log (m) - 2.0)                 // Bayesian Information Criterion (BIC)

        val facCho = new Fac_Cholesky (x.t * x)                  // create a Cholesky factorization
        val l_inv  = facCho.factor1 ().inverse                   // take inverse of l from Cholesky factorization
        val varEst = sse / df                                    // variance estimate
        val varCov = l_inv.t * l_inv * varEst                    // variance-covariance matrix

        stdErr = varCov.getDiag ().map (sqrt (_))                          // standard error of coefficients
        t      = b / stdErr                                                // Student's T statistic
        p      = t.map ((x: Double) => 2.0 * studentTCDF (-abs (x), df))   // p values
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
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatT): VectoD = z * b

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
        println ("λ:               %.4f".format (λ))
        println ("-" * 80)
    } // report

} // LassoRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegressionTest` object tests `LassoRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.LassoRegressionTest
 */
object LassoRegressionTest extends App
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

    val rg = new LassoRegression (x, y)
    rg.train ().eval ()
    println ("b = " + rg.coefficient)
    rg.report ()

    println ("fit = " + rg.fit)
    val yp = rg.predict (z)                              // predict y for one point
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                             // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

} // LassoRegressionTest object

