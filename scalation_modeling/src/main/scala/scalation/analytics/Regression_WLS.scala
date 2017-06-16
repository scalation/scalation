
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.3
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
import scalation.math.double_exp
import scalation.plot.Plot
import scalation.random.CDF.studentTCDF
import scalation.util.{banner, Error, time}
import scalation.util.Unicode.sub

import RegTechnique._
import Regression_WLS._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_WLS` class supports weighted multiple linear regression.
 *  In this case, 'xx' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the regression equation
 *  <p>
 *      yy  =  b dot xx + e  =  b_0 + b_1 * xx_1 + ... b_k * xx_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Weighted Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *     b  =  fac.solve (.)
 *  <p>
 *  The data matrix 'xx' is reweighted 'x = rootW * xx' and the response vector 'yy'
 *  is reweighted 'y = rootW * yy' where 'rootW' is the square root of the weights.
 *  @see en.wikipedia.org/wiki/Least_squares#Weighted_least_squares
 *  These are then pass to OLS Regression.  Four factorization techniques are provided:
 *  <p>
 *      'QR'         // QR Factorization: slower, more stable (default)
 *      'Cholesky'   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      'SVD'        // Singular Value Decomposition: slowest, most robust
 *      'LU'         // LU Factorization: better than Inverse
 *      'Inverse'    // Inverse/Gaussian Elimination, classical textbook technique
 *  <p>
 *  @see www.markirwin.net/stat149/Lecture/Lecture3.pdf
 *  @param xx         the input/data m-by-n matrix
                          (augment with a first column of ones to include intercept in model)
 *  @param yy         the response vector
 *  @param technique  the technique used to solve for b in x.t*w*x*b = x.t*w*y
 *  @param w          the weight vector (if null, computed in companion object)
 */
class Regression_WLS [MatT <: MatriD, VecT <: VectoD] (xx: MatT, yy: VecT, technique: RegTechnique = QR,
                      private var w: VectoD = null)
      extends Regression ({ setWeights (xx, yy, technique, w); reweightX (xx, w) }, reweightY (yy, w), technique)
{
    private var pre: VectoD = null                             // fitted / predicted values

    if (w == null) w = Regression_WLS.weights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the weight vector
     */
    def weights: VectoD = w

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     */
    override def diagnose (yy: VectoD)
    {
        pre    = xx * b                                        // predicted response vector
        sse    = e dot e                                       // sum of squared errors
        ssr    = (w * (pre - (w * pre / w.sum).sum)~^2.0).sum  // regression sum of squares
        sst    = ssr + sse                                     // total sum of squares
        rmse   = sqrt (sse / e.dim)                            // root mean square error
        rSq    = (sst - sse) / sst                             // coefficient of determination R^2
        rBarSq = 1.0 - (1.0-rSq) * r_df                        // R-bar-squared (adjusted R-squared)
        fStat  = ((sst - sse) * df) / (sse * k)                // F statistic (msr / mse)
        aic    = m * log (sse) - m * log (m) + 2.0 * (k+1)     // Akaike Information Criterion (AIC)
        bic    = aic + (k+1) * (log (m) - 2.0)                 // Bayesian Information Criterion (BIC)

        val facCho = if (technique == Cholesky) fac            // reuse Cholesky factorization
                     else new Fac_Cholesky (x.t * x)           // create a Cholesky factorization
        val l_inv  = facCho.factor1 ().inverse                 // take inverse of l from Cholesky factorization
        val varEst = sse / df                                  // variance estimate
        val varCov = l_inv.t * l_inv * varEst                  // variance-covariance matrix

        stdErr = varCov.getDiag ().map (sqrt (_))              // standard error of coefficients
        t      = b / stdErr                                    // Student's T statistic

        p = if (df > 0) t.map ((x: Double) => 2.0 * studentTCDF (-abs (x), df))   // p values
            else -VectorD.one (k+1)
    } // diagnose

 } // Regression_WLS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_WLS` companion object provides methods for setting weights
 *  and testing.
 */
object Regression_WLS
{
    private var w: VectoD     = null                           // weights for current model
    private var rootW: VectoD = null                           // square root of weights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the weight vector for the current model.
     */
    def weights: VectoD = w

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate weights for the variables according to the reciprocal predicted
     *  rad's.  Save the weight vector 'w' and root weight vector 'rootW'
     *  for the current model in companion object variables.
     *  @param x          the input/data m-by-n matrix
     *  @param y          the response vector
     *  @param technique  the technique used to solve for b in x.t*w*x*b = x.t*w*y
     *  @param w          the weight vector (if null, computed it)
     */
    def setWeights [MatT <: MatriD, VecT <: VectoD] (x: MatT, y: VecT, technique: RegTechnique = QR,
                    w0: VectoD = null)
    {
        if (w0 == null) {
            val k = x.dim2 - 1
            val ols_y = new Regression (x, y, technique)       // run OLS on data
            ols_y.train ()
            val e = ols_y.residual                             // deviations/errors
            val r = e.map ((a: Double) => sqrt (abs (a)))      // root absolute deviations (rad's)

            val ols_r = new Regression (x, r, technique)       // run OLS on rad
            ols_r.train ()
            val rp = ols_r.predict (x)                         // predicted rad
            w = rp.recip * (k + 1)                             // set weight vector for WLS to reciprocal of rp
//          w = rp.recip                                       // set weight vector for WLS to reciprocal of rp
        } else {
            w = w0                                             // set weights using custom values
        } // if
        rootW = w.map (sqrt _)                                 // set root of weight vector
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reweight the data matrix 'x' by multiplying by the root weight 'rtW'.
     *  @param x   the input/data m-by-n matrix
     *  @param rW  the root weight vector (rtW: either rootW or rW)
     */
    def reweightX (x: MatriD, rW: VectoD): MatriD =
    {
        val rtW = if (rW == null) rootW else rW                // root weight vector
        rtW **: x                                              // vector (as if diagonal matrix) * matrix (right associative)
    } // reweightX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reweight the response vector matrix 'y' by multiplying by the root weight 'rtW'.
     *  @param y   the response vector
     *  @param rW  the root weight vector (rtW: either rootW or rW)
     */
    def reweightY (y: VectoD, rW: VectoD): VectoD =
    {
        val rtW = if (rW == null) rootW else rW                // root weight vector
        rtW * y                                                // vector * vector (element by element)
    } // reweightY
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test various regression techniques.
     *  @param x  the data matrix
     *  @param y  the response vector
     *  @param z  a vector to predict
     *  @param w  the root weights 
     */
    def test (x: MatriD, y: VectoD, z: VectoD, w: VectoD = null)
    {
        for (tec <- techniques) {                              // use 'tec' Factorization
            banner (s"Fit the parameter vector b using $tec")
            val rg = new Regression_WLS (x, y, tec)
            rg.train ()
            println ("w   = " + rg.weights)
            println ("b   = " + rg.coefficient)
            println (rg.fitLabels)
            println ("fit = " + rg.fit)
            println ("e   = " + rg.residual)
            rg.report ()

            val yp = rg.predict (z)                            // predict y for one point
            println ("predict (" + z + ") = " + yp)

            val yyp = rg.predict (x)                           // predict y for several points
            println ("predict (" + x + ") = " + yyp)

            // new Plot (y, yyp, null, tec.toString)
        } // for
    } // test

} // Regression_WLS object


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
    
//  println ("model: y = b_0 + b_1*x_1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    banner ("weights set internally")
    test (x, y, z)                                                // weights set internally

//  val w0 = VectorD (0.106085, 0.0997944, 0.0831033, 0.160486, 0.171810)
    val w0 = VectorD (0.318254, 0.299383,  0.249310,  0.481457, 0.515431)

    banner ("custom weights")
    test (x, y, z, w0)                                            // custom weights explicitly given

} // Regression_WLSTest object

