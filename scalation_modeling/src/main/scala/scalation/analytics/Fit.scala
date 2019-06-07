
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Thu Mar 22 22:31:32 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see facweb.cs.depaul.edu/sjost/csc423/documents/f-test-reg.htm
 *  @see avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf
 *  @see en.wikipedia.org/wiki/Bayesian_information_criterion
 */

package scalation.analytics

import scala.collection.mutable.{LinkedHashMap, Map}
import scala.math.{abs, log, sqrt}

import scalation.linalgebra.{VectoD, VectorD}
import scalation.math.double_exp
import scalation.random.CDF.{fisherCDF, studentTCDF}
import scalation.stat.Statistic
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` class provides methods to determine basic quality of fit measures.
 *  @param y   the values in the m-dimensional response vector
 *  @param n   the number of parameters (b.dim)
 *  @param df  the degrees of freedom (df._1, df._2) for (regression, error)
 */
class Fit (y: VectoD, n: Int, private var df: PairD = (0.0, 0.0))
      extends Error
{
    private val DEBUG  = false                              // debug flag
    private val m      = y.dim                              // number of instances
    private val ln_m   = log (m)                            // natural log of m (ln(m))

    private var df_t   = df._1 + df._2                      // total degrees of freedom
    private var r_df   = df_t / df._2                       // ratio of degrees of freedom (total / error)

    private var sse    = -1.0                               // sum of squares for error  (rss)
    private var ssr    = -1.0                               // sum of squares regression/model
    private var sst    = -1.0                               // sum of squares total (ssr + sse)

    private var mse0   = -1.0                               // raw/MLE mean squared error
    private var rmse   = -1.0                               // root mean squared error
    private var mae    = -1.0                               // mean absolute error
    private var rSq    = -1.0                               // coefficient of determination (quality of fit)

    private var mse    = -1.0                               // mean of squares for error (unbiased)
    private var rse    = -1.0                               // residual standard error
    private var msr    = -1.0                               // mean of squares for regression/model
    private var rBarSq = -1.0                               // adjusted R-squared
    private var fStat  = -1.0                               // F statistic (quality of fit)
    private var p_fS   = -1.0                               // p-value for fStat 
    private var aic    = -1.0                               // Akaike Information Criterion (AIC = -2LL + 2n)
    private var bic    = -1.0                               // Bayesian Information Criterion (BIC = -2LL + n ln(m))
    private var mape   = -1.0                               // Mean Absolute Percentage Error

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the degrees of freedom to the new updated values.  For some models,
     *  the degrees of freedom is not known until after the model is built.
     *  @param df_update  the updated degrees of freedom (model, error)
     */
    def resetDF (df_update: PairD)
    {
        df     = df_update                                  // degrees of freedom
        df_t   = df._1 + df._2                              // total degrees of freedom
        r_df   = df_t / df._2                               // ratio of degrees of freedom (total / error)
        if (DEBUG) println (s"resetDF: df = $df")
    } // resetDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean of squares for error (sse / df._2).  Must call diagnose first.
     */
    def mse_ : Double = mse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Diagnose the health of the model by computing the Quality of Fit (QoF) measures,
     *  from the error/residual vector and the predicted & actual responses.
     *  For some models the instances may be weighted.
     *  @see `Regression_WLS`
     *  @param e   the m-dimensional error/residual vector (yy - yp)
     *  @param yy  the actual response vector to use (test/full)
     *  @param yp  the predicted response vector (test/full), (defaults to null)
     *  @param w   the weights on the instances (defaults to null)
     */
    def diagnose (e: VectoD, yy: VectoD, yp : VectoD = null, w: VectoD = null)
    {
        val m  = yy.dim                                   // size of the response vector (test/full)
        if (e.dim != m) flaw ("diagnose", s"e.dim = ${e.dim} != yy.dim = $m")

        sse = e dot e                                     // sum of squares for error 
        if (w == null) {
            sst = (yy dot yy) - yy.sum~^2 / m             // sum of squares total (ssr + sse)
            ssr = sst - sse                               // sum of squares regression/model
        } else {
            ssr = (w * (yp - (w * yp / w.sum).sum)~^2).sum  // regression sum of squares
            sst = ssr + sse
        } // if
        mse0   = sse / m                                  // raw/MLE mean squared error
        rmse   = sqrt (mse0)                              // root mean squared error
        mae    = e.norm1 / m                              // mean absolute error
        rSq    = ssr / sst                                // coefficient of determination (quality of fit)

        if (df._1 <= 0 || df._2 <= 0) flaw ("diagnose", s"degrees of freedom df = $df must be positive")
        mse    = sse / df._2                              // mean of squares for error
        msr    = ssr / df._1                              // mean of squares for regression/model

        rse    = sqrt (mse)                               // residual standard error
        rBarSq = 1 - (1-rSq) * r_df                       // adjusted R-squared
        fStat  = msr / mse                                // F statistic (quality of fit)
        p_fS   = 1.0 - fisherCDF (fStat, df._1.toInt, df._2.toInt)   // p-value for fStat 
        if (p_fS.isNaN) p_fS = 0.0                        // FIX - why NaN
        aic    = m * log (mse0) + 2 * n                   // Akaike Information Criterion (AIC = -2LL + 2n)
        bic    = aic + n * (ln_m - 2)                     // Bayesian Information Criterion (BIC = -2LL + n ln(m))
        mape   = (e / yy).norm1 / m                       // mean absolute percentage error / 100
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including  'rSq', 'sst', 'sse', 'mse0', rmse', 'mae',
     *  'df._2', 'rBarSq', 'fStat', 'aic', 'bic', 'mape'.
     *  Note, if 'sse > sst', the model introduces errors and the 'rSq' may be negative,
     *  otherwise, R^2 ('rSq') ranges from 0 (weak) to 1 (strong).
     *  Override to add more quality of fit measures.
     */
    def fit: VectoD = VectorD (rSq, sst, sse, mse0, rmse, mae,
                               df._2, rBarSq, fStat, aic, bic, mape)

    val index_rSq    = Fit.index_rSq                      // index of rSq    - update with fit
    val index_sst    = Fit.index_sst                      // index of sst    - update with fit
    val index_rSqBar = Fit.index_rSqBar                   // index of rSqBar - update with fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the quality of fit measures.
     *  Override to add more quality of fit measures.
     */
    def fitLabel: Seq [String] = Seq ("rSq", "sst", "sse", "mse0", "rmse", "mae",
                                      "df", "rBarSq", "fStat", "aic", "bic", "mape")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Format a double value.
     *  @param z  the double value to format
     */
    private def f_ (z: Double): String = "%.5f".format (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of quality of fit measures (use of `LinedHashMap` makes it ordered).
     *  Override to add more quality of fit measures.
     */
    def fitMap: Map [String, String] =
    {
        val lm = LinkedHashMap [String, String] ()          // empty list map
        val fl = fitLabel                                   // fit labels
        val fv = fit                                        // fit values
        for (i <- fl.indices) lm += fl(i) -> f_(fv(i))
        lm
    } // fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a summary report with diagnostics for each predictor 'x_j' and
     *  the overall quality of fit.
     *  @param b        the parameters/coefficients for the model
     *  @param stddErr  the standard error for parameters/coefficients
     *  @param show     flag indicating whether to print the summary
     */
    def summary (b: VectoD, stdErr: VectoD = null, show: Boolean = false): String =
    {
        val stats = (sumCoeff (b, stdErr), f_(rse), f_(rSq), f_(rBarSq))
        if (DEBUG) println (s"summary: stats = $stats")

        val sum = s"""
SUMMARY
    Coefficients:
             Estimate    Std. Error \t t value \t Pr(>|t|)
    ${stats._1}

    Residual standard error: ${stats._2} on ${df._2} degrees of freedom
    Multiple R-squared:  ${stats._3},	Adjusted R-squared:  ${stats._4}
    F-statistic: $fStat on ${df._1} and ${df._2} DF,  p-value: $p_fS
       """

       if (show) println (sum)
       sum
    } // summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the summary report portion for the cofficients.
     *  @param b        the parameters/coefficients for the model
     *  @param stddErr  the standard error for parameters/coefficients
     */
    private def sumCoeff (b: VectoD, stdErr: VectoD = null): String =
    {
        if (DEBUG) println (s"stdErr = $stdErr")
        var t: VectoD = null
        var p: VectoD = null
        if (stdErr != null) {
            t = b / stdErr                                    // Student's T statistic
            p = if (df._2 > 0) t.map ((x: Double) => 2 * studentTCDF (-abs (x), df._2))   // p values
                else -VectorD.one (n)
        } // if
        val sb = new StringBuilder ()
        for (j <- b.range) sb.append ("\n    x" + j + "\t " + f_(b(j)) +
                 (if (stdErr != null) { val p_j = if (p(j).isNaN) 0.0 else p(j);          // FIX - why NaN
                       "\t " + f_(stdErr(j)) + "\t " + f_(t(j)) + "\t " + f_(p_j) }
                  else "?"))
        sb.mkString
    } // sumCoeff

} // Fit class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` companion object provides factory methods for assessing quality of
 *  fit for standard types of modeling techniques.
 */
object Fit
{
    val index_rSq    = 0                                    // index of rSq    - update with fit
    val index_sst    = 1                                    // index of sst    - update with fit
    val index_rSqBar = 7                                    // index of rSqBar - update with fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect qof results for a model having say 'l' parameters and return them
     *  in a vector.  Adjust 'index_?' to customize Quality of Fit (QoF) measures.
     *  @param fit     the fit vector with regard to the training set
     *  @param cv_fit  the fit array of statistics for cross-validation (upon test sets)
     */
    def qofVector (fit: VectoD, cv_fit: Array [Statistic]): VectorD =
    {
        val cv = cv_fit(index_rSq).mean                    // mean for R^2 cv
        VectorD (100 * fit(index_rSq),                     // R^2 percentage
                 100 * fit(index_rSqBar),                  // R^2 Bar percentage
                 100 * cv)                                 // R^2 cv percentage
    } // qofVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Fit` object that provides methods to determine basic quality
     *  of fit measures.  This factory method assume a standard regression model
     *  with an intercept.
     *  @param y  the values in the m-dimensional output/response vector
     *  @param n  the number of parameters (b.dim)
     */
    def apply (y: VectoD, e: VectoD, n: Int): Fit = new Fit (y, n, (n-1, y.dim-n))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the sum of squares total (ssr + sse).
     *  @param y  the values in the m-dimensional output/response vector
     */
    def sstf (y: VectoD): Double =  { val m = y.dim; (y dot y) - y.sum~^2 / m }
 
} // Fit object

