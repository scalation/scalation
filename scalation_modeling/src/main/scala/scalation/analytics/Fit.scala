
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
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

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` class provides methods to determine basic quality of fit measures.
 *  @param y   the values in the m-dimensional response vector
 *  @param n   the number of parameters (b.dim)
 *  @param df  the degrees of freedom (df._1, df._2) for (regression, error)
 */
class Fit (y: VectoD, n: Int, val df: (Double, Double) = (0.0, 0.0))
{
    private val m      = y.dim                              // number of instances
    private val ln_m   = log (m)                            // natural log of m (ln(m))
    private val df_t   = df._1 + df._2                      // total degrees of freedom
    private val r_df   = df_t / df._2                       // ratio of degrees of freedom (total / error)

    private var sse    = -1.0                               // sum of squares for error  (rss)
    private var ssr    = -1.0                               // sum of squares regression/model
    private var sst    = -1.0                               // sum of squares total (ssr + sse)

    private var mse0   = -1.0                               // raw/MLE mean squared error
    private var rmse   = -1.0                               // root mean squared error
    private var mae    = -1.0                               // mean absolute error
    private var rSq    = -1.0                               // coefficient of determination (quality of fit)

    private var mse    = -1.0                               // mean of squares for error
    private var rse    = -1.0                               // residual standard error
    private var msr    = -1.0                               // mean of squares for regression/model
    private var rBarSq = -1.0                               // adjusted R-squared
    private var fStat  = -1.0                               // F statistic (quality of fit)
    private var p_fS   = -1.0                               // p-value for fStat 
    private var aic    = -1.0                               // Akaike Information Criterion (AIC = -2LL + 2n)
    private var bic    = -1.0                               // Bayesian Information Criterion (BIC = -2LL + n ln(m))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean of squares for error (sse / df._2).  Must call diagnose first.
     */
    def mse_ : Double = mse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the error/residual vector, compute the quality of fit measures.
     *  @param e   the corresponding m-dimensional error vector (y - yp)
     *  @param w   the weights on the instances
     *  @param yp  the predicted response vector (x * b)
     */
    def diagnose (e: VectoD, w: VectoD = null, yp: VectoD = null)
    {
        sse    = e dot e                                    // sum of squares for error 
        if (w == null) {
            sst = (y dot y) - y.sum~^2 / m                  // sum of squares total (ssr + sse)
            ssr = sst - sse                                 // sum of squares regression/model
        } else {
            ssr = (w * (yp - (w * yp / w.sum).sum)~^2).sum  // regression sum of squares
            sst = ssr + sse
        } // if
        mse0   = sse / m                                    // raw/MLE mean squared error
        rmse   = sqrt (mse0)                                // root mean squared error
        mae    = e.norm1 / m                                // mean absolute error
        rSq    = ssr / sst                                  // coefficient of determination (quality of fit)
        mse    = sse / df._2                                // mean of squares for error
        rse    = sqrt (mse)                                 // residual standard error
        msr    = ssr / df._1                                // mean of squares for regression/model
        rBarSq = 1 - (1-rSq) * r_df                         // adjusted R-squared
        fStat  = msr / mse                                  // F statistic (quality of fit)
        p_fS   = fisherCDF (fStat, df._1.toInt, df._2.toInt)   // p-value for fStat 
        aic    = m * log (mse0) + 2 * n                     // Akaike Information Criterion (AIC = -2LL + 2n)
        bic    = aic + n * (ln_m - 2)                       // Bayesian Information Criterion (BIC = -2LL + n ln(m))
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'sst', 'sse', 'mse0', rmse', 'mae', 'rSq',
     *  'df._2', 'rBarSq', 'fStat', 'aic', 'bic'.
     *  Note, if 'sse > sst', the model introduces errors and the 'rSq' may be negative,
     *  otherwise, R^2 ('rSq') ranges from 0 (weak) to 1 (strong).
     *  Note that 'rSq' is the number 5 measure.
     *  Override to add more quality of fit measures.
     */
    def fit: VectoD = VectorD (rSq, sst, sse, mse0, rmse, mae,
                               df._2, rBarSq, fStat, aic, bic)

    val index_rSq = 5                                       // index of rSq

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the quality of fit measures.
     *  Override to add more quality of fit measures.
     */
    def fitLabel: Seq [String] = Seq ("rSq", "sst", "sse", "mse0", "rmse", "mae",
                                      "df", "rBarSq", "fStat", "aic", "bic")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Format a double value.
     *  @param z  the double value to format
     */
    def f_ (z: Double): String = "%.5f".format (z)

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
    /** Produce the summary report portion for the cofficients.
     *  @param b        the parameters/coefficients for the model
     *  @param stddErr  the standard error for parameters/coefficients
     */
    def sumCoeff (b: VectoD, stdErr: VectoD = null): String =
    {
        var t: VectoD = null
        var p: VectoD = null
        if (stdErr != null) {
            t = b / stdErr                                    // Student's T statistic
            p = if (df._2 > 0) t.map ((x: Double) => 2 * studentTCDF (-abs (x), df._2))   // p values
                else -VectorD.one (n)
        } // if
        val sb = new StringBuilder ()
        for (j <- b.range) sb.append (s"\n    x$j \t f_(b(j)) \t " +
                 (if (stdErr != null) s"f_(stdErr(j)) \t f_(t(j)) \t f_(p(j))" else ""))
        sb.mkString
    } // sumCoeff

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a summary report with diagnostics for each predictor 'x_j' and
     *  the overall quality of fit.
     *  @param b        the parameters/coefficients for the model
     *  @param stddErr  the standard error for parameters/coefficients
     */
    def summary (b: VectoD, stdErr: VectoD = null): String =
    {
        s"""
    Coefficients:
                Estimate Std. Error t value Pr(>|t|)
    ${sumCoeff (b, stdErr)}

    Residual standard error: f_(rse) on ${df._2} degrees of freedom
    Multiple R-squared:  f_(rSq),	Adjusted R-squared:  f_(rBarSq)
    F-statistic: $fStat on ${df._1} and ${df._2} DF,  p-value: $p_fS
       """ 
    } // summary

} // Fit class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` companion object provides factory methods for assessing quality of
 *  fit for standard types of modeling techniques.
 */
object Fit
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Fit` object that provides methods to determine basic quality
     *  of fit measures.  This factory method assume a standard regression model
     *  with an intercept.
     *  @param y  the values in the m-dimensional response vector
     *  @param n  the number of parameters (b.dim)
     */
    def apply (y: VectoD, e: VectoD, n: Int): Fit = new Fit (y, n, (n-1, y.dim-n))
 
} // Fit object

