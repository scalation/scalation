
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Sat Dec  8 14:32:12 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package forecaster

import scala.math.min

import scalation.linalgebra.{MatriD, VectoD, VectorD}
import scalation.plot.Plot

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ForecasterVec` abstract class provides a common framework for several
 *  forecasters.  Note, the 'train' method must be called first followed by 'eval'.
 *  @param y       the response vector (time series data)
 *  @param n       the number of parameters
 *  @param hparam  the hyper-parameters
 */
abstract class ForecasterVec (y: VectoD, n: Int, hparam: HyperParameter = null)
         extends Fit (y, n, (n, y.dim - n)) with Predictor
{
    protected val m  = y.dim                              // size of the input vector
    protected val ml = min (m, ForecasterVec.MAX_LAGS)    // maximum lag to consider
    protected val mu = y.mean                             // the sample mean
    protected var e: VectoD = null                        // residual/error vector [e_0, e_1, ... e_m-1]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a time series 'y', train the forecasting function 'y = f(y_)', where
     *  'f(y_)' is a function of the lagged values of 'y', by fitting its parameters.
     *  @param yy  the response/output vector
     */
    def train (yy: VectoD = y): ForecasterVec

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error (difference between actual and predicted) and useful
     *  diagnostics for the dataset.
     *  @param xx  the data/input matrix (impl. classes should default xx to x)
     *  @param yy  the actual response/output vector (impl. classes should default yy to y)
     */
    def eval (xx: MatriD, yy: VectoD): ForecasterVec =
    {
        throw new UnsupportedOperationException ("only available for models with exogenous variable")
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error (difference between actual and predicted) and useful
     *  diagnostics for the dataset.
     *  @param yy  the actual response/output vector to use
     */
    def eval (yy: VectoD = y): ForecasterVec =
    {
        val yp =  predictAll
        e      = yy - yp
        diagnose (e, yy, yp)
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = hparam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on the trained model.
     */
    def report: String =
    {
        s"""
REPORT
    hparameter hp  = $hparameter
    parameter  phi = $parameter
    fitMap     qof = $fitMap
        """
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the last predicted value for the training data.
     *  @param yy  the actual response/output vector to use (ignored)
     */
    def predict (yy: VectoD = null): Double = forecast ()(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of predicted values for all the data.
     *  @param yy  the actual response/output vector to use
     */
    def predictAll: VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce forecasts for 'h' steps ahead into the future.
     *  @param h  the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (h: Int = 1): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot a function, e.g.,  Auto-Correlation Function 'ACF', Partial Auto-Correlation
     *  Function 'PACF'.
     *  @param fVec  the vector given function values
     *  @param name  the name of the function
     */
    def plotFunc (fVec: VectoD, name: String)
    {
        val lag_axis = new VectorD (ml+1)
        for (i <- 0 until fVec.dim) lag_axis(i) = i
        val zero = new VectorD (ml+1)
        new Plot (lag_axis, fVec, zero, "Plot of " + name, true)
    } // plotFunc

} // ForecasterVec abstract class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ForecasterVec` companion object provides functions useful for forecasting
 *  models.  It also contains a sample dataset.
 */
object ForecasterVec
{
    import scalation.stat.vectorD2StatVector

    private val MAX_LAGS = 19

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Auto-Correlation Function (ACF) returning the variance, the
     *  auto-covariance vector, and the auto-correlation function.
     *  @param y_  the vector for which the ACF is sought
     */
    def acf (y_ : VectoD, lags: Int = MAX_LAGS): (Double, VectoD, VectoD) =
    {
        val y = if (y_.isInstanceOf [VectorD]) y_.asInstanceOf [VectorD]
                else y_.toDense
        val sig2 = y.variance                             // the sample variance
        val acv  = new VectorD (lags + 1)                 // auto-covariance vector
        for (k <- acv.range) acv(k) = y acov k            // k-th lag auto-covariance
        val acr  = acv / sig2                             // auto-correlation function
        (sig2, acv, acr)
    } // acf

    // Forecasting lake levels
    // @see cran.r-project.org/web/packages/fpp/fpp.pdf

    val t = VectorD.range (0, 98)
    val y = VectorD (580.38, 581.86, 580.97, 580.80, 579.79, 580.39, 580.42, 580.82, 581.40, 581.32,
                     581.44, 581.68, 581.17, 580.53, 580.01, 579.91, 579.14, 579.16, 579.55, 579.67,
                     578.44, 578.24, 579.10, 579.09, 579.35, 578.82, 579.32, 579.01, 579.00, 579.80,
                     579.83, 579.72, 579.89, 580.01, 579.37, 578.69, 578.19, 578.67, 579.55, 578.92,
                     578.09, 579.37, 580.13, 580.14, 579.51, 579.24, 578.66, 578.86, 578.05, 577.79,
                     576.75, 576.75, 577.82, 578.64, 580.58, 579.48, 577.38, 576.90, 576.94, 576.24,
                     576.84, 576.85, 576.90, 577.79, 578.18, 577.51, 577.23, 578.42, 579.61, 579.05,
                     579.26, 579.22, 579.38, 579.10, 577.95, 578.12, 579.75, 580.85, 580.41, 579.96,
                     579.61, 578.76, 578.18, 577.21, 577.13, 579.10, 578.25, 577.91, 576.89, 575.96,
                     576.80, 577.68, 578.38, 578.52, 579.74, 579.31, 579.89, 579.96)

} // ForecasterVec


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ForecasterVecTest` object tests the `ForecasterVec` companion object.
 *  > runMain scalation.analytics.forecaster.ForecasterVecTest
 */
object ForecasterVecTest extends App
{
    import ForecasterVec._

    val (sig2, acv, acr) = acf (y)
    val zero = new VectorD (acr.dim)
    new Plot (t(0 until acr.dim), acr, zero, "ACF vs. t", true) 

} // ForecasterVecTest object

