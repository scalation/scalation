
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vamsi Nadella, Hao Peng
 *  @version 1.6
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.forecaster

import scala.collection.mutable.{LinkedHashMap, Map}
import scala.math.sqrt

import scalation.linalgebra.{VectoD, VectorD}
import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster` trait provides a common framework for several forecasters.
 *  Note, the 'train' method must be called first followed by 'eval'.
 */
trait Forecaster extends Error
{
    protected var sse       = -1.0                  // sum of squares error
    protected var ssr       = -1.0                  // sum of squares regression/model
    protected var sst       = -1.0                  // sum of squares total (ssr + sse)
    protected var mae       = -1.0                  // mean absolute error
    protected var mape      = -1.0                  // mean absolute percentage error
    protected var mse       = -1.0                  // mean squared error
    protected var rmse      = -1.0                  // root mean squared error
    protected var rSq       = -1.0                  // coefficient of determination (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a time series 'y', train the prediction function 'y = f(y_)', where
     *  'f(y_)' is a function of the lagged values of 'y', by fitting its parameters.
     */
    def train (): Forecaster

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     */
    def eval ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagnostics for the forecaster.  Override to add more diagnostics.
     *  Note, for 'mse' and 'rmse', 'sse' is divided by the number of instances
     *  'm' rather than the degrees of freedom.
     *  @see en.wikipedia.org/wiki/Mean_squared_error
     *  @param yy  the response vector, actual values
     *  @param ee  the residual/error vector
     */
    protected def diagnose (yy: VectoD, ee: VectoD)
    {
        val m  = yy.dim                              // number of instances
        sst    = (yy dot yy) - yy.sum~^2.0 / m       // sum of squares total
        sse    = ee dot ee                           // sum of squares error
        ssr    = sst - sse                           // sum of squares regression (not returned by fit)
        mse    = sse / m                             // raw mean square error
        rmse   = sqrt (mse)                          // root mean square error
        mae    = ee.norm1 / m                        // mean absolute error
        mape   = (ee / yy).norm1 / m                 // mean absolute percentage error
        rSq    = ssr / sst                           // coefficient of determination R^2
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'sst', 'sse', 'mae', rmse' and 'rSq'.
     *  Note, if 'sse > sst', the model introduces errors and the 'rSq' may be negative,
     *  otherwise, R^2 ('rSq') ranges from 0 (weak) to 1 (strong).
     *  Note that 'rSq' is the last or number 5 measure.
     *  Override to add more quality of fit measures.
     */
    def fit: VectoD = VectorD (sst, sse, mse, rmse, mae, mape, rSq)

    val index_rSq = 6                               // index of rSq           

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.  Override when necessary.
     */
    def fitLabel: Seq [String] = Seq ("sst", "sse", "mse", "rmse", "mae", "mape", "rSq")

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
    /** Return the vector of predicted values on the training data
     */
    def predict (): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce forecasts for 'h' steps ahead into the future.
     *  @param h  the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (h: Int): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce forecasts for one step ahead into the future.
     */
    def forecast (): VectoD = forecast (1)

} // Forecaster trait

