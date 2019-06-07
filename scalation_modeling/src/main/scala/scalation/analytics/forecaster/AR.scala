
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Michael Cotterell
 *  @version 1.6
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

package scalation.analytics
package forecaster

import scala.math.{max, min}

import scalation.analytics.{BASE_DIR, Regression}
import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.random.{Normal, Random}
import scalation.stat.vectorD2StatVector
import scalation.util.banner

import ForecasterVec.acf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR` class provides basic time series analysis capabilities for Auto-
 *  Regressive 'AR'.  In an  'AR(p)' model, 'p' refers to the order of the
 *  Auto-Regressive components of the model.  `AR` models are often used for forecasting.
 *  Given time series data stored in vector 'y', its next value 'y_t = y(t)'
 *  may be predicted based on prior values of 'y' and its noise:
 *  <p>
 *      y_t = c + Σ(φ_k y_t-k)
 *  <p>
 *  where 'c' is a constant, 'φ' (phi) is the autoregressive coefficient vector,
 *  and 'e' is the noise vector.
 *------------------------------------------------------------------------------
 *  @param y       the response/output vector (time series data)
 *  @param hparam  the hyper-parameters
 */
class AR (y: VectoD, hparam: HyperParameter = AR.hp)
      extends ForecasterVec (y, hparam("p").toInt, hparam)
{
    private val DEBUG = true                               // debug flag
    private var p     = hparam("p").toInt                  // p-th order Auto-Regressive model
    private val z     = y - mu                             // work with mean zero time series

    private val (sig2, acv, acr) = acf (z, ml)             // variance, auto-covariance, auto-correlation
    private var pacf: VectoD = null                        // Partial Auto-Correlation Function (PACF)

    def acF: VectoD  = acr
    def pacF: VectoD = pacf

    private var phi: VectoD = null                           // AR(p) parameters/coefficients
    private var psi = new MatrixD (ml+1, ml+1)             // psi matrix (ml = max lags)

    if (DEBUG) {
        println ("m    = " + m)                            // number of data points
        println ("z    = " + z)                            // zero-mean time series
        println ("mu   = " + mu)                           // mean
        println ("sig2 = " + sig2)                         // variance
        println ("acv  = " + acv)                          // auto-covariance
        println ("acr  = " + acr)                          // ACF
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `AR` model to the times series data in vector 'yy'.
     *  Estimate the coefficient vector 'φ' (phi) for a 'p'th order Auto-Regressive 'AR(p)'
     *  model.
     *  <p>
     *      z_t = φ_0 * z_t-1 + ... + φ_p-1 * z_t-p + e_t
     *  <p>
     *  Uses the Durbin-Levinson Algorithm to determine the coefficients.
     *  The 'φ' vector is 'p'th row of 'psi' matrix (ignoring the first (0th) column).
     *  @param yy  the actual response/output vector
     */
    def train (yy: VectoD = y): AR =
    {
        durbinLevinson (acv, ml)                           // pass in auto-covariance and max lags
//      if (DEBUG) println ("psi = " + psi)
        pacf = psi.getDiag ()                              // PACF is the diagonal of the psi matrix
        phi  = psi(p).slice (1, p+1)                       // AR(p) coefficients: φ_0, ..., φ_p-1
        if (DEBUG) println (s"parameters for AR($p): φ = $phi")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain/fit an `AR` model to the times series data using another order 'pp'
     *  Estimate the coefficient vector 'φ' (phi) for a 'p'th order Auto-Regressive 'AR(p)'
     *  model.
     *  <p>
     *      z_t = φ_0 * z_t-1 + ... + φ_p-1 * z_t-p + e_t
     *  <p>
     *  Uses the Durbin-Levinson Algorithm to determine the coefficients.
     *  The 'φ' vector is 'p'th row of 'psi' matrix (ignoring the first (0th) column).
     *  @param yy  the actual response/output vector
     *  @param pp  another order
     */
    def retrain (pp: Int): AR =
    {
        p = pp
        resetDF (p, m - p)
        phi = psi(p).slice (1, p+1)                        // AR(p) coefficients: φ_0, ..., φ_p-1
        if (DEBUG) println (s"parameters for AR($p): φ = $phi")
        this
    } // retrain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector.
     */
    def parameter: VectoD = phi

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Durbin-Levinson Algorithm to iteratively compute the 'psi' matrix.
     *  The last row of the matrix gives 'AR' coefficients.
     *  @see www.stat.tamu.edu/~suhasini/teaching673/time_series.pdf, p. 247
     *  @param g   the auto-covariance vector (gamma)
     *  @param ml  the maximum number of lags
     */
    private def durbinLevinson (g: VectoD, ml: Int)
    {
        val r   = new VectorD (ml+1); r(0) = g(0)

        for (k <- 1 to ml) {                               // range up to max lags
            var sum = 0.0
            for (j <- 1 until k) sum += psi(k-1, j) * g(k-j)
            val a = (g(k) - sum) / r(k-1)
            psi(k, k) = a
            for (j <- 1 until k) psi(k, j) = psi(k-1, j) - a * psi(k-1, k-j)
            r(k) = r(k-1) * (1.0 - a * a)
        } // for
    } // durbinLevinson

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a 'p'th order Auto-Regressive 'AR(p)' model.
     */
    def predictAll: VectoD =
    {
        val zp = new VectorD (m)                           // forecasts for all z
        for (t <- 0 until m) {
            var sum = 0.0
            for (j <- 0 until p if t-j > 0) sum += phi(j) * z(t-1-j)
            zp(t) = sum
        } // for
        e = z - zp
        zp + mu                                            // return the vector of predicted values
    } // predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the multi-step forecast for AR models.
     *  @param steps  the number of steps to forecast, must be at least one.
     */
    def forecast (steps: Int = 1): VectoD =
    {
        val zf = z.slice (z.dim - p) ++ new VectorD (steps)
        for (k <- p until zf.dim) {                        // start at t = p (enough data and first value to forecast)
            var sum = 0.0
            for (j <- 0 until p) sum += phi(j) * zf(k-1-j)
            zf(k) = sum
        } // for
        zf.slice (p) + mu                                  // return the vector of forecasts
    } // forecast

} // AR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR` companion object provides factory methods for the `AR` class.
 */
object AR
{
    /** Base hyper-parameter specification for `LassoRegression`
     */
    val hp = new HyperParameter; hp += ("p", 1, 1)

} // AR object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARTest` object is used to test the `AR` class.
 *  > runMain scalation.analytics.forecaster.ARTest
 */
object ARTest extends App
{
    val ran = Random ()
    val n = 100
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield t(i) + 10.0 * ran.gen)

    banner ("Build AR(1) Model")
    val ar = new AR (y)                            // time series data
    ar.train ().eval ()                            // train for AR(1) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, "Plot of y, AR(1) vs. t", true)

    banner ("Select model based on ACF and PACF")
    ar.plotFunc (ar.acF, "ACF")
    ar.plotFunc (ar.pacF, "PACF")

    banner ("Build AR(2) Model")
    ar.retrain (2).eval ()                         // retrain for AR(2) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, "Plot of y, AR(2) vs. t", true)

} // ARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARTest2` object is used to test the `AR` class.  The order 'p' hyper-parameter
 *  is re-assigned.
 *  > runMain scalation.analytics.forecaster.ARTest2
 */
object ARTest2 extends App
{
    val noise = Normal (0.0, 1.0)
    val n = 30
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield i + noise.gen)

    val p = 2                                      // try different values for p
    banner ("Build AR($p) Model")
    AR.hp("p") = p                                 // reassign hyper-parameter p
    val ar = new AR (y)                            // time series data
    ar.train ().eval ()                            // train for AR(p) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, s"Plot of y, ar($p) vs. t", true)

    banner ("Make Forecasts")
    val steps = 10                                 // number of steps for the forecasts
    val ar_f = ar.forecast (steps)
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")
    val tf = VectorD.range (n, n + steps)
    new Plot (tf, ar_f, null, s"Plot ar($p) forecasts vs. t", true)

} // ARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARTest3` object is used to test the `AR` class.  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.analytics.forecaster.ARTest3
 */
object ARTest3 extends App
{
    import ForecasterVec.{t, y}

    banner ("Build AR(1) Model")
    val ar = new AR (y)                            // time series data
    ar.train ().eval ()                            // train for AR(1) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, s"Plot of y, AR(1) vs. t", true)

    banner ("Select model based on ACF and PACF")
    ar.plotFunc (ar.acF, "ACF")
    ar.plotFunc (ar.pacF, "PACF")

    banner ("Build AR(2) model")
    ar.retrain (2).eval ()                         // retrain for AR(2) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, s"Plot of y, AR(2) vs. t", true)

    banner ("Make Forecasts")
    val steps = 2                                  // number of steps for the forecasts
    val ar_f = ar.forecast (steps)
    println (s"$steps-step ahead forecasts using AR(2) model = $ar_f")

} // ARTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARTest4` object is used to test the `AR` class.  The order hyper-parameter 'p'
 *  is reassigned.
 *  > runMain scalation.analytics.forecaster.ARTest4
 */
object ARTest4 extends App
{
    val path = BASE_DIR + "travelTime.csv"
    val data = MatrixD (path)

    val (t, y) = (data.col(0), data.col(1))

    val p = 1                                      // try different values for p
    banner (s"Build AR($p) model")
    AR.hp("p") = p                                 // reassign hyper-parameter p
    val ar = new AR (y)                            // time series data
    ar.train ().eval ()                            // train for AR(p) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, s"Plot of y, ar($p) vs. t", true)

    banner ("Make Forecasts")
    val steps = 1                                  // number of steps for the forecasts
    val ar_f = ar.forecast (steps)
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")

} // ARTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARTest5` object is used to test the `AR` class.
 *  > runMain scalation.analytics.forecaster.ARTest5
 */
object ARTest5 extends App
{
    val sig2  = 10000.0
    val noise = Normal (0.0, sig2)
    val n = 50
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield 40 * (i-1) - (i-2) * (i-2) + noise.gen)

    banner ("Build AR(1) model")
    val ar = new AR (y)                            // time series data
    ar.train ().eval ()                            // train for AR(1) model
    println (ar.report)
    new Plot (t, y, ar.predictAll, s"Plot of y, AR(1) vs. t", true)

    banner ("Select model based on ACF and PACF")
    ar.plotFunc (ar.acF, "ACF")
    ar.plotFunc (ar.pacF, "PACF")

    banner ("Build AR(2) model")
    ar.retrain (2).eval ()                         // retrain for AR(2) model 
    println (ar.report)
    new Plot (t, y, ar.predictAll, s"Plot of y, AR(2) vs. t", true)

    banner ("Make Forecasts")
    val steps = 2                                  // number of steps for the forecasts
    val ar_f = ar.forecast (steps)
    println (s"$steps-step ahead forecasts using AR(2) model = $ar_f")

} // ARTest5 object

