
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.0
 *  @date    Sat Jan 12 20:12:12 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import math.min

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.DoubleWithExp._
import scalation.plot.Plot
import scalation.random.Random
import scalation.stat.StatVector
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class provide basic time series analysis capabilities for
 *  Auto-Regressive (AR) and Moving Average (MA) models.
 *  In an ARMA(p, q) model, p and q refer to the order of the
 *  Auto-Regressive and Moving Average components of the model. 
 *  ARMA models are often used for forecasting.
 *  @param y  the input vector (time series data)
 *  @param t  the time vector
 */
class ARMA (y: VectorD, t: VectorD)
      extends Predictor with Error
{
    private val n = y.dim                   // size of the input vector
            val mu = y.sum / n.toDouble     // the mean
    private val x = y - mu                  // work with mean zero time series

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficients for a pth order Auto-Regressive AR(p) model.
     *  x_t = phi_0 * x_t-1 + ... + phi_p-1 * x_t-p + e_t
     *  FIX: use Durbin-Levinson Algorithm
     *  @param p  the order of the AR model
     */
    def est_ar (p: Int = 1): VectorD =
    {
        if (p != 1) flaw ("est_ar", "estimation for p > 1 not yet implemented")

        val phi = new VectorD (p)      // AR(p) coeffiecients: phi_0, ..., phi_p-1
        var sum1 = 0.; var sum2 = 0.
        for (i <- 1 until n) {
            sum1 += x(i) * x(i-1)
            sum2 += x(i-1) * x(i-1)
        } // for
        phi(0) = sum1 / sum2
        phi
    } // est_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a pth order Auto-Regressive (AR(p))
     *  model.
     *  f_t = phi_0 * x_t-1 + ... + phi_p-1 * x_t-p + e_t
     *  @param phi  the estimated AR(p) coefficients
     */
    def ar (phi: VectorD): VectorD =
    {
        val p = phi.dim                    // order p for AR(p) model
        val f = new VectorD (n)            // forecasts for x
        for (t <- p until n) {             // start at t = p (enough data)
            var sum = 0.
            for (j <- 0 until p) sum += phi(j) * x(t-1-j)
            f(t) = sum
        } // for
        f                                  // return the vector of forecasts
    } // ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficients for a qth order a Moving Average(q) model.
     *  x_t = e_t - theta_0 * e_t-1 - ... - theta_q-1 * e_t-q
     *  FIX: use Method of Innovations
     *  @param p  the order of the AR model
     */
    def est_ma (q: Int = 1): VectorD =
    {
        if (q != 1) flaw ("est_ma", "estimation for q > 1 not yet implemented")

        val theta = new VectorD (q)          // MA(q) cofficients: theta_0, ..., theta_q-1
        theta(0) = 0.      // FIX: need to estimate theta(0)
        theta           
    } // est_ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a qth order Moving Average (MA)
     *  model.
     *  x_t = e_t - theta_0 * e_t-1 - ... - theta_q-1 * e_t-q
     *  @param theta  the estimated MA(q) coefficients (theta)
     */
    def ma (theta: VectorD): VectorD =
    {
        val q = theta.dim
        if (q != 1) flaw ("est_ma", "estimation for q > 1 not yet implemented")

        val k = n-q
        val f = new VectorD (k)            // forecasts for x
        for (t <- 0 until k) {
            var sum = 0.
            for (j <- 1 until min (k, t)) sum -= theta(0)~^j * x(t-j)
            f(t) = sum
        } // for
        f                                  // return the vector of forecasts
    } // ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fit an ARMA model to times series data.
     */
    def train ()
    {
        // FIX: to be implemented
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For all the time points in vector t, predict the value of y = f(t) by ...
     *  @param z  the time-vector indicating time points to forecast
     */
    def predict (y: VectorD): Double = 
    {
        0.    // FIX: not yet implemented
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several time vectors, forecast the y-values.
     *  @param z  the matrix containing row time-vectors to use for prediction
     */
    def predict (z: MatrixD): VectorD =
    {
        throw new UnsupportedOperationException ()
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Auto-Correlation Function (ACF) for lags 1 to m.
     *  @param m  the maximum lag to examine
     */
    def acf (m: Int = 20): VectorD =
    {
        val lags  = min (m, x.dim)
        val statV = new StatVector (x)          // make a stat vector for the time series data
        val acfV  = new VectorD (lags)
        for (j <- 0 until lags) acfV(j) = statV.acorr (j)
        acfV
    } // acf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Partial Auto-Correlation Function (PACF) for lags 1 to m.
     *  @param m  the maximum lag to examine
     */
    def pacf (m: Int = 20): VectorD =
    {
        val lags  = min (m, x.dim)
        val statV = new StatVector (x)          // make a stat vector for the time series data
        val pacfV = new VectorD (lags)
        for (j <- 0 until lags) pacfV(j) = 0.   // FIX
        pacfV
    } // pacf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot a function, e.g.,  Auto-Correlation Function (ACF), Partial ACF (PACF).
     *  @param fVec  the vector given function values
     *  @param name  the name of the function
     */
    def plotFunc (fVec: VectorD, name: String)
    {
        val lag_axis = new VectorD (fVec.dim)
        for (i <- 0 until fVec.dim) lag_axis(i) = i
        val zero = new VectorD (fVec.dim)
        new Plot (lag_axis, fVec, zero, "Plot of " + name)
    } // plotFunc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the y vector by taking the lth order moving average.
     *  @param l  the number of points to average
     */
    def smooth (l: Int): VectorD =
    {
        val ld = l.toDouble
        val z  = new VectorD (n-l)
        for (i <- 0 until n-l) z(i) = y(i until i+l).sum / ld
        z
    } // smooth

} // ARMA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the ARMA class.
 */
object ARMATest extends App
{
    def genTimes (n: Int): VectorD =
    {
        val t = new VectorD (n)
        for (i <- 0 until n) t(i) = i.toDouble
        t
    } // genTimes

    val n = 100
    val r = Random ()
    val t = genTimes (n)
    val y = new VectorD (n)
    for (i <- 0 until n) y(i) = t(i) + 10. * r.gen

    val ts = new ARMA (y, t)   // time series data: y vs. t

    // Examine the times series data:
    // (a) smooth the data, (b) compute ACF, compute PACF

    new Plot (t, y, ts.smooth (5), "Plot of y, smooth vs. t")
    ts.plotFunc (ts.acf (), "ACF")
    ts.plotFunc (ts.pacf (), "PACF")         // FIX

    // Build AR(1) and MA(1) models for the time series data

    val phi = ts.est_ar (1)
    new Plot (t, y, ts.ar (phi) + ts.mu, "Plot of y, ar(1) vs. t")

    val theta = ts.est_ma (1)                                 // FIX
    new Plot (t, y, ts.ma (theta) + ts.mu, "Plot of y, ma(1) vs. t")

} // ARMATest object

