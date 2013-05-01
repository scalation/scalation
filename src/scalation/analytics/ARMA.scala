
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
    private val DEBUG = true                      // debug flag
    private val n    = y.dim                      // size of the input vector
    private val m    = min (n, 20)                // maximum lag to consider
            val mu   = y.sum / n.toDouble         // the sample mean
    private val x    = new StatVector (y - mu)    // work with mean zero time series
            val sig2 = x.variance                 // the sample variance
    private val c    = new VectorD (m+1)          // auto-covariance
    for (t <- 0 until m+1) c(t) = x.acov (t)
            val acf = c / sig2                    // Auto-Correlation Function (ACF)
            var pacf: VectorD = null              // Partial Auto-Correlation Function (PACF)

    if (DEBUG) {
        println ("n    = " + n)                   // number of data points
        println ("mu   = " + mu)                  // mean
        println ("sig2 = " + sig2)                // variance
        println ("c    = " + c)                   // auto-covariance
        println ("acf  = " + acf)                 // ACF
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficients for a pth order Auto-Regressive AR(p) model.
     *  x_t = phi_0 * x_t-1 + ... + phi_p-1 * x_t-p + e_t
     *  Uses the Durbin-Levinson Algorithm to determine the coefficients.
     *  The phi vector is pth row of psi matrix (ignoring the first (0th) column).
     *  @param p  the order of the AR model
     */
    def est_ar (p: Int = 1): VectorD =
    {
        val phi = durbinLevinson(p).slice (1, p+1)   // AR(p) coefficients: phi_0, ..., phi_p-1
        println ("for AR(" + p + "): phi = " + phi)
        phi
    } // est_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Durbin-Levinson Algorithm to iteratively compute the psi matrix.
     *  The last row of the matrix gives AR coefficients.
     *  @see http://www.stat.tamu.edu/~suhasini/teaching673/time_series.pdf
     */
    def durbinLevinson: MatrixD =
    {
        val psi = new MatrixD (m+1, m+1)
        val r   = new VectorD (m+1); r(0) = c(0)
        for (t <- 1 to m) {
            var sum = 0.
            for (j <- 1 until t) sum += psi(t-1, j) * c(t-j)
            val a = (c(t) - sum) / r(t-1)
            psi(t, t) = a
            for (j <- 1 until t) psi(t, j) = psi(t-1, j) - a * psi(t-1, t-j)
            r(t) = r(t-1) * (1. - a * a)
        } // for
        println ("psi = " + psi)
        pacf = psi.getDiag ().slice (1, m+1)    // PACF is the diagonal of the psi matrix
        psi                                     // return the psi matrix
    } // durbinLevinson

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
     *  x_t = e_t + theta_0 * e_t-1 + ... + theta_q-1 * e_t-q
     *  @param theta  the estimated MA(q) coefficients
     */
    def ma (theta: VectorD): VectorD =
    {
        val q = theta.dim
        if (q != 1) flaw ("est_ma", "estimation for q > 1 not yet implemented")

        val k = n-q
        val f = new VectorD (k)            // forecasts for x
        for (t <- 0 until k) {
            var sum = 0.
            for (j <- 1 until min (k, t)) sum += theta(0)~^j * x(t-j)
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
    /** Plot a function, e.g.,  Auto-Correlation Function (ACF), Partial ACF (PACF).
     *  @param fVec  the vector given function values
     *  @param name  the name of the function
     */
    def plotFunc (fVec: VectorD, name: String)
    {
        val lag_axis = new VectorD (m+1)
        for (i <- 0 until fVec.dim) lag_axis(i) = i
        val zero = new VectorD (m+1)
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

    ts.plotFunc (ts.acf, "ACF")

    // Build AR(1), AR(2) and MA(1) models for the time series data

    val phi_a = ts.est_ar (1)
    new Plot (t, y, ts.ar (phi_a) + ts.mu, "Plot of y, ar(1) vs. t")

    val phi_b = ts.est_ar (2)
    new Plot (t, y, ts.ar (phi_b) + ts.mu, "Plot of y, ar(2) vs. t")

    val theta = ts.est_ma (1)                                       // FIX
    new Plot (t, y, ts.ma (theta) + ts.mu, "Plot of y, ma(1) vs. t")

    ts.plotFunc (ts.pacf, "PACF")

} // ARMATest object

