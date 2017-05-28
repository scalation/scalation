
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.3
 *  @date    Sat Jan 12 20:12:12 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import scala.math.min

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.plot.Plot
import scalation.random.Random
import scalation.stat.vectorD2StatVector
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARMA` class provide basic time series analysis capabilities for Auto-
 *  Regressive 'AR' and Moving-Average 'MA' models.  In an 'ARMA(p, q)' model,
 *  'p' and 'q' refer to the order of the Auto-Regressive and Moving-Average
 *  components of the model.  `ARMA` models are often used for forecasting.
 *  Given time series data stored in vector 'y', its next value 'y_t = y(t)'
 *  may be predicted based on prior values of 'y' and its noise:
 *  <p>
 *      y_t = c + Σ(φ_i y_t-i) + Σ(θ_i e_t-i) + e_t
 *  <p>
 *  where 'c' is a constant, 'φ' is the autoregressive coefficient vector,
 *  'θ' is the moving-average coefficient vector, and 'e' is the noise vector.
 *------------------------------------------------------------------------------
 *  @param y  the input vector (time series data)
 *  @param t  the time vector
 */
class ARMA (y: VectoD, t: VectoD)
      extends Predictor with Error
{
    private val DEBUG = true                            // debug flag
    private val n    = y.dim                            // size of the input vector
    private val m    = min (n, 20)                      // maximum lag to consider
            val mu   = y.sum / n.toDouble               // the sample mean
    private val x    = (y - mu).asInstanceOf [VectorD]  // work with mean zero time series
            val sig2 = x.variance                       // the sample variance
    private val ac   = new VectorD (m+1)                // auto-covariance
    for (t <- 0 until m+1) ac(t) = x acov t
            val acf = ac / sig2                         // Auto-Correlation Function (ACF)
            var pacf: VectoD = null                     // Partial Auto-Correlation Function (PACF)

    if (DEBUG) {
        println ("n    = " + n)                         // number of data points
        println ("mu   = " + mu)                        // mean
        println ("sig2 = " + sig2)                      // variance
        println ("ac   = " + ac)                        // auto-covariance
        println ("acf  = " + acf)                       // ACF
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Auto-Regressive 'AR(p)' models
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficient vector 'φ' for a 'p'th order Auto-Regressive 'AR(p)'
     *  model.
     *  <p>
     *      x_t = φ_0 * x_t-1 + ... + φ_p-1 * x_t-p + e_t
     *  <p>
     *  Uses the Durbin-Levinson Algorithm to determine the coefficients.
     *  The 'φ' vector is 'p'th row of 'psi' matrix (ignoring the first (0th) column).
     *  @param p  the order of the AR model
     */
    def est_ar (p: Int = 1): VectoD =
    {
        val φ = durbinLevinson (p).slice (1, p+1)       // AR(p) coefficients: φ_0, ..., φ_p-1
        println ("coefficients for AR(" + p + "): φ = " + φ)
        φ
    } // est_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Durbin-Levinson Algorithm to iteratively compute the 'psi' matrix.
     *  The last row of the matrix gives 'AR' coefficients.
     *  @see www.stat.tamu.edu/~suhasini/teaching673/time_series.pdf
     */
    def durbinLevinson: MatrixD =
    {
        val psi = new MatrixD (m+1, m+1)
        val r   = new VectorD (m+1); r(0) = ac(0)
        for (t <- 1 to m) {
            var sum = 0.0
            for (j <- 1 until t) sum += psi(t-1, j) * ac(t-j)
            val a = (ac(t) - sum) / r(t-1)
            psi(t, t) = a
            for (j <- 1 until t) psi(t, j) = psi(t-1, j) - a * psi(t-1, t-j)
            r(t) = r(t-1) * (1.0 - a * a)
        } // for
        println ("psi = " + psi)
        pacf = psi.getDiag ().slice (1, m+1)            // PACF is the diagonal of the psi matrix
        psi                                             // return the psi matrix
    } // durbinLevinson

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a 'p'th order Auto-Regressive 'AR(p)'
     *  model.
     *  @param φ  the estimated AR(p) coefficients
     */
    def ar (φ: VectoD): VectoD =
    {
        val p = φ.dim                                   // order p for AR(p) model
        val f = new VectorD (n)                         // forecasts for x
        for (t <- p until n) {                          // start at t = p (enough data)
            var sum = 0.0
            for (j <- 0 until p) sum += φ(j) * x(t-1-j)
            f(t) = sum
        } // for
        f                                               // return the vector of forecasts
    } // ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Moving-Average 'MA(q)' models
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficient vector 'θ' for a 'q'th order a Moving-Average 'MA(q)'
     *  model.
     *  Caveat:  only works for q = 1
     *  <p>
     *      x_t = θ_0 * e_t-1 + ... + θ_q-1 * e_t-q + e_t
     *  <p>
     *  @param q  the order of the AR model
     */
    def est_ma (q: Int = 1): VectoD =
    {
        if (q != 1) flaw ("ma", "only works for q = 1, use ARMA instead")
        val θ = methodOfInnovations                     // MA(q) coefficients: θ_0
        println ("coefficients for MA(" + q + "): θ = " + θ)
        θ           
    } // est_ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Method of Innovation to estimate coefficients for MA(q) model.
     *  Caveat:  only works for q = 1
     *  @see http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010/lectures/10.pdf
     */
    def methodOfInnovations: VectoD =
    {
        val θ = new VectorD (1)                         // MA(q) coefficients: θ_0
        // FIX - to be implemented
        θ           
    } // methodOfInnovations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a 'q'th order Moving-Average (MA)
     *  model.
     *  Caveat:  only works for q = 1
     *  @param θ  the estimated MA(q) coefficients
     */
    def ma (θ: VectoD): VectoD =
    {
        val q = θ.dim
        if (q != 1) flaw ("ma", "only works for q = 1, use ARMA instead")

        val k = n-q
        val f = new VectorD (k)                         // forecasts for x
        for (t <- 0 until k) {
            var sum = 0.0
            for (j <- 1 until min (k, t)) sum += θ(0)~^j * x(t-j)
            f(t) = sum
        } // for
        f                                               // return the vector of forecasts
    } // ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Auto-Regressive Moving-Average 'ARMA(p, q)' models
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficient vectors φ and θ for a ('p'th, 'q'th) order Auto-Regressive
     *  Moving-Average 'ARMA(p, q)' model.
     *  <p>
     *      x_t = φ_0 * x_t-1 + ... + φ_p-1 * x_t-p +
     *            θ_0 * e_t-1 + ... + θ_q-1 * e_t-q + e_t
     *  <p>
     *  @see www.math.kth.se/matstat/gru/sf2943/tsform.pdf
     *  @param p  the order of the AR part of the model
     *  @param q  the order of the MA part of the model
     */
    def est_arma (p: Int = 1, q: Int = 1): (VectoD, VectoD) =
    {
        val φθ = hannanRissanen (p, q)
        val φ   = φθ.slice (0, p)                        // AR(p) coefficients: φ_0, ..., φ_p-1
        val θ   = φθ.slice (p, p+q)                      // MA(q) coefficients: θ_0, ..., θ_q-1
        (φ, θ)
    } // est_arma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Hannan-Rissanen Algorithm to estimate the 'ARMA(p, q)' coefficients.
     *  @see halweb.uc3m.es/esp/Personal/personas/amalonso/esp/TSAtema9.pdf
     *  FIX - correct and complete implementation
     *  @param p  the order of the AR part of the model
     *  @param q  the order of the MA part of the model
     */
    def hannanRissanen (p: Int, q: Int): VectoD =
    {
        // use a high Auto-Regression model for initial estimates of residuals
        val k = p + q + 7
        val φφ = durbinLevinson (k).slice (1, p+1)       // AR(k) coefficients: φ_0, ..., φ_k-1
        println ("coefficients for AR(" + k + "): φφ = " + φφ)
        val e = new VectorD (n)                          // FIX - get noise values from AR(k)

        // estimate residuals a(t) 
        val a = new VectorD (n - k)
        for (t <- k until n) {
            var sum = 0.0
            for (i <- 0 until k) sum += φφ(i) * e(t-i)
            a(t) = e(t) - sum
        } // for

        // estimate coefficients using regression on noise and residuals
        val ax = new MatrixD (n, p + q + 1)
        val ay = new VectorD (n)
        // FIX - fill in ax and ay
        val reg = new Regression (ax, ay)
        reg.train ()
        reg.coefficient
    } // hannanRissanen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a ('p'th, 'q'th) order Auto-Regressive
     *  Moving-Average 'ARMA(p, q)' model.
     *  @param φ  the estimated AR(p) coefficients
     *  @param θ  the estimated MA(q) coefficients
     */
    def arma (φ: VectoD, θ: VectoD): VectoD =
    {
        null                 // FIX: to be implemented
    } // arma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARMA` model to times series data.
     */
    def train ()
    {
        // FIX: to be implemented
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.
     */
    def fit: VectoD =
    {
        VectorD (0.0)        // FIX: not yet implemented
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For all the time points in vector t, predict the value of y = f(t) by ...
     *  @param z  the time-vector indicating time points to forecast
     */
    def predict (y: VectoD): Double = 
    {
        0.0                  // FIX: not yet implemented
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot a function, e.g.,  Auto-Correlation Function 'ACF', Partial Auto-Correlation
     *  Function 'PACF'.
     *  @param fVec  the vector given function values
     *  @param name  the name of the function
     */
    def plotFunc (fVec: VectoD, name: String)
    {
        val lag_axis = new VectorD (m+1)
        for (i <- 0 until fVec.dim) lag_axis(i) = i
        val zero = new VectorD (m+1)
        new Plot (lag_axis, fVec, zero, "Plot of " + name)
    } // plotFunc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the 'y' vector by taking the 'l'th order moving average.
     *  @param l  the number of points to average
     */
    def smooth (l: Int): VectoD =
    {
        val ld = l.toDouble
        val z  = new VectorD (n-l)
        for (i <- 0 until n-l) z(i) = y(i until i+l).sum / ld
        z
    } // smooth

} // ARMA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARMATest` object is used to test the `ARMA` class.
 */
object ARMATest extends App
{
    def genTimes (n: Int): VectoD =
    {
        val t = new VectorD (n)
        for (i <- 0 until n) t(i) = i.toDouble
        t
    } // genTimes

    val n = 100
    val r = Random ()
    val t = genTimes (n)
    val y = new VectorD (n)
    for (i <- 0 until n) y(i) = t(i) + 10.0 * r.gen

    val ts = new ARMA (y, t)   // time series data: y vs. t

    ts.plotFunc (ts.acf, "ACF")

    // Build AR(1), AR(2) and MA(1) models for the time series data

    val φ_a = ts.est_ar (1)
    new Plot (t, y, ts.ar (φ_a) + ts.mu, "Plot of y, ar(1) vs. t")

    val φ_b = ts.est_ar (2)
    new Plot (t, y, ts.ar (φ_b) + ts.mu, "Plot of y, ar(2) vs. t")

    val θ = ts.est_ma (1)                                       // FIX
    new Plot (t, y, ts.ma (θ) + ts.mu, "Plot of y, ma(1) vs. t")

    ts.plotFunc (ts.pacf, "PACF")

} // ARMATest object

