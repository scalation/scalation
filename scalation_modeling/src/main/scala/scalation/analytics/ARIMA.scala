
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Michael Cotterell
 *  @version 1.4
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

package scalation.analytics

import scala.math.{log, max, min}
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.random.{Normal, Random}
import scalation.stat.vectorD2StatVector
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARIMA` class provides basic time series analysis capabilities for Auto-
 *  Regressive 'AR' Integrated 'I' Moving-Average 'MA' models.  In an
 *  'ARIMA(p, d, q)' model, 'p' and 'q' refer to the order of the Auto-Regressive
 *  and Moving-Average components of the model; 'd' refers to the order of
 *  differencing. `ARIMA` models are often used for forecasting.
 *  Given time series data stored in vector 'y', its next value 'y_t = y(t)'
 *  may be predicted based on prior values of 'y' and its noise:
 *  <p>
 *      y_t = c + Σ(φ_i y_t-i) + Σ(θ_i e_t-i) + e_t
 *  <p>
 *  where 'c' is a constant, 'φ' is the autoregressive coefficient vector,
 *  'θ' is the moving-average coefficient vector, and 'e' is the noise vector.
 *  If 'd' > 0, then the time series must be differenced first before applying
 *  the above model.
 *------------------------------------------------------------------------------
 *  @param y  the input vector (time series data)
 *  @param t  the time vector
 *  @param d  the order of Integration
 */
class ARIMA (y: VectoD, t: VectoD, d: Int = 0)
      extends Predictor with Error
{
    private val DEBUG = false                           // debug flag
            val mu    = y.mean                          // the sample mean
    private val xx    = new VectorD (y - mu)            // work with mean zero time series
    private val x     = difference ()                   // difference the time series
    private val n     = x.dim                           // size of the input vector
    private val mxLag = 20                              // maximum lag to consider
    private val m     = min (n, mxLag)                  // maximum lag to consider
    private val sig2  = x.variance                      // the sample variance
    private val ac    = new VectorD (m+1)               // auto-covariance
    for (t <- ac.range) ac(t) = x acov t
            val acf   = ac / sig2                       // Auto-Correlation Function (ACF)
            var pacf: VectoD = null                     // Partial Auto-Correlation Function (PACF)

    private var p = 0                                   // order of AR component
    private var q = 0                                   // order of MA component
    private var φ: VectoD = null                        // AR(p) coefficients
    private var θ: VectoD = null                        // MA(q) coefficients
    e = new VectorD (n)                                 // vector of residuals

    if (DEBUG) {
        println ("n    = " + n)                         // number of data points
        println ("x    = " + x)                         // zero-mean time series
        println ("mu   = " + mu)                        // mean
        println ("sig2 = " + sig2)                      // variance
        println ("ac   = " + ac)                        // auto-covariance
        println ("acf  = " + acf)                       // ACF
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Integrated 'I(d)' models.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Difference the time series.
     */
    def difference (): VectorD =                                                // FIX - should return trait - currently fails
    {
        d match {
            case 0 => xx
            case 1 => VectorD (for (i <- 0 until xx.dim-1) yield xx(i+1)-xx(i))
            case 2 => VectorD (for (i <- 0 until xx.dim-2) yield xx(i+2)-2*xx(i+1)+xx(i))
            case 3 => VectorD (for (i <- 0 until xx.dim-3) yield xx(i+3)-3*xx(i+2)+3*xx(i+1)-xx(i))
            case _ => println ("ARIMA does not support differencing higher than order 3"); null
        } // match
    } // difference

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the predictions/fitted values of a differenced time series back
     *  to the original scale.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xp  the vector of predictions/fitted values of a differenced time series
     */
    def transformBack (xp: VectoD): VectoD =
    {
        d match {
            case 0 => xp
            case 1 => val tbx = new VectorD (xx.dim)
                      tbx(0)  = xx(0)
                      for (i <- 0 until xx.dim-1) tbx(i+1) = xx(i) + xp(i)
                      e = xx - tbx
                      tbx
            case 2 => val tbx = new VectorD (xx.dim)
                      tbx(0)  = xx(0)
                      tbx(1)  = xx(1)
                      for (i <- 0 until xx.dim-2) tbx(i+2) = xx(i+1) + xp(i)
                      e = xx - tbx
                      tbx
            case 3 => val tbx = new VectorD (xx.dim)
                      tbx(0)  = xx(0)
                      tbx(1)  = xx(1)
                      tbx(2)  = xx(2)
                      for (i <- 0 until xx.dim-3) tbx(i+3) = xx(i+2) + xp(i)
                      e = xx - tbx
                      tbx
            case _ => println ("ARIMA does not support differencing higher than order 3"); null
        } // match
    } // transformBack

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the forecasted values of a differenced time series back to the
     *  original scale.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xf   the vector of forecasted values of a differenced time series
     */
    def transformBack_f (xf: VectoD): VectoD =
    {
        if (d > 0) {
            xf(0) += xx.last
            for (i <- 1 until xf.dim) xf(i) += xf(i-1)
        } // if
        xf
    } // transformBack_f

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
     *  @param p_  the order of the AR model
     */
    def est_ar (p_ : Int = 1): VectoD =
    {
        p = p_
        φ = durbinLevinson (p).slice (1, p+1)            // AR(p) coefficients: φ_0, ..., φ_p-1
        if (DEBUG) println (s"coefficients for AR($p): φ = $φ")
        φ
    } // est_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Durbin-Levinson Algorithm to iteratively compute the 'psi' matrix.
     *  The last row of the matrix gives 'AR' coefficients.
     *  @see www.stat.tamu.edu/~suhasini/teaching673/time_series.pdf
     */
    def durbinLevinson: MatriD =
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
        if (DEBUG) println ("psi = " + psi)
        pacf = psi.getDiag ().slice (1, m+1)             // PACF is the diagonal of the psi matrix
        psi                                              // return the psi matrix
    } // durbinLevinson

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a 'p'th order Auto-Regressive 'AR(p)' model.
     *  @param transBack  flag that determines whether to return the predicted values
     *                    in the original scale
     */
    def predict_ar (transBack: Boolean = true): VectoD =
    {
        val xp = new VectorD (n)                         // forecasts for x
        for (t <- 0 until n) {
            var sum = 0.0
            for (j <- 0 until p if t-j > 0) sum += φ(j) * x(t-1-j)
            xp(t) = sum
        } // for
        e = x - xp
        if (transBack) transformBack (xp) else xp        // return the vector of predicted
    } // predict_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the multi-step forecast for AR models.
     *  @param step  the number of steps to forecast, must be at least one.
     */
    def forecast_ar (steps: Int = 1): VectoD =
    {
        val xf = x.slice (x.dim - p) ++ new VectorD (steps)

        for (t <- p until xf.dim) {                      // start at t = p (enough data and first value to forecast)
            var sum = 0.0
            for (j <- 0 until p) sum += φ(j) * xf(t-1-j)
            xf(t) = sum
        } // for
        transformBack_f (xf.slice(p))                    // return the vector of forecasts
    } // forecast_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Moving-Average 'MA(q)' models
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficient vector 'θ' for a 'q'th order a Moving-Average 'MA(q)'
     *  model.
     *  <p>
     *      x_t = θ_0 * e_t-1 + ... + θ_q-1 * e_t-q + e_t
     *  <p>
     *  @param q_  the order of the AR model
     */
    def est_ma (q_ : Int = 1): VectoD =
    {
        q = q_
        θ = methodOfInnovations ()                       // MA(q) coefficients: θ_0, ..., θ_q-1
        if (DEBUG) println (s"coefficients for MA($q): θ = $θ")
        θ
    } // est_ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Method of Innovation to estimate coefficients for MA(q) model.
     *  @see www.stat.berkeley.edu/~bartlett/courses/153-fall2010/lectures/10.pdf
     *  @see www.math.kth.se/matstat/gru/sf2943/tsform.pdf
     */
    def methodOfInnovations (): VectoD =
    {
        val v = new VectorD (q+1)
        v(0)  = ac(0)                                    // auto-covariance (gamma) for lag 0
        val theta = new MatrixD (q, q)

        for (l <- 0 until q) {
            for (k <- 0 to l) {
                var sum1 = 0.0
                for (j <- 0 until k) sum1 += theta(l, l-j) * theta(k-1, k-j-1) * v(j)
                theta(l, l-k) = 1.0 / v(k) * (ac (l-k+1) - sum1)
            } // for
            var sum2 = 0.0
            for (j <- 0 to l) sum2 += theta(l, l-j) * theta(l, l-j) * v(j)
            v(l+1) = v(0) - sum2
        } // for
        theta (q-1)
    } // methodOfInnovations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector of predictions of an MA model and update the residuals
     *  @param transBack  flag that determines whether to return the predicted values
     *                    in the original scale
     */
    def predict_ma (transBack: Boolean = true): VectoD =
    {
        val xp = new VectorD (n)
        for (t <- xp.range) {
            var sum = 0.0
            for (j <- 0 until q if t-j > 0) sum += θ(j) * e(t-1-j)
            xp(t) = sum
            e(t)  = x(t) - sum
        } // for
        if (transBack) transformBack (xp) else xp
    } // predict_ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the one-step forecast for MA models
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param steps  the number of steps to forecast, must be at least one.
     */
    def forecast_ma (steps: Int = 1): VectoD =
    {
        if (steps > q) flaw ("forecast_ma", s"MA($q) is not forecastable for more than $q step(s) ahead")
        if (e.countZero == e.dim) predict_ma (false)         // update the residuals

        val xf = x.slice (x.dim - q) ++ new VectorD (steps)
        val ef = e.slice (e.dim - q) ++ new VectorD (steps)

        for (t <- q until xf.dim) {                          // start at t = q (enough data and first value to forecast)
            var sum = 0.0
            for (j <- 0 until q) sum += θ(j) * ef(t-1-j)
            xf(t) = sum
        } // for

        transformBack_f(xf.slice (q))                        // return the vector of forecasts
    } // forecast_ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Auto-Regressive Moving-Average 'ARMA(p, q)' models
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficient vectors φ and θ for a ('p'th, 'q'th) order Auto-Regressive
     *  Moving-Average 'ARIMA(p, q)' model.
     *  <p>
     *      x_t = φ_0 * x_t-1 + ... + φ_p-1 * x_t-p +
     *            θ_0 * e_t-1 + ... + θ_q-1 * e_t-q + e_t
     *  <p>
     *  @see www.math.kth.se/matstat/gru/sf2943/tsform.pdf
     *  @param p_  the order of the AR part of the model
     *  @param q_  the order of the MA part of the model
     */
    def est_arma (p_ : Int = 1, q_ : Int = 1): (VectoD, VectoD) =
    {
        p = p_
        q = q_
        val φθ = hannanRissanen ()
//      val φθ = optimize_MLE ()                          // FIX: parameter optimization using MLE currently not working
        φ = φθ.slice (0, p)                               // AR(p) coefficients: φ_0, ..., φ_p-1
        θ = φθ.slice (p, p+q)                             // MA(q) coefficients: θ_0, ..., θ_q-1
        (φ, θ)
    } // est_ARMA

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Hannan-Rissanen Algorithm to estimate the 'ARMA(p, q)' coefficients.
     *  @see halweb.uc3m.es/esp/Personal/personas/amalonso/esp/TSAtema9.pdf
     */
    def hannanRissanen (): VectoD =
    {
        val maxIterations = 20                              // maximum iterations for step 2 of the hannan rissanen algorithm
        val minIterations = 5                               // minimum iterations for step 2 of the hannan rissanen algorithm
                                                            // in order to achieve convergence

        // use a high Auto-Regression model for initial estimates of residuals
        val k = 3 * (p + q)                                 // k must be greater than max (p, q)
        if (DEBUG) println (s"k = $k")
        val φφ = durbinLevinson (k).slice (1, k+1)          // AR(k) coefficients: φ_0, ..., φ_k-1
        if (DEBUG) println ("coefficients for AR(" + k + "): φφ = " + φφ)

        predict_ar (false)                                  // initial estimate of residuals e(t)

        if (DEBUG) println (s"initial residuals of ar($k) model = $e")
        if (DEBUG) println (s"initial sse of ar($k) model = ${e dot e}")

        // estimate coefficients using regression on lags (ar) and residuals (ma)
        val cap = max (p, q)
        val ax = new MatrixD (n - cap, p + q)               // no intercept term in the design matrix since 'x' is zero-mean
        val ay = x.slice (cap, n)
        for (j <- 0 until p) ax.setCol (j, x.slice(cap - j - 1, n - j - 1))
        for (j <- 0 until q) ax.setCol (p + j, e.slice(cap - j - 1, n - j - 1))

        if (DEBUG) println(s"ax = $ax")
        if (DEBUG) println(s"ay = $ay")

        var reg = new Regression (ax, ay)
        reg.train ()
        var φθ = reg.coefficient
        if (DEBUG) println (s"initial φθ = $φθ")

        var minSSE = Double.MaxValue                        // use SSE as criterion to determine when to stop iterating
        var bestφθ = φθ                                     // keep track of the best parameters (lowest sse)
        var beste  = e                                      // keep track of the best residuals

        // iteratively update sse and re-estimate φθ
        breakable {for (i <- 0 until maxIterations) {
            val φ = φθ.slice (0, p)                         // AR(p) coefficients: φ_0, ..., φ_p-1
            val θ = φθ.slice (p, p+q)                       // MA(q) coefficients: θ_0, ..., θ_q-1

            predict_arma (false)                            // update residuals

            val sse = e dot e
            if (DEBUG) println (s"updated sse = $sse")

            if (i > minIterations) {                        // can only break the loop if it has run at least 'minIterations' times
                if (sse < minSSE) { minSSE = sse; bestφθ = φθ; beste = e.copy }
                else              { φθ = bestφθ; e = beste; break }
            } // if

            for (j <- 0 until q) ax.setCol(p + j, e.slice(cap - j - 1, n - j - 1))

            reg = new Regression (ax, ay)
            reg.train ()
            φθ = reg.coefficient
            if (DEBUG) println (s"updated φθ = $φθ")
        }} // breakable for
        φθ
    } // hannanRissanen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the objective function for MLE optimization.  FIX
     *  @see halweb.uc3m.es/esp/Personal/personas/amalonso/esp/TSAtema9.pdf
     *  @param φθ  a single vector of AR and MA coefficients
     */
    def obj_f (φθ: VectoD): Double =
    {
        val φ    = φθ.slice (0, p)
        val θ    = φθ.slice (p, p+q)
        var sum  = 0.0
        var sum2 = 0.0
        val _1n  = 1/n.toDouble
        val xp   = predict_arma (false)
        for (j <- 0 until n) {
            sum  += (x(j) - xp(j))~^2 / acf(j)
            sum2 += log (acf(j))
        } // for
        log (_1n * sum) + _1n * sum2
    } // obj_f

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Hannan-Rissanen Algorithm first to estimate the 'ARIMA(p, q)'
     *  coefficients, then optimize the parameters using MLE.  FIX
     *  @see halweb.uc3m.es/esp/Personal/personas/amalonso/esp/TSAtema9.pdf
     */
    def optimize_MLE (): VectoD =
    {
        val φθ     = hannanRissanen ()
        val solver = new QuasiNewton (obj_f)
        solver.solve (new VectorD (φθ))
    } // optimize_MLE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a ('p'th, 'q'th) order Auto-Regressive
     *  Moving-Average 'ARMA(p, q)' model.
     *  @param transBack  flag that determines whether to return the predicted values
     *                    in the original scale
     */
    def predict_arma (transBack: Boolean = true): VectoD =
    {
        val xp = new VectorD (n)                         // forecasts for x
        for (t <- 0 until n) {                           // start at t = p (enough data)
            var sum = 0.0
            for (j <- 0 until p if t-j > 0) sum += φ(j) * x(t-1-j)
            for (j <- 0 until q if t-j > 0) sum += θ(j) * e(t-1-j)
            xp(t) = sum
            e(t) = x(t) - sum
        } // for
        if (transBack) transformBack (xp) else xp        // return the vector of forecasts
    } // predict_ARMA

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the one-step forecast for ARMA models
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param steps  the number of steps to forecast, must be at least one.
     */
    def forecast_arma (steps: Int = 1): VectoD =
    {
        val cap = max (p, q)

        val xf = x.slice (x.dim - cap) ++ new VectorD (steps)
        val ef = e.slice (e.dim - cap) ++ new VectorD (steps)

        for (t <- cap until xf.dim) {                    // start at t = cap (enough data and first value to forecast)
            var sum = 0.0
            for (j <- 0 until p) sum += φ(j) * xf(t-1-j)
            for (j <- 0 until q) sum += θ(j) * ef(t-1-j)
            xf(t) = sum
        } // for
        transformBack_f(xf.slice(cap))                   // return the vector of forecasts
    } // forecast_arma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // General `ARIMA` model.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set values for 'p' and 'q'.
     *  @param p_  the order of the AR part of the model
     *  @param q_  the order of the MA part of the model
     */
    def setPQ (p_ : Int, q_ : Int) { p = p_; q = q_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARIMA` model to times the series data.  Must call setPQ first.
     */
    def train (yy: VectoD)
    {
        if (p > 0 && q == 0)      est_ar (p)
        else if (p == 0 && q > 0) est_ma (q)
        else if (p > 0 && q > 0)  est_arma (p, q)
        else flaw ("train", "either p or q must be at least one")
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARIMA` model to the times series data on 'y'.
     */
    def train () { train (y) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For all the time points, predict all the values of 'y = f(t)'.
     */
    def predictAll: VectoD =
    {
        if (p > 0 && q == 0)      predict_ar ()
        else if (p == 0 && q > 0) predict_ma ()
        else if (p > 0 && q > 0)  predict_arma ()
        else { flaw ("train", "either p or q must be at least one"); null }
    } // predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the last time points in vector 't', predict the value of 'y = f(t)'.
     *  @param t  the time-vector indicating time points to forecast
     */
    def predict (t: VectoD): Double = predictAll(t.dim-1)

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

} // ARIMA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARIMATest` object is used to test the `ARIMA` class.
 *  > runMain scalation.analytics.ARIMATest
 */
object ARIMATest extends App
{
    val ran = Random ()
    val n = 100
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield t(i) + 10.0 * ran.gen)

    val ts = new ARIMA (y, t)          // time series data: y vs. t

    ts.plotFunc (ts.acf, "ACF")

    // Build AR(1), AR(2) and MA(1) models for the time series data

    val φ_a = ts.est_ar (1)
    println (s"φ_a = $φ_a")
    new Plot (t, y, ts.predict_ar () + ts.mu, "Plot of y, ar(1) vs. t") //, true)

    val φ_b = ts.est_ar (2)
    println (s"φ_b = $φ_b")
    new Plot (t, y, ts.predict_ar () + ts.mu, "Plot of y, ar(2) vs. t") //, true)

    val θ = ts.est_ma (1)
    println (s"θ = $θ")
    new Plot (t, y, ts.predict_ma () + ts.mu, "Plot of y, ma(1) vs. t") //, true)

    ts.plotFunc (ts.pacf, "PACF")

} // ARIMATest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARIMATest2` object is used to test the `ARIMA` class.
 *  > runMain scalation.analytics.ARIMATest2
 */
object ARIMATest2 extends App
{
    val noise = Normal (0.0, 1.0)
    val n = 20
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield i + noise.gen)

    println (s"y = $y")

    val p = 1
    val q = 2
    val d = 1                          // apply 1st order differencing
    val steps = 2                      // number of steps for the forecasts

    val ts = new ARIMA (y, t, d)       // time series data: y vs. t

    // Build AR(1), MA(1) and ARMA(1, 1) models for the (differenced) time series data

    val φ_a = ts.est_ar (p)
    println (s"φ_a = $φ_a")
    new Plot (t, y, ts.predict_ar () + ts.mu, s"Plot of y, ar($p) vs. t") //, true)
    val ar_f = ts.forecast_ar (steps) + ts.mu
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")

    val θ_a = ts.est_ma (q)
    println (s"θ_a = $θ_a")
    new Plot (t, y, ts.predict_ma () + ts.mu, s"Plot of y, ma($q) vs. t") //, true)
    val ma_f = ts.forecast_ma (steps) + ts.mu
    println (s"$steps-step ahead forecasts using MA($q) model = $ma_f")

    val (φ, θ) = ts.est_arma (p, q)
    println (s"φ = $φ, θ = $θ")
    new Plot (t, y, ts.predict_arma () + ts.mu, s"Plot of y, arma($p, $q) vs. t") //, true)
    val arma_f = ts.forecast_arma (steps) + ts.mu
    println (s"$steps-step ahead forecasts using ARMA($p, $q) model = $arma_f")

} // ARIMATest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARIMATest3` object is used to test the `ARIMA` class.
 *  Forecasting lake levels.
 *  @see ???
 * > runMain scalation.analytics.ARIMATest3
 */
object ARIMATest3 extends App
{
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

    val p = 1
    val q = 3
    val d = 2                          // apply 2nd order differencing
    val steps = 2                      // number of steps for the forecasts

    val ts = new ARIMA (y, t, d)       // time series data: y vs. t

    // Build AR(2), MA(1) and ARMA(2, 1) models for the (differenced) time series data

    val φ_a = ts.est_ar (p)
    println (s"φ_a = $φ_a")
    new Plot (t, y, ts.predict_ar () + ts.mu, s"Plot of y, ar($p) vs. t") //, true)
    val ar_f = ts.forecast_ar (steps) + ts.mu
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")

    val θ_a = ts.est_ma (q)
    println (s"θ_a = $θ_a")
    new Plot (t, y, ts.predict_ma () + ts.mu, s"Plot of y, ma($q) vs. t") //, true)
    val ma_f = ts.forecast_ma (steps) + ts.mu
    println (s"$steps-step ahead forecasts using MA($q) model = $ma_f")

    val (φ, θ) = ts.est_arma (p, q)
    println (s"φ = $φ, θ = $θ")
    new Plot (t, y, ts.predict_arma () + ts.mu, s"Plot of y, arma($p, $q) vs. t") //, true)
    val arma_f = ts.forecast_arma (steps) + ts.mu
    println (s"$steps-step ahead forecasts using ARMA($p, $q) model = $arma_f")

} // ARIMATest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARIMATest4` object is used to test the `ARIMA` class.
 * > runMain scalation.analytics.ARIMATest4
 */
object ARIMATest4 extends App
{
    val path = BASE_DIR + "travelTime.csv"

    val data = MatrixD (path)

    val t = data.col(0)
    val y = data.col(1)

    val p = 1
    val q = 1
    val d = 1                          // apply 1st order differencing
    val steps = 1                      // number of steps for the forecasts

    val ts = new ARIMA (y, t, d)       // time series data: y vs. t

    println (s"y = $y")

    // Build AR(1), MA(1) and ARMA(1, 1) models for the (differenced) time series data

    val φ_a = ts.est_ar (p)
    println (s"φ_a = $φ_a")
    new Plot (t, y, ts.predict_ar () + ts.mu, s"Plot of y, ar($p) vs. t") //, true)
    val ar_f = ts.forecast_ar (steps) + ts.mu
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")

    val θ_a = ts.est_ma (q)
    println (s"θ_a = $θ_a")
    new Plot (t, y, ts.predict_ma () + ts.mu, s"Plot of y, ma($q) vs. t") //, true)
    val ma_f = ts.forecast_ma (steps) + ts.mu
    println (s"$steps-step ahead forecasts using MA($q) model = $ma_f")

    val (φ, θ) = ts.est_arma (p, q)
    println (s"φ = $φ, θ = $θ")
    new Plot (t, y, ts.predict_arma () + ts.mu, s"Plot of y, arma($p, $q) vs. t") //, true)
    val arma_f = ts.forecast_arma (steps) + ts.mu
    println (s"$steps-step ahead forecasts using ARMA($p, $q) model = $arma_f")

} // ARIMATest4 object

