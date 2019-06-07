
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

import scala.collection.mutable.HashMap
import scala.math.{log, max, min}

import scalation.linalgebra._
import scalation.math.double_exp
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.random.{Normal, Random}
import scalation.stat.vectorD2StatVector

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for class `SARIMA`.  Includes features related to differencing
 *  and automated order selection.
 *  @see www.jstatsoft.org/article/view/v027i03/v27i03.pdf
 */
object SARIMA
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Difference the time series or external regressors.  Return both the completely
     *  differenced time series and the intermediate (differenced once) time series
     *  (needed to scale back results later).
     *  @param xx      the time series or external regressors to be differenced
     *  @param d       the order of simple differencing
     *  @param dd      the order of seasonal differencing
     *  @param period  the seasonal period
     */
    def difference (xx: VectoD, d: Int, dd: Int, period: Int): (VectoD, VectoD) =
    {
        val x_ = differenceTrend (xx, d)
        differenceSeason (x_, dd, period)
    } // difference

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Difference for trend.
     *  @param xx  the time series or external regressors to be trend differenced
     *  @param d   the order of simple differencing
     */
    def differenceTrend (xx: VectoD, d: Int): VectoD =
    {
        d match {
            case 0 => xx
            case 1 => VectorD (for (i <- 0 until xx.dim-1) yield xx(i+1)-xx(i))
            case 2 => VectorD (for (i <- 0 until xx.dim-2) yield xx(i+2)-2*xx(i+1)+xx(i))
            case 3 => VectorD (for (i <- 0 until xx.dim-3) yield xx(i+3)-3*xx(i+2)+3*xx(i+1)-xx(i))
            case _ => println ("SARIMA does not support differencing higher than order 3"); null
        } // match
    } // differenceTrend

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Difference for seasonality.
     *  @param x_      the time series or external regressors to be seasonally differenced,
     *                 this is usually the intermediate result after simple differencing.
     *  @param dd      the order of seasonal differencing
     *  @param period  the seasonal period
     */
    def differenceSeason (x_ : VectoD, dd: Int, period: Int): (VectoD, VectoD) =
    {
        dd match {
            case 0 => (x_, x_)
            case 1 => (VectorD (for (i <- 0 until x_.dim-period)   yield x_(i+period)-x_(i)), x_)
            case 2 => (VectorD (for (i <- 0 until x_.dim-2*period) yield x_(i+2*period)-2*x_(i+period)+x_(i)), x_)
            case 3 => (VectorD (for (i <- 0 until x_.dim-3*period) yield x_(i+3*period)-3*x_(i+2*period)+3*x_(i+period)-x_(i)), x_)
            case _ => println ("SARIMA does not support seasonal differencing higher than order 3"); null
        } // match
    } // differenceSeason

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the fitted values on the training data of a differenced time series back
     *  to the original scale.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xp      the vector of predictions/fitted values of a differenced time series
     *  @param x_      the intermediate result after differencing for trend, but before differencing for seasonality
     *  @param xx      the original zero-mean time series
     *  @param d       the order of simple differencing
     *  @param dd      the order of seasonal differencing
     *  @param period  the seasonal period
     */
    def transformBack (xp: VectoD, x_ : VectoD, xx: VectoD, d: Int, dd: Int, period: Int): VectoD =
    {
        val xp_ = transformBackSeason (xp, x_, dd, period)
        transformBackTrend (xp_, xx, d)
    } // transformBack

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the fitted values on the training data of a differenced time series back
     *  to the original scale. Undo seasonal differencing only.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xp      the vector of predictions/fitted values of a differenced time series
     *  @param x_      the intermediate result after differencing for trend, but before differencing for seasonality
     *  @param dd      the order of seasonal differencing
     *  @param period  the seasonal period
     */
    def transformBackSeason (xp: VectoD, x_ : VectoD, dd: Int, period: Int): VectoD =
    {
        dd match {
            case 0 => xp
            case 1 => val tbx = new VectorD (xp.dim + period)
                for (i <- 0 until period) tbx(i) = x_(i)
                for (i <- 0 until x_.dim-period) tbx(i+period) = xp(i) + x_(i)
                tbx
            case 2 => val tbx = new VectorD (xp.dim + 2*period)
                for (i <- 0 until 2*period) tbx(i) = x_(i)
                for (i <- 0 until x_.dim-2*period) tbx(i+2*period) = xp(i) + 2*x_(i+period) - x_(i)
                tbx
            case 3 => val tbx = new VectorD (xp.dim + 3*period)
                for (i <- 0 until 3*period) tbx(i) = x_(i)
                for (i <- 0 until x_.dim-3*period) tbx(i+3*period) = xp(i) + 3*x_(i+2*period) - 3*x_(i+period) + x_(i)
                tbx
            case _ => println ("SARIMA does not support seasonal differencing higher than order 3"); null
        } // match
    } // transformBackSeason

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the fitted values on the training data of a differenced time series back
     *  to the original scale. Undo trend differencing only.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xp_  the vector of predictions/fitted values after undoing seasonal differencing
     *  @param xx   the original zero-mean time series
     *  @param d    the order of simple differencing
     */
    def transformBackTrend (xp_ : VectoD, xx: VectoD, d: Int): VectoD =
    {
        d match {
            case 0 => xp_
            case 1 => val tbx = new VectorD (xx.dim)
                tbx(0)  = xx(0)
                for (i <- 0 until xx.dim-1) tbx(i+1) = xp_(i) + xx(i)
                tbx
            case 2 => val tbx = new VectorD (xx.dim)
                tbx(0)  = xx(0)
                tbx(1)  = xx(1)
                for (i <- 0 until xx.dim-2) tbx(i+2) = xp_(i) + 2*xx(i+1) - xx(i)
                tbx
            case 3 => val tbx = new VectorD (xx.dim)
                tbx(0)  = xx(0)
                tbx(1)  = xx(1)
                tbx(2)  = xx(2)
                for (i <- 0 until xx.dim-3) tbx(i+3) = xp_(i) + 3*xx(i+2) - 3*xx(i+1) + xx(i)
                tbx
            case _ => println ("SARIMA does not support differencing higher than order 3"); null
        } // match
    } // transformBackTrend

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the forecast values of a differenced time series back to the
     *  original scale.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xf      the vector of forecasted values of a differenced time series
     *  @param x_      the intermediate result after differencing for trend, but before differencing for seasonality
     *  @param xx      the original zero-mean time series
     *  @param d       the order of simple differencing
     *  @param dd      the order of seasonal differencing
     *  @param period  the seasonal period
     */
    def transformBackF (xf: VectoD, x_ : VectoD, xx: VectoD, d: Int, dd: Int, period: Int): VectoD =
    {
        val xf_ = transformBackFSeason (xf, x_, dd, period)
        transformBackFTrend (xf_, xx, d)
    } // transformBackF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the forecast values of a differenced time series back to the
     *  original scale. Undo seasonal differencing only.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xf      the vector of forecasted values of a differenced time series
     *  @param x_      the intermediate result after differencing for trend, but before differencing for seasonality
     *  @param dd      the order of seasonal differencing
     *  @param period  the seasonal period
     */
    def transformBackFSeason (xf: VectoD, x_ : VectoD, dd: Int, period: Int): VectoD =
    {
        dd match {
            case 0 => xf
            case 1 => val tbx = x_.slice(x_.size - period) ++ xf
                for (i <- period until tbx.dim) tbx(i) += tbx(i-period)
                tbx.slice(period)
            case 2 => val tbx = x_.slice(x_.size - 2*period) ++ xf
                for (i <- 2*period until tbx.dim) tbx(i) += (2*tbx(i-period) - tbx(i-2*period))
                tbx.slice(2*period)
            case 3 => val tbx = x_.slice(x_.size - 3*period) ++ xf
                for (i <- 3*period until tbx.dim) tbx(i) += (3*tbx(i-period) - 3*tbx(i-2*period) + tbx(i-3*period))
                tbx.slice(3*period)
            case _ => println ("SARIMA does not support seasonal differencing higher than order 3"); null
        } // match
    } // transformBackFSeason

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the forecast values of a differenced time series back to the
     *  original scale. Undo trend differencing only.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param xf_      the vector of forecasted values after undoing seasonal differencing
     *  @param xx       the original zero-mean time series
     *  @param d        the order of simple differencing
     */
    def transformBackFTrend (xf_ : VectoD, xx: VectoD, d: Int): VectoD =
    {
        d match {
            case 0 => xf_
            case 1 => val tbx = xx.slice(xx.size - 1) ++ xf_
                for (i <- 1 until tbx.dim) tbx(i) += tbx(i-1)
                tbx.slice(1)
            case 2 => val tbx = xx.slice(xx.size - 2) ++ xf_
                for (i <- 2 until tbx.dim) tbx(i) += (2*tbx(i-1) - tbx(i-2))
                tbx.slice(2)
            case 3 => val tbx = xx.slice(xx.size - 3) ++ xf_
                for (i <- 3 until tbx.dim) tbx(i) += (3*tbx(i-1) - 3*tbx(i-2) + tbx(i-3))
                tbx.slice(3)
            case _ => println ("SARIMA does not support differencing higher than order 3"); null
        } // match
    } // transformBackFTrend

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Automated order selection based on the algorithm proposed in
     *  @see www.jstatsoft.org/article/view/v027i03/v27i03.pdf.
     *  @param y       the time series or external regressors to be differenced
     *  @param d_      order of trend differencing
     *  @param dd      order of seasonal differencing
     *  @param period  seasonal period
     *  @param mp      maximum autoregressive order
     *  @param mq      maximum moving average order
     *  @param mpp     maximum seasonal autoregressive order
     *  @param mqq     maximum seasonal moving average order
     *  @param md      maximum order for differencing trend
     *  @param par     flag for running the process in parallel
     */
    def apply (y: VectoD, d_ : Int = -1, dd : Int = 0, period: Int = 1, mp: Int = 5, mq: Int = 5, mpp: Int = 2,
               mqq: Int = 2, md: Int = 2, par: Boolean = false, alpha: Double = 0.05): SARIMA =
    {
        val seasonal = period > 1
        val ysd      = if (seasonal) differenceSeason (y, dd, period)._1 else y     // apply seasonal differencing as necessary
        var ystd     = ysd
        var keepDiff = true
        var d        = if (d_ > -1) d_ else 0
        if (d_ == -1) {                                                    // automatically determine the order of differencing
            while (keepDiff) {
                ystd           = differenceTrend (ystd, d)
                val kpssTester = new KPSS (ystd, "short", "c")
                kpssTester.pvalue ()
                if (kpssTester.canReject (alpha) && d + 1 <= md) d += 1    // current ystd is not yet stationary
                else keepDiff = false                                      // current ystd is stationary or 'md' limited exceeded
            } // while
        } // if

        val allModels = new HashMap[(Int, Int, Int, Int), Double]()        // hashmap storing all the models that have been tried
        var numModels = 4
        var models = Array.ofDim[SARIMA] (numModels)
        var scores = VectorD.fill (numModels)(Double.PositiveInfinity)
        var orders = if (seasonal) Array((2,2,1,1),(0,0,0,0),(1,0,1,0),(0,1,0,1))
                     else          Array((2,2,0,0),(0,0,0,0),(1,0,0,0),(0,1,0,0))
        var testRange = if (par) (0 until numModels).par else (0 until numModels)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Test a model and record its orders and value of the scoring function AICc.
         */
        def testModel (i: Int)
        {
            val arima = new SARIMA (ystd, period = period)
            val (p, q, pp, qq) = orders (i)
            arima.setPQ (p, q, pp, qq)
            arima.train ()
            models(i)  = arima
            scores(i)  = arima.fit(8)                                      // index 8 represents AICc
            allModels += (orders(i) -> scores(i))
        } // testModel

        for (i <- testRange) testModel (i)

        var idx = scores.argmin ()
        var currentModel = models(idx)                                     // getting the current/best model
        var currentScore = scores(idx)
        var (cp, cd, cq, cpp, cdd, cqq, cperiod) = currentModel.getParam   // parameters for the current model

        numModels = if (seasonal) 16 else 8
        models    = Array.ofDim[SARIMA] (numModels)
        scores    = VectorD.fill (numModels)(Double.PositiveInfinity)
        orders    = Array.ofDim [(Int, Int, Int, Int)](numModels)

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Populate the orders of arima parameters to be tested in the next round.
         */
        def fillOrders ()
        {
            orders(0) = if (cp + 1 <= mp && !allModels.contains((cp + 1, cq, cpp, cqq))) (cp + 1, cq, cpp, cqq) else null
            orders(1) = if (cp - 1 >=  0 && !allModels.contains((cp - 1, cq, cpp, cqq))) (cp - 1, cq, cpp, cqq) else null
            orders(2) = if (cq + 1 <= mq && !allModels.contains((cp, cq + 1, cpp, cqq))) (cp, cq + 1, cpp, cqq) else null
            orders(3) = if (cq - 1 >=  0 && !allModels.contains((cp, cq - 1, cpp, cqq))) (cp, cq - 1, cpp, cqq) else null

            orders(4) = if (cp + 1 <= mp && cq + 1 <= mq && !allModels.contains((cp + 1, cq + 1, cpp, cqq))) (cp + 1, cq + 1, cpp, cqq) else null
            orders(5) = if (cp + 1 <= mp && cq - 1 >=  0 && !allModels.contains((cp + 1, cq - 1, cpp, cqq))) (cp + 1, cq - 1, cpp, cqq) else null
            orders(6) = if (cp - 1 >=  0 && cq + 1 <= mq && !allModels.contains((cp - 1, cq + 1, cpp, cqq))) (cp - 1, cq + 1, cpp, cqq) else null
            orders(7) = if (cp - 1 >=  0 && cq - 1 >=  0 && !allModels.contains((cp - 1, cq - 1, cpp, cqq))) (cp - 1, cq - 1, cpp, cqq) else null

            if (seasonal) {
                orders(8)  = if (cpp + 1 <= mpp && !allModels.contains((cp, cq, cpp + 1, cqq))) (cp, cq, cpp + 1, cqq) else null
                orders(9)  = if (cpp - 1 >=   0 && !allModels.contains((cp, cq, cpp - 1, cqq))) (cp, cq, cpp - 1, cqq) else null
                orders(10) = if (cqq + 1 <= mqq && !allModels.contains((cp, cq, cpp, cqq + 1))) (cp, cq, cpp, cqq + 1) else null
                orders(11) = if (cqq - 1 >=   0 && !allModels.contains((cp, cq, cpp, cqq - 1))) (cp, cq, cpp, cqq - 1) else null

                orders(12) = if (cpp + 1 <= mpp && cqq + 1 <= mqq && !allModels.contains((cp, cq, cpp + 1, cqq + 1))) (cp, cq, cpp + 1, cqq + 1) else null
                orders(13) = if (cpp + 1 <= mpp && cqq - 1 >=   0 && !allModels.contains((cp, cq, cpp + 1, cqq - 1))) (cp, cq, cpp + 1, cqq - 1) else null
                orders(14) = if (cpp - 1 >=   0 && cqq + 1 <= mqq && !allModels.contains((cp, cq, cpp - 1, cqq + 1))) (cp, cq, cpp - 1, cqq + 1) else null
                orders(15) = if (cpp - 1 >=   0 && cqq - 1 >=   0 && !allModels.contains((cp, cq, cpp - 1, cqq - 1))) (cp, cq, cpp - 1, cqq - 1) else null
            } // if

        } // fillOrders

        fillOrders ()

        testRange = if (par) (0 until numModels).par else (0 until numModels)
        var keepSearching = true

        while (keepSearching) {
            for (i <- testRange) {
                if (orders(i) == null) {
                    models(i) = null
                    scores(i) = Double.PositiveInfinity
                } else testModel (i)
            } // for

            idx = scores.argmin()
            if (scores(idx) < currentScore) {
                currentModel = models (idx)             // getting the current/best model
                currentScore = scores (idx)
                val params = currentModel.getParam
                cp = params._1
                cd = params._2
                cq = params._3
                cpp = params._4
                cdd = params._5
                cqq = params._6
                cperiod = params._7
                fillOrders ()
            } else keepSearching = false
        } // while

        val arima = new SARIMA (y, d, dd, period)
        arima.setPQ (cp, cq, cpp, cqq)
        arima
    } // apply

} // SARIMA object

import SARIMA._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMA` class provides basic time series analysis capabilities for Auto-
 *  Regressive 'AR' Integrated 'I' Moving-Average 'MA' models.  In an
 *  'SARIMA(p, d, q)' model, 'p' and 'q' refer to the order of the Auto-Regressive
 *  and Moving-Average components of the model; 'd' refers to the order of
 *  differencing. `SARIMA` models are often used for forecasting.
 *  Given time series data stored in vector 'y', its next value 'y_t = y(t)'
 *  may be predicted based on prior values of 'y' and its noise:
 *  <p>
 *      y_t = c + Σ(φ_i y_t-i) + Σ(θ_i e_t-i) + e_t
 *  <p>
 *  where 'c' is a constant, 'φ' is the autoregressive coefficient vector,
 *  'θ' is the moving-average coefficient vector, and 'e' is the noise vector.
 *  If 'd' > 0, then the time series must be differenced first before applying
 *  the above model. Seasonal differencing, autoregressive and moving average
 *  factors can be incorporated into the model by applying seasonal differencing
 *  (possibly in addition to simple differencing) first, then add the seasonal
 *  autoregressive and moving average terms, that rely on lagged values and errors,
 *  respectively, from one or more seasonal periods in the past, on the right hand
 *  side of the equation. Exogeous/External regressor may also be added to the
 *  right-hand size of the model in a similar manner to Regression models.
 *------------------------------------------------------------------------------
 *  @param y       the input vector (time series data)
 *  @param d       the order of Integration/simple differencing
 *  @param dd      the order of seasonal differencing
 *  @param period  the seasonal period
 *  @param xxreg   optional matrix of external regressors used for dynamic regression
 */
class SARIMA (y: VectoD, d: Int = 0, dd: Int = 0, period: Int = 1, xxreg: MatriD = null)
      extends Forecaster
{
    protected var e: VectoD = null                  // residual/error vector [e_0, e_1, ... e_m-1
    private val DEBUG        = false                          // debug flag
    private val differenced  = d > 0 || dd > 0                // whether differencing will be applied
    private val mxLag        = 100                            // maximum lag to consider

    private var mu           = 0.0                            // the sample mean
    private var xx:   VectoD = null                           // zero-mean transformation of 'y'
    private var x:    VectoD = null                           // time series after differencing
    private var x_ :  VectoD = null                           // intermediate results after simple differencing
    private var n            = 0                              // size of the time series (after differencing)
    private var r            = 0                              // number of external regressors
    private var xreg: MatriD = null                           // external regressors (after differencing)
    private var xp:   VectoD = null                           // vector of predicted/fitted values
    private var m            = 0                              // maximum lag to consider
    private var sig2         = 0.0                            // the sample variance
    private var ac:   VectoD = null                           // auto-covariance
            var acf:  VectoD = null                           // Auto-Correlation Function (ACF)

    setTS (y, xxreg)                                          // initialize most of the above parameters

    private var p            = 0                              // AR order
    private var q            = 0                              // MA order
    private var pp           = 0                              // Seasonal AR order
    private var qq           = 0                              // Seasonal MA order
    private var σ2           = 0.0                            // variance of the residual (updated using MLE)

    private var φ:    VectoD = null                           // AR(p) coefficients
    private var φφ:   VectoD = null                           // Seasonal AR(pp) coefficients
    private var θ:    VectoD = null                           // MA(q) coefficients
    private var θθ:   VectoD = null                           // Seasonal MA(qq) coefficients
    private var b:    VectoD = null                           // external regressor coefficients, if any

    private var ll           = 0.0                            // log likelihood
    private var aic          = 0.0                            // aic value
    private var aicc         = 0.0                            // corrected aic value
    private var bic          = 0.0                            // bic value

    if (DEBUG) {
        println ("n    = " + n)                               // number of data points
        println ("x    = " + x)                               // zero-mean time series
        println ("mu   = " + mu)                              // mean
        println ("sig2 = " + sig2)                            // variance
        println ("ac   = " + ac)                              // auto-covariance
        println ("acf  = " + acf)                             // ACF
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set/change the internal time series. May be used to set the time series
     *  to a different time window (typically future when new data become available)
     *  in order to produce newer forecast (typically with the new data) without re-training
     *  the model for parameters (use existing parameters from previous training).
     *  @param y      the new time series
     *  @param xxreg  the new external regressors
     */
    def setTS (y: VectoD, xxreg: MatriD = null)
    {
        mu            = y.mean                                // the sample mean
        xx            = new VectorD (y - mu)                  // work with mean zero time series
        val (x0, x0_) = difference (xx, d, dd, period)        // difference the time series
        x             = x0                                    // time series after differencing
        x_            = x0_                                   // intermediate results after simple differencing
        n             = x.dim                                 // size of the time series (after differencing)
        r             = if       (xxreg == null) 0            // number of external regressors
                        else if  (differenced) xxreg.dim2
                        else     xxreg.dim2 + 1
        xreg          = getXreg (xxreg)                       // external regressors
        e             = new VectorD (n)                       // vector of residuals
        xp            = new VectorD (n)                       // vector of predicted/fitted values
        m             = min (n, mxLag)                        // maximum lag to consider
        sig2          = x.variance                            // the sample variance
        if (DEBUG) {                                          // avoid calculating ACF for efficiency purposes if not in DEBUG mode
            ac      = new VectorD (m+1)                       // auto-covariance
            for (t <- ac.range) ac(t) = x.asInstanceOf [VectorD] acov t
            acf     = ac / sig2                               // Auto-Correlation Function (ACF)
        } // if
    } // setTS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Processing the external regressors such as differencing and adding a column
     *  of one's.
     *  @param xxreg  the external regressors to be processed
     *  @param dim1   the correct number of rows of 'xreg' after applying differencing
     */
    def getXreg (xxreg: MatriD, dim1: Int = n): MatriD =
    {
        if (xxreg != null) {
            val xreg = new MatrixD (dim1, r)
            if (differenced) {
                for (i <- 0 until r) xreg.setCol (i, difference(xxreg.col(i), d, dd, period)._1)
            } else {
                xreg.setCol (0, VectorD.one (dim1))
                for (i <- 1 until r) xreg.setCol (i, xxreg.col(i-1))
            } // if
            return xreg
        } // if
        null
    } // getXreg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set values for 'p', 'q', 'pp' and 'qq'.
     *  @param p_   the order of the AR part of the model
     *  @param q_   the order of the MA part of the model
     *  @param pp_  the order of the Seasonal AR part of the model
     *  @param qq_  the order of the Seasonal MA part of the model
     */
    def setPQ (p_ : Int = 0, q_ : Int = 0, pp_ : Int = 0, qq_ : Int = 0)
    {
        p  = p_ ; if (p  > 0) φ  = new VectorD (p)
        q  = q_ ; if (q  > 0) θ  = new VectorD (q)
        pp = pp_; if (pp > 0) φφ = new VectorD (pp)
        qq = qq_; if (qq > 0) θθ = new VectorD (qq)
    } // setPQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `SARIMA` model to the times series data.  Must call 'SetPQ' first.
     */
    def train (): SARIMA =
    {
        val init   = new VectorD (r+p+pp+q+qq+1)
        val solver = new QuasiNewton (nll)

        if (r > 0) {
            val reg = new RidgeRegression (xreg, x)
            reg.train ()
            b = reg.parameter
            for (i <- 0 until r if (!b(i).isNaN && !b(i).isInfinity)) init(i) = b(i)
        } // if

        init(init.size-1) = Math.sqrt (sig2)
        solver.solve (init)
        if (DEBUG) {
            println (s"b   = $b")
            println (s"φ   = $φ")
            println (s"φφ  = $φφ")
            println (s"θ   = $θ")
            println (s"θθ  = $θθ")
            println (s"σ2  = $σ2")
            println (fitLabel)
            println (fit)
        } // i
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The negative log-likelihood function to be minimized.
     *  @see spia.uga.edu/faculty_pages/monogan/teaching/ts/Barima.pdf
     *  @see stats.stackexchange.com/questions/77663/arima-estimation-by-hand
     *  @param input  the input parameter vector
     */
    def nll (input: VectoD): Double =
    {
        if (input.size != r+p+pp+q+qq+1) flaw ("nll", "Input param vector size incorrect")
        for (i <- 0 until r)                   b(i)          = input(i)
        for (i <- r until r+p)                 φ(i-r)        = input(i)
        for (i <- r+p until r+p+pp)           φφ(i-r-p)      = input(i)
        for (i <- r+p+pp until r+p+pp+q)       θ(i-r-p-pp)   = input(i)
        for (i <- r+p+pp+q until r+p+pp+q+qq) θθ(i-r-p-pp-q) = input(i)
        σ2 = input.last~^2

        updateFittedValues ()
    } // nll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update 'xp', the vector of fitted values; 'e', the vector of errors;
     *  ll, aic, aicc and bic.
     */
    def updateFittedValues (): Double =
    {
        for (t <- 0 until xp.dim) {
            var sum = 0.0
            if  (r > 0)                                  sum +=  b  dot xreg(t)
            for (j <- 0 until p  if t-j > 0)             sum +=  φ(j) * x(t-1-j)
            for (j <- 0 until pp if t-(1+j)*period >= 0) sum += φφ(j) * x(t-(1+j)*period)
            for (j <- 0 until q  if t-j > 0)             sum +=  θ(j) * e(t-1-j)
            for (j <- 0 until qq if t-(1+j)*period >= 0) sum += θθ(j) * e(t-(1+j)*period)
            xp(t) = sum
            e(t)  = x(t) - sum
        } // for

        ll    = -(n/2.0 * log (2*Math.PI*σ2) + (e dot e)/(2*σ2))
        val k = r + p + pp + q + qq + 1
        aic   = -2 * ll + 2 * k
        aicc  = aic + (2.0 * k * (k+1)) / (n - k - 1)
        bic   = aic + (log(n) - 2.0) * k
        -ll
    } // updateFittedValues

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabel: Seq [String] = super.fitLabel ++ Seq ("ll", "aic", "aicc", "bic")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit. Additional metrics include loglikelihood, aic,
     *  aicc and bic.
     */
    override def fit: VectoD = super.fit ++ VectorD (ll, aic, aicc, bic)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     */
    def eval () = diagnose (x, e)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the orders of this SARIMA model.
     */
    def getParam = (p, d, q, pp, dd, qq, period)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of fitted values on the training data.
     */
    def predict (): VectoD = transformBack (xp, x_, xx, d, dd, period) += mu

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce h-steps-ahead forecast for SARIMA models.
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param h  the number of steps to forecast, must be at least one.
     */
    def forecast (h: Int): VectoD = forecast (h, null)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce n-steps-ahead forecast for SARIMA models.
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param h        the number of steps to forecast, must be at least one.
     *  @param xxreg_f  future values of external regressors may be guesses based on the past (e.g., past mean)
     */
    def forecast (h: Int = 1, xxreg_f : MatriD = null): VectoD =
    {
        val xreg_f = getXreg (xxreg_f, h)
        val cap    = Array (p, q, pp*period, qq*period).max
        val xf     = new VectorD (cap + h)
        val ef     = new VectorD (cap + h)

        for (i <- 0 until cap) {
            xf(i) = x(n-cap+i)
            ef(i) = e(n-cap+i)
        } // for

        for (t <- cap until xf.dim) {                               // start at t = cap (enough data and first value to forecast)
            var sum = 0.0
            if  (xreg_f != null)  sum +=  b  dot xreg_f(t - cap)    // still produce forecast without xxreg_f if it's not provided
            for (j <- 0 until  p) sum +=  φ(j) * xf(t-1-j)
            for (j <- 0 until pp) sum += φφ(j) * xf(t-(1+j)*period)
            for (j <- 0 until  q) sum +=  θ(j) * ef(t-1-j)
            for (j <- 0 until qq) sum += θθ(j) * ef(t-(1+j)*period)
            xf(t) = sum
        } // for

        transformBackF (xf.slice(cap), x_, xx, d, dd, period) += mu // return the vector of forecasts
    } // forecast

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize various vector and matrices to be used for Kalman Filter.
     *  Current only support undifferenced, non-seasonal time series.
     *  @see      www.stat.berkeley.edu/classes/s244/as154.pdf
     *  @param φ  the AR coefficients
     *  @param θ  the MA coefficients
     */
    def initKalman (φ: VectoD = φ, θ: VectoD = θ): (VectoD, MatriD, MatriD, MatriD, MatriD) =
    {
        if (φ != null && p != φ.dim || θ != null && q != θ.dim) {
            flaw ("initKalman", "p or q mismatch with dims of φ or θ")
        } // fi
        val r  = max (p, q+1)

        val x0 = new VectorD (r)

        val ff = new MatrixD (r, r)
        for (i <- 0 until p) ff(i, 0)   = φ(i)
        for (i <- 1 until r) ff(i-1, i) = 1

        val hh = new MatrixD (1, r)
        hh(0, 0) = 1

        val _1θ = new MatrixD (r, 1)
        _1θ(0, 0) = 1
        if (θ != null) for (i <- 1 to q) _1θ(i, 0) = θ(i-1)
        val qq = _1θ * _1θ.t

        val rr = new MatrixD (1, 1)

        (x0, ff, hh, qq, rr)
    } // initKalman

} // SARIMA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMATest` object is used to test the `SARIMA` class.
 *  > runMain scalation.analytics.SARIMATest
 */
object SARIMATest extends App
{
    val ran = Random ()
    val n = 100
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield t(i) + 10.0 * ran.gen)

    val ts = new SARIMA (y, 1)          // time series data: y vs. t, apply 1st order differencing

//  ts.plotFunc (ts.acf, "ACF")         // Must turn on DEBUG so that ACF is actually computed

    // Build AR(1), AR(2) and MA(1) models for the time series data

    ts.setPQ (1)
    ts.train ()
    new Plot (t, y, ts.predict (), "Plot of y, ar(1) vs. t", true)

    ts.setPQ (2)
    ts.train ()
    new Plot (t, y, ts.predict (), "Plot of y, ar(2) vs. t", true)

    ts.setPQ (0, 1)
    ts.train ()
    new Plot (t, y, ts.predict () , "Plot of y, ma(1) vs. t", true)

} // SARIMATest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMATest2` object is used to test the `SARIMA` class.
 *  > runMain scalation.analytics.SARIMATest2
 */
object SARIMATest2 extends App
{
    val noise = Normal (0.0, 1.0)
    val n = 20
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield i + noise.gen)

    println (s"y = $y")

    val p = 1
    val d = 1                           // apply 1st order differencing
    val q = 1
    val steps = 2                       // number of steps for the forecasts

    val ts = new SARIMA (y, d)          // time series data: y vs. t

    // Build AR(1), MA(1) and ARMA(1, 1) models for the (differenced) time series data

    ts.setPQ (p)
    ts.train ()
    new Plot (t, y, ts.predict (), s"Plot of y, ar($p) vs. t", true)
    val ar_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")

    ts.setPQ (0, q)
    ts.train ()
    new Plot (t, y, ts.predict (), s"Plot of y, ma($q) vs. t", true)
    val ma_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using MA($q) model = $ma_f")

    ts.setPQ (p, q)
    ts.train ()
    new Plot (t, y, ts.predict (), s"Plot of y, arma($p, $q) vs. t", true)
    val arma_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using ARMA($p, $q) model = $arma_f")

} // SARIMATest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMATest3` object is used to test the `SARIMA` class.
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 * > runMain scalation.analytics.SARIMATest3
 */
object SARIMATest3 extends App
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

    val p = 2
    val q = 1
    val d = 0                           // apply 2nd order differencing
    val steps = 3                       // number of steps for the forecasts

    val ts = new SARIMA (y, d)          // time series data: y vs. t

    // Build AR(2), MA(1) and ARMA(2, 1) models for the (differenced) time series data

    ts.setPQ (p)
    ts.train ()
//  ts.est_ar (p)     // depreciated, to use, uncomment this line and comment out two previous lines, also turn DEBUG flag to true
    var xp = ts.predict ()
    println(s"xp = $xp")
    new Plot (t, y, xp , s"Plot of y, ar($p) vs. t", true)
    val ar_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")


    ts.setPQ (0, q)
    ts.train ()
//  ts.est_ma (q)               // depreciated
    xp = ts.predict ()
    println(s"xp = $xp")
    new Plot (t, y, xp, s"Plot of y, ma($q) vs. t", true)
    val ma_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using MA($q) model = $ma_f")

    ts.setPQ (p, q)
    ts.train ()
//  ts.est_arma(p, q) // depreciated
    xp = ts.predict ()
    println(s"xp = $xp")
    new Plot (t, y, xp, s"Plot of y, arma($p, $q) vs. t", true)
    val arma_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using ARMA($p, $q) model = $arma_f")

} // SARIMATest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMATest4` object is used to test the `SARIMA` class.
 * > runMain scalation.analytics.SARIMATest4
 */
object SARIMATest4 extends App
{
    val path = BASE_DIR + "travelTime.csv"

    val data = MatrixD (path)

    val t = data.col(0)
    val y = data.col(1)

    val p = 1
    val q = 1
    val d = 1                           // apply 1st order differencing
    val steps = 1                       // number of steps for the forecasts

    val ts = new SARIMA (y, d)          // time series data: y vs. t

    println (s"y = $y")

    // Build AR(1), MA(1) and ARMA(1, 1) models for the (differenced) time series data

    ts.setPQ (p)
    ts.train ()
    new Plot (t, y, ts.predict (), s"Plot of y, ar($p) vs. t", true)
    val ar_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using AR($p) model = $ar_f")

    ts.setPQ (0, q)
    ts.train ()
    new Plot (t, y, ts.predict (), s"Plot of y, ma($q) vs. t", true)
    val ma_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using MA($q) model = $ma_f")

    ts.setPQ (p, q)
    ts.train ()
    new Plot (t, y, ts.predict (), s"Plot of y, arma($p, $q) vs. t", true)
    val arma_f = ts.forecast (steps)
    println (s"$steps-step ahead forecasts using ARMA($p, $q) model = $arma_f")

} // SARIMATest4 object

