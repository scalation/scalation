
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.5
 *  @date    Sun May 28 13:26:35 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.itl.nist.gov/div898/handbook/pmc/section4/pmc432.htm
 *  @see en.wikipedia.org/wiki/Exponential_smoothing#Triple_exponential_smoothing
 */

package scalation.analytics
package forecaster

import scalation.linalgebra.{VectoD, VectorD}
import scalation.minima.L_BFGS_B
import scalation.plot.Plot
import scalation.random.Random
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpSmoothing` class provide very basic time series analysis capabilities of
 *  Exponential Smoothing models.  `ExpSmoothing` models are often used for forecasting.
 *  Given time series data stored in vector 'y', its next value 'y_t = y(t)'
 *  may be predicted based on prior/smoothed values of 'y':
 *  <p>
 *      y_t = s_t-1 + α (s_t-1 - s_t-2)
 *  <p>
 *  where vector 's' is the smoothed version of vector 'y' and 'α in [0, 1]' is the
 *  smoothing parameter. Trend and seasonality can be factored into the model with
 *  two additional smoothing parameters 'β' and 'γ', respectively.
 *  @param y_              the input vector (time series data)
 *  @param ll              seasonal period
 *  @param multiplicative  whether to use multiplicative seasonality or not
 *                         if false, use additive seasonality
 *  @param validateSteps   number of steps ahead within-sample forecast sse to minimize
 */
class ExpSmoothing (y_ : VectoD, ll: Int = 1, multiplicative : Boolean = false, validateSteps : Int = 1)
      extends Forecaster
{
    private val DEBUG      = false                           // debug flag
    private var rSquared   = -1.0                            // coefficient of determination (quality of fit)
    private var fStat      = -1.0                            // F statistic (quality of fit)
    private var α          = 0.3                             // the default value for the smoothing parameter
    private var β          = 0.1                             // the default value for the trend smoothing parameter
    private var γ          = 0.1                             // the default value for the seasonal change smoothing parameter

    private var y: VectoD  = null                            // the time series
    private var n          = 0                               // size of the input vector
    private var df         = -1                              // degrees of freedom
    private var mu         = 0.0                             // the sample mean
    private var sig2       = 0.0                             // the sample variance
    private var s: VectoD  = null                            // holder for the smoothed data (constant part)
    private var b: VectoD  = null                            // holder for the smoothed data (trend part)
    private var c: VectoD  = null                            // holder for the smoothed data (seasonal part)
    private var nn         = 0                               // number of complete cycles/seasons in data
    private var aa: VectoD = null                            // holder for the average value of 'y' in each cycle/season of data
    private var yp: VectoD = null                            // vector of predicted/fitted values

    setTS (y_)                                               // initialize the above parameters

    if (DEBUG) {
        println ("n    = " + n)                              // number of data points
        println ("mu   = " + mu)                             // mean
        println ("sig2 = " + sig2)                           // variance
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set/change the internal time series. May be used to set the time series
     *  to a different time window (typically future when new data become available)
     *  in order to produce newer forecast (typically with the new data) without re-training
     *  the model for parameters (use existing parameters from previous training).
     *  @param y_   the new time series
     */
    def setTS (y_ : VectoD)
    {
        y        = y_
        n        = y.dim                                     // size of the input vector
        df       = n - 1                                     // degrees of freedom
        mu       = y.mean                                    // the sample mean
        sig2     = y.variance                                // the sample variance
        s        = new VectorD (n)                           // holder for the smoothed data (constant part)
        b        = new VectorD (n)                           // holder for the smoothed data (trend part)
        c        = new VectorD (n)                           // holder for the smoothed data (seasonal part)
        nn       = n / ll                                    // number of complete cycles/seasons in data
        aa       = new VectorD (nn)                          // holder for the average value of 'y' in each cycle/season of data
        e        = new VectorD (n)                           // initialize the error vector
        yp       = new VectorD (n)                           // initialize vector of fitted values

        if (n < 2*ll) flaw ("ExpSmoothing", "Usually a minimal of 2 full seasons are needed to initialize seasonal parameters")
        init ()
    } // setTS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute initial values of 's', 'b', 'aa' and 'c'.
     */
    def init ()
    {
        s(0) = y(0)

        var sum = 0.0
        for (i <- 0 until ll) sum += (y(ll+i) - y(i)) / ll
        b(0) = sum / ll

        for (j <- 0 until nn) {
            sum = 0.0
            for (i <- 0 until ll) sum += y(ll*j+i)
            aa(j) = sum / ll
        } // for

        for (i <- 0 until ll) {
            sum = 0.0
            for (j <- 0 until nn) sum += y(ll*j+i) / aa(j)
            c(i) = sum / nn
        } // for
    } // init

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the times series data.
     *  @param input  the input vector of 'α', 'β' and 'γ' to be optimized
     */
    def smooth (input: VectoD = VectorD(α, β, γ)): VectoD =
    {
        α = input(0)
        β = input(1)
        γ = input(2)

        val α_1 = 1.0 - α
        val β_1 = 1.0 - β
        val γ_1 = 1.0 - γ

        if (multiplicative) {
            for (t <- 1 until n) {
                if (t - ll >= 0) {
                    s(t) = α * y(t) / c(t-ll) + α_1 * (s(t-1) + b(t-1))
                    b(t) = β * (s(t) - s(t-1)) + β_1 * b(t-1)
                    c(t) = γ * y(t) / s(t) + γ_1 * c(t-ll)
                } else {
                    s(t) = α * y(t) + α_1 * (s(t-1) + b(t-1))
                    b(t) = β * (s(t) - s(t-1)) + β_1 * b(t-1)
                } // if
            } // for
        } else {
            for (t <- 1 until n) {
                if (t - ll >= 0) {
                    s(t) = α * (y(t) - c(t-ll)) + α_1 * (s(t-1) + b(t-1))
                    b(t) = β * (s(t) - s(t-1)) + β_1 * b(t-1)
                    c(t) = γ * (y(t) - s(t)) + γ_1 * c(t-ll)
                } else {
                    s(t) = α * y(t) + α_1 * (s(t-1) + b(t-1))
                    b(t) = β * (s(t) - s(t-1)) + β_1 * b(t-1)
                } // if
            } // for
        } // if

        for (i <- validateSteps until n) {
            yp(i) = forecast (validateSteps, i-validateSteps).last
            e(i)  = y(i) - yp(i)
        } // for
        sse = e dot e
        s
    } // smooth

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function to be minimized (sum of squared errors).
     *  @param input  the input vector of 'α', 'β' and 'γ' to be optimized
     */
    def f_obj (input: VectoD): Double = {smooth (input); sse }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the `ExpSmoothing` model to times series data, by finding the value for
     *  the smoothing parameter 'α', 'β' and 'γ' that minimizes the sum of squared errors (sse).
     */
    def train (): ExpSmoothing =
    {
        val l = VectorD.fill(3)(0.0)
        val u = VectorD.fill(3)(1.0)
        val solver = new L_BFGS_B (f_obj, l=l, u=u)
        val result = solver.solve (VectorD(α, β, γ), 1E-4)      // optimized value for α, β, γ
        smooth (result)
        if (DEBUG) println(s"(α, β, γ) = ${(α, β, γ)}")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     */
    def eval () = diagnose (y, e)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the Exponential Smoothing model.
     *  @param yy  the response vector
     */
    override def diagnose (yy: VectoD, ee: VectoD)
    {
        super.diagnose (yy, ee)
        fStat = ((sst - sse) * df) / sse                       // F statistic (msr / mse)
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of fitted values on the training data.
     */
    def fittedValues (): VectoD = yp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.  Not providing 'rBarSq'.
     */
    override def fit: VectorD = super.fit.asInstanceOf [VectorD] ++ VectorD (fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit. 
     */
    override def fitLabel: Seq [String] = super.fitLabel ++ Seq ("fStat")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce n-steps-ahead forecast for ARIMA models.
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param h    the number of steps to forecast, must be at least one.
     */
    def forecast (h: Int): VectoD = forecast (h, n-1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast 'h'-steps ahead based on data up to time 't'.
     *  @param h  the step size
     *  @param t  the time point to start the forecast
     */
    def forecast (h: Int = 1, t: Int = n-1): VectoD =
    {
        val yf = new VectorD (h)
        for (i <- 1 to h) {
            if (multiplicative) {
                yf(i-1) = if (t-ll >= -1) (s(t) + i * b(t)) * c(t-ll+1+(i-1)%ll) else s(t) + i * b(t)
            } else {
                yf(i-1) = if (t-ll >= -1) s(t) + i * b(t) + c(t-ll+1+(i-1)%ll) else s(t) + i * b(t)
            } // if
        } // for
        yf
    } // forecast

} // ExpSmoothing class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpSmoothingTest` object is used to test the `ExpSmoothing` class.
 *  > runMain scalation.analytics.ExpSmoothingTest
 */
object ExpSmoothingTest extends App
{
    val n = 100
    val r = Random ()
    val t = VectorD.range (0, n)
    val y = VectorD (for (i <- 0 until n) yield t(i) + 10.0 * r.gen)
    val h = 3
    val ts = new ExpSmoothing (y)                                 // smooth time series data: y vs. t

    banner ("Customized Exponential Smoothing")
    ts.smooth ()                                                  // use customized parameters
    ts.eval ()
    val s = ts.fittedValues ()
    println (s"fit = ${ts.fit}")
    println (s"predict (s) = ${ts.forecast (h)}")
    new Plot (t, y, s, "Plot of y, s vs. t")

    banner ("Optimized Exponential Smoothing")
    ts.train ().eval ()                                           // use optimal α
    val s2 = ts.fittedValues ()
    println (s"fit = ${ts.fit}")
    println (s"predict (s2) = ${ts.forecast (h)}")
    new Plot (t, y, s2, "Plot of y, s2 vs. t")

} // ExpSmoothingTest object

