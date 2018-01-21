
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun May 28 13:26:35 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.itl.nist.gov/div898/handbook/pmc/section4/pmc432.htm
 */

package scalation.analytics

import scala.math.min

import scalation.linalgebra.{VectoD, VectorD}
import scalation.math.double_exp
import scalation.minima.GoldenSectionLS
import scalation.plot.Plot
import scalation.random.Random
import scalation.stat.vectorD2StatVector
import scalation.util.{banner, Error}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpSmoothing` class provide very basic time series analysis capabilities of
 *  Exponential Smoothing models.  `ExpSmoothing` models are often used for forecasting.
 *  Given time series data stored in vector 'y', its next value 'y_t = y(t)'
 *  may be predicted based on prior/smoothed values of 'y':
 *  <p>
 *      y_t = s_t-1 + α (s_t-1 - s_t-2)
 *  <p>
 *  where vector 's' is the smoothed version of vector 'y' and 'α in [0, 1]' is the
 *  smoothing parameter.
 *------------------------------------------------------------------------------
 *  @param y  the input vector (time series data)
 *  @param t  the time vector
 */
class ExpSmoothing (y: VectoD, t: VectoD)
      extends Predictor with Error
{
    private val DEBUG    = true                                // debug flag
    private val n        = y.dim                               // size of the input vector
    private val df       = n - 1                               // degrees of freedom
    private var rSquared = -1.0                                // coefficient of determination (quality of fit)
    private var fStat    = -1.0                                // F statistic (quality of fit)
    private val mu       = y.mean                              // the sample mean
    private val sig2     = y.variance                          // the sample variance
    private var α        = 0.5                                 // the default value for the smoothing parameter
    private val s        = new VectorD (n)                     // holder for the smoothed data

    if (DEBUG) {
        println ("n    = " + n)                                // number of data points
        println ("mu   = " + mu)                               // mean
        println ("sig2 = " + sig2)                             // variance
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the times series data.
     *  @param α_new  the new smoothing parameter, skip to use default
     */
    def smooth (α_new: Double = α): VectorD =
    {
        α = α_new
        val α_1 = 1.0 - α
        s(0)    = y(0)
        for (i <- 1 until y.dim) s(i) = α * y(i) + α_1 * y(i-1)
        sse = { e = y - s; e.normSq }
        println (s"smooth: (α, sse) = ($α, $sse}")
        s
    } // smooth

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function to be minimized (sum of squared errors for the given 'αα').
     *  @param αα  the parameter of the objective function to be optimized
     */
    def f_obj (αα: Double): Double = { e = y - smooth (αα); e.normSq }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the `ExpSmoothing` model to times series data, by finding the value for
     *  the smoothing parameter 'α' that minimizes the sum of squared errors (sse).
     *  FIX - use either a penalty or cross-validation, else α -> 1
     *  @param yy  the response vector
     */
    def train (yy: VectoD)
    {
        val α1    = α                                          // initial value for α
        val sse_1 = f_obj (α1)                                 // initial sum of squared errors
        val gs    = new GoldenSectionLS (f_obj)                // use Golden Section Search
        val α2    = gs.search (1.0)                            // optimized value for α
        val sse_2 = f_obj (α2)                                 // optimized sum of squared errors
        println (s"(α1, sse_1) = ($α1, $sse_1) vs. (α2, sse_2) = ($α2, $sse_2)")
        α = if (sse_1 < sse_2) α1 else α2
        diagnose (y)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the `ExpSmoothing` model to times series data, by finding the value for
     *  the smoothing parameter 'α' that minimizes the sum of squared errors (sse).
     *  Use the response passed into the class 'y'.
     */
    def train () { train (y) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the Exponential Smoothing model.
     *  @param yy  the response vector
     */
    override def diagnose (yy: VectoD)
    {
        super.diagnose (yy)
        fStat = ((sst - sse) * df) / sse                       // F statistic (msr / mse)
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'. Not providing 'rBarSq'.
     */
    override def fit: VectoD = super.fit.asInstanceOf [VectorD] ++ VectorD (fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit. 
     */
    override def fitLabels: Seq [String] = super.fitLabels ++ Seq ("fStat")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the next (unknown) value in the time-series.
     *  @param s  the smoothed time-series data
     */
    def predict (s: VectoD): Double = 
    {
        s(n-1) + α * (s(n-1) - s(n-2))
    } // predict

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

    val ts = new ExpSmoothing (y, t)                           // smooth time series data: y vs. t
    val α  = 0.5                                               // customized value for α

    banner ("Customized Exponential Smoothing")
    val s = ts.smooth ().copy                                  // use customized α
    ts.diagnose (y)
    println (s"fit = ${ts.fit}")
    println (s"predict (s) = ${ts.predict (s)}")
    new Plot (t, y, s, "Plot of y, s vs. t")

    banner ("Optimized Exponential Smoothing")
    ts.train ()                                                // use optimal α
    val s2 = ts.smooth ()
    println (s"fit = ${ts.fit}")
    println (s"predict (s2) = ${ts.predict (s2)}")
    new Plot (t, y, s2, "Plot of y, s2 vs. t")

} // ExpSmoothingTest object

