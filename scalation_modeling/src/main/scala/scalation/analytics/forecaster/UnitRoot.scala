
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng
 *  @version 1.5
 *  @date    Sun Nov 19 12:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  Unit Root Tests for Time Series Stationarity
 *  (1 is a root of the processes characteristic equation)
 *  @see github.com/olmallet81/URT
 */

package scalation.analytics.forecaster

import scala.collection.immutable.HashMap
import scala.Double.NaN
import scala.util.control.Breaks.{break, breakable}

import scalation.analytics.Regression
import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectorS}
import scalation.math.double_exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UnitRoot` abstract class provides a common framework for various unit
 *  root testers for Time Series Stationarity.
 *  This code is translated from the C++ code found in
 *  @see github.com/olmallet81/URT.
 *  @param data   the time series vector
 *  @param lags   the number of lags to use
 *  @param trend  type of trend to test for
 */
abstract class UnitRoot (data: VectoD, var lags: Int, var trend: String)
{
    protected var lagsType: String     = null              // default lags value long or short
    protected var maxLags              = 0                 // maximum number of lags for lag length optimization
    protected var validTrends: VectorS = null              // vector of test valid regression trends
    protected var y                    = data              // copy of the reference to the original time series
    protected var coeff: HashMap [Double, HashMap [Int, VectorD]] = null  // HashMap containing critical values coefficients
    protected var testName: String     = null              // test name
    protected var stat                 = 0.0               // test statistic
    protected var pval                 = 1.0               // test p-value
    protected var nobs                 = data.size         // number of observations
    protected var npar                 = 0                 // number of parameters excluding lag difference terms
    protected var newLags              = false             // control if a new number of lags has been chosen
    protected var newTrend             = false             // control if a new trend has been chosen

    private var prevLagsType: String   = null              // previous type of lags
    private var prevTrend: String      = null              // previous regression trend
    private var trendType: String      = null              // regression trend for outputting test results
    private var prevLags               = 0                 // previous number of lags
    private var optim                  = false             // control if lag length is optimized
    private var newTest                = false             // control if a new test is run (true) or all parameters remain the same (false)

    private val probas          = Array (0.001, 0.005, 0.01, 0.025, 0.05,
                                         0.10 , 0.20 , 0.50, 0.80 , 0.90,
                                         0.95 , 0.975, 0.99, 0.995, 0.999)  // array of probabilities for p-value computation
    private val criticalValues  = Array.ofDim[Double](15)                   // array containing tne test critical values


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get test statistic.
     */
    def getStat (): Double = stat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get test pvalue.
     */
    def getPval (): Double = pval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get test valid trends.
     */
    def getTrends (): VectorS = validTrends

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute test statistic.
     */
    def statistic (): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute test statistic p-value.
     */
    def pvalue (): Double =
    {
        if (stat.isNaN) pval = NaN
        else if (newTest) {     // if a new test has been run
            computeCV ()        // computing critical values
            computePval ()      // computing p-value
            newTest = false
        } // if
        pval
    } // pvalue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Output test results (can be overridden by derived classes).
     */
    def show ()
    {
        println ("\n  " + testName + " Test Results")       // outputting test name
        println ("  ====================================")
        println (s"  Statistic = $stat")                    // outputting test statistic
        print   ("  P-value = ")                            // outputting p-value

        if      (pval <= probas(0))  print ("< 0.001")
        else if (pval >= probas(14)) print ("> 0.999")
        else                         print (pval)
        println ()

        println (s"  Lags  = $lags")
        println (s"  Trend = $trendType")
        println ("  ------------------------------------\n")

        println ("  Test Hypothesis")                       // outputting test hypothesis
        println ("  ------------------------------------")

        println ("  H0: The process is weakly stationary")  // KPSS hypotheses
        println ("  H1: The process contains a unit root")
        println ()

        println ("  Critical Values")                       // outputting critical values
        println ("  ---------------")

        val idx = Array (12, 10, 9)                         // declaring array of critical value indexes

        if (stat.isNaN) {
            println (s"   1% $NaN")
            println (s"   5% $NaN")
            println (s"  10% $NaN")
        } else {
            println (s"   1% ${criticalValues(idx(0))}")
            println (s"   5% ${criticalValues(idx(1))}")
            println (s"  10% ${criticalValues(idx(2))}")
        } // if
        println ()

        println ("  Test Conclusion")                       // outputting test conclusion
        println ("  ---------------")

        if      (pval <= 0.01) println ("We can reject H0 at the 1% significance level")
        else if (pval <= 0.05) println ("We can reject H0 at the 5% significance level")
        else if (pval <= 0.10) println ("We can reject H0 at the 10% significance level")
        else if (! pval.isNaN) println ("We cannot reject H0")
        else                   println ("We cannot conclude, NaN produced")
    } // show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset 'y' to the original data.
     */
    protected def setData () = y = data

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set number of lags, checking input validity. This method needs to know
     *  optim so it needs to be run after set_method ().
     */
    protected def setLags ()
    {
        if (! optim) {
            if (lags < 0) {                                               // number of lags cannot be strictly negative
                lags = 0
                println ("\n  WARNING: number of lags cannot be negative, it has been set to 0 by default.\n")
            } // if
            if (! lagsType.isEmpty && lags != prevLags) lagsType = ""     // if user has switched from a default lags value
                                                                          // to a value of his choice (for all tests)
        } else if (maxLags < 0) {  // maxLags cannot be strictly negative
            maxLags = 0
            println ("\n  WARNING: maximum number of lags cannot be negative, it has been set to a default value (L12-rule).\n")
        } // if

        // updating lags only for PP and KPSS tests, for ADF and DFGLS tests lags will be updated at the next optimization or
        // set back to prevLags if maxLags, trend, method and level are the same as before

        if ((optim && (maxLags == 0)) || !lagsType.isEmpty) {                     // computing default lags value for KPSS test
            maxLags = if (lagsType == "short") (4 *  (0.01 * nobs)~^0.25).toInt   // short => L4-rule (Schwert 1989)
                      else                     (12 * (0.01 * nobs)~^0.25).toInt   // long => L12-rule (Schwert 1989)
            lags = maxLags
        } // if

        if (!optim && lags != prevLags) {      // for all tests if the number of lags is still different than its previous value
            newTest  = true
            newLags  = true
            prevLags = lags
        } // if
    } // setLags

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set lags type long or short for PP and KPSS default lags value or ADF
     *  and DFGLS default maxlags value.
     */
    protected def setLagsType ()
    {
        if (lagsType.isEmpty || (lagsType == prevLagsType)) return   // skipping this method if lagsType is empty or did not change
        if ((lagsType != "long") && (lagsType != "short")) {
            println("\n  WARNING: unknown default type of lags, long has been selected by default.\n")
            lagsType = "long"      // default lags type is long
        } // if
        prevLagsType = lagsType
    } // setLagsType

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set regression trend.
     */
    protected def setTrend ()
    {
        if (trend == prevTrend) return           // skipping this method if trend did not change

        if (! validTrends.contains (trend)) {    // checking input trend validity, setting default trend to constant
            trend      = "c"
            trendType = "constant"
            npar       = 2
            println ("\n  WARNING: unknown regression trend selected, regression with constant term has been selected by default.\n")
            println (s"  Possible trends for this test are $validTrends")
        } else {
            if (trend == "c") {
                trendType = "constant"
                npar = 2
            } else if (trend == "nc") {
                trendType = "no constant"
                npar = 1
            } else if (trend == "ct") {
                trendType = "constant trend"
                npar = 3
            } else if (trend == "ctt") {
                trendType = "quadratic trend"
                npar = 4
            } // if
        } // if

        if (trend != prevTrend) {
            newTest   = true
            newTrend  = true
            prevTrend = trend
        } // if
    } // setTrend

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** OLS demeaning or detrending.
     */
    protected def olsDetrend ()
    {
        var w: MatrixD = null

        if (trend == "ct") {
            val v = Array.ofDim [Double] (nobs, 2)
            for (i <- 0 until v.length) { v(i)(0) = 1.0; v(i)(1) = i }
            w = new MatrixD (v)
        } else if (trend == "nc") {
            throw new IllegalArgumentException ("\n ERROR: olsDetrend: no detrending possible when regression trend set to no constant.\n")
        } // if

        if (trend == "c") {
            val mu = y.mean
            y = VectorD (for (i <- y.range) yield y(i) - mu)
        } else {
            val reg = new Regression (w, y)
            reg.train ().eval ()
            y = reg.residual
        } // if
    } // olsDetrend

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute critical value from probabilities.
     */
    private def computeCV ()
    {
        val n = nobs - lags - 1                    // computing adjusted number of observations

        for (i <- 0 until 15) {
            criticalValues(i) = 0                  // computing critical value
            val n0 = coeff.getOrElse(probas(i), null).getOrElse (0, null).size
            for (j <- 0 until n0) criticalValues(i) += coeff.getOrElse (probas(i), null).getOrElse (0, null)(j) / (n~^j)

            val n1 = coeff.getOrElse(probas(i), null).getOrElse (1, null).size
            for (j <- 0 until n1) criticalValues(i) += coeff.getOrElse (probas(i), null).getOrElse (1, null)(j) * ((lags.toDouble/n)~^(j+1))
        } // for
    } // computeCV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute p-value by linear interpolation from critical values.
     */
    private def computePval ()
    {
        if (stat <= criticalValues(0)) pval = probas(0)        // if stat is smaller than critical value for first probability (in absolute value)
        else {
            breakable {for (i <- 1 until 15 if stat <= criticalValues(i)) {
                pval = probas(i-1) + (stat - criticalValues(i-1)) * (probas(i) - probas(i-1)) / (criticalValues(i) - criticalValues(i-1))
                break
            }} // breakable for
        } // if
        if (stat > criticalValues.last) pval = probas.last     // if stat is greater than critical value for last probability in absolute value
    } // computePval

} // UnitRoot abstract class

