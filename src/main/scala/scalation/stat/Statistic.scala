
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Aug 26 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import collection.mutable.ListBuffer
import math.sqrt

import scalation.math.ExtremeD
import scalation.random.{Quantile, Uniform}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Statistic` class is used to collect values and compute sample statistics
 *  on them (e.g., waiting time).  Contrast with `TimeStatistic` defined below.
 *  @param name      the name for this statistic (e.g., 'watingTime')
 *  @param unbiased  whether the estimators are restricted to be unbiased
 */
class Statistic (val name: String = "stat", unbiased: Boolean = false)
      extends Error
{
    /** The number of samples
     */
    protected var n = 0

    /** Sum of the sample values
     */
    protected var sum = 0.0

    /** Sum of the sample values squared
     */
    protected var sumSq = 0.0

    /** The minimum sample value
     */
    protected var minX = ExtremeD.MAX_VALUE

    /** The maximum sample value
     */
    protected var maxX = ExtremeD.MOST_NEGATIVE 

    def reset ()
    {
        n     = 0
        sum   = 0.0
        sumSq = 0.0
        minX  = ExtremeD.MAX_VALUE
        maxX  = ExtremeD.MOST_NEGATIVE
    } // reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally the next value and update accumulators.
     *  @param x  the value to tally
     */
    def tally (x: Double)
    {
        n     += 1
        sum   += x
        sumSq += x * x
        if (x < minX) minX = x
        if (x > maxX) maxX = x
    } // tally


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of samples.
     */
    def num: Int = n

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of samples as a double.
     */
    def nd: Double = n.toDouble

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum value in sample.
     */
    def min: Double = if (n == 0) 0.0 else minX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum value in sample.
     */
    def max: Double = maxX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the sample mean.
     */
    def mean: Double = if (n == 0) 0.0 else sum / nd

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the sample variance.  The denominator is one less for
     *  unbiased (n-1) vs. maximum likelihood (n) estimators.  Also use n for
     *  population variance.
     */
    def variance: Double =
    {
        if (n == 0) 0.0 else (sumSq - sum*sum/nd) / (if (unbiased) nd - 1.0 else nd)
    } // variance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the sample standard deviation.
     */
    def stddev: Double = sqrt (variance)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the mean square (ms).
     */
    def ms: Double = sumSq / n.toDouble

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the root mean square (rms).
     */
    def rms: Double = sqrt (ms)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the confidence interval half-width for the given confidence level.
     *  @param p  the confidence level
     */
    def interval (p: Double = .95): Double =
    {
        val df = n - 1               // degrees of freedom
        if (df < 1) return 0.0       // flaw ("interval", "must have at least 2 observations")
        val pp = 1 - (1 - p) / 2.0   // e.g., .95 --> .975 (two tails)
        val t = Quantile.studentTInv (pp, df)
        t * stddev / sqrt (n.toDouble)
    } // interval

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the values of this collector's accumulators.
     */
    def show: String = "Statistic: " + n + ", " + sum + ", " + sumSq + ", " + minX + ", " + maxX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the summary statistics as a row/Array.
     */
    def statRow = Array (name, num, min, max, mean, stddev, interval ())

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a row of statistical results as a string.
     */
    override def toString: String = 
    {
        "| %11s | %5s | %10.3g | %10.3g | %10.3g | %10.3g | %10.3g |".format (
        name, num, min, max, mean, stddev, interval ())
    } // toString

} // Statistic class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Statistic` companion object provides additional values and functions.
 */
object Statistic
       extends Error
{
    /** The line separator
     */
    val line = "----------------------------------------------------------------------------------------"

    /** The statistical labels (column headers) as an Array
     */
    val label = Array ("name", "num", "min", "max", "mean", "stdDev", "interval")

    /** The statistical labels (column headers) as a formatted String
     */
    val labels = "| %11s | %5s | %10s | %10s | %10s | %10s | %10s |".format (
                 "name", "num", "min", "max", "mean", "stdDev", "interval")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a statistical object and set its accumulators and extreme values.
     *  @param n         the number of samples
     *  @param sum       sum of the sample values
     *  @param sumSq     sum of the sample values squared
     *  @param minX      the minimum sample value
     *  @param maxX      the maximum sample value
     *  @param name      the name for this statistic (e.g., 'waitingTime')
     *  @param unbiased  whether the estimators are restricted to be unbiased
     */
    def apply (n: Int, sum: Double, sumSq: Double, minX: Double, maxX: Double,
               name: String = "stat", unbiased: Boolean = false): Statistic =
    {
        val stat   = new Statistic (name, unbiased)
        stat.n     = n
        stat.sum   = sum
        stat.sumSq = sumSq
        stat.minX  = minX
        stat.maxX  = maxX
        stat
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate the sub-statistics by summing all accumulators and determining
     *  new overall extreme values.
     *  @param subStats  the list of sub-statistics
     *  @param name      the name for the aggregated statistic
     *  @param unbiased  whether the estimators are restricted to be unbiased
     */
    def aggregate (subStats: ListBuffer [Statistic], name: String = "a-stat",
                   unbiased: Boolean = false): Statistic =
    {
        val m = subStats.size
        if (m < 1) { flaw ("average", "there are no subparts to average"); return null }
        var n     = 0
        var sum   = 0.0
        var sumSq = 0.0
        var minX  = Double.MaxValue
        var maxX  = 0.0
        for (s <- subStats) {
            n     += s.n
            sum   += s.sum
            sumSq += s.sumSq
            if (s.minX < minX) minX = s.minX
            if (s.maxX > maxX) maxX = s.maxX
        } // for
        Statistic (n, sum, sumSq, minX, maxX, name, unbiased)
    } // aggregate

} // Statistic object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TimeStatistic` class is used to collect values and compute time-persistent
 *  statistics on them (e.g., Number in Queue).
 *  @see staff.unak.is/andy/Year%203%20Simulation/Laboratories/v4manual/internal.htm
 *  @param name        the name for this statistic (e.g., 'numberInQueue' or 'tellerQ')
 *  @param _lastTime   the time of last observation
 *  @param _startTime  the time observation began
 */
class TimeStatistic (override val name: String = "p-stat",
                      private var _lastTime:  Double = 0.0,
                      private var _startTime: Double = 0.0)
      extends Statistic (name)
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the last time statistics were recorded by this collector.
     */
    def lastTime: Double = _lastTime

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the time observation began.
     */
    def startTime: Double = _startTime

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Disable the tally method (it is for sample statistics, not time-persistent
     *  statistics.
     *  @param x  the value to tally
     */
    override def tally (x: Double)
    {
        flaw ("tally", "this method must not be called from TimeStatistic")
    } // tally

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Accumulate the next value weighted by its time duration and update accumulators.
     *  @param x  the value to accumulate
     *  @param t  the time of the observation
     */
    def accum (x: Double, t: Double)
    {
        val duration = t - _lastTime    // the duration of value x
        _lastTime    = t

        n     += 1
        sum   += x * duration
        sumSq += x * x * duration
        if (x < minX) minX = x
        if (x > maxX) maxX = x
    } // accum

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the time-weighted mean.
     */
    override def mean: Double =
    {
        val totalTime = _lastTime - _startTime
        if (totalTime <= 0.0) 0.0 else sum / totalTime
    } // mean

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the time-weighted variance.
     */
    override def variance: Double =
    {
        val totalTime = _lastTime - _startTime
        if (totalTime <= 0.0) 0.0 else (sumSq - sum*sum/totalTime) / totalTime
    } // variance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the values of this collector's accumulators.
     */
    override def show: String = "Time" + super.show + ", " + _lastTime + ", " + _startTime

} // TimeStatistic class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TimeStatistic` companion object provides additional functions.
 */
object TimeStatistic
       extends Error
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a statistical object and set its accumulators and extreme values.
     *  @param n           the number of samples
     *  @param sum         sum of the sample values
     *  @param sumSq       sum of the sample values squared
     *  @param minX        the minimum sample value
     *  @param maxX        the maximum sample value
     *  @param name        the name for this statistic (e.g., 'waitingTime')
     *  @param _lastTime   the time of latest observation
     *  @param _startTime  the time observation began
     */
    def apply (n: Int, sum: Double, sumSq: Double, minX: Double, maxX: Double, name: String = "p-stat",
               _lastTime: Double = 0.0, _startTime: Double = 0.0): TimeStatistic =
    {
        val stat   = new TimeStatistic (name, _lastTime, _startTime)
        stat.n     = n
        stat.sum   = sum
        stat.sumSq = sumSq
        stat.minX  = minX
        stat.maxX  = maxX
        stat
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Aggregate the sub-statistics by summing all accumulators and determining
     *  new overall extreme values.
     *  @param subStats  the list of sub-statistics
     *  @param name      the name for the aggregated statistic
     */
    def aggregate (subStats: ListBuffer [TimeStatistic], name: String = "ap-stat"): TimeStatistic =
    {
        val m = subStats.size
        if (m < 1) { flaw ("average", "there are no subparts to average"); return null }
        var n     = 0
        var sum   = 0.0
        var sumSq = 0.0
        var minX  = Double.MaxValue
        var maxX  = 0.0
        var starT = 0.0
        var lastT = 0.0
        for (s <- subStats) {
            n     += s.n
            sum   += s.sum
            sumSq += s.sumSq
            if (s.minX < minX)      minX = s.minX
            if (s.maxX > maxX)      maxX = s.maxX
            if (s.startTime < starT) starT = s.startTime
            if (s.lastTime  > lastT) lastT = s.lastTime
        } // for
        TimeStatistic (n, sum, sumSq, minX, maxX, name, lastT, starT)
    } // aggregate

} // TimeStatistic object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StatisticTest` object is used to test the `Statistic` and `TimeStatistic`
 *  classes.
 */
object StatisticTest extends App
{
    val rv = Uniform (0, 10)

    //:: Test ordinary sample statistics

    println ("\nTest sample statistics")
    val stat1 = new Statistic ()
    for (i <- 1 to 1000) stat1.tally (rv.gen)
    println (Statistic.labels)
    println (stat1)

    //:: Test time persistent statistics

    println ("\nTest time persistent statistics")
    val stat2 = new TimeStatistic ()
    for (i <- 1 to 1000) stat2.accum (rv.gen, 2.0 * i)
    println (Statistic.labels)
    println (stat2)

} // StatisticTest object

