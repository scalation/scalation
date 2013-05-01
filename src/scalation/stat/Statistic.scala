
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Aug 26 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.sqrt

import scalation.random.{Quantile, Uniform}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to collect values and compute sample statistics on them
 *  (e.g., Waiting Time).  Contrast with TimeStatistic defined below.
 *  @param name      the name for this statistic (e.g., WatingTime or tellerQ)
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
    protected var sum = 0.

    /** Sum of the sample values squared
     */
    protected var sumSq = 0.

    /** the minimum sample value
     */
    protected var minX = Double.MaxValue

    /** the maximum sample value
     */
    protected var maxX = 0.

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
    /** Get the number of samples.
     */
    def num: Int = n

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the minimum value in sample.
     */
    def min: Double = if (n == 0) 0. else minX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the maximum value in sample.
     */
    def max: Double = maxX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the sample mean.
     */
    def mean: Double = if (n == 0) 0. else sum / n.toDouble

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute/estimate the sample variance.  The denominator is one less for
     *  unbiased (n-1) vs. maximum likelihood (n) estimators.  Also use n for
     *  population variance.
     */
    def variance: Double =
    {
        if (n == 0) 0.
        else sumSq / (if (unbiased) n - 1. else n).toDouble - mean * mean
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
        val df = n - 1              // degrees of freedom
        if (df < 1) return 0.       // flaw ("interval", "must have at least 2 observations")
        val pp = 1 - (1 - p) / 2.   // e.g., .95 --> .975 (two tails)
        val t = Quantile.studentTInv (pp, df)
        t * stddev / sqrt (n.toDouble)
    } // interval

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a header of statistical labels as a string.
     */
    def labels (): String =
    {
        "| %4s | %9s | %9s | %9s | %9s | %9s |".format ("num", "min", "max", "mean", "stdDev", "interval")
    } // labels

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a row of statistical results as a string.
     */
    override def toString: String = 
    {
        "| %4d | %9.3f | %9.3f | %9.3f | %9.3f | %9.3f |".format (num, min, max, mean, stddev, interval ())
    } // toString

} // Statistic class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to collect values and compute time-persistent statistics
 *  on them (e.g., Number in Queue).
 *  @param lastTime  the time of previous observation
 *  @param name      the name for this statistic (e.g., NumberInQueue or tellerQ)
 */
class TimeStatistic (override val name: String = "timeStat",
                              var lastTime: Double = 0.)
      extends Statistic (name)
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Disable the tally method (it is for sample statistics, not time persistent
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
    def accumulate (x: Double, t: Double)
    {
        val duration = t - lastTime    // the duration of value x
        lastTime     = t

        n     += 1
        sum   += x * duration
        sumSq += x * x * duration * duration
        if (x < minX) minX = x
        if (x > maxX) maxX = x
    } // accumulate

} // TimeStatistic class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Statistic and TimeStatistic classes.
 */
object StatisticTest extends App
{
    val rv   = Uniform (0, 10)

    //:: Test ordinary sample statistics

    println ("\nTest sample statistics")
    val stat1 = new Statistic ()
    for (i <- 1 to 1000) stat1.tally (rv.gen)
    println (stat1.labels)
    println (stat1)

    //:: Test time persistent statistics

    println ("\nTest time persistent statistics")
    val stat2 = new TimeStatistic ()
    for (i <- 1 to 1000) stat2.accumulate (rv.gen, 2. * i)
    println (stat2.labels)
    println (stat2)

} // StatisticTest object

