
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Dec 28 15:57:04 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.stat

import math.{log, sqrt}

import scalation.linalgebra.VectorD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Normality` object provides methods for testing Normality based on skewness
 *  and kurtosis.  Such test are more suitable for large sample sizes where more
 *  powerful goodness-of-fit tests tend to frequently reject Normality.
 *  @see stats.stackexchange.com/questions/29731/regression-when-the-ols-residuals-are-not-normally-distributed
 *  @see stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless
 *  @see en.wikipedia.org/wiki/D%27Agostino%27s_K-squared_test
 */
object Normality
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test vector data 'd' to see if its skewness is sufficiently close to Normal.
     *  @param d  the data vector to be tested for Normality
     */
    def normalSkew (d: VectorD): Double =
    {
        val n   = d.nd
        val g1  = d.skew ()
        val mu2 = 6.0 * (n-2.0) / ((n+1.0) * (n+3.0))
        val gm2 = 36.0 * (n-7.0) * (n*n + 2*n - 5.0) / ((n-2.0) * (n+5.0) * (n+7.0) * (n+9.0))
        val w2  = sqrt (2.0 * gm2 + 4.0) - 1.0
        val dl  = 1.0 / sqrt (log (sqrt (w2)))
        val a   = 1.0 / (w2 - 1.0)
        dl * log (g1 / (a * sqrt (mu2)) + sqrt (g1*g1 / (a*a*mu2) + 1.0))
    } // normalSkew

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test vector data 'd' to see if its kurtosis is sufficiently close to Normal.
     *  @param d  the data vector to be tested for Normality
     */
    def normalKurtosis (d: VectorD): Double =
    {
        0.0                  // FIX - to be implemented
    } // normalKurtosis

} // Normality


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NormalityTest` object is used to test the `Normality` object.
 *  @see www.seattlecentral.edu/qelp/sets/057/057.html
 *  > run-main scalation.stat.NormalityTest
 */
object NormalityTest extends App
{
    import scalation.random.{Normal, Uniform}

    val d = VectorD (36.0, 37.0, 38.0, 38.0, 39.0, 39.0, 40.0, 40.0, 40.0, 40.0,
                     41.0, 41.0, 41.0, 41.0, 41.0, 41.0, 42.0, 42.0, 42.0, 42.0,
                     42.0, 42.0, 42.0, 43.0, 43.0, 43.0, 43.0, 43.0, 43.0, 43.0,
                     43.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0, 44.0,
                     45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0, 45.0,
                     46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0, 46.0,
                     47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 47.0, 48.0,
                     48.0, 48.0, 48.0, 48.0, 48.0, 48.0, 48.0, 49.0, 49.0, 49.0,
                     49.0, 49.0, 49.0, 49.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0,
                     51.0, 51.0, 51.0, 51.0, 52.0, 52.0, 53.0, 53.0, 54.0, 55.0)

    val dmin  = d.min ()         // the minimum
    val dmax  = d.max ()         // the minimum
    val dmu   = d.mean           // the mean
    val dsig2 = d.variance       // the variance
    val dsig  = sqrt (dsig2)     // the standard deviation

    println ("-------------------------------------------------------------")
    println (" Basic Statistics")
    println ("-------------------------------------------------------------")
    println ("n     = " + d.dim)
    println ("dmin  = " + dmin)
    println ("dmax  = " + dmax)
    println ("dmu   = " + dmu)
    println ("dsig2 = " + dsig2)
    println ("dsig  = " + dsig)
    println ("-------------------------------------------------------------")
    println ("normality = " + Normality.normalSkew (d))
    println ("-------------------------------------------------------------")

} // NormalityTest object

