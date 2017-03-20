
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  Many of the algorithms used are from:
 *    Averill M. Law and W. David Kelton
 *    Simulation Modeling and Analysis, 2nd Edition
 *    McGraw-Hill, Inc., NY, 1991.
 */

package scalation.random

import scala.math.{ceil, exp, floor, log, Pi, round, sqrt, tan}

import scalation.linalgebra.{VectoD, VectorD}
import scalation.math.Combinatorics.{betaF, choose, fac, gammaF}
import scalation.math.{double_exp, FunctionS2S, nexp}
import scalation.math.ExtremeD.approx
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Variate` abstract class serves as a base class for all the random variate
 *  (RV)  generators.  They use one of the Random Number Generators (RNG's) from
 *  `Random` to generate numbers following their particular distribution.
 *  Random Variate Generators (RVG's) for thirty popular probability distributions
 *  are implemented as extensions of Variate.  Still need to add one.
 *  @see http://www.math.uah.edu/stat/special/index.html
 *  @see `VariateVec` for Random MultiVariate Generators (RMVG's).
 *-----------------------------------------------------------------------------
 *  @param  stream  the random number stream
 */
abstract class Variate (stream: Int = 0)
         extends Error
{
    /** Random number stream selected by the stream number
     */
    protected val r = Random (stream)

    /** Allow (lax) calling 'igen' on continuous distributions
     */
    private val LAX = true

    /** Indicates whether the distribution is discrete or continuous (default)
     */
    protected var _discrete = false

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the distribution is discrete or continuous.
     */
    def discrete: Boolean = _discrete

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Precompute the mean for the particular distribution.
     */
    val mean: Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability function (pf):  Either
     *  (a) the probability density function (pdf) for continuous RV's or
     *  (b) the probability mass function (pmf) for discrete RV's.
     *  @param z  the mass point whose probability density/mass is sought
     */
    def pf (z: Double): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the entire probability mass function (pmf) for finite discrete RV's.
     *  @param k  number of objects of the first type
     */
    def pmf (k: Int = 0): Array [Double] = Array (0.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random number for the particular distribution.
     */
    def gen: Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random integer for the particular distribution.
     *  It is only valid for discrete random variates.
     */
    def igen: Int =
    {
        if (LAX || _discrete) round (gen).toInt
        else { flaw ("igen", "should not be invoked on continuous RV's"); 0 }
    } // igen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random string for the particular distribution.
     *  For better random strings, overide this method.
     */
    def sgen: String = "s" + "%g".format (gen)

} // Variate class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Bernoulli` random variates.
 *  This discrete RV models the one trial (success is 1, failure is 0).
 *  @see http://www.math.uah.edu/stat/bernoulli/Introduction.html
 *  @param p       the probability of success
 *  @param stream  the random number stream
 */
case class Bernoulli (p: Double = .5, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0.0 || p > 1.0) flaw ("constructor", "parameter p must be in [0, 1]")
    _discrete = true
    private val q = 1.0 - p     // probability of failure

    val mean = p

    def pf (z: Double): Double = if (approx (z, 0.0)) q else if (approx (z, 1.0)) p else 0.0

    override def pmf (k: Int): Array [Double] = Array (q, p)

    def gen: Double = if (r.gen < p) 1.0 else 0.0

} // Bernoulli class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Beta` random variates.
 *  This continuous RV models random proportions.
 *  Beta =  Gamma1 / (Gamma1 + Gamma2).
 *  @see http://www.math.uah.edu/stat/special/Beta.html
 *  @param alpha   the shape parameter for Gamma1
 *  @param beta    the shape parameter for Gamma2
 *  @param stream  the random number stream
 */
case class Beta (alpha: Double = 2, beta: Double = 3, stream: Int = 0)
     extends Variate (stream)
{
    if (alpha <= 0 || beta <= 0) flaw ("constructor", "parameters alpha and beta must be positive")
    private val gamma1 = Gamma (alpha, 1.0, stream)
    private val gamma2 = Gamma (beta, 1.0, stream)

    val mean = alpha / (alpha + beta)

    def pf (z: Double): Double =
    {
        if (0.0 < z && z < 1.0) z~^(alpha-1.0) * (1.0-z)~^(beta-1.0) / betaF (alpha, beta)
        else                    0.0
    } // pf

    def gen: Double = { val g1 = gamma1.gen; g1 / (g1 + gamma2.gen) }

} // Beta class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Binomial` random variates.
 *  This discrete RV models the number of successes in n trials.
 *  @see http://www.math.uah.edu/stat/bernoulli/Binomial.html
 *  @param p       the probability of success
 *  @param n       the number of independent trials
 *  @param stream  the random number stream
 */
case class Binomial (p: Double = .5, n: Int = 10, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0.0 || p > 1.0) flaw ("constructor", "parameter p must be in [0, 1]")
    if (n <= 0)             flaw ("constructor", "parameter n must be positive")
    _discrete = true
    private val q    = 1.0 - p                  // probability of failure
    private val p_q  = p / q                    // the ratio p divided by q
    private val coin = Bernoulli (p, stream)    // coin with prob of success of p

    val mean = p * n

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (z == k && 0 <= k && k <= n)
            choose (n, k) * p~^k * q~^(n-k)
        else 
            0.0
    } // pf

    def pf (k: Int): Double =    // ex: for n = 10, (k, l) = (4, 6)
    {
        if (0 <= k && k <= n)
            choose (n, k) * p~^k * q~^(n-k)
        else 
            0.0
    } // pf

    override def pmf (k: Int): Array [Double] =
    {
        val d = Array.ofDim [Double] (n+1)      // array to hold pmf distribution
        d(0) = q~^n
        for (k <- 1 to n) d(k) = d(k-1) * p_q * (n-k+1) / k.toDouble
        d
    } // pmf

    def gen: Double =
    {
        var sum = 0.0
        for (i <- 0 until n) sum += coin.gen
        sum
    } // gen

} // Binomial class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Cauchy` (or Lorentz) random variates.
 *  This continuous RV models data with heavier tails than normally distributed.
 *  @see http://www.math.uah.edu/stat/special/Cauchy.html
 *  @param alpha   the location parameter (median)
 *  @param beta    the scale parameter 
 *  @param stream  the random number stream
 */
case class Cauchy (alpha: Double = 2.5, beta: Double = 1.0, stream: Int = 0)
     extends Variate (stream)
{
    if (beta <= 0.0) flaw ("constructor", "parameter beta must be positive")

    val mean = alpha                    // but, technically does not exist

    def pf (z: Double): Double = 
    {
        beta / (((z-alpha)~^2.0 + beta~^2.0) * Pi)
    } // pf

    def gen: Double = beta * tan (Pi * (r.gen-.5)) + alpha

} // Cauchy class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `ChiSquare` random variates.
 *  This continuous RV models the variance of a distribution.
 *  @see www.math.uah.edu/stat/special/ChiSquare.html
 *  @param df      the degrees of freedom 
 *  @param stream  the random number stream
 */
case class ChiSquare (df: Int = 2, stream: Int = 0)
     extends Variate (stream)
{
    if (df <= 0) flaw ("constructor", "parameter df must be positive")

    private val gamma  = Gamma (df/2, 2.0, stream)
    private val normal = Normal (0.0, 1.0, stream)
    private val k      = df/2.0

    val mean = df.toDouble

    def pf (z: Double): Double = 0.5~^k * z~^(k-1) * nexp (z/2.0) / gammaF (k)

    def gen: Double = gamma.gen + (if (df % 2 == 0) 0.0 else normal.gen~^2.0)

} // ChiSquare class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Dice` random variates for a given distribution specified
 *  using a cumulative distribution function (cdf). This discrete RV models the
 *  roll of dice numbered 0, 1, ..., n-1.  Add 1 for 1 to n.
 *  @param cdf     the distribution function (cdf)
 *  @param stream  the random number stream
 */
case class Dice (cdf: Array [Double] = Array (.1, .3, .5, .7, .9, 1.0), stream: Int = 0)
     extends Variate (stream)
{
    _discrete = true
    private val n = cdf.length

    val mean = { var sum = 0.0; for (i <- 1 until n) sum += i * (cdf(i) - cdf(i-1)); sum }

    def pf (z: Double): Double =
    {
        val j = floor (z).toInt
        if (j == z) if (j == 0) cdf(0) else cdf(j) - cdf(j-1) else 0.0
    } // pf

    def gen: Double =
    {
        val y = r.gen
        for (i <- 0 until n if y <= cdf(i)) return i.toDouble
        n.toDouble - 1.0
    } // gen

} // Dice class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates generalized `Discrete` random variates for a given
 *  distribution specified using either a probability mass function (pmf)
 *  or a cumulative distribution function (cdf).
 *  This discrete RV models arbitrary experiments with discrete outcomes.
 *  @param dist        the distribution function (pdf or cdf)
 *  @param x           the x-coordinate values (mass points)
 *  @param cumulative  whether 'dist' is cumulative (cdf) or not (pmf)
 *  @param stream      the random number stream
 */
case class Discrete (dist: VectoD = VectorD (.2, .2, .2, .2, .2), x: VectorD = null,
                     cumulative: Boolean = false, stream: Int = 0)
     extends Variate (stream)
{
    private val cdf = if (cumulative) dist else dist.cumulate
    private val n   = dist.dim
    private val xx = if (x == null || x.dim == 0) VectorD.range (0, dist.dim) else x

    if (xx.dim != dist.dim) flaw ("Discrete", "dist and xx must have the same length")
    _discrete = true

    val mean = { var sum = xx(0) * cdf(0); for (i <- 1 until n) sum += xx(i) * (cdf(i) - cdf(i-1)); sum }

    def pf (z: Double): Double =
    {
        var j = -1
        for (i <- 0 until n if xx(i) == z) j = i
        if (j >= 0) if (j == 0) cdf(0) else cdf(j) - cdf(j-1) else 0 
    } // pf

    def gen: Double =
    {
        val y = r.gen
        for (i <- 0 until n if y <= cdf(i)) return xx(i)
        xx(cdf.dim - 1)
    } // gen

} // Discrete class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DiscreteF` class generates generalized `Discrete` random variates for a
 *  distribution specified via an array of functions (FunctionS2S).  At a
 *  particular time 't', the functions must some to one.
 *  This discrete RV models time-varying experiments with discrete outcomes.
 *  @param f       the array of time-based functions 
 *  @param stream  the random number stream
 */
case class DiscreteF (f: Array [FunctionS2S] = Array ((x: Double) => x), stream: Int = 0)
     extends Variate (stream)
{
    _discrete = true

    val n = f.length                        // the number of functions
    val x = VectorD.range (0, n)            // outcomes range 0 until n
    var t = 0.0                             // time

    val mean = sum (x) / n.toDouble

    def setTime (tt: Double) { t = tt }

    def pf (z: Double): Double = f(floor (z).toInt)(t)
   
    def gen: Double =
    {
        val y = r.gen
        var sum = 0.0
        for (i <- 0 until n) {
            sum += f(i)(t)
            if (y <= sum) return i.toDouble
        } // for
        n
    } // gen

    private def sum (w: VectorD): Double =
    {
       var s = 0.0
       for (i <- 0 until n) s += f(i)(t) * w(i)
       s 
    } // sum

} // DiscreteF class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Erlang` random variates.
 *  This continuous RV models the time until k stages complete.
 *  @see http://www.math.uah.edu/stat/poisson/Gamma.html
 *  @param mu      the mean of exponential samples (Erlang mean = mu * k)
 *  @param k       the number of stages (or Exponential samples)
 *  @param stream  the random number stream
 */
case class Erlang (mu: Double = 1.0, k: Int = 2, stream: Int = 0)
     extends Variate (stream)
{
    if (mu <= 0.0 || k <= 0) flaw ("constructor", "parameters mu and k must be positive")

    private val l = 1.0 / mu   // lambda

    val mean = mu * k

    def pf (z: Double): Double = l~^k * z~^(k-1) * nexp (l*z) / fac (k-1)

    def gen: Double =
    {
        var prod = 1.0
        for (i <- 0 until k) prod *= r.gen
        -mu * log (prod)
    } // gen

} // Erlang class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Exponential` random variates.
 *  This continuous RV models the time until an event occurs.
 *  @see www.math.uah.edu/stat/poisson/Exponential.html
 *  @param mu      the mean
 *  @param stream  the random number stream
 */
case class Exponential (mu: Double = 1.0, stream: Int = 0)
     extends Variate (stream)
{
    if (mu <= 0.0) flaw ("constructor", "parameter mu must be positive")

    private val λ = 1.0 / mu          // lambda, the rate parameter

    val mean = mu

    def pf (z: Double): Double = if (z >= 0) λ * nexp (λ*z) else 0.0

    def gen: Double = -mu * log (r.gen)

} // Exponential class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Fisher` (F-Distribution) random variates.
 *  This continuous RV models the ratio of variances.
 *  @see http://www.math.uah.edu/stat/special/Fisher.html
 *  @param df1     the degrees of freedom for numerator Chi-Square
 *  @param df2     the degrees of freedom for denominator Chi-Square
 *  @param stream  the random number stream
 */
case class Fisher (df1: Int = 6, df2: Int = 4, stream: Int = 0)
     extends Variate (stream)
{
    if (df1 <= 0 || df2 <= 2)
        flaw ("constructor", "parameters df1 and df2 must be at least 1 and 3, respectively")

    private val chi1 = ChiSquare (df1, stream)
    private val chi2 = ChiSquare (df2, stream)

    val mean = df2 / (df2-2.0)

    def pf (z: Double): Double =
    {
        if (z >= 0.0) sqrt ((df1*z)~^df1 * df2.toDouble~^df2 / (df1*z + df2)~^(df1+df2)) /
                           (z * betaF (df1/2.0, df2/2.0))
        else          0.0
    } // pf

    def gen: Double =
    {
        val c1 = chi1.gen
        val c2 = chi2.gen
        (df2 * c1) / (df1 * c2)
    } // gen

} // Fisher class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Gamma` random variates.
 *  This continuous RV models the time until an event occurs.
 *  Note: variance = alpha * beta ^ 2.
 *  @see http://www.math.uah.edu/stat/poisson/Gamma.html
 *  @param alpha   the shape parameter
 *  @param beta    the scale parameter
 *  @param stream  the random number stream
 */
case class Gamma (alpha: Double = 1.0, beta: Double = 1.0, stream: Int = 0)
     extends Variate (stream)
{
    if (alpha <= 0.0 || beta <= 0.0) flaw ("constructor", "parameters alpha and beta must be positive")

    private val a    = floor (alpha).toInt          // integral part
    private val b    = alpha - a                    // fractional part  
    private val erl1 = Erlang (beta, a, stream)
    private val erl2 = Erlang (beta, a + 1, stream)

    val mean = alpha * beta
    
    def pf (z: Double): Double =
    {
        if (z > 0.0) beta~^(-alpha) * z~^(alpha-1.0) * nexp (z/beta) / gammaF (alpha)
        else         0.0
    } // pf

    def gen: Double =
    {
        var x = 0.0
        var y = 0.0
        if (alpha < 1.0) {                                // 0 < alpha < 1
            do {
                x = r.gen~^(1.0/alpha)
                y = r.gen~^(1.0/(1.0-alpha))
            } while (x + y > 1)
            return (x / (x + y)) * (-log (r.gen)) * beta
        } else if (alpha < 5.0) {                         // 1 <= alpha < 5
            do {
                x = alpha / a
                var prod = 1.0
                for (i <- 0 until a) prod *= r.gen
                x *= -log (prod)
            } while (r.gen > (x / alpha)~^b * nexp (b * x / (alpha-1.0)))
            return x * beta
        } else {                                          // alpha >= 5
            if (r.gen >= b) erl1.gen else erl2.gen
        } // if
    } // gen

} // Gamma class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Geometric` random variates.
 *  This discrete RV models the number of failures before the first success.
 *  @see http://www.math.uah.edu/stat/bernoulli/Geometric.html
 *  @param p       the probability of success
 *  @param stream  the random number stream
 */
case class Geometric (p: Double = .5, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0 || p > 1) flaw ("constructor", "parameter p must be in [0, 1]")
    _discrete = true

    private val q      = 1.0 - p
    private val log_q  = log (q)
    private val q_by_p = q / p

    val mean = q_by_p

    def pf (z: Double): Double = 
    {
        val k = floor (z).toInt
        if (z == k && k >= 0) p * q~^k else 0.0
    } // pf

    def gen: Double = floor (log (r.gen) / log_q).toInt

} // Geometric class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `HyperExponential` random variates (two rates).
 *  This continuous RV models the time until an event occurs (higher coefficient
 *  of variation than exponential distribution).  
 *  @param p       the probability of first vs. second rates
 *  @param mu1     the first mean (1 / lambda1)
 *  @param mu2     the second mean (1 / lambda2)
 *  @param stream  the random number stream
 */
case class HyperExponential (p: Double = .5, mu1: Double = 1, mu2: Double = 2, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0.0 || p > 1.0)       flaw ("constructor", "parameter p must be in [0, 1]")
    if (mu1 <= 0.0 || mu2 <= 0.0) flaw ("constructor", "parameters mu1 and mu2 must be positive")

    private val q  = 1.0 - p
    private val l1 = 1.0 / mu1      // lambda 1
    private val l2 = 1.0 / mu2      // lambda 2

    val mean = p * mu1 + q * mu2

    def pf (z: Double): Double = if (z >= 0.0) p * l1 * nexp (l1*z) + q * l2 * nexp (l2*z) else 0.0

    def gen: Double = log (r.gen) * (if (r.gen < p) -mu1 else -mu2)

} // HyperExponential class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `HyperExponential` random variates.
 *  This continuous RV models the time until an event occurs (higher coefficient
 *  of variation than exponential distribution). FIX
 *  @param mu      the mean
 *  @param sigma   the standard deviation
 *  @param stream  the random number stream
 */
case class _HyperExponential (mu: Double = 1.0, sigma: Double = 2, stream: Int = 0)
     extends Variate (stream)
{
    if (mu <= 0.0 || sigma <= 0.0) flaw ("constructor", "parameters mu and sigma must be positive")

    private val cv2 = (sigma / mu)~^2.0
    private val p   = .5 * (1.0 - sqrt ((cv2-1.0) / (cv2+1.0)))

    private val l = 2.0 * p / mu   // adjusted lambda

    val mean = mu

    def pf (z: Double): Double = if (z >= 0.0) l * nexp (l*z) else 0.0

    def gen: Double =
    {
        val z = if (r.gen > p) mu / (1 - p) else mu / p
        -0.5 * z * log (r.gen)
    } // gen

} // _HyperExponential class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `HyperGeometric` random variates.
 *  This discrete RV models the number of successes in n draws from a finite population.
 *  @see http://www.math.uah.edu/stat/urn/Hypergeometric.html
 *  @param p       the probability of success (red balls)
 *  @param n       the number of draws (balls drawn)
 *  @param pop     the size of the finite population (total number of balls)
 *  @param stream  the random number stream
 */
case class HyperGeometric (p: Double = .5, n: Int = 5, pop: Int = 10, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0.0 || p > 1.0) flaw ("constructor", "parameter p must be in [0, 1]")
    if (n <= 0 || pop <= 0) flaw ("constructor", "parameters n and pop must be positive")
    _discrete = true

    private val reds: Int = floor (p * pop).toInt    // number of red balls

    val mean = n * reds / pop.toDouble

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (k == z && 0 <= k && k <= reds && k <= n)
            choose (reds, k) * choose (pop-reds, n-k) / choose (pop, n).toDouble
        else
            0.0
    } // pf

    def gen: Double =
    {
        var b: Double = pop     // population of number of balls
        var rd = reds           // number of red/success balls in population
        var s = 0               // count number of successes
        for (i <- 0 until n) {
            if (r.gen <= rd / b) { s += 1; rd -= 1 }
            b -= 1
        } // for
        s
    } // gen

} // HyperGeometric class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Logistic` random variates.
 *  This continuous RV models logistically distributed data (stretched Normal).
 *  @see http://www.math.uah.edu/stat/special/Logistic.html
 *  @param a       the location parameter
 *  @param b       the scale parameter
 *  @param stream  the random number stream
 */
case class Logistic (a: Double = 0.0, b: Double = 1.0, stream: Int = 0)
     extends Variate (stream)
{
    val mean = a

    def pf (z: Double): Double =
    {
        val y = exp ((z - a) / b)
        y / (b * (1.0 + y)~^2.0)
    } // pf

    def gen: Double =
    { 
        val y = r.gen
        a + b * log (y / (1.0 - y))
    } // gen

} // Logistic class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `LogNormal` random variates.
 *  This continuous RV models data that is normally distributed after a log transformation.
 *  @see http://www.math.uah.edu/stat/special/LogNormal.html
 *  @param mu      the mean for underlying Normal
 *  @param sigma2  the variance (sigma squared) for underlying Normal
 *  @param stream  the random number stream
 */
case class LogNormal (mu: Double = 0.0, sigma2: Double = 1.0, stream: Int = 0)
     extends Variate (stream)
{
    if (sigma2 <= 0.0) flaw ("constructor", "parameter sigma2 must be positive")

    private val _2sigma2 = 2.0 * sigma2
    private val normal   = Normal (mu, sigma2, stream)   // associated Normal distribution

    val mean = exp (mu + sigma2/2.0)

    def pf (z: Double): Double =
    {
        val denom = z * sqrt (Pi * _2sigma2)
        if (z > 0.0) nexp ( (log (z) - mu)~^2.0 / _2sigma2) / denom else 0.0
    } // pf

    def gen: Double = exp (normal.gen)

} // LogNormal class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `NegativeBinomial` random variates.
 *  This discrete RV models the number of failures before s-th success.
 *  @see http://www.math.uah.edu/stat/bernoulli/NegativeBinomial.html
 *  @param p       the probability of success
 *  @param s       the number of successes
 *  @param stream  the random number stream
 */
case class NegativeBinomial (p: Double = .5, s: Int = 2, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0.0 || p > 1.0) flaw ("constructor", "parameter p must be in [0, 1]")
    if (s <= 0)             flaw ("constructor", "parameter s must be positive")
    _discrete = true

    private val q    = 1.0 - p
    private val geom = Geometric (p, stream)

    val mean = s * q / p

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (k == z && k >= 0) choose (s+k-1, k) * p~^s * q~^k else 0.0
    } // pf

    def gen: Double = 
    {
        var sum = 0
        for (i <- 0 until s) sum += geom.gen.toInt
        sum
    } // gen

} // NegativeBinomial class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Normal` (Gaussian) random variates.
 *  This continuous RV models normally distributed data (bell curve).
 *  When summed, most distributions tend to Normal (Central Limit Theorem).
 *  @see http://www.math.uah.edu/stat/special/Normal.html
 *  @param mu      the mean
 *  @param sigma2  the variance (sigma squared)
 *  @param stream  the random number stream
 */
case class Normal (mu: Double = 0.0, sigma2: Double = 1.0, stream: Int = 0)
     extends Variate (stream)
{
    if (sigma2 < 0.0) flaw ("constructor", "parameter sigma2 must be nonnegative")

    private val sigma    = sqrt (sigma2)
    private val _2sigma2 = 2.0 * sigma2
    private var computed = true        // toggle, since values gen in pairs
    private var save     = 0.0         // save second in pair

    val mean = mu

    def pf (z: Double): Double = (1.0 / sqrt (Pi*_2sigma2)) * nexp ( (z-mu)~^2.0 / _2sigma2)

    def gen: Double =             // use acceptance-rejection method
    {
        var (a, b, w, t) = (0.0, 0.0, 0.0, 0.0)

        computed = ! computed;
        if (computed) return save * sigma + mu
        do {
            a = 2.0 * r.gen - 1.0
            b = 2.0 * r.gen - 1.0
            w = a*a + b*b
        } while (w > 1.0)
        t    = sqrt (-2.0 * log (w) / w)
        save = b * t
        (a * t) * sigma + mu
    } // gen

    def gen2: Double = Quantile.normalInv (r.gen)     // use inverse transform method

} // Normal class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Pareto` random variates.
 *  This continuous RV models Pareto distributed data.
 *  @see http://www.math.uah.edu/stat/special/Pareto.html
 *  @param a       the shape parameter
 *  @param b       the scale parameter
 *  @param stream  the random number stream
 */
case class Pareto (a: Double = 1.0, b: Double = 0.0, stream: Int = 0)
     extends Variate (stream)
{
    if (a <= 0.0 || b < 0.0) flaw ("constructor", "parameters must be positive, nonegative")

    val mean = if (a > 1.0) a * b / (a-1.0) else Double.PositiveInfinity

    def pf (z: Double): Double = if (z >= b) a * b~^a / z~^(a+1.0) else 0.0

    def gen: Double = b / (1.0 - r.gen)~^(1.0/a)

} // Pareto class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Poisson` random variates (discrete).
 *  This discrete RV models the number of events in a time interval of unit length.
 *  @see http://www.math.uah.edu/stat/poisson/Poisson.html
 *  @param mu      the mean
 *  @param stream  the random number stream
 */
case class Poisson (mu: Double = 2.0, stream: Int = 0)
     extends Variate (stream)
{
    if (mu <= 0.0) flaw ("constructor", "parameter mu must be positive")
    _discrete = true

    private val cutoff = nexp (mu)

    val mean = mu

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (k == z && k >= 0) nexp (mu) * mu~^k / fac (k) else 0.0
    } // pf

    def gen: Double = 
    {
        var n = -1
        var prod = 1.0
        do { prod *= r.gen; n += 1 } while (prod >= cutoff)
        n
    } // gen

} // Poisson class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `PowerLaw` random variates:  'cx^-y' for 'x in [a, b]'.
 *  This continuous RV models power-law distributions.
 *  @see http://mathworld.wolfram.com/RandomNumber.html
 *  @param a       the minimum value parameter
 *  @param b       the maximum value parameter
 *  @param y       the power parameter to be used
 *  @param stream  the random number stream
 */
case class PowerLaw (a: Double = 1.0, b: Double = 10.0, y: Double = 2.1, stream: Int = 0)
     extends Variate (stream)
{
    if (a >= b)  flaw ("constructor", "requires parameter a < b")
    if (y < 1.0) flaw ("constructor", "parameter y must be >= 1.0")

    private val exp  = 1.0 - y                            // exponent
    private val a_up = a~^exp
    private val b_up = b~^exp
    private val diff = b~^exp - a_up
    private val root = 1.0 / exp.toDouble
    private val c    = exp / diff                         // coefficient

    val mean = c * (b*b_up - a*a_up) / (exp + 1.0)

    def pf (z: Double): Double = if (z >= a && z <= b) c * z~^(-y) else 0.0

    def gen: Double = (diff * r.gen  + a_up) ~^ root

} // PowerLaw class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Randi` random variates (random integers: a, ..., b).
 *  This discrete RV models equi-probable integral outcomes.
 *  @see http://www.math.uah.edu/stat/special/UniformDiscrete.html
 *  @param a       the lower bound (inclusive)
 *  @param b       the upper bound (inclusive)
 *  @param stream  the random number stream
 */
case class Randi (a: Int = 0, b: Int = 5, stream: Int = 0)
     extends Variate (stream)
{
    if (a > b) flaw ("constructor", "parameter a must not be greater than b")
    _discrete = true

    private val width = b + 1 - a

    val mean = (a + b) / 2.0

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (k == z && a <= k && k <= b) 1.0 / width.toDouble else 0.0
    } // pf

    def gen: Double = floor (a + width * r.gen).toInt

} // Randi class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Randi0` random variates (random integers: 0, ..., b).
 *  This discrete RV models equi-probable integral outcomes starting with 0.
 *  @param b       the upper bound (>= 0) (inclusive)
 *  @param stream  the random number stream
 */
case class Randi0 (b: Int = 5, stream: Int = 0)
     extends Variate (stream)
{
    if (b < 0) flaw ("constructor", "parameter b must be non-negative")
    _discrete = true

    private val width = b + 1

    val mean = b / 2.0

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (k == z && k <= b) 1.0 / width.toDouble else 0.0
    } // pf

    def gen: Double = floor (width * r.gen).toInt

    def iigen (bb: Int): Int = floor ((bb + 1) * r.gen).toInt

} // Randi0


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Randi0` random variates (random integers: 0, ..., b).
 *  This discrete RV models equi-probable integral outcomes starting with 0.
 *  The 'iigen' methods will produce unique random integers.
 *  @param b       the upper bound (>= 0) (inclusive)
 *  @param stream  the random number stream
 */
case class RandiU0 (b: Int = 5, stream: Int = 0)
     extends Variate (stream)
{
    if (b < 0) flaw ("constructor", "parameter b must be non-negative")
    _discrete = true

    val previous = collection.mutable.Set [Int] ()

    private val width = b + 1

    val mean = b / 2.0

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt
        if (k == z && k <= b) 1.0 / width.toDouble else 0.0
    } // pf

    def gen: Double = floor (width * r.gen).toInt

    def iigen (bb: Int): Int = 
    {
        if (previous.size == bb) {
            flaw ("iigen", "all unique values have been exhausted - starting over")
            previous.clear ()
        } // if
        var i = -1
        do i  = floor ((bb + 1) * r.gen).toInt while (previous contains i)
        previous += i
        i
    } // iigen

} // RandiU0

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Sharp` (Deterministic) random variates.
 *  This discrete RV models the case when the variance is 0.
 *  @param x       the value for this constant distribution
 *  @param stream  the random number stream
 */
case class Sharp (x: Double = 1, stream: Int = 0)
     extends Variate (stream)
{
    _discrete = true

    val mean = x

    def pf (z: Double): Double = if (approx (z, x)) 1.0 else 0.0

    def gen: Double = x

} // Sharp class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `StudentT` random variates.
 *  This continuous RV models cases where data are normally distributed, but
 *  variability increases since the variance is unknown.
 *  @see http://www.math.uah.edu/stat/special/Student.html
 *  @param df      the degrees of freedom
 *  @param stream  the random number stream
 */
case class StudentT (df: Int = 4, stream: Int = 0)
     extends Variate (stream)
{
    if (df <= 0) flaw ("constructor", "parameter df must be positive")

    private val _df    = (df + 1) / 2.0
    private val normal = Normal (0.0, 1.0, stream)
    private val chi    = ChiSquare (df, stream)

    val mean = 0.0

    def pf (z: Double): Double =
    {
        gammaF (_df) * (1.0 + (z*z)/df)~^(-_df) / (sqrt (df*Pi) * gammaF (df/2.0))
    } // pf

    def gen: Double = normal.gen / sqrt (chi.gen / df) 

} // StudentT case


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Trapezoidal` random variates.
 *  This continuous RV models cases where outcomes cluster between two modes.
 *  Both `Uniform and `Triangular` are special cases of `Trapezoidal`.
 *  @see http://iopscience.iop.org/0026-1394/44/2/003/pdf/0026-1394_44_2_003.pdf
 *  @param a       the minimum
 *  @param c       the first mode
 *  @param d       the second mode
 *  @param b       the maximum
 *  @param stream  the random number stream
 */
case class Trapezoidal (a: Double = 0.0, c: Double = 2.0, d: Double = 7.0, b: Double = 10.0,
                        stream: Int = 0)
     extends Variate (stream)
{
    private val l  = b + d - a - c                     // length factor
    private val h  = 2.0 / l                           // maximum height
    private val lf = c - a                             // triangle on left
    private val rt = b - d                             // triangle on right

    val mean = (b~^2 - a~^2 + d~^2 - c~^2 - a*c + b*d) / (3.0 * l)

    def pf (z: Double): Double =
    {
        if (a <= z && z <= c) h * (z - a) / lf 
        else if (z <= d)      h
        else if (z <= b)      h * (b - z) / rt
        else                  0.0
    } // pf

    def gen: Double = 
    {
        val y   = r.gen
        val h_2 = h / 2.0
        if      (y <= h_2 * lf)       a + sqrt (2.0 * lf * y / h)
        else if (y <= 1.0 - h_2 * rt) (a + c) / 2.0 + y / h
        else                          b - sqrt (2.0 * rt * (1.0 - y) / h)
    } // gen

} // Trapezoidal class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates simple `Triangular` random variates with the mode in the middle.
 *  This continuous RV models cases where outcomes cluster around the mode.
 *  @see http://www.math.uah.edu/stat/special/Triangle.html
 *  @param a       the lower bound
 *  @param b       the upper bound
 *  @param c       the mode
 *  @param stream  the random number stream
 */
case class Triangular (a: Double = 0, b: Double = 5, c: Double = Double.MaxValue, stream: Int = 0)
     extends Variate (stream)
{
    if (a > b) flaw ("constructor", "parameter a must not be greater than b")

    private val width = b - a
    private val mode  = if (c < Double.MaxValue) c else (a + b) / 2.0
    private val left  = mode - a
    private val right = b - mode

    val mean = (a + mode + b) / 3.0

    def pf (z: Double): Double =
    {
        if (a <= z && z <= mode) 2 * (z - a) / (width * left)
        else if (z <= b)         2 * (b - z) / (width * right)
        else                     0
    } // pf

    def gen: Double = 
    {
        val y = r.gen    
        if (y <= left / width) a + sqrt (left * width * y)
        else                   b - sqrt (right * width * (1.0 - y))
    } // gen

} // Triangular class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Trinomial` random variates.  While Binomial is based on
 *  trials with two outcomes, success (1) or failure (0).  Trinomial is based on
 *  trials with three outcomes, high (2), medium (1) or low (0).
 *  This discrete RV models the result of 'n' trials.
 *  @see https://onlinecourses.science.psu.edu/stat414/node/106
 *  @param p       the probability of high (2)
 *  @param q       the probability of medium (1)
 *  @param n       the number of independent trials
 *  @param stream  the random number stream
 */
case class Trinomial (p: Double = 1.0/3.0, q: Double = 1.0/3.0, n: Int = 10, stream: Int = 0)
     extends Variate (stream)
{
    if (p < 0.0)   flaw ("constructor", "parameter p must be non-negative")
    if (q < 0.0)   flaw ("constructor", "parameter q must be non-negative")
    if (p+q > 1.0) flaw ("constructor", "p+q must not exceed one")
    if (n <= 0)    flaw ("constructor", "parameter n must be positive")
    _discrete = true

    private val qq   = 1.0 - p - q             // the probability of low (0)
    private val p_qq = p / qq                 // the ratio of high to low
    private val q_qq = q / qq                 // the ratio of medium to low
    private val dice = Dice (Array (qq, qq+q, 1.0), stream)

    val mean = (q + 2.0*p) * n

    def pf (z: Double): Double =
    {
        val k = floor (z).toInt   // k and l in lock step (one dimensional)
        val l = floor (z).toInt
        if (z == k && 0 <= k && k+l <= n)
            choose (n, k, l) * p~^k * q~^l * qq~^(n-k-l)
        else
            0.0
    } // pf

    def pf (y: Double, z: Double): Double =
    {
        val k = floor (y).toInt   // k and l independent (two dimensional)
        val l = floor (z).toInt
        if (y == k && z == l && 0 <= k && 0 <= l && k+l <= n)
            choose (n, k, l) * p~^k * q~^l * qq~^(n-k-l)
        else
            0.0
    } // pf

    def pf (k: Int, l: Int): Double =   // ex: n = 10, (k, l, m) = (2, 3, 5)
    {
        if (0 <= k && 0 <= l && k+l <= n)
            choose (n, k, l) * p~^k * q~^l * qq~^(n-k-l)
        else
            0.0
    } // pf

    override def pmf (k: Int): Array [Double] =
    {
//      val d = Array.ofDim [Array [Double]] (n+1)      // array to hold pmf distribution
//      d(0)  = Array (qq~^n)
//      for (k <- 1 to n) {
//          d(k) = Array.ofDim [Double] (k+1)
//          d(k)(0) = d(k-1)(0) * p_qq * (n-k+1) / k.toDouble
//          for (l <- 1 to k) d(k)(l) = d(k)(l-1) * q_qq * (k-l+1) / l.toDouble
//      } // for

	import scalation.math.Combinatorics._
        val d = Array.ofDim [Double] (n-k+1)      // array to hold pmf distribution
        d(0) = choose (n, k) * p~^k * qq~^n-k
        for (l <- 1 to n-k) d(l) = d(l-1) * q_qq * (k-l+1) / l.toDouble

        d
    } // pmf2

    def gen: Double =
    {
        var sum = 0.0
        for (i <- 0 until n) sum += dice.gen      // add 0, 1 or 2
        sum
    } // gen

} // Trinomial class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Uniform` random variates in the range (a, b).
 *  This continuous RV models equi-probable outcomes.
 *  @see http://www.math.uah.edu/stat/special/UniformContinuous.html
 *  @param a       the lower bound
 *  @param b       the upper bound
 *  @param stream  the random number stream
 */
case class Uniform (a: Double = 0.0, b: Double = 5.0, stream: Int = 0)
     extends Variate (stream)
{
    if (a > b) flaw ("constructor", "parameter a must not be greater than b")

    private val width = b - a

    val mean = (a + b) / 2.0

    def pf (z: Double): Double = if (a <= z && z <= b) 1.0 / width else 0.0

    def gen: Double = a + width * r.gen

} // Uniform class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates `Weibull` random variates.
 *  This continuous RV models the time for an event to occur.
 *  @see http://www.math.uah.edu/stat/special/Weibull.html
 *  @param alpha   the shape parameter
 *  @param beta    the scale parameter
 *  @param stream  the random number stream
 */
case class Weibull (alpha: Double = 2.0, beta: Double = 2.0, stream: Int = 0)
     extends Variate (stream)
{
    if (alpha <= 0.0 || beta <= 0.0) flaw ("constructor", "parameters alpha and beta must be positive")

    private val shape_recip = 1.0 / alpha

    val mean = beta * shape_recip * gammaF (shape_recip)

    def pf (z: Double): Double =
    {
        if (z > 0.0) alpha * beta~^(-alpha) * z~^(alpha-1.0) * nexp ( (z/beta)~^alpha)
        else         0.0
    } // pf

    def gen: Double = -beta * log (r.gen)~^shape_recip

} // Weibull


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateTest` object conducts two simple tests of the Random Variate
 *  Generators: (1) Means Test and (2) Chi-square Goodness of Fit Test.
 *  FIX: need to add (3) Variance Test and (4) K-S Goodness of Fit Test.
 */
object VariateTest extends App
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a Means Test (average of generated rv's close to mean for distribution).
     *  @param rv  the random variate to test
     */
    def meansTest (rv: Variate)
    {
        println ("\nTest the `" + rv.getClass.getSimpleName () + "` random variate generator")

        var ran = 0.0
        var sum = 0.0
        val rep = 100000
        print ("rv.gen  = {")
        for (i <- 1 to rep) {
            ran = rv.gen
            if (i <= 10) print (" " + ran)
            sum += ran
        } // for
        println (" ... }")
        println ("rv.mean = " + rv.mean + " estimate = " + sum / rep.toDouble)
    } // meansTest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a Chi-square Goodness of Fit Test.  Compare the random variable's
     *  histogram (generated by repeatedly calling 'gen') to the probability
     *  function pf (either pmf or pdf).
     *  @param rv  the random variate to test
     */
    def distrTest (rv: Variate)
    {
        println ("\nTest the " + rv.getClass.getSimpleName () + " random variate generator")

        val rep    = 50000          // replications
        var j      = 0              // interval number
        var x      = 0.0            // x coordinate
        var o      = 0.0            // observed value: height of histogram
        var e      = 0.0            // expected value: pf (x)
        var chi2   = 0.0            // ChiSquare statistic
        var n      = 0              // number of nonzero intervals
        val offset = if (rv.isInstanceOf [StudentT] || rv.isInstanceOf [Normal]) 25 else 0
        val sum    = new Array [Int] (51)

        for (i <- 1 to rep) {
            j = floor (rv.gen * 10.0).toInt + offset
            if (0 <= j && j <= 50) sum (j) += 1
        } // for

        for (i <- 0 until sum.length) {
            x = (i - offset) / 10.0
            o = sum(i)
            e = round (if (rv.discrete) 50000 * rv.pf (x) else 5000 * rv.pf (x + .05) )
            if (e >= 5) {
                chi2 += (o-e)~^2.0 / e
                n += 1
            } // if
            print ("\tsum (" + x + ") = " + o + " : " + e + " ")
            if (i % 5 == 4) println ()
        } // for
        n -= 1
        if (n < 2)  n = 2
        if (n > 49) n = 49 
        println ("\nchi2 = " + chi2 + " : chi2(0.95, " + n + ") = " + Quantile.chiSquareInv (0.95, n))
    } // distrTest

    val distribution = Array (Bernoulli (),
                              Beta (),
                              Binomial (),
                              Cauchy (),
                              ChiSquare (),
                              Dice (),
                              Discrete (),
                              DiscreteF (),
                              Erlang (),
                              Exponential (),
                              Fisher (),
                              Gamma (),
                              Geometric (),
                              HyperExponential (),
                              HyperGeometric (),
                              LogNormal (),
                              Logistic (),
                              NegativeBinomial (),
                              Normal (),
                              Pareto (),
                              Poisson (),
                              PowerLaw (),
                              Randi (),
                              Randi0 (),
                              RandiU0 (),
                              Sharp (),
                              StudentT (),
                              Trapezoidal (),
                              Triangular (),
                              Trinomial (),
                              Uniform (),
                              Weibull ())

    val testAll = false                       // test all distributions, else only those in 'include' set
    val include = Set (7, 11)                 // adjust as needed
    val nTests  = if (testAll) distribution.length else include.size
    println ("Testing " + nTests + " Random Variate Generators")

    for (i <- distribution.indices) {
        if (testAll || (include contains i)) {
            meansTest (distribution (i))
            distrTest (distribution (i))
        } // if
    } // for

    val trinom = Trinomial (1.0/3.0, 1.0/3.0, 9)
    println ("trinom = " + trinom.pmf(0).deep)

} // VariateTest object

