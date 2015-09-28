
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.2
 *  @date    Sat Mar 15 15:28:44 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  Many of the algorithms used are from:
 *    Averill M. Law and W. David Kelton
 *    Simulation Modeling and Analysis, 2nd Edition
 *    McGraw-Hill, Inc., NY, 1991.
 */

package scalation.random

import math.{ceil, E, exp, floor, log, Pi, pow, round, sqrt}

import scalation.linalgebra.VectorD
import scalation.math._
import scalation.math.Combinatorics.{fac, gammaF, logfac}
import scalation.math.ExtremeD.TOL

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TimeVariate` abstract class serves as a superclass for time-based
 *  random variates such Poisson Processes.
 *  @param stream  the random number stream
 */
abstract class TimeVariate (stream: Int)
         extends Variate (stream)
{
              val mean  = -1.0       // mean changes with time, use meanF function instead
    protected val MAXFAC = 170       // maximum factorial for Double data type

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean as a function of time.
     *  @param tt  the time point for computing the mean
     */
    def meanF (tt: Double): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability function (pf):
     *  The probability density function (pdf) for continuous RV's or
     *  the probability mass function (pmf) for discrete RV's.
     *  @param z  the mass point whose probability is sought
     */
    def pf (z: Double): Double = pf (floor (z).toInt)

    def pf (z: Int): Double

    def pf (z: Int, tt: Double): Double

    def pf (z: Int, aa: Double, bb: Double): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Comute the mean as a function of time.
     *  @param tt  the time point for computing the mean
     */
    def count (tt: Double): Int =
    {
        var i = 0
        while (gen < tt) i += 1
        i
    } // count

    def count (a: Double, b: Double): Int =
    {
        var i = 0
        var done = false
        while (!done) {
            val c = gen
            if ((c >= a) && (c < b)) i += 1
            if (c >= b) done = true
        } // while
        i
    } // count

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the time-based process to the beginning.
     */
    def reset ()

} // TimeVariate class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates arrival times according to a `PoissonProcess`.
 *  Given the current arrival time 't', generate the next arrival time.
 *  @see  http://en.wikipedia.org/wiki/Poisson_process
 *  @param lambda  the arrival rate (arrivals per unit time)
 *  @param stream  the random number stream
 */
case class PoissonProcess (lambda: Double, stream: Int = 0)
     extends TimeVariate (stream)
{
    if (lambda <= 0.0) flaw ("constructor", "parameter lambda must be positive")

    private val e_rv  = Exponential (1.0 / lambda, stream)    // exponential rv generator
    private val _1_30 = 1.0 / 30.0                            // one thirtith
    private val _1_6  = 1.0 / 6.0                             // one sixth
    private var t     = 0.0                                   // current time
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean number of arrivals for amount of time tt.
     *  @param tt  a number of intervals
     */
    def meanF (tt: Double): Double = lambda * tt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability P[ N(t) = k ] using a general factorial function
     *  implemented with the Gamma function and Ramanujan's Approximation.
     *  @see  http://en.wikipedia.org/wiki/Poisson_process
     *  @param k    the number of arrivals in the interval
     */
    def pf (k: Int): Double = (lambda * t)~^k * exp (-lambda * t) / fac (k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability P[ (N(t + tau) - N(t)) = k] using a general
     *  factorial function implemented with the Gamma function and Ramanujan's
     *  Approximation.  Switches to pf_ln for k >= 170 to handle large k-values.
     *  @param k    the number of arrivals in the interval
     *  @param tau  the length of the interval
     */
    def pf (k: Int, tau: Double): Double = 
    {
        if (k < MAXFAC) (lambda * tau)~^k * exp (-lambda * tau) / fac (k)
        else            pf_ln (k, tau)
    } // pf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability P[ (N(t + tau) - N(t)) = k] using the log of 
     *  Ramanujan's Approximation formula.
     *  @param k    the number of arrivals in the interval
     *  @param tau  the length of the interval
     */
    def pf_ln (k: Int, tau: Double): Double =
    {
        val n = k.toDouble
        exp (-lambda * tau + n + n * log (lambda * tau) - n * log (n) - 0.5 * log (Pi)
            - _1_6 * log (((8.0 * n + 4.0) * n + 1.0) * n + _1_30))
    } // pf_ln

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability P [ (N(b) - N(a)) = k ].
     *  @param k  the number of arrivals in the interval
     *  @param a  the left end of the interval
     *  @param b  the right end of the interval
     */
    def pf (k: Int, a: Double, b: Double): Double = pf (k, b - a)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate Poisson arrival times using and exponential random variable.
     */
    def gen: Double = { t += e_rv.gen; t }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the global time value to zero.
     */
    def reset () { t = 0.0 }

} // PoissonProcess class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class generates arrival times according to a `NHPoissonProces`, an
 *  Non-Homogeneous Process Process (NHPP), where the arrival rate function
 *  'lambda(t)' is piecewise constant.  Rates are constant over basic time
 *  intervals of length 'dt'.
 *  @see  http://en.wikipedia.org/wiki/Poisson_process#Non-homogeneous
 *  @param lambda  the vector of arrival rates
 *  @param dt      the length the basic time intervals
 *  @param stream  the random number stream
 */
case class NHPoissonProcess (lambda: VectorD, dt: Double = 1.0, stream: Int = 0)
     extends TimeVariate (stream)
{
    if (! lambda.isNonnegative) flaw ("constructor", "parameter vector lambda must be nonnegative")

    private val lsum    = lambda.cumulate * dt        // cumulative lambda
    private val e_rv    = Exponential (1.0, stream)   // exponential rv generator with mean 1
    private var e       = 0.0                         // cumulative exponential rv's
    private var tlast   = 0.0                         // previous arrival time
    private var t       = 0.0                         // current arrival time

    def meanF (tt: Double): Double = 
    {
        val i1 = floor (tt / dt).toInt
        val i2 = i1 + 1
        if (i2 >= lsum.dim) {
            flaw ("meanF", "i2 is beyond the end of lsum vector")
            return -1.0
        } // if
        val t1 = i1 * dt
        val t2 = t1 + dt
        val l1 = lsum(i1 - 1)
        val l2 = lsum(i2 - 1)
        (l2 - l1) * (tt - t1) / (t2 - t1) + l1                
    } // meanF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
    /** Compute the probability of k arrivals occurring in the time
     *  interval [0, t] where t is the current time.
     *  @param k  the number of arrivals in the time interval
     */
    def pf (k: Int): Double = pf (k, 0.0, t)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: 
    /** Compute the probability of k arrivals occurring in the time
     *  interval [0, tt].
     *  @param k   the number of arrivals in the time interval
     *  @param tt  the upper bound time value
     */
    def pf (k: Int, tt: Double): Double = pf (k, 0.0, tt)
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability P[ (N(b) - N(a)) = k ].
     *  @param k  the number of arrivals in interval [a,b]
     *  @param a  the left end of the interval
     *  @param b  the right end of the interval
     */
    def pf (k: Int, a: Double, b: Double): Double =
    {
        if (a < 0.0 || b > lambda.dim * dt) { flaw ("pf", "time bounds are outside total time interval"); -1.0 }
        else {
            val aRes = a % dt
            val bRes = b % dt
            val iLow  = ((a - aRes) / dt).toInt
            val iHigh = ((b - bRes) / dt).toInt 
            var i1    = iLow
            val i2    = iHigh - 1
            if (aRes > TOL) i1 = iLow + 1
            var sum   = 0.0
            if (aRes > TOL) sum += lambda(iLow) * (dt - aRes)
            for (i <- i1 to i2) sum += lambda (i) * dt
            if (bRes > TOL) sum += lambda (iHigh) * (bRes)
            if (k < MAXFAC) exp (-sum) * sum~^k / fac (k) 
            else {
                val s = -sum + k * log (sum) - logfac (k)
                exp (s)
            } // else
        } // else
    } // pf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute arrival times of the NHPP.
     */
    def genTime: Double =
    {
        tlast = t
        e += e_rv.gen                                      // add next exponential rv
        for (i <- 0 until lsum.dim if e <= lsum(i)) {      // find where lsum(i-1) < e <= lsum(i)
           val lsum_i_1 = if (i == 0) 0.0 else lsum(i-1)
           val d = e - lsum_i_1                            // distance past lsum(i-1)
           t = dt * (i + d / (lsum(i) - lsum_i_1))         // calculate the new arrival time
           return t
        } //
        flaw ("gen", "cumulative e value larger than last lsum")
        -1.0
    } // gen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute inter-arrival times of the NHPP. tlast is a global
     *  variable.
     */
    def gen: Double = genTime - tlast

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the NHPP by resetting e to zero.
     */
    def reset () { e = 0.0 }

} // NHPoissonProcess class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonProcessTest` object is used to test both the `PoissonProcess` and
 *  `NHPoissonProcess` classes.
 */
object PoissonProcessTest extends App
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a means test (average of generated rv's close to mean for distribution).
     *  @param rv  the random variate to test
     *  @param tt  the time point for the test
     */
    def meansTest (rv: TimeVariate, tt: Double)
    {
        println ("\nTest the " + rv.getClass.getSimpleName () + " random variate generator at " + tt)

        var ran = 0.0
        var sum = 0.0
        val rep = 10000
        for (i <- 1 to rep) {
            rv.reset ()
            sum += rv.count (tt)
        } // for
        println ("rv.mean = " + rv.meanF (tt) + " estimate = " + sum / rep.toDouble)
    } // meansTest

    def distrTest (rv: TimeVariate, a: Double, b: Double)
    {
        val tt   = b - a
        val name = rv.getClass.getSimpleName ()

        println ("\nTest the " + name + " random variate generator at " + tt)

        val rep  = 50000              // replications
        var j    = 0                  // interval number
        var x    = 0.0                // x coordinate
        var o    = 0.0                // observed value: height of histogram
        var e    = 0.0                // expected value: pf (x)
        var chi2 = 0.0                // ChiSquare statistic
        var n    = 0                  // number of nonzero intervals
        val sum  = new Array [Int] (51)

        for (i <- 1 to rep) {
            rv.reset ()
            if (name == "PoissonProcess") j = rv.count (tt)
            else j = rv.count (a, b)
            if (0 <= j && j <= 50) sum (j) += 1
        } // for

        for (i <- 0 until sum.length) {
            x = i / 10.0
            o = sum(i)
            rv.gen
            if (name == "PoissonProcess") e = round (rep * rv.pf (i, tt))
            else                          e = round (rep * rv.pf (i, a, b))
            if (e >= 5) {
                chi2 += pow (o - e, 2) / e
                n += 1
            } // if
            print ("\tsum (" + i + ") = " + o + " : " + e + " ")
            if (i % 5 == 4) println ()
        } // for
        n -= 1
        if (n < 2)  n = 2
        if (n > 49) n = 49 
        println ("\nchi2 = " + chi2 + " : chi2(0.95, " + n + ") = " + Quantile.chiSquareInv (0.95, n))
    } // distrTest

    val lambda = VectorD (2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
                          4, 4, 4, 4, 4, 4, 4, 4, 4, 4)

    val pp   = PoissonProcess (lambda.sum / lambda.dim.toDouble)
    val nhpp = NHPoissonProcess (lambda, 1.0, 1)

//  distrTest (pp, 0.0, 12.0)
//  distrTest (nhpp, args(0).toDouble, args(1).toDouble)

//  println ("nhpp.pf (17, " + args(0) + ", " + args(1) + ") = " + nhpp.pf (17, args (0).toDouble, args(1).toDouble))

    var v = 0.0
    while (v >= 0.0) { 
        v = nhpp.genTime
        println (v)
    } // while  

} // PoissonProcessTest

