
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.0
 *  @date    Sat Jan 12 20:12:12 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import math.{min, pow, sqrt}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.DoubleWithExp._
import scalation.plot.Plot
import scalation.random.Random
import scalation.stat.StatVector
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class provide basic time series analysis capabilities for
 *  Auto-Regressive (AR) and Moving Average (MA) models.
 *  In an ARMA(p, q) model, p and q refer to the order of the
 *  Auto-Regressive and Moving Average components of the model. 
 *  ARMA models are often used for forecasting.
 *  @param y  the input vector (time series data)
 *  @param t  the time vector
 */
class ARMA (y: VectorD, t: VectorD)
      extends Predictor with Error
{
    private val n = y.dim                   // size of the input vector
            val mu = y.sum / n.toDouble     // the mean
    private val x = y - mu                  // work with mean zero time series

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficients for a pth order Auto-Regressive AR(p) model.
     *  x_t = phi_0 * x_t-1 + ... + phi_p-1 * x_t-p + e_t
     *  FIX: use Durbin-Levinson Algorithm
     *  @param p  the order of the AR model
     */
    def est_ar (p: Int = 1): VectorD =
    {
        // Using the Burg Method from:
        // Bell, B.M. and Percival, D.B., "A two step Burg algorithm [spectral analysis]", 
        // Signal Processing, IEEE Transactions on , vol.39, no.1, pp.185-189, Jan 1991
        // doi: 10.1109/78.80803

        if (p < 1 || p % 2 != 0) flaw("est_ar", "the maximum order p must be a positive even integer")

        // the delta value
        val delta = 0.1

        // number of rows is n + 1 in order to make indexing match the paper
        val f = new MatrixD (n + 1, p)
        val b = new MatrixD (n + 1, p)

        // A1 (initialization)
        // Let f(t, 0) = b(t, 0) = x(t) for t <- 1 to n
        for (t <- 1 to n) {
            f (t, 0) = x (t)
            b (t, 0) = x (t)
        } // for

        // create and initialize phi. number of rows is n + 1 in order to make 
        // indexing match the paper. I think this turns into a Toeplitz matrix.
        val phi = new MatrixD (p + 1, p + 1)

        // A2 (main loop)
        // repeat A2.1 to A2.8 for k <- 2 to p
        for (k <- 2 to p) {

            // A2.1 (calculate coefficients for polynomials q and r)

            // calculate coefficients for polynomial q
            val q0 = (k + 1 to n) map (t => f(t, k - 2)~^2 + b(t - k, k - 2)~^2) sum
            val q1 = -2.0 * ((k + 1 to n) map (t => f(t, k - 2) * b(t - k + 1, k - 2) + f(t - 1, k - 2) * b(t - k, k - 2)) sum)
            val q2 = (k + 1 to n) map (t => f(t - 1, k - 2)~^2 + b(t - k + 1, k - 2)~^2) sum
            
            // define polynomial q
            def q (z: Double): Double = q0 + q1 * z + q2 * z * z

            // calculate coefficients for polynomial r
            val r0 = 2.0 * ((k + 1 to n) map (t => f(t, k - 2) * b(t - k, k - 2)) sum)
            val r1 = -2.0 * ((k + 1 to n) map (t => f(t, k - 2) * f(t - 1, k - 2) + b(t - k + 1, k - 2) * b(t - k, k - 2)) sum)
            val r2 = 2.0 * ((k + 1 to n) map (t => f(t - 1, k - 2) * b(t - k + 1, k - 2)) sum)
            
            // define polynomial r
            def r (z: Double): Double = r0 + r1 * z + r2 * z * z

            // A2.2 (calculate coefficients for polynomial in (9))
            // Conditions in the paper imply the following:
            // q' * q~^2 - 2 * q * r * r' + q' * r~^2 = 0

            // calculate coefficients for polynomial s
            val s_j = new VectorD (6)
            s_j(0) = q1 * (q0 * q0 + r0 * r0) - 2.0 * q0 * r1 * r0
            s_j(1) = 2.0 * q2 * (q0 * q0 + r0 * r0) + 2.0 * q0 * (q1 * q1 - r1 * r1) - 4.0 * q0 * r2 * r0
            s_j(2) = q1 * q1 * q1 - q1 * r1 * r1 + 6.0 * q0 * (q2 * q1 - r2 * r1) + 2.0 * r0 * (q2 * r1 - q1 * r2)
            s_j(3) = 4.0 * q1 * q1 * q2 - 4.0 * q1 * r2 * r1 + 4.0 * q0 * (q2 * q2 - r2 * r2)
            s_j(4) = q1 * (5.0 * q2 * q2 - 3.0 * r2 * r2) - 2.0 * q2 * r2 * r1
            s_j(5) = 2.0 * q2 * (q2 * q2 - r2 * r2)

            // define polynomial s
            def s (z: Double): Double = (0 to 6) map (j => s_j(j) * z~^j) sum

            // A2.3 (determine the set T_delta)
            // "Let T_delta be the set of real zeros of the polynomial s 
            //  together with the values -1 + delta  and 1 - delta."
            import scala.collection.mutable.Set
            val T_delta = Set(delta - 1.0, 1.0 - delta)

            if (s_j(5) != 0) { 
                
                // solve quintic
            
            } else if (s_j(4) != 0) { 
                
                // solve quartic
            
            } else if (s_j(3) != 0) { 
                
                // solve cubic

            } else if (s_j(2) != 0) { 
                
                // solve quadratic 
                val descriminant = s_j(1)~^2 - 4.0 * s_j(2) * s_j(0)
                if (descriminant == 0) {
                    T_delta add (-s_j(1) / (2 * s_j(2)))
                } else if (descriminant > 0) {
                    T_delta add ((-s_j(1) + sqrt(descriminant)) / (2 * s_j(2)))
                    T_delta add ((-s_j(1) - sqrt(descriminant)) / (2 * s_j(2)))
                } // if

            } else if (s_j(1) != 0) {

                // solve linear
                T_delta add (-s_j(0) / s_j(1))

            } else {

                // solve constant
                T_delta add (0.0)

            } // if

            // A2.4 (determine phi(k - 1, k - 1))

            val z = (T_delta map (z => ((q(z) - (r(z)~^2 / q(z)), z))) min)._2
            phi(k - 1, k - 1) = z

            // A2.5 (determine phi(k, k))
            phi(k, k) = r(phi(k - 1, k - 1)) / q(phi(k - 1, k - 1))

            // A2.6 (compute new estimated forward and backward prediction errors)
            for (t <- k + 1 to n) {
                f(t, k - 1)     = f(t, k - 2) - phi(k - 1, k - 1) * b(t - k + 1, k - 2)
                b(t - k, k - 1) = b(t - k, k - 2) - phi(k - 1, k - 1) * f(t - 1, k - 2)
                f(t, k)         = f(t, k - 1) - phi(k, k) * b(t - k, k - 1)
                b(t - k, k)     = b(t - k, k - 1) - phi(k, k) * f(t, k - 1) 
            } // for

            // A2.7 (Levinson-Durbin recursions for first step)
            for (j <- 1 to k - 2) phi(j, k - 1) = phi(j, k - 2) - phi(k - 1, k - 1) * phi(k - 1 - j, k - 2)

            // A2.8 (Levinson-Durbin recursions for second step)
            for (j <- 1 to k) phi(j, k) = phi(j, k - 1) - phi(k, k) * phi(k - j, k - 1)


        } // for

        /*
        if (p != 1) flaw ("est_ar", "estimation for p > 1 not yet implemented")

        val phi = new VectorD (p)      // AR(p) coeffiecients: phi_0, ..., phi_p-1
        var sum1 = 0.; var sum2 = 0.
        for (i <- 1 until n) {
            sum1 += x(i) * x(i-1)
            sum2 += x(i-1) * x(i-1)
        } // for
        phi(0) = sum1 / sum2
        */

        // FIX: remove the first entry?
        phi(0)
    } // est_ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a pth order Auto-Regressive (AR(p))
     *  model. 
     *  f_t = phi_0 * x_t-1 + ... + phi_p-1 * x_t-p + e_t
     *  @param phi  the estimated AR(p) coefficients
     */
    def ar (phi: VectorD): VectorD =
    {
        val p = phi.dim                    // order p for AR(p) model
        val f = new VectorD (n)            // forecasts for x
        for (t <- p until n) {             // start at t = p (enough data)
            var sum = 0.
            for (j <- 0 until p) sum += phi(j) * x(t-1-j)
            f(t) = sum
        } // for
        f                                  // return the vector of forecasts
    } // ar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the coefficients for a qth order a Moving Average(q) model.
     *  x_t = e_t - theta_0 * e_t-1 - ... - theta_q-1 * e_t-q
     *  FIX: use Method of Innovations
     *  @param p  the order of the AR model
     */
    def est_ma (q: Int = 1): VectorD =
    {
        if (q != 1) flaw ("est_ma", "estimation for q > 1 not yet implemented")

        val theta = new VectorD (q)          // MA(q) cofficients: theta_0, ..., theta_q-1
        theta(0) = 0.      // FIX: need to estimate theta(0)
        theta           
    } // est_ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a vector that is the predictions of a qth order Moving Average (MA)
     *  model.
     *  x_t = e_t - theta_0 * e_t-1 - ... - theta_q-1 * e_t-q
     *  @param theta  the estimated MA(q) coefficients (theta)
     */
    def ma (theta: VectorD): VectorD =
    {
        val q = theta.dim
        if (q != 1) flaw ("est_ma", "estimation for q > 1 not yet implemented")

        val k = n-q
        val f = new VectorD (k)            // forecasts for x
        for (t <- 0 until k) {
            var sum = 0.
            for (j <- 1 until min (k, t)) sum -= theta(0)~^j * x(t-j)
            f(t) = sum
        } // for
        f                                  // return the vector of forecasts
    } // ma

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fit an ARMA model to times series data.
     */
    def train ()
    {
        // FIX: to be implemented
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For all the time points in vector t, predict the value of y = f(t) by ...
     *  @param z  the time-vector indicating time points to forecast
     */
    def predict (y: VectorD): Double = 
    {
        0.    // FIX: not yet implemented
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several time vectors, forecast the y-values.
     *  @param z  the matrix containing row time-vectors to use for prediction
     */
    def predict (z: MatrixD): VectorD =
    {
        throw new UnsupportedOperationException ()
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Auto-Correlation Function (ACF) for lags 1 to m.
     *  @param m  the maximum lag to examine
     */
    def acf (m: Int = 20): VectorD =
    {
        val lags  = min (m, x.dim)
        val statV = new StatVector (x)          // make a stat vector for the time series data
        val acfV  = new VectorD (lags)
        for (j <- 0 until lags) acfV(j) = statV.acorr (j)
        acfV
    } // acf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Partial Auto-Correlation Function (PACF) for lags 1 to m.
     *  @param m  the maximum lag to examine
     */
    def pacf (m: Int = 20): VectorD =
    {
        val lags  = min (m, x.dim)
        val statV = new StatVector (x)          // make a stat vector for the time series data
        val pacfV = new VectorD (lags)
        for (j <- 0 until lags) pacfV(j) = 0.   // FIX
        pacfV
    } // pacf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot a function, e.g.,  Auto-Correlation Function (ACF), Partial ACF (PACF).
     *  @param fVec  the vector given function values
     *  @param name  the name of the function
     */
    def plotFunc (fVec: VectorD, name: String)
    {
        val lag_axis = new VectorD (fVec.dim)
        for (i <- 0 until fVec.dim) lag_axis(i) = i
        val zero = new VectorD (fVec.dim)
        new Plot (lag_axis, fVec, zero, "Plot of " + name)
    } // plotFunc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the y vector by taking the lth order moving average.
     *  @param l  the number of points to average
     */
    def smooth (l: Int): VectorD =
    {
        val ld = l.toDouble
        val z  = new VectorD (n-l)
        for (i <- 0 until n-l) z(i) = y(i until i+l).sum / ld
        z
    } // smooth

} // ARMA class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the ARMA class.
 */
object ARMATest extends App
{
    def genTimes (n: Int): VectorD =
    {
        val t = new VectorD (n)
        for (i <- 0 until n) t(i) = i.toDouble
        t
    } // genTimes

    val n = 100
    val r = Random ()
    val t = genTimes (n)
    val y = new VectorD (n)
    for (i <- 0 until n) y(i) = t(i) + 10. * r.gen

    val ts = new ARMA (y, t)   // time series data: y vs. t

    // Examine the times series data:
    // (a) smooth the data, (b) compute ACF, compute PACF

    new Plot (t, y, ts.smooth (5), "Plot of y, smooth vs. t")
    ts.plotFunc (ts.acf (), "ACF")
    ts.plotFunc (ts.pacf (), "PACF")         // FIX

    // Build AR(1) and MA(1) models for the time series data

    val phi = ts.est_ar (1)
    new Plot (t, y, ts.ar (phi) + ts.mu, "Plot of y, ar(1) vs. t")

    val theta = ts.est_ma (1)                                 // FIX
    new Plot (t, y, ts.ma (theta) + ts.mu, "Plot of y, ma(1) vs. t")

} // ARMATest object

