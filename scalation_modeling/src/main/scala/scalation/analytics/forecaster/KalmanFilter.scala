
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Sun Sep 13 20:37:41 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.forecaster

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.random.NormalVec

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KalmanFilter` class is used to fit state-space models.
 *  @see en.wikipedia.org/wiki/Kalman_filter
 *  FIX: needs more thorough testing
 *  @param x0  the initial state vector
 *  @param ff  the state transition matrix
 *  @param hh  the observation matrix
 *  @param qq  the process noise covariance matrix
 *  @param rr  the observation noise covariance matrix
 *  @param bb  the optional control-input matrix
 *  @param u   the control vector
 */
class KalmanFilter (x0: VectoD, ff: MatriD, hh: MatriD, qq: MatriD, rr: MatriD, bb: MatriD = null, u: VectoD = null)
{
    private val MAX_ITER   = 20                            // maximum number of iterations
    private val doPlot     = true                          // flag for drawing plot
    private val n          = ff.dim1                       // dimension of the state vector
    private val _0         = VectorD (n)                   // vector of 0's
    private val ii         = MatrixD.eye (n)               // identity matrix
    private val fft        = ff.t                          // transpose of ff
    private val hht        = hh.t                          // transpose of hh
    private var x          = x0                            // the state estimate
    private var pp: MatriD = new MatrixD (n, n)            // the covariance estimate

    val traj = if (doPlot) new MatrixD (MAX_ITER, n) else new MatrixD (0, 0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the state of the process at the next time point
     */
    def predict ()
    {
        x  = ff * x                                 // new predicted state
        if (u != null && bb != null) x += bb * u
        pp = ff * pp * fft + qq                     // new predicted covariance
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the state and covariance estimates with the current and possibly
     *  noisy measurements
     *  @param z  current measurement/observation of the state
     */
    def update (z: VectoD)
    {
        val y  = z - hh * x                         // measurement residual
        val ss = hh * pp * hht + rr                 // residual covariance
        val kk = pp * hht * ss.inverse              // optimal Kalman gain
        x  = x + kk * y                             // updated state estimate
        pp = (ii - kk * hh) * pp                    // updated covariance estimate
    } // update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively solve for 'x' using predict and update phases.
     *  @param dt  the time increment (delta t)
     *  @param u   the control vector
     */
    def solve (dt: Double, u: VectoD = null): VectoD =
    {
        var t  = 0.0                                    // initial time
        var pp: MatriD = new MatrixD (n, n)

        for (k <- 0 until MAX_ITER) {

            t += dt                                     // advance time
            if (doPlot) traj(k) = x ++ t                // add current time t, state x to trajectory

            // predict
            predict ()

            // update
            val v  = NormalVec (_0, rr.asInstanceOf [MatrixD]).gen      // observation noise - FIX - should work in trait
            val z  = hh * x + v                         // new observation

            update (z)
        } // for
        x
    } // solve

} // KalmanFilter class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KalmanFilterTest` object is used to test the `KalmanFilter` class.
 *  @see en.wikipedia.org/wiki/Kalman_filter
 *  > runMain scalation.analytics.KalmanFilterTest
 */
object KalmanFilterTest extends App
{
    import scalation.math.double_exp

    println ("KalmanFilterTest")

    val dt    = 0.1                       // time increment (delta t)
    val var_a = 0.5                       // variance of uncontrolled acceleration a
    val var_z = 0.5                       // variance from observation noise

    val ff = new MatrixD ((2, 2), 1.0, dt,
        0.0, 1.0)

    val hh = new MatrixD ((1, 2), 1.0, 0.0)

    val qq = new MatrixD ((2, 2), dt~^4/4, dt~^3/2,
        dt~^3/2, dt~^2) * var_a

    val rr = new MatrixD ((1, 1), var_z)

    val x0 = VectorD (0.0, 0.0)

    val kf = new KalmanFilter (x0, ff, hh, qq, rr)

    println ("solve = " + kf.solve (dt))

    new Plot (kf.traj.col (2), kf.traj.col (0), kf.traj.col (1))

} // KalmanFilterTest object

