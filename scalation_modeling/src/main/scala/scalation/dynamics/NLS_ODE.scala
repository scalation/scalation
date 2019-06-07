
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Oct 12 15:38:31 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see http://faculty.bscb.cornell.edu/~hooker/ODE_Estimation.pdf
 *  @see http://www.pitt.edu/~caginalp/pub102.pdf
 *  @see scalation.analytics.NonLinearRegression
 */

//  U N D E R   D E V E L O P M E N T 

package scalation.dynamics

import scala.collection.mutable.{LinkedHashMap, Map}

import scalation.analytics.{HyperParameter, Predictor}
import scalation.linalgebra.{FunctionV_2S, MatriD, VectoD, VectorD}
import scalation.linalgebra.VectorD.one
import scalation.minima.QuasiNewton
import scalation.util.Error

import Derivatives.Derivative
import DormandPrince._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Given an Ordinary Differential Equation 'ODE' parameterized using the vector 'b'
 *  with Initial Value 'IV' 'y0', estimate the parameter values 'b' for the ODE
 *  using weighted Non-linear Least Squares 'NLS'.
 *  <p>
 *      ODE:  dy/dt = f(t, y)
 *      IV:   y(t0) = y0
 *  <p>
 *      Times series data:  z(t0), z(t1), ... z(tn)
 *  <p>
 *  @param z       the observed values
 *  @param ts      the time points of the observations
 *  @param b_init  the initial guess for the parameter values 'b' 
 *  @param w       the optional weights
 */
class NLS_ODE (z: VectorD, ts: VectorD, b_init: VectorD, private var w: VectorD = null)
      extends Predictor with Error
{
    if (z.dim != ts.dim) flaw ("constructor", "number of observations z must match time series ts")
    if (w == null) w = one (z.dim)

    private var objectiveF: FunctionV_2S = null             // objective function to minimize (measures quality of fit)
    private var y0 = 0.0                                    // initial value/condition y(0) = y0

    private var b: VectoD = null                            // parameter/coefficient vector [b_0, b_1, ... b_k]
    private var e: VectoD = null                            // residual/error vector [e_0, e_1, ... e_m-1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize `NLS-ODE` with the objective function and initial value/condition.
     *  @param _objectiveF  the objective function indicating departure from observation
     *  @param _y           the initial value/condition y(0) = y0
     */
    def init (_objectiveF: FunctionV_2S, _y0: Double)
    {
        objectiveF = _objectiveF
        y0         = _y0
    } // init

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function to compute the Weighted Sum of Squares Error 'SSE' for given values
     *  for parameter vector 'b'.
     *  @param b  the parameter vector
     */
    def wsseF (dy_dt: Derivative): Double =
    {
        val hmin = 0.01
        val hmax = 1.0
        val y = VectorD (for (i <- ts.indices) yield integrate2 (dy_dt, y0, ts(i), hmin, hmax))
        e = z - y                                              // residuals/errors
        (w * e.sq).sum                                         // sum of weighted squared error
    } // wsseF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) using a
     *  non-linear least squares method.
     */
    def train (yy: VectoD): NLS_ODE =
    {
        throw new UnsupportedOperationException ("train (yy) not implemented yet")
        null
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) using a
     *  non-linear least squares method.
     */
    def train (): NLS_ODE =
    {
        println (s"initial objectiveF ($b_init) = ${objectiveF (b_init)}")
        val bfgs = new QuasiNewton (objectiveF)                // minimize sse using NLP
        b        = bfgs.solve (b_init)                         // estimate for b from optimizer
        println (s"final objectiveF ($b) = ${objectiveF (b)}")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param xx  the test data/input matrix
     *  @param yy  the test response/output vector
     */
    def eval (xx: MatriD, yy: VectoD): NLS_ODE =
    {
        val yp = xx * b                                        // predicted y value - FIX
        e = yy - yp                                            // compute residual/error vector e
//      diagnose (e, yy)                                       // compute diagnostics - FIX
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values.
     */
    def parameter: VectoD = b

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on the trained model.
     *  @see 'summary' method for more details
     */
    def report: String =
    {
        s"""
REPORT
    hparameter hp  = $hparameter
    parameter  b   = $parameter
    fitMap     qof = $fitMap
        """
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    def fit: VectorD = VectorD (objectiveF (b))       // FIX - add more indicators of quality

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    def fitLabel: Seq [String] = Seq ("objectiveF")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Format a double value.
     *  @param z  the double value to format
     */
    private def f_ (z: Double): String = "%.5f".format (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of quality of fit measures (use of `LinedHashMap` makes it ordered).
     *  Override to add more quality of fit measures.
     */
    def fitMap: Map [String, String] =
    {
        val lm = LinkedHashMap [String, String] ()          // empty list map
        val fl = fitLabel                                   // fit labels
        val fv = fit                                        // fit values
        for (i <- fl.indices) lm += fl(i) -> f_(fv(i))
        lm
    } // fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(zz)'.
     *  @param zz  the new vector to predict
     */
    def predict (zz: VectoD): Double = 0.0                     // FIX - meaning?

} // NLS_ODE class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorDTest` object tests the operations provided by `VectorD`.
 *  > runMain scalation.dynamics.NLS_ODETest
 */
object NLS_ODETest extends App
{
    val z      = VectorD (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val ts     = VectorD (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val b_init = VectorD (1.0, 1.0)

    val nls = new NLS_ODE (z, ts, b_init)

    def objectiveF (b: VectoD): Double =
    {
        def dy_dt (t: Double, y: Double) = b(0)*t + b(1)*y
        nls.wsseF (dy_dt)
    } // objectiveF

    val y0 = 1.0

    nls.init (objectiveF, y0)
    nls.train ()
    println ("b = " + nls.parameter)

} // NLS_ODETest object

