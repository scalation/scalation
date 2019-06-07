
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell, Hao Peng
 *  @version 1.6
 *  @date    Tue Oct 11 16:12:54 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Functional Data Analysis, Second Edition
 *  @see faculty.bscb.cornell.edu/~hooker/ShortCourseHandout.pdf
 */

package scalation.analytics
package fda

import scala.collection.mutable.Map

import scalation.linalgebra._
import scalation.calculus.{DB_Spline, DBasisFunction, functionS2S2Hilbert}
import scalation.math.FunctionS2S
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.util.Error

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_F2S` class performs functional linear regression with
 *  scaler response and functional covariates.
 *  <p>
 *      y = a + <b(t),x(t)> + ε
 *  <p>
 *  where `<b, x>` denotes the inner product of `b` and `x`. 
 *  @param x    the covariate matrix - treated as functional
 *  @param y    the response vector
 *  @param t    the time vector
 *  @param bf   the basis function object with derivatives support
 */
class Regression_F2S (x: MatriD, y: VectoD, t: VectoD, bf: DBasisFunction, technique: RegTechnique = Cholesky,
                      lambda: Double = 1E-4)
    extends Predictor
{
    private val DEBUG = false                             // debug flag
    private var b: VectoD = null                          // parameter vector [b0]
    private val ord   = bf.getOrder                       // order of the basis function
    val (t0, tn)      = (t(0), t(t.dim-1))                // region of integration

    private var rg: RidgeRegression = null

    val e    = new VectorD (y.dim)
    val xmoo = for (i <- y.range) yield {                 // smooth the data
        val moo = new Smoothing_F (x(i), t, bf)
        moo.train()
        moo
    } // for

    val _1 = VectorD.one (y.dim)
    val z  = _1 +^: MatrixD (for (i <- 0 until y.dim) yield x_i(i), false)    // design matrix

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The inner product of phi and a smoothed function
     *  @param i  the i-th smoothed function function
     */
    def x_i (i: Int) = VectorD (for (j <- bf.range(ord)) yield (bf(ord)(j) _) dot (xmoo(i).predict, t0, tn))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model using the smoothed data to find the regression coefficients 'b'.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): Regression_F2S =
    {
//      val eye = MatrixD.eye(z.dim2)
//      b = (z.t * z + eye * lambda).inverse * z.t * yy               // direct solution, may produce NaN

        val hp = RidgeRegression.hp.updateReturn ("lambda", lambda)
        rg = new RidgeRegression (z, yy, null, hp, technique)
        rg.train (yy)
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model using the smoothed data to find the regression coefficients 'b'.
     */
    def train (): Regression_F2S = train (y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     *  @param xx  the test data/input matrix
     *  @param yy  the test response/output vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y) = rg.eval (xx, yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = null  // hparam = FIX

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
    /** Return the quality of fit map.
     */
    def fitMap: Map [String, String] = rg.fitMap

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the y-value at time point 'tt'.
     *  @param xf  the given function of time
     */
    def predict (xf: FunctionS2S): Double =
    {
        val zz = VectorD (1.0) ++ VectorD (for (j <- bf.range(ord)) yield (bf(ord)(j) _) dot (xf, t0, tn))
        rg.predict (zz)
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectoD): Double = throw new UnsupportedOperationException("predict requires a FunctionS2S as input")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model on the training set for cross-validation purposes
     *  @param itest    indicies of the testing set, to be excluded for training
     */
    def train (itest: IndexedSeq [Int])
    {
        val itrain = VectorI (z.range1 diff itest)
        val zs     = z (itrain)
        val ys     = y (itrain)

        val hp = RidgeRegression.hp.updateReturn ("lambda", lambda)
        rg = new RidgeRegression (zs, ys, null, hp, technique)
        rg.train ()
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the model on the testing set for cross-validation purposes
     *  @param itest    indicies of the testing set
     */
    def test (itest: VectoI) =
    {
        for (i <- itest) {
            def xf (tt: Double) = xmoo(i).predict (tt)
            e(i) = y(i) - predict(xf _)
        } // for
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the accuracy of the predicted results by cross-validation, returning
     *  the sse.
     *  @param k  number of crosses and cross-validations (defaults to 10x).
     */
    def crossValidate (k: Int): Double =
    {
        val size        = e.size
        val permutedVec = PermutedVecI (VectorI.range(0, size), ranStream)
        val randOrder   = permutedVec.igen
        val itestA      = randOrder.split (k)

        for (itest <- itestA) {
            train (itest())
            test (itest)
        } // for
        e dot e
    } // crossValidateRand

} // Regression_F2S class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_F2STest` object is used to test the `Regression_F2S` class.
 *  > runMain scalation.analytics.fda.Regression_F2STest
 */
object Regression_F2STest extends App
{
    // x is first 10 rows of gene expression dataset
    // @see https://github.com/scalation/fda/blob/master/scalation_1.3/data/gene_expression.csv
    val x = MatrixD ((10, 12), 39.20,13.64,20.14,28.22,30.11,41.86,30.82,23.88,18.43,14.08,16.44,15.31,
                               82.07,135.47,101.39,113.03,103.14,93.99,79.91,68.01,70.06,50.94,54.49,59.76,
                               20.07,26.61,26.33,38.49,46.60,60.91,76.12,37.44,57.84,52.81,55.76,70.20,
                               6.97,2.47,4.01,4.80,5.11,6.02,7.12,3.05,4.60,2.94,2.79,3.16,
                               1.17,24.49,49.82,48.42,41.77,27.80,30.00,28.80,14.21,8.94,4.18,4.15,
                               407.31,58.90,5.69,2.70,3.86,4.86,7.09,15.25,12.20,10.06,4.46,4.98,
                               0.01,0.10,0.24,0.15,0.16,0.24,0.83,0.89,1.37,1.18,0.84,1.13,
                               3.48,3.51,5.17,5.20,4.85,5.85,4.11,2.99,2.50,2.09,2.41,2.42,
                               1.47,1.55,2.39,1.42,1.32,1.05,1.57,1.21,1.85,1.79,2.33,3.01,
                               0.00,0.00,0.00,0.00,0.01,0.01,0.04,0.18,0.36,0.54,0.51,0.57)

    val y = VectorD (for (i <- x.range1) yield x(i).mean)
    val t  = VectorD.range (0, x.dim2)                            // time-points for plotting

    println (s"y = $y \nx = $x \n t = $t")

    val ord = 4

    val bf  = new DB_Spline (t, ord)
    val rgf = new Regression_F2S (x, y, t, bf)
    rgf.train()

    val pred = new VectorD(rgf.xmoo.size)
    for (i <- 0 until rgf.xmoo.size) {
        val moo = rgf.xmoo(i)
        def xf (tt: Double) = moo.predict (tt)
        pred(i) = rgf.predict(xf _)
    } // for

    println(s"y         = $y")
    println(s"pred      = $pred")
    val e = y - pred
    println(s"residual  = ${e}")
    val sse = e dot e
    println(s"sse = ${sse}")
    val rmse = Math.sqrt(sse / y.dim)
    println(s"rmse = $rmse")

} // Regression_F2STest object

