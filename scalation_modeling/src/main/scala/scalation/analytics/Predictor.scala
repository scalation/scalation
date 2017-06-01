
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vamsi Nadella
 *  @version 1.3
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{VectoD, VectorD, VectoI}
import scalation.math.double_exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` trait provides a common framework for several predictors.
 *  A predictor is for potentially unbounded responses (real or integer).
 *  When the number of distinct responses is bounded by some relatively small
 *  integer 'k', a classifier is likdely more appropriate.
 *  Note, the 'train' method must be called first.
 */
trait Predictor
{
    protected var b: VectoD = null                  // coefficient/parameter vector [b_0, b_1, ... b_k]
    protected var e: VectoD = null                  // residual/error vector [e_0, e_1, ... e_m-1]
    protected var sse       = -1.0                  // sum of squared errors
    protected var sst       = -1.0                  // total of squared errors
    protected var rmse      = -1.0                  // root mean squared error
    protected var rSq       = -1.0                  // coefficient of determination (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'yy's,
     *  train the prediction function 'yy = f(x)' by fitting its parameters.
     *  The 'x' values must be provided by the implementing class.  Also, 'train'
     *  must call 'diagnose'.
     *  @param yy  the response vector
     */
    def train (yy: VectoD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'y's,
     *  passed into the implementing class, train the prediction function 'y = f(x)'
     *  by fitting its parameters.
     */
    def train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the predictor.  Override to add more diagostics.
     *  Note, for 'rmse', 'sse' is divided by the number of instances 'm' rather
     *  than degrees of freedom.
     *  @see en.wikipedia.org/wiki/Mean_squared_error
     *  @param yy  the response vector
     */
    def diagnose (yy: VectoD)
    {
        val m = e.dim                               // number of instances
        sse   = e dot e                             // sum of squared errors
        sst   = (yy dot yy) - yy.sum~^2.0 / m       // total sum of squares
        rmse  = sqrt (sse / e.dim)                  // root mean square error
        rSq   = (sst - sse) / sst                   // coefficient of determination R^2
    } // diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficient/parameter values.
     */
    def coefficient: VectoD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'sse', rmse' and 'rSq'.  Override to
     *  add more.
     */
    def fit: VectoD = VectorD (sse, rmse, rSq)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.  Override when necessary.
     */
    def fitLabels: Seq [String] = Seq ("sse", "rmse", "rSq")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectoD): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectoI): Double = predict (z.toDouble)

} // Predictor trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorTest` object tests all the classes in the `scalation.analytics`
 *  package that extend the `Predictor` trait.
 *  > run-main scalation.analytics.PredictorTest
 */
object PredictorTest extends App
{
    import scalation.util.banner

    banner ("ANCOVATest");            ANCOVATest.main (null)
    banner ("ANOVATest");             ANOVATest.main (null)
    banner ("ARMATest");              ARMATest.main (null)
    banner ("ExpRegressionTest");     ExpRegressionTest.main (null)
    banner ("ExpSmoothingTest");      ExpSmoothingTest.main (null)
    banner ("LassoRegressionTest");   LassoRegressionTest.main (null)
    banner ("NeuralNetTest");         NeuralNetTest.main (null)
    banner ("NonLinRegressionTest");  NonLinRegressionTest.main (null)
    banner ("PerceptronTest");        PerceptronTest.main (null)
    banner ("PoissonRegressionTest"); PoissonRegressionTest.main (null)
    banner ("PolyRegressionTest");    PolyRegressionTest.main (null)
    banner ("RegressionTest");        RegressionTest.main (null)
    banner ("Regression_WLSTest");    Regression_WLSTest.main (null)
    banner ("ResponseSurfaceTest");   ResponseSurfaceTest.main (null)
    banner ("RidgeRegressionTest");   RidgeRegressionTest.main (null)
    banner ("SimpleRegressionTest");  SimpleRegressionTest.main (null)
    banner ("TranRegressionTest");    TranRegressionTest.main (null)
    banner ("TrigRegressionTest");    TrigRegressionTest.main (null)

} // PredictorTest object

