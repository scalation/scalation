
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vamsi Nadella
 *  @version 1.4
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.immutable.ListMap
import scala.math.sqrt

import scalation.linalgebra.{MatriD, VectoD, VectorD, VectoI}
import scalation.math.double_exp
import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` trait provides a common framework for several predictors.
 *  A predictor is for potentially unbounded responses (real or integer).
 *  When the number of distinct responses is bounded by some relatively small
 *  integer 'k', a classifier is likdely more appropriate.
 *  Note, the 'train' method must be called first followed by 'eval'.
 */
trait Predictor
{
    protected var b: VectoD = null                  // coefficient/parameter vector [b_0, b_1, ... b_k]
    protected var e: VectoD = null                  // residual/error vector [e_0, e_1, ... e_m-1]
    protected var sse       = -1.0                  // sum of squares error
    protected var ssr       = -1.0                  // sum of squares regression/model
    protected var sst       = -1.0                  // sum of squares total (ssr + sse)
    protected var mae       = -1.0                  // mean absolute error
    protected var mse       = -1.0                  // mean squared error
    protected var rmse      = -1.0                  // root mean squared error
    protected var rSq       = -1.0                  // coefficient of determination (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'yy's,
     *  train the prediction function 'yy = f(x)' by fitting its parameters.
     *  The 'x' values must be provided by the implementing class.
     *  @param yy  the response vector
     */
    def train (yy: VectoD): Predictor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'y's,
     *  passed into the implementing class, train the prediction function 'y = f(x)'
     *  by fitting its parameters.
     */
//    def train (): Predictor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param yy  the response vector
     */
    def eval (yy: VectoD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the predictor.  Override to add more diagostics.
     *  Note, for 'mse' and 'rmse', 'sse' is divided by the number of instances
     *  'm' rather than the degrees of freedom.
     *  @see en.wikipedia.org/wiki/Mean_squared_error
     *  @param yy  the response vector
     */
    protected def diagnose (yy: VectoD)
    {
        val m = e.dim                               // number of instances
        sst   = (yy dot yy) - yy.sum~^2.0 / m       // sum of squares total
        sse   = e dot e                             // sum of squares error
        ssr   = sst - sse                           // sum of squares regression (not returned by fit)
        mse   = sse / m                             // raw mean square error
        rmse  = sqrt (mse)                          // root mean square error
        mae   = e.norm1 / m                         // mean absolute error
        rSq   = ssr / sst                           // coefficient of determination R^2
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
    /** Return the quality of fit including 'sst', 'sse', 'mae', rmse' and 'rSq'.
     *  Note, if 'sse > sst', the model introduces errors and the 'rSq' may be negative,
     *  otherwise, R^2 ('rSq') ranges from 0 (weak) to 1 (strong).
     *  Note that 'rSq' is the last or fifth measure (5).
     *  Override to add more quality of fit measures.
     */
    def fit: VectoD = VectorD (sst, sse, mse, rmse, mae, rSq)

    val index_rSq = 5                               // index of rSq           

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.  Override when necessary.
     */
    def fitLabels: Seq [String] = Seq ("sst", "sse", "mse", "rmse", "mae", "rSq")

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of selected quality of fit measures/metrics.
     */
    def metrics: Map [String, Any] =
    {
        ListMap ("R-Squared" -> "%.4f".format (rSq),
                 "SSE"       -> "%.4f".format (sse),
                 "RMSE"      -> "%.4f".format (rmse))
    } // metrics

    def build (x: MatriD, y: VectoD): Predictor = ???

} // Predictor trait


object Predictor
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use 'k'-fold cross-validation to compute test quality of fit measures by
     *  dividing the dataset into a test dataset and a training dataset.
     *  The test dataset starts at 'testStart' and ends at 'testEnd', while
     *  the rest of the data is training dataset".
     *  @param k  the number of crosses and cross-validations (defaults to 10x).
     *
    def crossValidate (x: MatriD, y: VectoD, alg: Predictor, k: Int = 10): Array [Statistic] =
    {
        val stats = Array.fill (alg.fitLabels.length) (new Statistic ())
        val tSize = x.dim1 / k 

        var model = alg
        var i = 0
        do {
            val tRange = Range (i * tSize, (i+1) * tSize)   // row range for test dataset
            val x_te = x.slice (tRange)                     // test data matrix
            val y_te = y.slice (tRange)                     // test response vector
            val x_tr = x.sliceExclude (tRange)              // training data matrix
            val y_tr = y.sliceExclude (tRange)              // training response vector
            model = model.build (x_tr, y_tr)                // build next model for next training dataset
            model.eval (x_te, y_te)                         // evaluate model on testing dataset
            val qm = model.fit                              // get quality of fit measures
            for (i <- qm.indices) stats(i).tally (qm(i))    // tally these measures
            i += 1
        } while (i < k)

        println ("------------------------------------------------------------")
        println ("stats = " + stats.deep)
        println ("------------------------------------------------------------")
        stats
    } // crossValidate
     */

} // Predictor object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorTest` object tests all the classes in the `scalation.analytics`
 *  package that directly or indirectly extend the `Predictor` trait.
 *  > runMain scalation.analytics.PredictorTest
 */
object PredictorTest extends App
{
    import scalation.util.banner

    banner ("ANCOVATest");            ANCOVATest.main (null)
    banner ("ANOVATest");             ANOVATest.main (null)
    banner ("ARIMATest");             ARIMATest.main (null)
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

