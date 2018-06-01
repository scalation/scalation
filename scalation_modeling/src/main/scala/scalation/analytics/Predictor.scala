
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vamsi Nadella
 *  @version 1.5
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'yy's,
     *  train the prediction function 'yy = f(x)' by fitting its parameters.
     *  The 'x' values must be provided by the implementing class.
     *  @param yy  the response vector
     */
    def train (yy: VectoD): Predictor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     */
    def eval ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the test dataset.
     *  @param xx  the test data matrix
     *  @param yy  the test response vector
     *  FIX - implement in classes
     */
    def eval (xx: MatriD, yy: VectoD) {}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficient/parameter values.
     */
    def coefficient: VectoD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD = e

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
 *  package that directly or indirectly extend the `Predictor` trait.
 *  FIX - make first test uniform so that the modeling techniques may be compared
 *  > runMain scalation.analytics.PredictorTest
 */
object PredictorTest extends App
{
    import scalation.util.banner

    banner ("ANCOVATest");            ANCOVATest.main (null)
    banner ("ANOVA1Test");            ANOVA1Test.main (null)
    banner ("ExpRegressionTest");     ExpRegressionTest.main (null)
    banner ("LassoRegressionTest");   LassoRegressionTest.main (null)
    banner ("NeuralNet_2LTest");      NeuralNet_2LTest.main (null)
    banner ("NeuralNet_3LTest");      NeuralNet_3LTest.main (null)
    banner ("NeuralNet_XLTest");      NeuralNet_XLTest.main (null)
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

