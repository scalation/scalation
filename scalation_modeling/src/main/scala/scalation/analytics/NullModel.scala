
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Jan  5 14:03:36 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModel` class implements the simplest type of predictive modeling technique
 *  that just predicts the response 'y' to be the mean.
 *  Fit the parameter vector 'b' in the null regression equation
 *  <p>
 *      y  =  b dot x + e  =  b0 + e
 *  <p>
 *  where 'e' represents the residual/error vector (the part not explained by the model).
 *  @param y  the response vector
 */
class NullModel (y: VectoD)
      extends Fit (y, 1, (1, y.dim)) with Predictor with Error
{
    private var b: VectoD = null                            // parameter vector [b0]
    private var e: VectoD = null                            // residual/error vector [e_0, e_1, ... e_m-1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  null regression equation.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): NullModel =
    {
        b = VectorD (yy.mean)                               // parameter vector [b0]
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error vector 'e' (difference between actual and predicted) and
     *  useful diagnostics.
     *  @param xx  the test data/input matrix (not relevant for NullModel)
     *  @param yy  the test response/output vector
     */
    def eval (xx: MatriD = null, yy: VectoD = y): NullModel =
    {
        val yp = VectorD.fill (yy.dim)(b(0))                // y predicted for xx (test/full)
        e = yy - yp                                         // compute residual/error vector e
        diagnose (e, yy)                                    // compute diagnostics
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters (the NullModel has none).
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
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot z',
     *  i.e., '[b0] dot [z0]'.
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot z',
     *  for each row of matrix 'z'.
     *  @param z  the new matrix to predict (only used for dimension)
     */
    def predict (z: MatriD = null): VectoD = VectorD.fill (y.dim)(b(0))

} // NullModel class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModel` companion object provides a simple factory method
 *  for building null models.
 */
object NullModel extends Error
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Simplee Linear Regression model from a combined data matrix.
     *  @param xy  the combined data matrix
     */
    def apply (xy: MatriD): NullModel =
    {
        val n = xy.dim2
        if (n < 1) { flaw ("apply", "the length of the 'xy' matrix must be at least 1"); null }
        else new NullModel (xy.col(n-1))
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Analyze a dataset using the given model using ordinary training with the
     *  'train' method.
     *  @param model  the model to be used
     */
    def analyze (model: NullModel)
    {
        model.train ().eval ()
        println ("hparameter hp  = " + model.hparameter)
        println ("parameter  b   = " + model.parameter)
        println ("fitMap     qof = " + model.fitMap)
    } // analyze

} // NullModel object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModelTest` object is used to test the `NullModel` class.
 *  <p>
 *      y = b dot x + e = b0 + e
 *  <p>
 *  > runMain scalation.analytics.NullModelTest
 */
object NullModelTest extends App
{
    // 4 data points:
    val y = VectorD (1, 3, 3, 4)                        // y vector
    println (s"y = $y")

    val rg = new NullModel (y)
    rg.train ().eval ()
    println ("parameter = " + rg.parameter)
    println ("fitMap    = " + rg.fitMap)
    val z  = VectorD (5)                                // predict y for one point
    val yp = rg.predict (z)
    println (s"predict ($z) = $yp")

} // NullModelTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NullModelTest2` object is used to test the `NullModel` class.
 *  <p>
 *      y = b dot x + e = b0 + e
 *  <p>
 *  > runMain scalation.analytics.NullModelTest2
 */
object NullModelTest2 extends App
{
    // 5 data points:
    val y = VectorD (2.0, 3.0, 5.0, 4.0, 6.0)           // response vector y
    println (s"y = $y")

    val rg = new NullModel (y)                          // create a NullModel
    rg.train ().eval ()                                 // train on data and evaluate
    println ("parameter = " + rg.parameter)             // parameter values
    println ("fitMap    = " + rg.fitMap)                // quality of fit
    val z  = VectorD (5.0)                              // predict y for one point
    val yp = rg.predict (z)                             // yp (y-predicted or y-hat)
    println (s"predict ($z) = $yp")

} // NullModelTest2 object

