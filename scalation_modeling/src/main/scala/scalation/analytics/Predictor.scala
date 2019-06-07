
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vamsi Nadella
 *  @version 1.6
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, VectoD, VectoI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` trait provides a common framework for several predictors.
 *  A predictor is for potentially unbounded responses (real or integer).
 *  When the number of distinct responses is bounded by some relatively small
 *  integer 'k', a classifier is likely more appropriate.
 *  Note, the 'train' method must be called first followed by 'eval'.
 */
trait Predictor
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'yy's,
     *  train the prediction function 'yy = f(x)' by fitting its parameters.
     *  The 'x' values must be provided by the implementing class.
     *  @param yy  the response vector (impl. classes should default yy to y)
     */
    def train (yy: VectoD): Predictor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error (difference between actual and predicted) and useful
     *  diagnostics for the test dataset.
     *  @param xx  the test data/input matrix (impl. classes should default xx to x)
     *  @param yy  the test response/output vector (impl. classes should default yy to y)
     */
    def eval (xx: MatriD, yy: VectoD): Predictor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters (if none, return null).
     */
    def hparameter: HyperParameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values.
     */
    def parameter: VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on the trained model.
     *  @see 'summary' method for more details
     */
    def report: String

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD

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

