
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{VectoD, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` trait provides a common framework for several predictors.
 *  A predictor is for unbounded responses (real or integer).  When the number
 *  of distinct responses is bounded by some integer 'k', a classifier should
 *  be used.  
 */
trait Predictor
{
    /** Coefficient/parameter vector [b_0, b_1, ... b_k]
     */
    protected var b: VectoD = _

    /** Residual/error vector [e_0, e_1, ... e_m-1]
     */
    protected var e: VectoD = null

    /** Sum of squared errors
     */
    protected var sse: Double = -1.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'y's,
     *  train the prediction function 'y = f(x)' by fitting its parameters.
     */
    def train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficient/parameter values.
     */
    def coefficient: VectoD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sum of squared errors.  Compute 'sse' if not done already.
     */
    def sseF (): Double = { if (sse < 0.0) sse = e dot e; sse }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.
     */
    def fit: VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.  Override when necessary.
     */
    def fitLabels: Seq [String] = Seq ("rSquared", "rBarSq", "fStat")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectoD): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectorI): Double = predict (z.toDouble)

} // Predictor trait

