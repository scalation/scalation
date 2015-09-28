
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, VectorD, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` trait provides a common framework for several predictors.
 */
trait Predictor
{
    /** Coefficient/parameter vector [b_0, b_1, ... b_k]
     */
    protected var b: VectorD = null

    /** Rresidual/error vector [e_0, e_1, ... e_m-1]
     */
    protected var e: VectorD = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors (x's) and their corresponding (y's), fit a
     *  prediction function y = f(x).
     */
    def train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficient/parameter values.
     */
    def coefficient: VectorD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectorD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including rSquared.
     */
    def fit: VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectorD): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    def predict (z: VectorI): Double = predict (z.toDouble)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several new continuous data vectors stored as rows in a matrix,
     *  predict all the y-values of f(z_i).
     *  @param z  the matrix containing row vectors to use for prediction
     */
    def predict (z: MatriD): VectorD    // FIX

} // Predictor trait

