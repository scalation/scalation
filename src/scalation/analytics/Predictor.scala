
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra_gen.Vectors.VectorI

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This trait provides a common framework for several predictors.
 */
trait Predictor
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors (x's) and their corresponding (y's), fit a
     *  prediction function y = f(x).
     */
    def train ()

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
    def predict (z: MatrixD): VectorD

} // Predictor trait

