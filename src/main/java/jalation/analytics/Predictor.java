
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Fri Sep 19 12:42:31 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.analytics;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` interface provides a common framework for several predictors.
 */
interface Predictor
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors (x's) and their corresponding (y's), fit a
     *  prediction function y = f(x), returning the quality of the fit.
     */
    double [] train ();

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, predict the y-value of f(z).
     *  @param z  the vector to use for prediction
     */
    double predict (double [] z);

} // Predictor interface

