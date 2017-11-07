
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Dec 28 12:00:07 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{exp, log}

import scalation.linalgebra.VectoD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ActivationFunc` object contains common Activation functions.
 *  @see en.wikipedia.org/wiki/Activation_function
 */
object ActivationFunc
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the 'logistic' function at 't'.
     *  @param t  the logistic function argument
     *  @param a  the shift parameter
     *  @param b  the spread parameter
     */
    def logistic (t: Double, a: Double, b: Double): Double = 1.0 / (1.0 + exp (-(a + b*t)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the 'logistic' function applied to vector 't'.
     *  The values are computed in-place.
     *  @param t  the logistic function argument
     *  @param a  the shift parameter
     *  @param b  the spread parameter
     */
    def logisticV (t: VectoD, a: Double, b: Double): VectoD = 
    {
        for (i <- 0 until t.dim) t(i) = 1.0 / (1.0 + exp (-(a + b*t(i))))
        t
    } // logisticV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the odds of an event occurring (e.g., success, 1).
     *  The inverse of the 'logit' function is the standard logistic function
     *  (sigmoid function).
     *  @param p  the probability, a number between 0 and 1.
     */
    def logit (p: Double): Double = log (p / (1.0 - p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the 'sigmoid' function at 't'.  This is a special case of
     *  the logistic function, where 'a = 0' and 'b = 1'.  It is also referred to as
     *  the standard logistic function.  It is also the inverse of the logit function.
     *  @param t  the sigmoid function argument
     */
    def sigmoid (t: Double): Double = 1.0 / (1.0 + exp (-t))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of values of the 'sigmoid' function applied to vector 't.'
     *  The values are computed in-place.
     *  @param t  the sigmoid function vector argument
     */
    def sigmoidV (t: VectoD): VectoD =
    {
        for (i <- 0 until t.dim) t(i) = 1.0 / (1.0 + exp (-t(i)))
        t
    } // sigmoidV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the 'gaussian' function at 't'.
     *  @param t  the gaussian function argument
     */
    def gaussian (t: Double): Double = exp (-t * t)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of values of the 'gaussian' function applied to vector 't.'
     *  The values are computed in-place.
     *  @param t  the gaussian function vector argument
     */
    def gaussianV (t: VectoD): VectoD =
    {
        for (i <- 0 until t.dim) t(i) = exp (-t(i) * t(i))
        t
    } // gaussianV

} // ActivationFunc object

