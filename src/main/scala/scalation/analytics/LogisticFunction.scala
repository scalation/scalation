
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Dec 28 12:00:07 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.{exp, log}

import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LogisticFunction` object contains Activation functions.
 */
object LogisticFunction
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the logistic function at 't'.
     *  @param t  the logistic function argument
     *  @param a  the shift parameter
     *  @param b  the spread parameter
     */
    def logistic (t: Double, a: Double, b: Double): Double = 1.0 / (1.0 + exp (-(a + b*t)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the logistic function applied to vector 't'.
     *  The values are computed in-place.
     *  @param t  the logistic function argument
     *  @param a  the shift parameter
     *  @param b  the spread parameter
     */
    def logistic (t: VectorD, a: Double, b: Double): VectorD = 
    {
        for (i <- 0 until t.dim) t(i) = 1.0 / (1.0 + exp (-(a + b*t(i))))
        t
    } // logistic

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the odds of an event ocurring (e.g., success, 1).
     *  The inverse of the logit function is the standard logistic function
     *  (sigmoid function).
     *  @param p  the probability, a number between 0 and 1.
     */
    def logit (p: Double): Double = log (p / (1.0 - p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the sigmoid function at 't'.  This is a special case of
     *  the logistic function, where 'a = 0' and 'b = 1'.  It is also referred to as
     *  the standard logistic function.  It is also the inverse of the logit function.
     *  @param t  the sigmoid function argument
     */
    def sigmoid (t: Double): Double = 1.0 / (1.0 + exp (-t))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of values of the sigmoid function applied to vector 't.'
     *  The values are computed in-place.
     *  @param t  the sigmoid function vector argument
     */
    def sigmoid (t: VectorD): VectorD =
    {
        for (i <- 0 until t.dim) t(i) = 1.0 / (1.0 + exp (-t(i)))
        t
    } // sigmoid

} // LogisticFunction

