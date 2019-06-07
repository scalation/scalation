
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Sun Jan 27 15:34:08 EST 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, VectoD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StoppingRule` class provides a stopping rule to terminating the iterative
 *  steps in an optimization early.
 *  @param up_limit   the stopping rule limit (number of increasing steps allowed)
 */
class StoppingRule (up_limit: Int = 4)
{
    private val EPSILON            = 1E-7                                 // number close to zero
    private var up                 = 0                                    // consecutive steps up
    private var b_best:  VectoD    = null                                 // best parameter vector for Perceptron
    private var bb_best: NetParams = null                                 // best parameters for neurals networks

    private var sse0     = Double.MaxValue                                // previous sum of squared errors
    private var sse_best = Double.MaxValue                                // best sse, so far

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Stop when too many steps have the cost measure (e.g., sse) increasing.
     *  Signal a stopping condition by returning the best parameter vector, else null.
     *  @param b    the current value of the parameter vector
     *  @param sse  the current value of cost measure (e.g., sum of sqaured errors)
     */
    def stopWhen (b: VectoD, sse: Double): (VectoD, Double) =
    {
        if (sse > sse0 + EPSILON) up += 1                                 // getting worse
        else {                                                            // getting better
            up = 0
            if (sse < sse_best) { b_best = b.copy; sse_best = sse }       // lower see => save as best
        } // if
        sse0 = sse                                                        // make current the previous
        if (up > up_limit) (b_best, sse_best) else (null, sse_best)       // if at limit, return best
    } // stopWhen

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Stop when too many steps have the cost measure (e.g., sse) increasing.
     *  Signal a stopping condition by returning the best parameter vector, else null.
     *  @param b    the current parameter value (weights and biases)
     *  @param sse  the current value of cost measure (e.g., sum of sqaured errors)
     */
    def stopWhen (b: NetParams, sse: Double): (NetParams, Double) =
    {
        if (sse > sse0 + EPSILON) up += 1                                 // getting worse
        else {                                                            // getting better
            up = 0
            if (sse < sse_best) {                                         // lower see => save as best
                bb_best = for (l <- b.indices) yield b(l).copy            // copy for each layer l
                sse_best = sse
            } // if
        } // if
        sse0 = sse                                                        // make current the previous
        if (up > up_limit) (bb_best, sse_best)                            // if at limit, return best
        else               (null, sse_best)                               // return null => continue
    } // stopWhen

} // StoppingRule class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StoppingRule` companion object provides a function for computing 'sse'.
 */
object StoppingRule
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squared errors (sse).
     *  @param y   the actual response/output vector
     *  @param yp  the predicted response/output vector
     */
    def sseF (y: VectoD, yp: VectoD): Double =
    {
        val e = y - yp                                                    // error vector
        e.normSq
    } // sseF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squared errors (sse).
     *  @param y   the actual response/output matrix
     *  @param yp  the predicted response/output matrix
     */
    def sseF (y: MatriD, yp: MatriD): Double =
    {
        val ee = y - yp                                                   // error matrix
        ee.normFSq
    } // sseF

} // StoppingRule object

