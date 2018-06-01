
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Mon May 21 15:07:29 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Markov Decision Process (MDP)- Value Iteration Algorithm
 *  @see pdfs.semanticscholar.org/968b/ab782e52faf0f7957ca0f38b9e9078454afe.pdf
 */

package scalation.state

import scala.math.{abs, max}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.tenalgebra.Tensor3D
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDP` class provides Markov Decision Processes.
 *  @param t    the transitional conditional probability t(s1, a, s2) = P(s2 | s1, a),
 *              where s1, s2 in S, a in A
 *  @param r    the reward tensor r(s1, a, s2)
 *  @param g    the discount factor in (0, 1)
 *  @param thr  the threshold (stop when v changes little)
 */
class MDP (t: Tensor3D, r: Tensor3D, g: Double = 0.9, thr: Double = 1E-4)
{
    private val DEBUG  = true                            // debug flag
    private val MAX_IT = 100                             // maximum number of iteration
    private val ns     = t.dim1                          // number states S = {0, 1, ... n-1}
    private val na     = t.dim2                          // number of actions A = {0, 1, ... p-1}
    private val rs     = 0 until ns                      // state range
    private val ra     = 0 until na                      // action range
    private val pi     = new VectorI (ns)                // policy maps each state to an (optimal) action

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine optimal values for the state value function 'v' and the state-action
     *  value function 'q'.
     */
    def optimize: (VectoD, MatriD) =
    {
        val v   = new VectorD (ns)                       // state value function/vector
        val q   = new MatrixD (ns, na)                   // state-action value function/matrix
        var it  = 0                                      // iteration counter
        var del = 0.0                                    // maximum value change for current iteration
        do {
            del = 0.0                                    // reset change for this iteration
            for (s <- rs) {
                val vs = v(s)                            // save initial value of state
                if (DEBUG) println (s"update v($s) = $vs")
                for (a <- ra) {
                    val vec_t = t(s, a)                  // transition vector
                    val vec_r = r(s, a) + v * g          // reward vector
                    q(s, a) = vec_t dot vec_r            // sum of products
                } // for
                pi(s) = q(s).argmax ()                   // optimal action for state s 
                v(s)  = q(s, pi(s))                      // value of optimal action for state s
                del  = max (del, abs (vs - v(s)))        // update change if larger
            } // for
            if (DEBUG) println (s"v = $v")
            it += 1
        } while (it < MAX_IT && del > thr)
        (v, q)
    } // optimize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the optimal policy, i.e., the best action for each state.
     */
    def getPi: VectoI = pi

} // MDP class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDPTest` object is used to test the `MDP` class.
 *  > runMain scalation.state.MDPTest
 */
object MDPTest extends App
{
    // transition probabilities                        sheet row column
    val t = Tensor3D ((3, 2, 3), 0.1, 0.2, 0.3,        // 0   0   0 - 2
                                 0.4, 0.5, 0.6,        // 0   1   0 - 2

                                 0.7, 0.8, 0.9,        // 1   0   0 - 2
                                 0.10, 0.11, 0.12,     // 1   1   0 - 2

                                 0.13, 0.14, 0.15,     // 2   0   0 - 2
                                 0.16, 0.17, 0.18)     // 2   1   0 - 2

    // rewards                                         sheet row  column
    val r = Tensor3D ((3, 2, 3), 1, 2, 3,              // 0   0   0 - 2
                                 4, 5, 6,              // 0   1   0 - 2

                                 7, 8, 9,              // 1   0   0 - 2
                                 10, 11, 12,           // 1   1   0 - 2

                                 13, 14, 15,           // 2   0   0 - 2
                                 16, 17, 18)           // 2   1   0 - 2

    println (s"t = $t")
    println (s"r = $r")

    val mdp = new MDP (t, r)
    val (v, q) = mdp.optimize
    banner ("state value function")
    println ("v = " + v)

    banner ("state-action value function")
    println ("q = " + q)

    banner ("optimal policy vector")
    println ("pi = " + mdp.getPi)

} // MDPTest object

