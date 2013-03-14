
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  John Miller
 * @version 1.0
 * @date    Wed Dec 30 15:13:32 EST 2009
 * @see     LICENSE (MIT style license file).
 * @compile scalac -cp ../../classes -d classes MarkovQueue.scala
 * @run     scala -cp ../../classes:classes state.MarkovQueue
 */

package state

import scalation.state.MarkovC
import scalation.linalgebra.MatrixD

/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * This object finds the steady-state solution and simulates a simple Markovian
 * Queue (an M/M/1/2) where the arrival rate is 4 and the service rate is 5.
 * Note, each diagonal value must make their row sum to 0.
 * @see scalation.state.MarkovTest, scalation.state.MarkovCTest for example
 * test code for discrete-time and continuous-time Markov chains, respectively.
 */
object MarkovQueue extends App
{
    /** The transition rate matrix
     */
    private val trMatrix = new MatrixD ((3, 3), -4.,  4.,  0.,
                                                 5., -9.,  4.,
                                                 0.,  5., -5.)

    println ("\nTransition Rate Matrix trMatrix = " + trMatrix + "\n")

    /** The continuous-time Markov Chain
     */
    private val mc = new MarkovC (trMatrix)

    println ("\nContinuous-Time Markov Chain mc = " + mc + "\n")

    println ("\nContinuous-Time Markov Chain: steady-state solution:")
    println ("\njump matrix  \tj = " + mc.jump)
    println ("\nsteady-state \tp = " + mc.limit)

    println ("\nContinuous-Time Markov Chain: simulation:")
    mc.simulate (0, 100.)

} // MarkovQueue object

