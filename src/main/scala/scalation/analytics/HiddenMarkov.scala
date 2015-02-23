
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sat Nov 16 14:32:49 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.log

import scalation.linalgebra.{MatrixD, VectorD, VectorI}
import scalation.random.ProbabilityVec

// U N D E R   D E V E L O P M E N T 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HiddenMarkov` classes provides Hidden Markov Models (HMM).  An HMM model
 *  consists of a probability vector 'pi' and probability matrices 'a' and 'b'.
 *  The discrete-time system is characterized by a hidden 'state(t)' and an
 *  'observed(t)' symbol at time 't'.
 *  <p>
 *      pi(j)   = P(state(t) = j)
 *      a(i, j) = P(state(t+1)  = j|state(t) = i) 
 *      b(i, k) = P(observed(t) = k|state(t) = i)
 *  <p>
 *  @see http://www.cs.sjsu.edu/faculty/stamp/RUA/HMM.pdf
 *  @param ob  the observation vector
 *  @param m   the number of observation symbols
 *  @param n   the number of states in the model
 */
class HiddenMarkov (ob: VectorI, m: Int, n: Int)
{
    private val MIT = 1000                         // Maximum ITerations
    private val tt  = ob.dim                       // the number of observations
    private val pvm = ProbabilityVec (m)           // probability generator (dim = m)
    private val pvn = ProbabilityVec (n)           // probability generator (dim = n)

    private val pi  = pvn.gen                      // state probability vector
    private val a   = new MatrixD (n, n)           // state transition probability matrix
    private val b   = new MatrixD (n, m)           // observation probability matrix

    private val c   = new VectorD (tt)             // multipliers for scaling
    private val rng = 0 until n                    // range
    private val alp = new MatrixD (tt, n)          // alpha matrix
    private val bet = new MatrixD (tt, n)          // beta matrix
    private val gam = new MatrixD (tt, n)          // gamma matrix
    private val gg  = Array.ofDim [MatrixD] (tt)   // array of gamma matrices

    for (t <- 0 until tt) gg(t) = new MatrixD (n, n)

    for (i <- rng) { a(i) = pvn.gen; b(i) = pvm.gen }   // initialize a and b matrices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The alpha-pass: a forward pass from time t = 0 to tt-1 that computes
     *  alpha 'alp'. 
     */
    def alp_pass ()
    {

        for (i <- rng) {
            alp(0, i) = pi(i) * b(i, ob(0))                // compute alpha_0 (at time t = 0)
        } // for
        c(0) = 1.0 / alp(0).sum
        alp(0) *= c(0)                                     // scale alpha_0 (at time t = 0)

        for (t <- 1 until tt) {                            // compute alpha_t (at time t)
            for (i <- rng) {
                alp(t, i) = 0.0
                for (j <- rng) {
                    alp(t, i) += alp(t-1, j) * a(j, i)
                } // for
                alp(t, i) *= b(i, ob(t))
            } // for
            c(t) = 1.0 / alp(t).sum
            alp(t) *= c(t)                                 // scale alpha_t (at time t)
        } // for
    } // alp_pass

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The beta-pass: a backward pass from time t = tt-1 to 0 that computes
     *  beta 'bet'.
     */
    def bet_pass ()
    {
        for (i <- rng) bet(tt-1, i) = c(tt-1)              // initialize to c at tt-1
        
        for (t <- tt-2 to 0 by -1) {
            for (i <- rng) {
                bet(t, i) = 0.0
                for (j <- rng) {
                    bet(t, i) += a(i, j) * b(j, ob(t+1)) * bet(t+1, i)
                } // for
            } // for
            bet(t) *= c(t)
        } // for
    } // bet_pass

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The gamma-pass: a forward pass from time t = 0 to tt-2 that computes
     *  gamma 'gam'. 
     */
    def gam_pass ()
    {
        for (t <- 0 until tt-1) {                          // compute alpha_t (at time t)

            var den = 0.0
            for (i <- rng; j <- rng) {
                den += alp(t, i) * a(i, j) * b(j, ob(t+1)) * bet(t+1, j)
            } // for

            for (i <- rng) {
                gam(t, i) = 0.0
                for (j <- rng) {
                    gg(t)(i, j) = alp(t, i) * a(i, j) * b(j, ob(t+1)) * bet(t+1, j) / den
                    gam(t, i)  += gg(t)(i, j)
                } // for
            } // for
        } // for
    } // gam_pass

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Re-estimate the probability vector 'pi' and the probability matrices
     *  'a' and 'b'.
     */
    def reestimate ()
    {
        for (i <- rng) {

            pi(i) = gam(0, i)                                  // re-estimate pi

            for (j <- rng) {
                var num = 0.0
                var den = 0.0
                for (t <- 0 until tt-1) {
                    num += gg(t)(i, j)
                    den += gam(t, i)
                } // for
                a(i, j) = num / den                            // re-estimate a
            } // for

            for (j <- 0 until m) {
                var num = 0.0
                var den = 0.0
                for (t <- 0 until tt-1) {
                    if (ob(t) == j) num += gam(t, i)
                    den += gam(t, i)
                } // for
                b(i, j) = num / den                            // re-estimate b
            } // for

        } // for
    } // reestimate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the probability of the observation vector 'ob' given
     *  the model 'pi, 'a' and 'b'.
     */
    def logProb (): Double =
    {
        var logP = 0.0
        for (t <- 0 until tt) logP += log (c(t))
        -logP
    } // logProb

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the Hidden Markov Model using the observation vector 'ob' to
     *  determine the model 'pi, 'a' and 'b'.
     */
    def train (): Tuple3 [VectorD, MatrixD, MatrixD] =
    {
        var oldLogPr = 0.0
        for (it <- 0 to MIT) {                    // up to Maximum ITerations
            val logPr = logProb ()                // compute the new log probability
            if (logPr > oldLogPr) {
                oldLogPr = logPr                  // improvement => continue
                alp_pass ()                       // alpha-pass
                bet_pass ()                       // beta-pass
                gam_pass ()                       // gamma-pass
                reestimate ()                     // re-estimate the model (pi, a, b)
            } else {
                println ("train: HMM model converged after " + it + " iterations")
                return (pi, a, b)                 // return the trained model
            } // if
        } // for
        println ("train: HMM model did not converged after " + MIT + " iterations")
        (pi, a, b)
    } // train

} // HiddenMarkov


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HiddenMarkovTest` object is used to test the `HiddenMarkov` class.
 */
object HiddenMarkovTest extends App
{
    val ob = VectorI (0, 1, 0, 2)
    val hmm = new HiddenMarkov (ob, 3, 2)
    println ("Train the Hidden Markov Model")
    println ("HMM model = " + hmm.train ())

} // HiddenMarkovTest object

