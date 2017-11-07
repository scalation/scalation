
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Nov 16 14:32:49 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.log

import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.random.ProbabilityVec

// U N D E R   D E V E L O P M E N T 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HiddenMarkov` classes provides Hidden Markov Models (HMM).  An HMM model
 *  consists of a probability vector 'pi' and probability matrices 'a' and 'b'.
 *  The discrete-time system is characterized by a hidden 'state(t)' and an
 *  'observed(t)' symbol at time 't'.
 *  <p>
 *      pi(j)   = P(state(t) = j)
 *      a(i, j) = P(state(t+1)  = j | state(t) = i) 
 *      b(i, k) = P(observed(t) = k | state(t) = i)
 *  <p>
 *      model (pi, a, b)
 *  <p>
 *  @see www.cs.sjsu.edu/faculty/stamp/RUA/HMM.pdf
 *  @param ob  the observation vector
 *  @param m   the number of observation symbols
 *  @param n   the number of (hidden) states in the model
 *  @param pi  the probabilty vector for the initial state
 *  @param a   the state transition probability matrix (n-by-n)
 *  @param b   the observation probability matrix (n-by-m)
 */
class HiddenMarkov (ob: VectorI, m: Int, n: Int, private var pi: VectorD = null,
                                                 private var a:  MatrixD = null,
                                                 private var b:  MatrixD = null)
      extends Classifier
{
    private val MIT = 1000                           // Maximum ITerations
    private val tt  = ob.dim                         // the number of observations
    private val pvm = ProbabilityVec (m)             // probability generator (dim = m)
    private val pvn = ProbabilityVec (n)             // probability generator (dim = n)

    private val c   = new VectorD (tt)               // multipliers for scaling
    private val rng = 0 until n                      // range
    private val gam = new MatrixD (tt, n)            // gamma matrix
    private val gg  = Array.ofDim [MatrixD] (tt)     // array of gamma matrices

    for (t <- 0 until tt) gg(t) = new MatrixD (n, n)

    // FIX - need better initialization - see p. 13
    if (pi == null) {
        pi = pvn.gen                                 // initialize the state probability vector
    } // if
    if (a == null) {
        a = new MatrixD (n, n)
        for (i <- rng) a(i) = pvn.gen                // initialize state transition probability matrix a
    } // if
    if (b == null) {
        b = new MatrixD (n, m)
        for (i <- rng) b(i) = pvm.gen                // initialize observation probability matrix b
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The alpha-pass: a forward pass from time 't = 0' to 'tt-1' that computes
     *  alpha 'alp'. 
     */
    def alp_pass (): MatrixD =
    {
        val alp = new MatrixD (tt, n          )            // alpha matrix
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
        alp
    } // alp_pass

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The beta-pass: a backward pass from time 't = tt-1' to 0 that computes
     *  beta 'bet'.
     */
    def bet_pass (): MatrixD =
    {
        val bet = new MatrixD (tt, n)                      // beta matrix
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
        bet
    } // bet_pass

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The gamma-pass: a forward pass from time 't = 0' to 'tt-2' that computes
     *  gamma 'gam'. 
     */
    def gam_pass (alp: MatrixD, bet: MatrixD)
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
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def train (testStart: Int, testEnd: Int) { train2 (testStart, testEnd) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the Hidden Markov Model using the observation vector 'ob' to
     *  determine the model 'pi, 'a' and 'b' and return the model.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def train2 (testStart: Int, testEnd: Int): Tuple3 [VectorD, MatrixD, MatrixD] =
    {
        var oldLogPr = 0.0
        for (it <- 0 to MIT) {                    // up to Maximum ITerations
            val logPr = logProb ()                // compute the new log probability
            if (logPr > oldLogPr) {
                oldLogPr = logPr                  // improvement => continue
                val alp = alp_pass ()             // alpha-pass
                val bet = bet_pass ()             // beta-pass
                gam_pass (alp, bet)               // gamma-pass
                reestimate ()                     // re-estimate the model (pi, a, b)
            } else {
                println ("train: HMM model converged after " + it + " iterations")
                return (pi, a, b)                 // return the trained model
            } // if
        } // for
        println ("train: HMM model did not converged after " + MIT + " iterations")
        (pi, a, b)
    } // train2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the (hidden) state space.
     */
    def size: Int = n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new discrete data vector z, determine which class it belongs to,
     *  returning the best class, its name and its relative probability.
     *  @param z  the vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) = ???   // FIX - implement

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new continuous data vector z, determine which class it belongs to,
     *  returning the best class, its name and its relative probability.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) = classify (z.toInt)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset global variables.  So far, not needed.
     */
    def reset () {}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the training with a test-set and return the fraction
     *  of correct classifications.
     *  @param testStart  the beginning of test region (inclusive).
     *  @param testEnd    the end of test region (exclusive).
     */
    def test (testStart: Int, testEnd: Int): Double = ???   // FIX - implement

} // HiddenMarkov


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HiddenMarkovTest` object is used to test the `HiddenMarkov` class.
 *  Given model '(pi, a, b)', determine the probability of the observations 'ob'.
 *  @see www.cs.sjsu.edu/~stamp/RUA/HMM.pdf (exercise 1).
 *  > run-main scalation.analytics.classifieu.HiddenMarkovTest
 */
object HiddenMarkovTest extends App
{
    val pi = VectorD (0.0, 1.0)                                   // probability vector for initial state

    val a = new MatrixD ((2, 2), 0.7, 0.3,                        // state transition matrix
                                 0.4, 0.6)

    val b = new MatrixD ((2, 3), 0.1, 0.4, 0.5,                   // observation probability matrix
                                 0.7, 0.2, 0.1)
    val ob = VectorI (1, 0, 2)                                    // observations: M(1), S(0), L(2)

    val nSymbols = b.dim2                                         // number of observable symbols |{S, M, L}|    
    val nStates  = a.dim1                                         // number of states |{H, C}|
    val hmm = new HiddenMarkov (ob, nSymbols, nStates, pi, a, b)  // Hidden Markov Model (HMM) 
    val alp = hmm.alp_pass ()
    println ("ob  = " + ob)                                       // observations
    println ("alp = " + alp)                                      // alpha matrix
    println ("P(ob|model) = " + alp(ob.dim-1).sum)                // probability of observations given model

} // HiddenMarkovTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HiddenMarkovTest2` object is used to test the `HiddenMarkov` class.
 *  Train the model (pi, a, b) based on the observed data.
 *  > run-main scalation.analytics.classifier.HiddenMarkovTest2
 */
object HiddenMarkovTest2 extends App
{
    val ob  = VectorI (0, 1, 0, 2)                                // observations
    val hmm = new HiddenMarkov (ob, 3, 2)                         // model (pi, a, b) to be determined
    println ("Train the Hidden Markov Model")
    println ("HMM model = " + hmm.train2 (0, 0))

} // HiddenMarkovTest2 object

