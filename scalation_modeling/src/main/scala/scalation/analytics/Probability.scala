
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Thu Oct 24 11:59:36 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.abs

import scalation.linalgebra.{MatrixD, VectorD, VectorI}
import scalation.linalgebra.MatrixD.outer
import scalation.math.{logb, log2}
import scalation.plot.Plot
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Probability` object provides methods for operating on univariate and
 *  bivariate probability distributions of discrete random variables 'X' and 'Y'.
 *  A probability distribution is specified by its probability mass functions (pmf)
 *  stored either as a "probability vector" for a univariate distribution or
 *  a "probability matrix" for a bivariate distribution.
 *  <p>
 *      joint probability matrix:       pxy(i, j)  = P(X = x_i, Y = y_j)
 *      marginal probability vector:    px(i)      = P(X = x_i)
 *      conditional probability matrix: px_y(i, j) = P(X = x_i|Y = y_j)
 *  <p>
 *  In addition to computing joint, marginal and conditional probabilities,
 *  methods for computing entropy and mutual information are also provided.
 *  Entropy provides a measure of disorder or randomness.  If there is
 *  little randomness, entropy will close to 0, while when randomness is
 *  high, entropy will be close to, e.g., 'log2 (px.dim)'.  Mutual information
 *  provides a robust measure of dependency between random variables
 *  (contrast with correlation).
 *  @see scalation.stat.StatVector
 */
object Probability
       extends Error
{
    private val EPSILON = 1E-9              // a number close to zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the vector 'px' is a legitimate "probability vector".
     *  The elements of the vector must be non-negative and add to one.
     *  @param px  the probability vector
     */
    def isProbability (px: VectorD): Boolean = px.min () >= 0.0 && abs (px.sum - 1.0) < EPSILON

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the matrix 'pxy' is a legitimate joint "probability matrix".
     *  The elements of the matrix must be non-negative and add to one.
     *  @param pxy  the probability matrix
     */
    def isProbability (pxy: MatrixD): Boolean = pxy.min () >= 0.0 && abs (pxy.sum - 1.0) < EPSILON

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given two independent random variables 'X' and 'Y', compute their
     *  "joint probability", which is the outer product of their probability
     *  vectors 'px' and 'py', i.e., P(X = x_i, Y = y_j).
     */
    def jointProbXY (px: VectorD, py: VectorD): MatrixD = outer (px, py)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the "marginal probability"
     *  for random variable 'X', i.e, P(X = x_i).
     *  @param pxy  the probability matrix
     */
    def margProbX (pxy: MatrixD): VectorD =
    {
        val px = new VectorD (pxy.dim1)
        for (i <- 0 until pxy.dim1) px(i) = pxy(i).sum
        px
    } // margProbX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the "marginal probability"
     *  for random variable 'Y', i.e, P(Y = y_j).
     *  @param pxy  the probability matrix
     */
    def margProbY (pxy: MatrixD): VectorD =
    {
        val py = new VectorD (pxy.dim2)
        for (j <- 0 until pxy.dim2) py(j) = pxy.col(j).sum
        py
    } // margProbY

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the "conditional probability"
     *  for random variable 'X' given random variable 'Y', i.e, P(X = x_i|Y = y_j).
     *  @param pxy  the joint probability matrix
     */
    def condProbX_Y (pxy: MatrixD): MatrixD =
    {
        val px   = margProbX (pxy)
        val px_y = new MatrixD (pxy.dim1, pxy.dim2)
        for (i <- 0 until pxy.dim1) px_y(i) = pxy(i) / px(i)
        px_y
    } // condProbX_Y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the "conditional probability"
     *  for random variable 'Y' given random variable 'X', i.e, P(Y = y_j|X = x_i).
     *  @param pxy  the joint probability matrix
     */
    def condProbY_X (pxy: MatrixD): MatrixD =
    {
        val py   = margProbY (pxy)
        val py_x = new MatrixD (pxy.dim2, pxy.dim1)
        for (j <- 0 until pxy.dim2) py_x(j) = pxy.col(j) / py(j)
        py_x
    } // condProbY_X

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a probability vector 'px', compute the "entropy" of random
     *  variable 'X'.
     *  @see http://en.wikipedia.org/wiki/Entropy_%28information_theory%29
     *  @param px  the probability vector
     */
    def entropy (px: VectorD): Double =
    {
        var sum = 0.0
        for (p <- px if p > 0.0) sum -= p * log2 (p)
        sum
    } // entropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a probability vector 'px', compute the "base-k entropy" of random
     *  variable 'X'.
     *  @see http://en.wikipedia.org/wiki/Entropy_%28information_theory%29
     *  @param px  the probability vector
     */
    def entropy_k (px: VectorD): Double =
    {
        val k = px.dim           // let the base k = # elements in probability vector
        var sum = 0.0
        for (p <- px if p > 0.0) sum -= p * logb (k, p)
        sum
    } // entropy_k

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the "joint entropy"
     *  of random variables 'X' and 'Y'.
     *  @param pxy  the joint probability matrix
     */
    def entropy (pxy: MatrixD): Double =
    {
        var sum = 0.0
        for (i <- 0 until pxy.dim1; j <- 0 until pxy.dim2) {
            val p = pxy(i, j)
            if (p > 0.0) sum -= p * log2 (p)
        } // for
        sum
    } // entropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy' and a conditional probability
     *  matrix 'py_x', compute the "conditional entropy" of random variable 'X'
     *  given random variable 'Y'.
     *  @param pxy   the joint probability matrix
     *  @param px_y  the conditional probability matrix
     */
    def entropy (pxy: MatrixD, px_y: MatrixD): Double =
    {
        if (pxy.dim1 != px_y.dim1 || pxy.dim2 != px_y.dim2)
            flaw ("entropy", "joint and conditional probability matrices are not compatible")

        var sum = 0.0
        for (i <- 0 until pxy.dim1; j <- 0 until pxy.dim2) {
            val p = pxy(i, j)
            if (p > 0.0) sum -= p * log2 (px_y(i, j))
        } // for
        sum
    } // entropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the mutual information
     *  for random variables 'X' and 'Y'.
     *  @param pxy  the probability matrix
     */
    def muInfo (pxy: MatrixD): Double =
    {
        val px = margProbX (pxy)
        val py = margProbY (pxy)
        var sum = 0.0
        for (i <- 0 until pxy.dim1; j <- 0 until pxy.dim2) {
            val p = pxy(i, j)
            sum  += p * log2 (p / (px(i) * py(j)))
        } // for
        sum
    } // muInfo

} // Probability object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**The `ProbabilityTest` object is used to test the `Probability` object.
 */
object ProbabilityTest extends App
{
    import Probability._

    // coin experiment: probability for 0, 1, 2 heads, when flipping 2 coins
    val px = VectorD (.25, .5, .25)

    // dice experiment: probability for 2, 3, ... 11, 12, when rolling 2 dice
    val py = VectorD (1/36.0, 2/36.0, 3/36.0, 4/36.0, 5/36.0, 6/36.0,
                      5/36.0, 4/36.0, 3/36.0, 2/36.0, 1/36.0)

    // joint probability for coin and dice experiments
    val pxy  = jointProbXY (px, py)

    println ("isProbability (" + px + ") = " + isProbability (px))
    println ("isProbability (" + py + ") = " + isProbability (py))
    println ("joint probability pxy = " + pxy)
    println ("isProbability (pxy) = " + isProbability (pxy))

    val x = VectorD.range (0, 3)
    new Plot (x, px)                          // plot the pmf for random variable X
    val y = VectorD.range (2, 13)
    new Plot (y, py)                          // plot the pmf for random variable Y

    val mpx  = margProbX (pxy)                // marginal probability should be the same as px
    val mpy  = margProbY (pxy)                // marginal probability should be the same as py
    val px_y = condProbX_Y (pxy)              // conditional probability P(X = x_i|Y = y_j)
    val py_x = condProbY_X (pxy)              // conditional probability P(Y = y_j|X = x_i)

    println ("marginal probability mpx     = " + mpx)
    println ("marginal probability mpy     = " + mpy)
    println ("conditional probability px_y = " + px_y)
    println ("conditional probability py_y = " + py_x)

    val hx   = entropy (px)                   // entropy of random variable X
    val hy   = entropy (py)                   // entropy of random variable Y
    val hkx  = entropy_k (px)                 // entropy_k of random variable X
    val hky  = entropy_k (py)                 // entropy_k of random variable Y
    val hxy  = entropy (pxy)                  // joint entropy of random variables X and Y
    val hx_y = entropy (pxy, px_y)            // conditional entropy of random variables X given Y
    val hy_x = entropy (pxy.t, py_x)          // conditional entropy of random variables Y given X
    val ixy  = muInfo (pxy)                   // mutual information of random variables X given Y

    println ("entropy hx               = " + hx)
    println ("entropy hy               = " + hy)
    println ("entropy_k hkx            = " + hkx)
    println ("entropy_k hky            = " + hky)
    println ("joint entropy hxy        = " + hxy)
    println ("conditional entropy hx_y = " + hx_y)
    println ("conditional entropy hy_x = " + hy_x)
    println ("mutual information ixy   = " + ixy)

} // ProbabilityTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**The `ProbabilityTest2` provides upper bound for 'entropy' and 'entropy_k'.
 */
object ProbabilityTest2 extends App
{
    import Probability._

    for (k <- 2 to 20) {
        println ("max entropy for k   = " + k + " \tis " + (-log2 (1.0/k.toDouble)))
    } // for

    for (k <- 2 to 20) {
        println ("max entropy_k for k = " + k + " \tis " + (-logb (k, 1.0/k.toDouble)))
    } // for

} // ProbabilityTest2

