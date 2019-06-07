
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Thu Oct 24 11:59:36 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.math.abs

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.linalgebra.MatrixD.outer
import scalation.math.{logb, log2}
import scalation.plot.Plot
import scalation.util.{banner, Error}

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
    private val DEBUG   = false                           // debug flag
    private val EPSILON = 1E-9                            // a number close to zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the vector 'px' is a legitimate "probability vector".
     *  The elements of the vector must be non-negative and add to one.
     *  @param px  the probability vector
     */
    def isProbability (px: VectoD): Boolean = px.min () >= 0.0 && abs (px.sum - 1.0) < EPSILON

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the matrix 'pxy' is a legitimate joint "probability matrix".
     *  The elements of the matrix must be non-negative and add to one.
     *  @param pxy  the probability matrix
     */
    def isProbability (pxy: MatriD): Boolean = pxy.min () >= 0.0 && abs (pxy.sum - 1.0) < EPSILON

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequency of occurrence of each distinct value within integer vector 'y',
     *  (e.g., result 'nu' = (5, 9) didn't play 5, played 9).
     *  Restriction: 'y' may not contain negative integer values.
     *  @param y     the feature/columne vector of integer values whose frequency counts are sought
     *  @param k     the maximum value of y + 1
     *  @param idx_  the index positions within y (if null, use all index positions)
     */
    def frequency (y: VectoI, k: Int, idx_ : Ints = null): VectoI =
    {
        val idx = if (idx_ == null) IndexedSeq.range (0, y.dim) else idx_
        val nu = new VectorI (k)
        for (i <- idx) nu(y(i)) += 1
        nu
    } // frequency

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequency of occurrence in vector 'x' of value 'vl' for each of 'y's
     *  classification values, e.g., 'x' is column 2 (Humidity), 'vl' is 1 (High)) and
     *  'y' can be 0 (no) or 1 (yes).  Also, determine the fraction of training cases
     *  where the feature has this value (e.g., fraction where Humidity is High = 7/14).
     *  @param x     the feature/column vector (e.g., column j of matrix)
     *  @param y     the response/classification vector
     *  @param k     the maximum value of y + 1
     *  @param vl    one of the possible branch values for feature x (e.g., 1 (High))
     *  @param idx_  the index positions within x (if null, use all index positions)
     */
    def frequency (x: VectoI, y: VectoI, k: Int, vl: Int, idx_ : Ints): (Double, VectoI) =
    {
        val idx = if (idx_ == null) IndexedSeq.range (0, y.dim) else idx_
        val nu  = new VectorI (k)                                 // frequency counts
        var cnt = 0                                               // count for branch value
        for (i <- idx if x(i) == vl) { nu(y(i)) += 1; cnt += 1 }
        if (DEBUG) println (s"frequency: k = $k, vl = $vl, nu = $nu")
        (cnt.toDouble / idx.size, nu)                             // return fraction and frequency vector
    } // frequency

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the frequency of occurrence in vector 'x' of value 'vl' for each of 'y's
     *  classification values, e.g., 'x' is column 2 (Humidity), 'vl' is 1 (High)) and
     *  'y' can be 0 (no) or 1 (yes).  Also, determine the fraction of training cases
     *  where the feature has this value (e.g., fraction where Humidity is High = 7/14).
     *  This method works for vectors with integer or continuous values.
     *  @param x      the feature/column vector (e.g., column j of matrix)
     *  @param y      the response/classification vector
     *  @param k      the maximum value of y + 1
     *  @param vl     one of the possible branch values for feature x (e.g., 1 (High))
     *  @param idx_   the index positions within x (if null, use all index positions)
     *  @param cont   whether feature/variable x is to be treated as continuous
     *  @param thres  the splitting threshold for features/variables treated as continuous
     */
    def frequency (x: VectoD, y: VectoI, k: Int, vl: Int, idx_ : Ints,
                   cont: Boolean, thres: Double): (Double, VectoI) =
    {
        val idx = if (idx_ == null) IndexedSeq.range (0, y.dim) else idx_
        val nu  = new VectorI (k)                                 // frequency counts
        var cnt = 0                                               // count for the value branch
        if (cont) {
            if (vl == 0) {
                for (i <- idx) if (x(i) <= thres) { nu(y(i)) += 1; cnt += 1 }
            } else {
                for (i <- idx) if (x(i) >  thres) { nu(y(i)) += 1; cnt += 1 }
            } // if
        } else {
            for (i <- idx) if (x(i) == vl) { nu(y(i)) += 1; cnt += 1 }
        } // if
        if (DEBUG) println (s"frequency: k = $k, vl = $vl, cont = $cont, thres = $thres, nu = $nu")
        (cnt.toDouble / idx.size, nu)                             // return fraction and frequency vector
    } // frequency

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the total number of occurrence in vector 'x' of value 'vl',  e.g.,
     *  'x' is column 2 (Humidity), 'vl' is 1 (High) matches 7 rows.
     *  This method works for vectors with integer or continuous values.
     *  @param x      the feature/column vector (e.g., column j of matrix)
     *  @param y      the response/classification vector
     *  @param k      the maximum value of y + 1
     *  @param vl     one of the possible branch values for feature x (e.g., 1 (High))
     *  @param idx_   the index positions within x (if null, use all index positions)
     *  @param cont   whether feature/variable x is to be treated as continuous
     *  @param thres  the splitting threshold for features/variables treated as continuous
     */
    def count (x: VectoD, vl: Int, idx_ : Ints, cont: Boolean, thres: Double): Int =
    {
        val idx = if (idx_ == null) IndexedSeq.range (0, x.dim) else idx_
        var cnt = 0                                               // count for the value branch
        if (cont) {
            if (vl == 0) {
                for (i <- idx) if (x(i) <= thres) cnt += 1
            } else {
                for (i <- idx) if (x(i) >  thres) cnt += 1
            } // if
        } else {
            for (i <- idx) if (x(i) == vl) cnt += 1
        } // if
        cnt
    } // count

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best split threshold 'thres' that divides feature/variable 'xj' into
     *  low (<= 'thesh') and high (> 'thres') values such that weighted entropy is minimized.
     *  @param xj    the vector for feature fea (column j of matrix)
     *  @param y     the classification/response vector
     *  @param idx_  the index positions within x (if null, use all index positions)
     *  @param k     the number of classes
     */
    def findSplit (xj: VectoD, y: VectoI, idx_ : Ints = null, k: Int = 2): Double =
    {
        val idx = if (idx_ == null) IndexedSeq.range (0, y.dim) else idx_
        var thres  = -0.0                                                 // keep track of best threshold
        var minEnt = Double.MaxValue                                      // keep track of maximum gain
        val values = xj.distinct                                          // distinct values from vector xj
        values.sort ()                                                    // sort these values into increasing order

        if (DEBUG) println (s"findSplit: possible values for threshold = $values")

        for (i <- 0 until values.dim - 1) {
            val mid            = (values(i) + values(i+1)) / 2.0          // mid point between i and i+1
            val (frac_0, nu_0) = frequency (xj, y, k, 0, idx, true, mid)  // up to threshold (v == 0)
            val (frac_1, nu_1) = frequency (xj, y, k, 1, idx, true, mid)  // beyond threhsold (v == 1)
            val ent = frac_0 * entropy (nu_0) + frac_1 * entropy (nu_1)   // compute entropy for this threshold
            if (DEBUG) println (s"findSplit: entropy for threshold $mid = $ent")
            if (ent < minEnt) {
                thres  = mid                                              // found a better threshold
                minEnt = ent                                              // save better gain
            } // if
        } // for

        if (DEBUG) println (s"findSplit: feature threshold = $thres")
        thres                                                             // save best threshold for this feature
    } // findSplit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a frequency vector, convert it to a probability vector.
     *  @param nu  the frequency vector
     *  @param n   the number of elements in the original vector
     */
    def toProbability (nu: VectoI, n: Int): VectoD = nu.toDouble / n.toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given two independent random variables 'X' and 'Y', compute their
     *  "joint probability", which is the outer product of their probability
     *  vectors 'px' and 'py', i.e., P(X = x_i, Y = y_j).
     */
    def jointProbXY (px: VectoD, py: VectoD): MatriD = outer (px, py)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a joint probability matrix 'pxy', compute the "marginal probability"
     *  for random variable 'X', i.e, P(X = x_i).
     *  @param pxy  the probability matrix
     */
    def margProbX (pxy: MatriD): VectoD =
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
    def margProbY (pxy: MatriD): VectoD =
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
    def condProbX_Y (pxy: MatriD): MatriD =
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
    def condProbY_X (pxy: MatriD): MatriD =
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
    def entropy (px: VectoD): Double =
    {
        var sum = 0.0
        for (p <- px if p > 0.0) sum -= p * log2 (p)
        sum
    } // entropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a frequency vector 'nu', compute the "entropy" of random variable 'X'.
     *  @see http://en.wikipedia.org/wiki/Entropy_%28information_theory%29
     *  @param nu  the frequency vector
     */
    def entropy (nu: VectoI): Double =
    {
        val tot = nu.sum.toDouble
        var sum = 0.0
        for (c <- nu if c > 0) { val p = c / tot; sum -= p * log2 (p) }
        sum
    } // entropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a probability vector 'px', compute the "base-k entropy" of random
     *  variable 'X'.
     *  @see http://en.wikipedia.org/wiki/Entropy_%28information_theory%29
     *  @param px  the probability vector
     */
    def entropy_k (px: VectoD): Double =
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
    def entropy (pxy: MatriD): Double =
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
    def entropy (pxy: MatriD, px_y: MatriD): Double =
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
    def muInfo (pxy: MatriD): Double =
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

import Probability._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**The `ProbabilityTest` object is used to test the `Probability` object.
 */
object ProbabilityTest extends App
{
    // coin experiment: probability for 0, 1, 2 heads, when flipping 2 coins
    val px = VectorD (.25, .5, .25)

    // dice experiment: probability for 2, 3, ... 11, 12, when rolling 2 dice
    val py = VectorD (1/36.0, 2/36.0, 3/36.0, 4/36.0, 5/36.0, 6/36.0,
                      5/36.0, 4/36.0, 3/36.0, 2/36.0, 1/36.0)

    // joint probability for coin and dice experiments
    val pxy  = jointProbXY (px, py)

    println ("isProbability (" + px + ") = " + isProbability (px))
    println ("isProbability (" + py + ") = " + isProbability (py))
    println ("joint probability pxy      = " + pxy)
    println ("isProbability (pxy)        = " + isProbability (pxy))

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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ProbabilityTest3` object is used to test the `Probability` class.
 *  Plot entropy.
 *  > runMain scalation.analytics.classifier.ProbabilityTest3
 */
object ProbabilityTest3 extends App
{
    banner ("Test: ProbabilityTest3: Plot entropy")
    val p = VectorD.range (1, 100) / 100.0
    val h = p.map (p => -p * log2 (p) - (1-p) * log2 (1-p))
    new Plot (p, h)

    val h2 = p.map (q => { val px = VectorD (q, 1-q); entropy (px) })
    new Plot (p, h2)

} // ProbabilityTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ProbabilityTest4` object is used to test the `Probability` class.
 *  Test the 'findSplit' method.
 *  > runMain scalation.analytics.classifier.ProbabilityTest4
 */
object ProbabilityTest4 extends App
{
    import ExampleTennisCont.xy

    val x1  = xy.col(1)
    val x2  = xy.col(2)
    val y   = xy.col (xy.dim2 - 1).toInt
    val idx = Array.range (0, y.dim)
    
    banner ("Test: ProbabilityTest4: 'findSplit' method for x1")
    println ("findSplit for column x1 thres = " + findSplit (x1, y, idx))

    banner ("Test: ProbabilityTest4: 'findSplit' method for x2")
    println ("findSplit for column x2 thres = " + findSplit (x2, y, idx))

} // ProbabilityTest4 object

