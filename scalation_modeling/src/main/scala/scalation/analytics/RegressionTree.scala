
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Dong Yu Yu, John Miller
 *  @version 1.6
 *  @date    Wed Nov  7 17:08:17 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.{ArrayBuffer, Queue, Set}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectoI, VectorD, VectorI}
import scalation.random.PermutedVecI
import scalation.stat.Statistic
import scalation.util.{banner, Error}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTree` companion object is used to count the number of leaves.
 */
object RegressionTree
{
    val hp = new HyperParameter;
    hp += ("maxDepth", 5, 5)
    hp += ("threshold", 0.1, 0.1)

    private var nLeaves_ = 0                   // the number of leaves in the tree

    def nLeaves: Int = nLeaves_

    def incLeaves () { nLeaves_ += 1 }

    def resetLeaves () { nLeaves_ = 0 }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionTree` object.
     *  @param x       the data matrix
     *  @param y       the dependent value
     *  @param fname   the names for all features/variables
     *  @param hparam  the hyper-parameters
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = hp): RegressionTree =
    {
        import PredictorMat.pullResponse

        val n = xy.dim2
        if (n < 2) {
            //flaw ("apply", s"dim2 = $n of the 'xy' matrix must be at least 2")
            null
        } else {
            val (x, y) = pullResponse (xy)
            new RegressionTree (x, y, fname, hparam)
       } // if
    } // apply

} // RegressionTree object

import RegressionTree.{nLeaves, incLeaves, resetLeaves}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Class that contains information for a tree node.
 *  @param f        the feature of the node, if it is leaf, contains the feature of its parent
 *  @param branch   the branch value (0 => left, 1 => right)
 *  @param yp       leaf node's prediction for y
 *  @param thresh   the threshold for continuous feature
 *  @param depth    the current depth of the node
 *  @param pthresh  the threshold for parent node
 *  @param pfea     the feature of parent node
 *  @param leaf     `Boolean` value indicate whether is a leaf node
 */
case class Node (f: Int, branch: Int, yp: Double, thresh: Double,
                 depth: Int, pthresh: Double, pfea: Int, leaf: Boolean = false)
{
    val child = new ArrayBuffer [Node] ()                      // children of node

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert node to a string.
     */
    override def toString: String =
    {
        if (child.length == 0)
             s"Leaf (pfeature = x$pfea, branch = $branch, feature = x$f, yp = $yp)"
        else if (depth == 0)
             s"Root (feature = x$f, threshold = $thresh)"
        else
             s"Node (pfeature = x$pfea, branch = $branch, feature = x$f, threshold = $thresh)"
    } // toString

} // Node class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTree` class implements a RegressionTree selecting splitting features
 *  using minimal variance in children nodes. To avoid exponential choices in the selection,
 *  supporting ordinal features currently. Use companion object is recommended for generate
 *  Regression Tree.
 *  @param x            the data vectors stored as rows of a matrix
 *  @param y            the response vector
 *  @param fname_       the names of the model's features/variables
 *  @param hparam       the hyper-parameters for the model
 *  @param curDepth     current depth
 *  @param branchValue  the branchValue for the tree node
 *  @param feature      the feature for the tree's parent node
 */
class RegressionTree (x: MatriD, y: VectoD,
                      fname_ : Strings = null, hparam: HyperParameter = RegressionTree.hp,
                      curDepth: Int = -1, branchValue: Int = -1, feature: Int = -1)
    extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG     = false                                     // debug flag
    private val maxDepth  = hparam ("maxDepth").toInt                 // the depth limit for tree
    private val thres     = hparam ("threshold")                      // the threshold for the tree's parent node
    private val threshold = new Array [PairD] (n)                     // store best splitting threshold for each feature
    private val stream    = 0                                         // the random number stream
    private val permGen   = PermutedVecI (VectorI.range (0, m), stream)

    private var root: Node = null                                     // root node   

    if (DEBUG) println ("Constructing a Regression Tree:")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split gives index of left and right child when spliting in 'thresh'.
     *  @param f       the feature indicator
     *  @param thresh  threshold
     */
    def split (f: Int, thresh: Double): (Array [Int], Array [Int]) =
    {
        val (sLeft, sRight) = (Set [Int] (), Set [Int] ())
        for (i <- x.range1) if (x(i, f) <= thresh) sLeft += i else sRight += i
        (sLeft.toArray, sRight.toArray)
    } // split

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given feature 'f', use fast threshold selection to find an optimal threshold/
     *  split point in O(NlogN) time.
     *  @see people.cs.umass.edu/~domke/courses/sml/12trees.pdf
     *  @param f          the given feature for which the threshold is desired
     *  @param x_f        column f in data matrix
     *  @param subSample  optional, use to select from the range
     */
    def fastThreshold (f: Int, x_f: VectoD, subSample: VectoI = null)
    {
        var thres = 0.0                                               // to hold optimal threshold
        var tSSE  = Double.MaxValue                                   // total sum of squared errors
        var ref   = Array.ofDim [(Double, Int)] (y.dim)               // pair column value with column index
        for (i <- x.range1) ref(i) = (x_f(i), i)                      // assign pairs
        ref = ref.sortBy (_._1)                                       // sort by column value

        val values = x_f.distinct                                     // get distinct values from column x_f
        values.sort ()                                                // sort these values

        val v = new VectorD (values.dim - 1)                          // mid points between all values
        if (v.dim == 0) { threshold(f) = (thres, -1.0); return }      // no values => return early
        for (i <- v.range) v(i) = (values(i) + values(i+1)) / 2.0

        val (totalSum, totalSqr) = (y.sum, y.normSq)                  // total sum and sum of squares
        var sum, square, mean = 0.0                                   // left sum, square and mean
        var (row, valu) = (0, v(0))                                   // candidate split value/threshold

        for (i <- ref.indices) {
            if (ref(i)._1 > valu) {
                val n_i   = ref.size - i                              // number of elements on left
                val rSum  = totalSum - sum                            // right sum
                val rSqr  = totalSqr - square                         // right sum of squares
                val rMean = rSum / n_i                                // right mean
                val lrSSE = square - 2 * sum  * mean  + i   * mean  * mean +
                            rSqr   - 2 * rSum * rMean + n_i * rMean * rMean

                if (lrSSE < tSSE) { tSSE = lrSSE; thres = valu }      // update if lrSSE is smaller
                row += 1
            } // if

            val yi  = y(ref(i)._2)
            sum    += yi                                               // left sum
            square += yi * yi                                          // left sum of squares
            mean    = (yi + i * mean) / (i + 1)                        // left mean
            if (row < v.dim) valu = v(row)
        } // for

        threshold(f) = (thres, tSSE)                                   // return best split point
    } // fastThreshold

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return new 'x' matrix and 'y' vector for next step of constructing regression tree.
     *  @param f     the feature index
     *  @param side  indicator for which side of child is chosen (i.e., 0 for left child)
     */
    def nextXY (f: Int, side: Int): (MatriD, VectoD) =
    {
        val (left, right) = split (f, threshold(f)._1)
        if (side == 0) (x.selectRows (left),  y.select (left))
        else           (x.selectRows (right), y.select (right))
    } // nextXY

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the regression tree by selecting threshold for all the features
     *  in 'yy' (can be used as all the samples or subsamples).
     *  @param yy  only the values in yy will be used in selecting threshold
     */
    def train (yy: VectoD = y): RegressionTree =
    {
        train (VectorI.range (0, yy.dim))
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the regression tree by selecting threshold for all the features
     *  in interval (subsamples).
     *  @param interval  only the values in interval will be used in selecting threshold
     */
    def train (interval: VectoI)
    {
        for (f <- 0 until n) fastThreshold (f, x.col(f), interval)   // set threshold for features
        var opt = (0, threshold(0)._2)                               // compute variance for feature 0

        if (DEBUG) println (s"train: for feature ${opt._1} the variance is ${opt._2}")

        for (f <- 0 until n) {
            val fVar = threshold(f)._2
            if (DEBUG) println (s"train: for feature $f the variance is $fVar")
            if (fVar <= opt._2) opt = (f, fVar)                      // save feature giving minimal variance
        } // for

        if (DEBUG) println (s"train: optimal feature is ${opt._1} with variance of ${opt._2}")
        buildTree (opt)
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the next most distinguishing feature/attribute, extend the regression tree.
     *  @param opt  the optimal feature and the variance
     */
    def buildTree (opt: (Int, Double))
    {
        root = if (curDepth == 0) Node (opt._1, -1, y.mean,  threshold(opt._1)._1, curDepth, -1.0, -1)
               else               Node (opt._1, branchValue, y.mean, threshold(opt._1)._1, curDepth, thres, feature)
        for (i <- 0 until 2) {
            val next = nextXY (opt._1, i)
            if (next._2.size != 0) {
                if (curDepth >= maxDepth - 1) {
                    val ypredict = next._2.mean
                    root.child  += Node (opt._1, root.child.length, ypredict, threshold(opt._1)._1, curDepth + 1,
                                        threshold(opt._1)._1, opt._1, true)
                    incLeaves ()
                    if (DEBUG) {
                        println (" --> Leaf = " + root.child)
                        println ("\t x      = " + next._1)
                        println ("\t y      = " + next._2)
                    } // if
                } else {
//                  if (next._2.size > 1) {
                    if (next._2.size > x.dim2) {
                        val hp = RegressionTree.hp.updateReturn ("threshold", threshold(opt._1)._1)
                        val subtree = new RegressionTree (next._1, next._2, fname, hp,
                                                          curDepth + 1, i, opt._1)
                        subtree.train ()
                        root.child += subtree.root
                    } else {
                        val ypredict = next._2.mean
                        root.child  += Node (opt._1, root.child.length, ypredict, threshold(opt._1)._1, curDepth + 1,
                                             threshold(opt._1)._1, opt._1, true)
                        incLeaves ()
                    } // if
                } // if
            } // if
        } // for
    } // buildTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.  Requires overriding for updating
     *  the degrees of freedom.
     *  @param xx  the data matrix used in prediction
     *  @param yy  the actual response vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): RegressionTree =
    {
        val yp = predict (xx)                                            // y predicted for xx (test/full
        e = yy - yp                                                      // compute residual/error vector e
        val df1 = nLeaves                                                // degrees of freedom model = number of leaves
        val df2 = yy.dim - df1                                           // degrees of freedom error
        resetDF ((df1, df2))
        diagnose (e, yy)                                                 // compute diagnostics
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the regression tree in Pre-Order using 'printT' method. 
     */
    def printTree ()
    {
        println ("Regression Tree: nLeaves = " + nLeaves)
        println ("fname = " + fname.deep)
        printT (root, 0)
        println ()
    } // printTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Recursively print the regression tree nodes.
     *  @param nod     the current node
     *  @param level   the level of node 'nod' in the tree
     */
    def printT (nod: Node, level: Int)
    {
        println ("\t" * level + "[ " + nod + " ]")
        for (cnode <-nod.child) printT (cnode, level + 1)
    } // printT

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print out the regression tree using Breadth First Search (BFS).
     */
    def printTree2 ()
    {
        println ("RegressionTree:")
        println ("fname = " + fname.deep)
        val queue = new Queue [Node] ()

        for (cnode <- root.child) queue += cnode
        println (root)
        var level = 0

        while (! queue.isEmpty) {
            val size = queue.size
            level   += 1
            for (i <- 0 until size) {
                val nod = queue.dequeue ()
                println ("\t" * level + "[ " + nod + " ]")
                for (cnode <- nod.child) queue += cnode
            } // for
            println ()
        } // while
    } // printTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, predict the value by following the tree to the leaf.
     *  @param z  the data vector to predict
     */
    override def predict (z: VectoD): Double =
    {
        var nd = root                                      // current node
        while (nd.child.length >= 2) {
            nd = if (z(nd.f) <= nd.thresh) nd.child(0) else nd.child(1)
        } // while
        nd.yp
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data matrix z, predict the value by following the tree to the leaf.
     *  @param z  the data matrix to predict
     */
    override def predict (z: MatriD): VectorD =
    {
        VectorD (for (i <- z.range1) yield predict(z(i)))
    } //predict

    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("RegressionTree does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("RegressionTree does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation.
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new RegressionTree (x, y, fname, hparam, 0, 0),
                                                 xx, k, rando)
    } // crossVal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'crossVal' abstract method must be coded in implementing classes to
     *  call the above 'crossValidate' method.
     *  FIX - should call 'crossValidate' from super class
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  flag for using randomized cross-validation
     *
    def crossVal (k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        val stats   = Array.fill (fitLabel.length) (new Statistic ())
        val indices = if (rando) permGen.igen.split (k)
                      else VectorI (0 until m).split (k)

        for (idx <- indices) {
            val x_te = x(idx)                                         // test data matrix
            val y_te = y(idx)                                         // test response vector
            val x_tr = x.selectRowsEx (idx)                           // training data matrix
            val y_tr = y.selectEx (idx)                               // training response vector

            if (DEBUG) {
                println ("x_te = " + x_te)
                println ("y_te = " + y_te)
                println ("x_tr = " + x_tr)
                println ("y_tr = " + y_tr)
            } // if

            val model = new RegressionTree (x_tr, y_tr, fname, hparam, 0, 0)   // construct next model using training dataset
            model.train ()                                            // train the model
            model.eval (x_te, y_te)                                   // evaluate model on test dataset
            val qm = model.fit                                        // get quality of fit measures
            for (q <- qm.indices) stats(q).tally (qm(q))              // tally these measures
        } // for

        if (DEBUG) println ("stats = " + stats.deep)
        stats
    } // crossVal
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset () { resetLeaves () }

} // RegressionTree class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTreeTest` object is used to test the `RegressionTree` class.
  *  It tests a simple case that does not require a file to be read.
  *  @see translate.google.com/translate?hl=en&sl=zh-CN&u=https://www.hrwhisper.me/machine-learning-decision-tree/&prev=search
  *  > runMain scalation.analytics.RegressionTreeTest
  */
object RegressionTreeTest extends App
{
    val x = new MatrixD ((10, 1), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val y = VectorD (5.56, 5.70, 5.91, 6.40, 6.80, 7.05, 8.90, 8.70, 9.00, 9.05)

    for (i <- 1 to 4) {
        banner (s"Regression with maxDepth = $i")
        val hp = RegressionTree.hp.updateReturn ("maxDepth", i)
        val rt = new RegressionTree (x, y, null, hp)
        rt.train ()
        rt.eval (x, y)
        rt.printTree ()
        println ("fitMap = " + rt.fitMap)
        rt.reset ()
    } // for

} // RegressionTreeTest

