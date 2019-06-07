
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Kevin Warrick, Susan George
 *  @version 1.6
 *  @date    Wed May 22 14:17:49 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  ID3 Decision/Classification Tree
 */

package scalation.analytics
package classifier

import scalation.linalgebra.{MatriI, MatrixI, VectoI, VectorI}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.util.banner

import Probability.{entropy, frequency}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3` class implements a Decision Tree classifier using the
 *  ID3 algorithm.  The classifier is trained using a data matrix 'x' and a
 *  classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).
 *  @param x       the input/data matrix with instances stored in rows
 *  @param y       the response/classification vector, where y_i = class for row i of matrix x
 *  @param fn_     the names for all features/variables
 *  @param k       the number of classes
 *  @param cn_     the names for all classes
 *  @param height  the maximum height of tree (max edge count root to leaf)
 */
class DecisionTree2ID3 (x: MatriI, y: VectoI, fn_ : Strings = null,
                        k: Int = 2, cn_ : Strings = null, height: Int = Int.MaxValue)
      extends ClassifierInt (x, y, fn_, k, cn_) with DecisionTree2
{
    private val DEBUG      = false                                        // debug flag
    private val THRES      = 0.01                                         // cutoff entropy threshold
    private val entropy_0  = entropy (frequency (y, k))                   // initial entropy of vector y
    private val allRows    = Array.range (0, x.dim1)                      // start with all data rows
    private val allColumns = Array.range (0, x.dim2)                      // start with all data columns
    private val feas       = Array.ofDim [Feature] (n)                    // array of features 
    for (f <- x.range2) feas(f) = Feature (f, x.col(f)) 

    banner ("DecisionTree2ID3: initial entropy: entropy_0 = " + entropy_0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the information gain due to using the values of a feature
     *  to distinguish the training cases (e.g., how well does Humidity with its
     *  values Normal and High indicate whether one will play tennis).
     *  @param fea     the feature to consider (e.g., 2 (Humidity))
     *  @param xj      the vector for feature fea (column j of matrix)
     *  @param rindex  the working row index
     */
    def gain (fea: Feature, xj: VectoI, rindex: Ints): (Double, VectoI) =
    {
        val nu  = new VectorI (k)                                         // aggregate frequency vector
        var sum = 0.0
        for (v <- fea.values) {
            val (frac_v, nu_v) = frequency (xj, y, k, v, rindex)          // frequency for value v
            if (DEBUG) println (s"gain (v = $v): (frac_v, nu_v) = ($frac_v, $nu_v")
            sum += frac_v * entropy (nu_v)                                // weighted entropy
            nu  += nu_v                                                   // aggregate frequency vector
        } // for
        val igain = entropy_0 - sum                                       // the drop in entropy = information gain
        if (DEBUG) println (s"gain: entropy = $sum, overall gain from feature ${fea.f} = $igain")
        (igain, nu)                                                       // return gain and aggregate frequency vector
    } // gain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the decision tree.
     *  @param itest  the indices for the test data
     */
    def train (itest: Ints): DecisionTree2ID3 =                           // FIX - use these parameters
    {
        buildTree ()
        println ("Entropy of tree = " + calcEntropy ())
        println ("No of leaves (original) = " + leaves.size)
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best feature 'f' / column 'xj' to expand the decision tree,
     *  returning that feature, its gain and its frequency vector.
     *  Note: the dataset is restricted to 'rindex' rows and 'cindex' columns.
     *  @param rindex  the working row index
     *  @param cindex  the working column index
     */
    private def findBest (rindex: Ints, cindex: Ints): (Int, Double, VectoI) =
    {
        var best = (-1, 0.0, null.asInstanceOf [VectoI])                  // best (feature, gain, frequency)
        for (f <- cindex) {
            if (DEBUG) println (s"--> findBest: check feature f = $f")
            val (gn, nu) = gain (feas(f), x.col(f), rindex)               // compute gain for feature f
            if (DEBUG) println (s"findBest: compare ($f, $gn, $nu) to $best")
            if (gn > best._2) best = (f, gn, nu)
        } // for
        if (best._2 <= 0.0) println ("findBest: no positive gain found")
        best
    } // findBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively build the decision tree until entropy drops to the cutoff
     *  threshold 'THRESH' or the tree 'depth' is at the specified tree 'height'.
     *  @param rindex  the working row index
     *  @param cindex  the working column index
     *  @param parent  the parent node (== null => at root)
     *  @param depth   the depth of the subtree being built
     */
    def buildTree (rindex: Ints = allRows, cindex: Ints = allColumns, parent: Node = null, depth: Int = 0): Node =
    {
        val (f, gn, nu) = findBest (rindex, cindex)                       // find the best feature
        println (s"buildTree: best (f, gn, nu) = ($f, $gn, $nu), depth = $depth")
        if (f < 0) return null                                            // no useful feature was found

        val node = Node (f, gn, nu, parent, nu.argmax (),                 // construct the next node
                         entropy (nu) <= THRES || depth >= height)        // leaf or internal?
        if (parent == null) addRoot (node)                                // if root, add it to tree

        if (! node.isLeaf && cindex.length > 1) {
            println ("-" * 80)
            val xf      = x.col(f)                                        // extract feature column
            val cindex2 = cindex diff Array (f)                           // remove column f from column index
            if (DEBUG) println (s"buildTree: cindex2 = $cindex2")
            for (v <- feas(f).values) {                                   // build subtree or leaf for each branch value
                if (DEBUG) println (s"- buildTree: explore branch $v for feature x$f at depth $depth")
                val rindex2 = trim (xf, rindex, v)                        // trim row index to those matching value v
                val child = buildTree (rindex2, cindex2, node, depth+1)   // build a subtree
                if (child != null) add (node, v, child)                   // if exists, add child to tree
            } // for
            println ("-" * 80)
        } // if
        node
    } // buildTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Trim the row index by only including those where column 'xj == v',
     *  returning the newly trimmed row index.
     *  @param xj      the column of the data matrix to be considered
     *  @param rindex  the working row index used to create the new trimmed version
     *  @param v       the value to matched
     */
    private def trim (xj: VectoI, rindex: Ints, v: Int): Ints =
    {
        for (i <- rindex if xj(i) == v) yield i
    } // trim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector 'z', classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.
     *  Return the best class, its name and FIX.
     *  @param z  the data vector to classify
     */
    override def classify (z: VectoI): (Int, String, Double) =
    {
        val best = classify2 (z)
        (best, cn(best), -1.0)
    } // classify

} // DecisionTree2ID3 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3` companion object provides factory methods.
 */
object DecisionTree2ID3
{
    import ClassifierInt.pullResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a decision tree for the given combined matrix where the last column
     *  is the response/classification vector.
     *  @param xy      the combined data matrix (features and response)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def apply (xy: MatriI, fn: Strings, k: Int, cn: Strings, height: Int = Int.MaxValue): DecisionTree2ID3 =
    {
        val (x, y) = pullResponse (xy)
        new DecisionTree2ID3 (x, y, fn, k, cn, height)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the decision tree on the given dataset passed in as a combined matrix.
     *  @param xy      the combined data matrix (features and response)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def test (xy: MatriI, fn: Strings, k: Int, cn: Strings, height: Int = Int.MaxValue): DecisionTree2ID3 =
    {
        banner ("create, train and print a ID3 decision tree")
        println (s"dataset xy: ${xy.dim1}-by-${xy.dim2} matrix")
        val (x, y) = pullResponse (xy)
        val ymin   = y.min ()
        println (s"unadjusted ymin = $ymin")
        if (ymin != 0) y -= ymin
        println (s"height = $height")
        val tree = new DecisionTree2ID3 (x, y, fn, k, cn, height)
        tree.train ()
        tree.printTree ()

        banner ("classify all intances and show confusion matrix")
        val yp = tree.classify (x)
//      for (i <- y.range) println (s"i: $i, \t y  = ${y(i)}, \t yp = ${yp(i)}")
        val ymax = y.max ()
        println (s"ymax = $ymax")
        val cm = new ConfusionMat (y, yp, ymax+1)
        println ("cm = " + cm.confusion)
        println ("accuracy    = " + cm.accuracy)
        println ("prec-recall = " + cm.prec_recl)
        tree
    } // test

} // DecisionTree2ID3 object

import DecisionTree2ID3.test

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3Test` object is used to test the `DecisionTree2ID3` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured features.
 *  @see www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 *  > runMain scalation.analytics.classifier.DecisionTree2ID3Test
 */
object DecisionTree2ID3Test extends App
{
    import ExampleTennis.{xy, fn, k, cn}

    val tree = test (xy, fn, k, cn, height = 2)                  // create and test decision tree

    banner ("Classify New Data")
    val z = VectorI (2, 2, 1, 1)                                 // new data vector to classify
    println (s"classify ($z) = ${tree.classify (z)}")

} // DecisionTreeI2D3Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3Test2` object is used to test the `DecisionTree2ID3` class.
 *  Ex: Classify whether a there is breast cancer.
 *  > runMain scalation.analytics.classifier.DecisionTree2ID3Test2
 */
object DecisionTree2ID3Test2 extends App
{
    val fname = BASE_DIR + "breast_cancer.csv"
    val xy    = MatrixI (fname)
    val fn    = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                       "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val cn    = Array ("benign", "malignant")
    val k     = cn.size
    val tree  = test (xy, fn, k, cn, height = 2)                // create and test decision tree

} // DecisionTree2ID3Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2ID3Test3` object is used to test the `DecisionTree2ID3` class.
 *  Plot entropy.
 *  > runMain scalation.analytics.classifier.DecisionTree2ID3Test3
 */
object DecisionTree2ID3Test3 extends App
{
    import scalation.linalgebra.VectorD
    import scalation.math.log2
    import scalation.plot.Plot

    val p = VectorD.range (1, 100) / 100.0
    val h = p.map (p => -p * log2 (p) - (1-p) * log2 (1-p))
    new Plot (p, h)

} // DecisionTree2ID3Test3 object

