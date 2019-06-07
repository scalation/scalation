
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Susan George
 *  @version 1.6
 *  @date    Wed May 22 14:17:49 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  C45 Decision/Classification Tree
 */

package scalation.analytics
package classifier

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectoI, VectorI}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.util.banner

import Probability.{entropy, findSplit, frequency}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2C45` class implements a Decision Tree classifier using the
 *  C45 algorithm.  The classifier is trained using a data matrix 'x' and a
 *  classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).
 *  @param x       the input/data matrix with instances stored in rows
 *  @param y       the response/classification vector, where y_i = class for row i of matrix x
 *  @param fn_     the names for all features/variables
 *  @param k       the number of classes
 *  @param cn_     the names for all classes
 *  @param height  the maximum height of tree (max edge count root to leaf)
 *  @param conts   the set of feature indices for variables that are treated as continuous
 */
class DecisionTree2C45 (x: MatriD, y: VectoI, fn_ : Strings = null,
                        k: Int = 2, cn_ : Strings = null, height: Int = Int.MaxValue, conts: Set [Int] = Set [Int] ())
      extends ClassifierReal (x, y, fn_, k, cn_) with DecisionTree2
{
    private val DEBUG      = true                                            // debug flag
    private val THRES      = 0.01                                            // cutoff entropy threshold
    private val entropy_0  = entropy (frequency (y, k))                      // initial entropy of vector y
    private val allRows    = Array.range (0, x.dim1)                         // start with all data rows
    private val allColumns = Array.range (0, x.dim2)                         // start with all data columns
    private val threshold  = Array.ofDim [Double] (n)                        // threshold for continuous features (below <=, above >)
    private val feas       = Array.ofDim [Feature] (n)                       // array of features 
    for (f <- x.range2) feas(f) = if (conts contains f) Feature (f, null, true) else Feature (f, x.col(f).toInt) 

    banner ("DecisionTree2C45: initial entropy: entropy_0 = " + entropy_0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the decision tree.
     *  @param itest  the indices for the test data
     */
    def train (itest: Ints): DecisionTree2C45 =                              // FIX - use these parameters
    {
        buildTree ()
        println ("Entropy of tree = " + calcEntropy ())
        println ("No of leaves (original) = " + leaves.size)
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the information gain due to using the values of a feature
     *  to distinguish the training cases (e.g., how well does Humidity with its
     *  values Normal and High indicate whether one will play tennis).
     *  @param fea     the feature to consider (e.g., 2 (Humidity))
     *  @param xj      the vector for feature fea (column j of matrix)
     *  @param rindex  the working row index
     */
    private def gain (fea: Feature, xj: VectoD, rindex: Ints): (Double, VectoI) =
    {
        val nu  = new VectorI (k)                                            // aggregate frequency vector
        var sum = 0.0
        val f   = fea.f
        for (v <- fea.values) {
            val (frac_v, nu_v) = frequency (xj, y, k, v, rindex,
                                 fea.isCont, threshold(f))                   // frequency for value v
            if (DEBUG) println (s"gain (v = $v): (frac_v, nu_v) = ($frac_v, $nu_v")
            sum += frac_v * entropy (nu_v)                                   // weighted entropy
            nu  += nu_v                                                      // aggregate frequency vector
        } // for
        val igain = entropy_0 - sum                                          // the drop in entropy = information gain
        if (DEBUG) println (s"gain: entropy = $sum, overall gain from feature $f = $igain")
        (igain, nu)                                                          // return gain and aggregate frequency vector
    } // gain

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best feature 'f' / column 'xj' to expand the decision tree,
     *  returning that feature, its gain and its frequency vector.
     *  Note: the dataset is restricted to 'rindex' rows and 'cindex' columns.
     *  @param rindex  the working row index
     *  @param cindex  the working column index
     */
    private def findBest (rindex: Ints, cindex: Ints): (Int, Double, VectoI) =
    {
        var best = (-1, 0.0, null.asInstanceOf [VectoI])                     // best (feature, gain, frequency)
        for (f <- cindex) {
//      for (f <- 0 until n) {                                               // FIX - cause infinite loop
            if (DEBUG) println (s"--> findBest: check feature f = $f")
            val xj = x.col(f)                                                // column j (feature f)
            if (feas(f).isCont) threshold(f) = findSplit (xj, y, rindex, k)  // isCont => calculate split threshold
            val (gn, nu) = gain (feas(f), xj, rindex)                        // compute gain for feature f
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
    private def buildTree (rindex: Ints = allRows, cindex: Ints = allColumns, parent: Node = null, depth: Int = 0): Node =
    {
        val (f, gn, nu) = findBest (rindex, cindex)                          // find the best feature
        println (s"buildTree: best (f, gn, nu) = ($f, $gn, $nu), depth = $depth")
        if (f < 0) return null                                               // no useful feature was found

        val leaf = entropy (nu) <= THRES || depth >= height                  // leaf or internal? 
        val node = Node (f, gn, nu, parent, nu.argmax (), leaf)              // construct the next node
        if (! leaf && feas(f).isCont) node.thres = threshold (f)             // for continuous features, store threshold in node
        if (parent == null) addRoot (node)                                   // if root, add it to tree

        if (! node.isLeaf && cindex.length > 1) {
            println ("-" * 80)
            val xf      = x.col(f)                                           // extract feature column
            val cindex2 = cindex diff Array (f)                              // remove column f from column index
            if (DEBUG) println (s"buildTree: cindex2 = $cindex2")
            for (vl <- feas(f).values) {                                     // build subtree or leaf for each branch value
                if (DEBUG) println (s"- buildTree: explore branch $vl for feature x$f at depth $depth")
                val rindex2 = trimRows (f, xf, rindex, vl, threshold(f))     // trim row index to those matching value v
                val child = buildTree (rindex2, cindex2, node, depth+1)      // build a subtree
                if (child != null) add (node, vl, child)                     // if exists, add child to tree
            } // for
            println ("-" * 80)
        } // if
        node
    } // buildTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Trim the row index by only including those where column 'xj == vl',
     *  returning the newly trimmed row index.
     *  @param xj      the column of the data matrix to be considered
     *  @param rindex  the working row index used to create the new trimmed version
     *  @param vl      the value to matched (for 'conts' its 0 (up to) or 1 (beyond) threshold)
     *  @param thres   the splitting threshold 
     */
    private def trimRows (f: Int, xj: VectoD, rindex: Ints, vl: Int, thres: Double = -0.0): Ints =
    {
        if (conts contains f) {
            if (vl == 0) {
                for (i <- rindex if xj(i) <= thres) yield i
            } else {
                for (i <- rindex if xj(i)  > thres) yield i
            } // if
        } else {
            for (i <- rindex if xj(i) == vl) yield i
        } // if
    } // trimRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector 'z', classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.
     *  Return the best class, its name and FIX.
     *  @param z  the data vector to classify
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        val best = classify2 (z)
        (best, cn(best), -1.0)
    } // classify

} // DecisionTree2C45 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2C45` companion object provides factory methods.
 */
object DecisionTree2C45
{
    import ClassifierReal.pullResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a decision tree for the given combined matrix where the last column
     *  is the response/classification vector.
     *  @param xy      the combined data matrix (features and response)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def apply (xy: MatriD, fn: Strings, k: Int, cn: Strings, height: Int = Int.MaxValue,
               conts: Set [Int] = Set [Int] ()): DecisionTree2C45 =
    {
        val (x, y) = pullResponse (xy)
        new DecisionTree2C45 (x, y.toInt, fn, k, cn, height, conts)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the decision tree on the given dataset passed in as a combined matrix.
     *  @param xy      the combined data matrix (features and response)
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def test (xy: MatriD, fn: Strings, k: Int, cn: Strings, height: Int = Int.MaxValue,
              conts: Set [Int] = Set [Int] ()): DecisionTree2C45 =
    {
        banner ("create, train and print a C45 decision tree")
        println (s"dataset xy: ${xy.dim1}-by-${xy.dim2} matrix")
        val (x, y) = pullResponse (xy)
        val ymin   = y.min ()
        println (s"unadjusted ymin = $ymin")
        if (ymin != 0) y -= ymin
        println (s"height = $height")
        val tree = new DecisionTree2C45 (x, y.toInt, fn, k, cn, height, conts)
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

} // DecisionTree2C45 object

import DecisionTree2C45.test

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2C45Test` object is used to test the `DecisionTree2C45` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured features.
 *  @see www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 *  > runMain scalation.analytics.classifier.DecisionTree2C45Test
 */
object DecisionTree2C45Test extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: Cold (0), Mild (1), Hot (2)
    // Humidity:    Normal (0), High (1)
    // Wind:        Weak (0), Strong (1)
    // features:    Outlook Temp Humidity Wind
    // classification vector: 0(no), 1(yes))

    import ExampleTennis.{xy, fn, k, cn}

    banner ("Test: DecisionTree2C45: Play Tennis Dataset")
    val tree = test (xy.toDouble, fn, k, cn)                       // create and test decision tree

    banner ("Classify New Data")
    val z = VectorI (2, 2, 1, 1)                                   // new data vector to classify
    println (s"classify ($z) = ${tree.classify (z)}")

} // DecisionTreeI2D3Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2C45Test2` object is used to test the `DecisionTree2C45` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured features.
 *  @see www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 *  > runMain scalation.analytics.classifier.DecisionTree2C45Test2
 */
object DecisionTree2C45Test2 extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: continuous
    // Humidity:    continuous
    // Wind:        Weak (0), Strong (1)
    // features:    Outlook Temp Humidity Wind
    // classification vector: 0(no), 1(yes))

    import ExampleTennisCont.{xy, fn, k, cn}

    banner ("Test: DecisionTree2C45: Play Tennis Continuous Dataset")
    val cont = Set (1, 2)                                          // columns 1 and 2 treated as continuous
    val tree = test (xy, fn, k, cn, conts = cont)                  // create and test decision tree

    banner ("Classify New Data")
    val z = VectorI (2, 80, 80, 1)                                 // new data vector to classify
    println (s"classify ($z) = ${tree.classify (z)}")

} // DecisionTreeI2D3Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2C45Test3` object is used to test the `DecisionTree2C45` class.
 *  Ex: Classify whether a there is breast cancer.
 *  > runMain scalation.analytics.classifier.DecisionTree2C45Test3
 */
object DecisionTree2C45Test3 extends App
{
    banner ("Test: DecisionTree2C45: Breast Cancer Dataset")
    val fname = BASE_DIR + "breast_cancer.csv"
    val xy    = MatrixD (fname)
    val fn    = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                       "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val cn    = Array ("benign", "malignant")
    val k     = cn.size
    val tree  = test (xy, fn, k, cn, height = 2)                   // create and test decision tree

} // DecisionTree2C45Test3 object

