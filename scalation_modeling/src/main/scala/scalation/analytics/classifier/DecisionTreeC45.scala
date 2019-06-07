
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Jerry Shi, John Miller, Dong Yu Yu, Susan George
 *  @version 1.6
 *  @date    Wed Jan  9 15:07:13 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://en.wikipedia.org/wiki/C4.5_algorithm
 */

package scalation.analytics.classifier

import scala.collection.mutable.{ArrayBuffer}
import scala.util.Sorting

import scalation.linalgebra.{VectoD, VectorD, VectoI, VectorI, MatriD, MatrixD}
import scalation.util.banner

import Probability.{count, entropy, findSplit, frequency}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45` class implements a Decision Tree classifier using the
 *  C4.5 algorithm.  The classifier is trained using a data matrix 'x' and a
 *  classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).  The 'vc' array gives the number of distinct values
 *  per feature (e.g., 2 for Humidity).
 *-----------------------------------------------------------------------------
 *  At node for feature 'x_f', create children for possible discrete values of 'x_f'
 *  (For continuous, pick a threshold to split into lower and higher values).  Upon
 *  splitting, some matrices need to be created for which 'x_f' column is removed and
 *  each child only contains rows for its given value of 'x_f'.
 *-----------------------------------------------------------------------------
 *  @param x       the data vectors stored as rows of a matrix
 *  @param y       the class array, where y_i = class for row i of the matrix x
 *  @param isCont  `Boolean` value to indicate whether according feature is continuous
 *  @param fn_     the names for all features/variables
 *  @param k       the number of classes
 *  @param cn_     the names for all classes
 *  @param vc      the value count array indicating number of distinct values per feature
 *  @param height  the maximum height of tree (max edge count root to leaf)
 */
class DecisionTreeC45 (val x: MatriD, val y: VectoI, isCont: Array [Boolean], fn_ : Strings = null,
                       k: Int = 2, cn_ : Strings = null, private var vc: Array [Int] = null,
                       height: Int = Int.MaxValue)
      extends ClassifierReal (x, y, fn_, k, cn_) with DecisionTree
{
    private val DEBUG     = true                                             // debug flag
    private val entropy_0 = entropy (frequency (y, k))                       // the initial entropy
    private val threshold = Array.ofDim [Double] (n)                         // threshold for continuous features (below <=, above >)
    
    if (vc == null) vc = vc_default                                          // set value count (vs) to default for binary data (2)
    for (i <- 0 until n if isCont(i)) vc(i) = 2                              // for continuous features set vc to 2 (below, above)

    banner ("DecisionTreeC45: initial entropy entropy_0 = " + entropy_0)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the decision tree.
     *  @param itest  the indices for the test data
     */
    def train (itest: IndexedSeq [Int]) =                                    // FIX the logic - use itest
    {
        root = buildTree (x, y, List [(Int, Int)] ())
        println ("Entropy of tree = " + Node2.calcEntropy (leaves))
        println ("No of leaves (original) = " + leaves.size)
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the information gain due to using the values of a feature/attribute
     *  to distinguish the training cases (e.g., how well does Humidity with its
     *  values Normal and High indicate whether one will play tennis).
     *  @param f   the feature to consider (e.g., 2 (Humidity))
     *  @param x_  the trimmed data matrix
     *  @param y_  the trimmed classification vector
     */
    private def gain (f: Int, x_ : MatriD, y_ : VectoI): (Double, VectoI) =
    {
        val nu  = new VectorI (k)                                            // frequency counts
        var sum = 0.0
        for (v <- 0 until vc(f)) {
            val (frac_v, nu_v) = frequency (x_.col(f), y_, k, v, null, isCont(f), threshold(f))
            sum += frac_v * entropy (nu_v)                                   // weighted entropy
            nu  += nu_v
        } // for
        val igain = entropy_0 - sum                                          // the drop in entropy = information gain
        if (DEBUG) println (s"gain: entropy = $sum, overall gain from feature $f = $igain")
        (igain, nu)                                                          // return the gain and frequency counts
    } // gain
 
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best feature 'f' / column 'xj' to expand the decision tree,
     *  returning that feature, its gain and its frequency vector.
     *  Note: the dataset is restricted to 'rindex' rows and 'cindex' columns.
     *  @param x_  the trimmed data matrix
     *  @param y_  the trimmed classification vector
     */
    private def findBest (x_ : MatriD, y_ : VectoI): (Int, Double, VectoI) =
    {
        var best = (-1, 0.0, null.asInstanceOf [VectoI])                     // best (feature, (gain, frequency))
        for (f <- 0 until n) {
            if (DEBUG) println (s"--> findBest: check feature f = $f")
            val xj = x_.col(f)                                               // column j (feature f)
            if (isCont(f)) threshold(f) = findSplit (xj, y_, k = k)          // isCont => calculate split threshold
            val (gn, nu) = gain (f, x_, y_)                                  // compute gain for feature f
            if (DEBUG) println (s"findBest: compare ($f, $gn, $nu) to $best")
            if (gn > best._2) best = (f, gn, nu) 
        } // for
        if (best._2 <= 0.0) println ("findBest: no positive gain found")
        best
    } // findBest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively build the decision tree given a subset of data.
     *  @param dset   the dataset to build the subtree
     *  @param path   an existing path in the tree ((feature, value), ...)
     *  @param depth  the depth of the subtree being built
     */  
    private def buildTree (x_ : MatriD, y_ : VectoI, path: List [(Int, Int)], depth: Int = 0): Node2 =
    { 
        val (f, gn, nu) = findBest (x_, y_)
        println ("-" * 80)
        println (s"buildTree: best feature = $f, gain = $gn, path = $path")

        if (f < 0) return null                                               // no useful feature was found

        val node = FeatureNode (f, path, nu)
        node.threshold = threshold(f)
          
        for (b <- 0 until vc(f)) {                                           // build subtree or leaf for each branch value
            if (DEBUG) println (s"- buildTree: explore branch $b of ${vc(f)} for feature x$f at depth $depth")
            val (xx, yy) = trimRows (f, b, x_, y_)                           // time matrix x_ and vector y_

            if (depth >= height) {                                           // at height limit
                for (v <- 0 until vc(f)) {
                    val (frac_v, nu_v) = frequency (xx.col(f), yy, k, v, null, isCont(f), threshold(f))
                    leaves += Node2.addLeaf (nu_v.argmax (), nu_v, node, v)  // add leaf node
                } // for
                println (s"buildTree: early termination: depth = $depth, height = $height")
                return node
            } else {       
                if (yy.dim != 0) {                                           // if additional split doesn't cause empty nodes
                    if (yy.countinct == 1) {                                 // if target contains a single value                  
                        leaves += Node2.addLeaf (yy(0), frequency (yy, k), node, b)      // add leaf node            
                    } else if (multivalued (xx)) {                           // if multivalued, build
                        node.branch += b -> buildTree (xx, yy, (f, b) :: path, depth+1)  // add feature node
                    } // if
                } // if
            } // if

        } // for
        node                                                                 // return root of tree
    } // buildTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Trim the data matrix 'xx' and classification vector 'yy', selecting rows based
     *  on threshold values for continuous features or discrete values, otherwise.
     *  Return a new 'x_' data matrix and a new 'y_' classification vector for next
     *  step of constructing decision tree based upon values of the given feature 'f'.
     *  @param f      the feature/column index
     *  @param value  one of the feature values or 0 (<=) / 1 (> threshold) for a continuous feature
     *  @param xx     the data matrix containing feature/column f
     *  @param yy     the corresponding response/classification vector
     */
    private def trimRows (f: Int, value: Int, xx: MatriD, yy: VectoI): (MatriD, VectoI) =
    {
        val x_f = xx.col(f)                                                  // column f of matrix xx
        val cnt = count (x_f, value, null, isCont(f), threshold(f))
        val x_  = new MatrixD (cnt, xx.dim2)                                 // new x matrix
        val y_  = new VectorI (cnt)                                          // new y vector

        var ii = 0
        if (isCont(f)) {                                                     // feature with continuous values
            if (value == 0) {
                for (i <- x_f.range if x_f(i) <= threshold(f)) { y_(ii) = yy(i); x_(ii) = xx(i); ii += 1 }
            } else {
                for (i <- x_f.range if x_f(i) >  threshold(f)) { y_(ii) = yy(i); x_(ii) = xx(i); ii += 1 }
            } // if
        } else {                                                             // feature with discrete values
            for (i <- x_f.range if x_f(i) == value) { y_(ii) = yy(i); x_(ii) = xx(i); ii += 1 }
        } // if
        (x_, y_)
    } // trimRows

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.
     *  Return the best class, its name and FIX.
     *  @param z  the data vector to classify (some continuous features)
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        var node = root                                                      // current node
        for (j <- 0 to n) {
            node match {
            case FeatureNode (f, path, count, branch) =>
                try {
                    val fn = node.asInstanceOf [FeatureNode]
                    node = if (isCont (f)) if (z(f) <= fn.threshold) fn.branch(0) else fn.branch(1)
                           else branch (z(f).toInt) 
                } catch { case nse: NoSuchElementException =>
                    val best = node.asInstanceOf [FeatureNode].nu.argmax ()
                    return (best, cn(best), -1.0)
                } // try
            case LeafNode (y, count) => 
                val best = y
                return (best, cn(best), -1.0)
            case _ =>
                println (s"classify: 'node match' failed for node = $node")
                return (-1, "?", -1.0)
            } // match
        } // for
        println ("classify: failed at leaf node")
        (-1, "?", -1.0)
    } // classify

} // DecisionTreeC45 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `DecisionTreeC45` is the companion object provides factory methods.
 */
object DecisionTreeC45
{
    import ClassifierReal.pullResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a decision tree for the given combined matrix where the last column
     *  is the response/classification vector.
     *  @param xy      the data vectors along with their classifications stored as rows of a matrix
     *  @param isCont  `Boolean` value to indicate whether according feature is continuous
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param vc      the value count array indicating number of distinct values per feature
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def apply (xy: MatriD, isCont: Array [Boolean], fn: Strings = null,
               k: Int = 2, cn: Strings = null, vc: Array [Int] = null, height: Int = Int.MaxValue) =
    {
        val (x, y) = pullResponse (xy)
        new DecisionTreeC45 (x, y, isCont, fn, k, cn, vc, height)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the decision tree on the given dataset passed in as a combined matrix.
     *  @param xy      the data vectors along with their classifications stored as rows of a matrix
     *  @param fn      the names for all features/variables
     *  @param isCont  `Boolean` value to indicate whether according feature is continuous
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param vc      the value count array indicating number of distinct values per feature
     *  @param height  the maximum height of tree (max edge count root to leaf)
     */
    def test (xy: MatriD, fn: Strings, isCont: Array [Boolean], k: Int = 2, cn: Strings = null,
              vc: Array [Int] = null, height: Int = Int.MaxValue): DecisionTreeC45 =
    {
        banner ("create, train and print a C4.5 decision tree")
        println (s"dataset xy: ${xy.dim1}-by-${xy.dim2} matrix")
        val (x, y) = pullResponse (xy)
        val ymin   = y.min ()
        println (s"unadjusted ymin = $ymin")
        if (ymin != 0) y -= ymin
        val tree = new DecisionTreeC45 (x, y, isCont, fn, k, cn, vc, height)
        tree.train ()
        tree.printTree (vc)

        banner ("classify all intances and show confusion matrix")
        val yp = tree.classify (x)
//      for (i <- y.range) println (s"i: $i, \t y  = ${y(i)}, \t yp = ${yp(i)}")
        val ymax = y.max ()
        println (s"ymax = $ymax")
        println ("fitMap = " + tree.fitMap (y, yp, ymax+1))
        val cm = new ConfusionMat (y, yp, ymax+1)
        println ("cm = " + cm.confusion)
        println ("accuracy    = " + cm.accuracy)
        println ("prec-recall = " + cm.prec_recl)
        tree
    } // test

} // DecisionTreeC45 object

import DecisionTreeC45.test
import ClassifierReal.makeIsCont

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45Test` object is used to test the `DecisionTreeC45` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured
 *  features.
 *  @see www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 *  > runMain scalation.analytics.classifier.DecisionTreeC45Test
 */
object DecisionTreeC45Test extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: Cold (0), Mild (1), Hot (2)
    // Humidity:    Normal (0), High (1)
    // Wind:        Weak (0), Strong (1)
    // features:    Outlook Temp Humidity Wind
    // classification vector: 0(no), 1(yes))

    import ExampleTennis.fn

    val xy = ExampleTennis.xy.toDouble 
    
    banner ("C4.5 Decision Tree for 'playtennis' dataset")

    val vc    = Array (3, 3, 2, 2)                                // distinct values for each feature
    val isCon = makeIsCont (xy.dim2-1)                            // continuous column flag
    val tree  = test (xy, fn, isCon, 2, null, vc)                 // create and test decision tree

    banner ("Classify New Data")
    val z = VectorI (2, 2, 1, 1)                                  // new data vector to classify
    println (s"classify ($z) = ${tree.classify (z)}")

    banner ("Prune the Tree")
    val threshold = 0.98                                          // pruning threshold
//  tree.prune (threshold)                                        // prune the decision tre

} // DecisionTreeC45Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45Test2` object is used to test the `DecisionTreeC45` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured
 *  features.  This one used the version with continuous features.
 *  @see https://sefiks.com/2018/05/13/a-step-by-step-c4-5-decision-tree-example/
 *  > runMain scalation.analytics.classifier.DecisionTreeC45Test2
 */
object DecisionTreeC45Test2 extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: continuous
    // Humidity:    continuous
    // Wind:        Weak (0), Strong (1)
    // features:    Outlook Temp Humidity Wind
    // classification vector: 0(no), 1(yes))

    import ExampleTennisCont.{fn, xy}

    banner ("C4.5 Decision Tree for 'playtennis' continuous version dataset")

    val vc    = Array (3, 3, 2, 2)                                // distinct values for each feature
    val isCon = Array (false, true, true, false)                  // continuous column flag
    val tree  = test (xy, fn, isCon, vc = vc)                     // create and test decision tree

    banner ("Classify New Data")
    val z = VectorD (2, 80, 80, 1)                                // new data vector to classify
    println (s"classify ($z) = ${tree.classify (z)}")

} // DecisionTreeC45Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45Test3` object is used to test the `DecisionTreeC45` class.
 *  Ex: Classify whether a there is breast cancer.
 *  > runMain scalation.analytics.classifier.DecisionTreeC45Test3
 */
object DecisionTreeC45Test3 extends App
{
    banner ("C4.5 Decision Tree for 'breast cancer' dataset")
    val fname  = BASE_DIR + "breast_cancer.csv"
    val xy     = MatrixD (fname)
    val fn     = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                       "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val isCon  = makeIsCont (xy.dim2-1)                            // continuous column flag
    val cn     = Array ("benign", "malignant")
    val k      = cn.size
    val vc     = (for (j <- 0 until xy.dim2-1) yield xy.col(j).max ().toInt + 1).toArray
    val height = 5
    val tree   = test (xy, fn, isCon, k, cn, vc, height)          // create and test decision tree

    banner ("Prune the Tree")
    val threshold = 0.4                                           // pruning threshold
//  tree.prune (threshold)                                        // prune the decision tree

} // DecisionTreeC45Test3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45Test4` object is used to test the `DecisionTreeC45` class
 *  using the well-known wine quality dataset.
 *  > runMain scalation.analytics.classifier.DecisionTreeC45Test4
 */
object DecisionTreeC45Test4 extends App
{
    banner ("C4.5 Decision Tree for 'winequality-white' dataset")
    val fname = BASE_DIR + "winequality-white.csv"                // data file
    val xy    = MatrixD (fname)                                   // combined data matrix
    val fn    = Array ("FixedAcidity", "VolatileAcidity",  "CitricAcid", "ResidualSugar", "Chlorides",
                       "FreeSulfurDioxide", "TotalSulfurDioxide", "Density", "pH", "Sulphates", "Alcohol") 
    val isCon = Array.fill (fn.length)(true)                      // continuous column flag
    val cn    = Array ("q3", "q4", "q5", "q6", "q7", "q8", "q9")
    val k     = cn.size
    val vc    = (for (j <- 0 until xy.dim2-1) yield xy.col(j).max ().toInt + 1).toArray
    val height = 10                                                // try several values for max tree depth
    val tree   = test (xy, fn, isCon, k, cn, vc, height)           // create and test decision tree

    banner ("Prune the Tree")
    val threshold = 0.98                                          // pruning threshold
//  tree.prune (threshold)                                        // prune the decision tree

} // DecisionTreeC45Test4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45Test5` object is used to test the 'makeIsCont' function.
 *  > runMain scalation.analytics.classifier.DecisionTreeC45Test5
 */
object DecisionTreeC45Test5 extends App
{
    println ("isCont = " + makeIsCont (16, 7, 11).deep)

} // DecisionTreeC45Test5 object

