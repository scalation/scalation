
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Kevin Warrick, John Miller, Susan George
 *  @version 1.6
 *  @date    Wed Jan  9 15:07:13 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.collection.mutable.{ArrayBuffer}

import scalation.linalgebra.{MatriI, MatrixI, VectoD, VectorD, VectoI, VectorI}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream
import scalation.util.banner

import Probability.{entropy, toProbability}
import Probability.{frequency => FREQUENCY}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3` class implements a Decision Tree classifier using the
 *  ID3 algorithm.  The classifier is trained using a data matrix 'x' and a
 *  classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).  The 'vc' array gives the number of distinct values
 *  per feature (e.g., 2 for Humidity).
 *  @param x    the data vectors stored as rows of a matrix
 *  @param y    the class array, where y_i = class for row i of the matrix x
 *  @param fn_  the names for all features/variables
 *  @param k    the number of classes
 *  @param cn_  the names for all classes
 *  @param vc   the value count array indicating number of distinct values per feature
 *  @param td   the maximum tree depth to allow (defaults to 0 => number of features, -1 no constraint
 */
class DecisionTreeID3 (x: MatriI, y: VectoI, fn_ : Strings = null,
                       k: Int = 2, cn_ : Strings = null,
                       protected var vc: Array [Int] = null,
                       protected var td: Int = -1)
      extends ClassifierInt (x, y, fn_, k, cn_) with DecisionTree
{
    private val DEBUG     = false                                // debug flag
    private val py        = toProbability (FREQUENCY (y, k), m)  // probability vector for y
    private val entropy_0 = entropy (py)                         // the initial entropy

    if (vc == null) vc = vc_default                              // set value count (vc) to default for binary data (2)

    private val hasDepthConstraint = td >= 0                     // tree depth constraint flag
    if (td == 0) td = n

    banner ("DecisionTreeID3: initial entropy entropy_0 = " + entropy_0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a feature column (e.g., 2 (Humidity)) and a value (e.g., 1 (High))
     *  use the frequency of occurrence of the value for each classification
     *  (e.g., 0 (no), 1 (yes)) to estimate k probabilities.  Also, determine
     *  the fraction of training cases where the feature has this value
     *  (e.g., fraction where Humidity is High = 7/14).
     *  @param dset   the list of dataset pairs to consider (e.g., (x-value, y-value))
     *  @param value  one of the possible values for this feature (e.g., 1 (High))
     */
    def frequency (dset: Array [(Int, Int)], value: Int): (Double, VectoI, VectoD) =
    {
        val nu    = new VectorI (k)                              // frequency counts
        var count = 0                                            // count for branch value
        for ((xi, yi) <- dset if xi == value) { nu(yi) += 1; count += 1 }

        (count.toDouble / dset.size, nu, nu.toDouble / count.toDouble)   // return fraction, frequency and probability vector
    } // frequency

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the information gain due to using the values of a feature/attribute
     *  to distinguish the training cases (e.g., how well does Humidity with its
     *  values Normal and High indicate whether one will play tennis).
     *  @param f     the feature to consider (e.g., 2 (Humidity))
     *  @param dset  the possibly restricted dataset to consider
     */
    def gain (f: Int, dset: Array [(Int, Int)]): (Double, VectoI) =
    {
        val nu   = new VectorI (k)                               // frequency counts
        var sum  = 0.0
        for (i <- 0 until vc(f)) {
            val (frac_fi, nu_fi, prob_fi) = frequency (dset, i)
            sum += frac_fi * entropy (prob_fi)
            nu  += nu_fi                                         // FIX - explain
        } // for
        val igain = entropy_0 - sum                              // the drop in entropy = information gain
        if (DEBUG) println (s"gain: entropy = $sum, overall gain from feature $f = $igain")
        (igain, nu)                                              // return the gain and frequency counts
    } // gain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract column from matrix, filtering out (x-value, y-value) pairs that are not on path.
     *  @param f     the feature to consider (e.g., 2 (Humidity))
     *  @param path  the path -- FIX explain                             
     */
    def dataset (f: Int, path: List [(Int, Int)]): Array [(Int, Int)] =
    {
        val col = x.col(f)().zipWithIndex
        col.filter (t => path.forall (tt => x(t._2, tt._1) == tt._2)).map (t => (t._1, y(t._2))).toArray
    } // dataset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the decision tree.
     *  @param itest  the indices for the test data
     */
    def train (itest: Ints): DecisionTreeID3 =                   // FIX - use these parameters
    {
        root = buildTree (List [(Int, Int)] (), 0)
        println ("Entropy of tree = " + Node2.calcEntropy (leaves))
        println ("No of leaves (original) = " + leaves.size)
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively build the decision tree given a path e.g. ((outlook, sunny), ...).
     *  @param path   an existing path in the tree ((feature, value), ...)
     *  @param depth  the depth of the subtree being built
     */
    def buildTree (path: List [(Int, Int)], depth: Int): FeatureNode =
    {
        val features = x.range2 diff path.map (_._1)                      // features to be considered, all except those on path

        var opt = (0, (0.0, null.asInstanceOf [VectoI]))                  // best (feature, (gain, frequency))
        for (f <- features) {
            val (fGain, nu) = gain (f, dataset (f, path))
            if (fGain > opt._2._1) opt = (f, (fGain, nu))
            if (DEBUG) { println ("-" * 60); println (s"buildTree: feature = x$f, fGain = $fGain, nu = $nu") }
        } // for
        println ("=" * 60)
        println (s"buildTree: optimal feature = x${opt._1}, gain = ${opt._2._1}, path = $path")

        val f = opt._1                                                    // best feature
        val node = FeatureNode (f, path, opt._2._2)

        for (b <- 0 until vc(f)) {                            // build subtree or leaf for each branch value
            if (DEBUG) println (s"- buildTree: explore branch $b of ${vc(f)} for feature x$f at depth $depth")
            if (hasDepthConstraint && depth == td - 1) {
                leaves += Node2.addLeaf (node.nu.argmax (), node.nu, node, b)
                println (s"buildTree: early termination: depth = $depth, td = $td")
                return node
            } else {
                val dset = dataset (f, (f, b) :: path)
                if (dset.size > 0) {
                    if (features.size == 0 || dset.map (_._2).toSet.size == 1) {
                        leaves += Node2.addLeaf (mode (dset.map (_._2)), freq4Node (dset), node, b)
                    } else {
                        node.branch += b -> buildTree ((f, b) :: path, depth+1)
                    } // if
                } // if
            } // else
        } // for
        node
    } // buildTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the frequency count of the classification vector for the node with
     *  dataset 'dset'.
     *  @param dset  the dataset under a node
     */
    private def freq4Node (dset: Array [(Int, Int)]): VectoI =
    {
        val nu = new VectorI (k)
        for ((xi, yi) <- dset) nu(yi) += 1
        nu
    } // freq4Node

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.  If no branch found
     *  given maximal decision of current node.
     *  Return the best class, its name and FIX.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        var node = root
        for (j <- 0 to n) {
            node match {
            case FeatureNode (f, path, count, branch) => 
                try {
                    node = branch (z(f))
                } catch { case nse: NoSuchElementException =>
                    val best = node.asInstanceOf [FeatureNode].nu.argmax ()
                    return (best, cn(best), -1.0)
                } // try
            case LeafNode (y, count) => 
                val best = y
                return (best, cn(best), -1.0)
            case _ =>
                println (s"classify: 'node match' failed for node node = $node")
                return (-1, "?", -1.0)
            } // match
        } // for
        println ("classify: failed at leaf node")
        (-1, "?", -1.0)
    } // classify

} // DecisionTreeID3 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3` companion object provides factory methods.
 */
object DecisionTreeID3
{
    import ClassifierInt.pullResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a decision tree for the given combined matrix where the last column
     *  is the response/classification vector.
     *  @param x   the data vectors stored as rows of a matrix
     *  @param y   the class array, where y_i = class for row i of the matrix x
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     *  @param vc  the value count array indicating number of distinct values per feature
     *  @param td  the maximum tree depth to allow (defaults to 0 => number of features, -1 no constraint
     */
    def apply (xy: MatriI, fn: Strings, k: Int, cn: Strings, vc: Array [Int], td: Int): DecisionTreeID3 =
    {
        val (x, y) = pullResponse (xy)
        new DecisionTreeID3 (x, y, fn, k, cn, vc, td)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the decision tree on the given dataset passed in as a combined matrix.
     *  @param xy      the data vectors along with their classifications stored as rows of a matrix
     *  @param fn      the names for all features/variables
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param vc      the value count array indicating number of distinct values per feature
     *  @param td      the maximum tree depth to allow (defaults to 0 => number of features, -1 no constraint
     */
    def test (xy: MatriI, fn: Strings, k: Int, cn: Strings,
              vc: Array [Int] = null, td: Int = 0): DecisionTreeID3 =
    {
        banner ("create, train and print a ID3 decision tree")
        println (s"dataset xy: ${xy.dim1}-by-${xy.dim2} matrix")
        val (x, y) = pullResponse (xy)
        val ymin   = y.min ()
        println (s"unadjusted ymin = $ymin")
        if (ymin != 0) y -= ymin
        val tree = new DecisionTreeID3 (x, y, fn, k, cn, vc, td)
        tree.train ()
        tree.printTree (vc)

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

} // DecisionTreeID3 object

import DecisionTreeID3.test

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3Test` object is used to test the `DecisionTreeID3` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured features.
 *  @see http://www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 *  > runMain scalation.analytics.classifier.DecisionTreeID3Test
 */
object DecisionTreeID3Test extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: Cold (0), Mild (1), Hot (2)
    // Humidity:    Normal (0), High (1)
    // Wind:        Weak (0), Strong (1)
    // features:    Outlook Temp Humidity Wind
    // classification vector: 0(no), 1(yes))

    banner ("ID3 Decision Tree for 'playtennis' dataset")
    val xy = new MatrixI ((14, 5), 2,   2,   1,   0,   0,        // day  1 - combined data matrix
                                   2,   2,   1,   1,   0,        // day  2
                                   1,   2,   1,   0,   1,        // day  3
                                   0,   1,   1,   0,   1,        // day  4
                                   0,   0,   0,   0,   1,        // day  5
                                   0,   0,   0,   1,   0,        // day  6
                                   1,   0,   0,   1,   1,        // day  7
                                   2,   1,   1,   0,   0,        // day  8
                                   2,   0,   0,   0,   1,        // day  9
                                   0,   1,   0,   0,   1,        // day 10
                                   2,   1,   0,   1,   1,        // day 11
                                   1,   1,   1,   1,   1,        // day 12
                                   1,   2,   0,   0,   1,        // day 13
                                   0,   1,   1,   1,   0)        // day 14

    val fn   = Array ("Outlook", "Temp", "Humidity", "Wind")     // feature names
    val cn   = Array ("no", "yes")
    val k    = cn.size
    val vc   = Array (3, 3, 2, 2)                                // distinct values for each feature
    val tree = test (xy, fn, k, cn, vc)                          // create and test decision tree

    banner ("Classify New Data")
    val z = VectorI (2, 2, 1, 1)                                 // new data vector to classify
    println (s"classify ($z) = ${tree.classify (z)}")

} // DecisionTreeID3Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3Test2` object is used to test the `DecisionTreeID3` class.
 *  Ex: Classify whether a there is breast cancer.
 *  > runMain scalation.analytics.classifier.DecisionTreeID3Test2
 */
object DecisionTreeID3Test2 extends App
{
    val fname = BASE_DIR + "breast_cancer.csv"
    val xy    = MatrixI (fname)
    val fn    = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                       "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val cn    = Array ("benign", "malignant")
    val k     = cn.size
    val vc    = (for (j <- 0 until xy.dim2-1) yield xy.col(j).max () + 1).toArray
    val td    = 5
    val tree  = test (xy, fn, k, cn, vc, td)                     // create and test decision tree

} // DecisionTreeID3Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3Test3` object is used to test the `DecisionTreeID3` class.
 *  Plot entropy.
 *  > runMain scalation.analytics.classifier.DecisionTreeID3Test3
 */
object DecisionTreeID3Test3 extends App
{
    import scalation.plot.Plot
    import scalation.math.log2

    val p = VectorD.range (1, 100) / 100.0
    val h = p.map (p => -p * log2 (p) - (1-p) * log2 (1-p))
    new Plot (p, h)

} // DecisionTreeID3Test3 object

