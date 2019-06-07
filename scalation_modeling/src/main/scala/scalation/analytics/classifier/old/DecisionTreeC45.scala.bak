//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Jerry Shi, John Miller, Dong Yu Yu
 *  @version 1.3
 *  @date    Wed Jan  9 15:07:13 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://en.wikipedia.org/wiki/C4.5_algorithm
 */

package scalation.analytics.classifier

import scala.collection.mutable.{MutableList, Queue}
import scala.math.{ceil, floor}
import scala.util.control.Breaks.{break, breakable}
import scala.util.Sorting
import scalation.linalgebra.{VectoD, VectorD, VectoI, VectorI, MatriD, MatrixD}
import scalation.util.Error
import scalation.analytics.Probability.entropy

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45` class implements a Decision Tree classifier using the
 *  C4.5 algorithm.  The classifier is trained using a data matrix 'x' and a
 *  classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).  The 'vc' array gives the number of distinct values
 *  per feature (e.g., 2 for Humidity).
 *  @param x       the data vectors stored as rows of a matrix
 *  @param y       the class array, where y_i = class for row i of the matrix x
 *  @param fn      the names for all features/variables
 *  @param isCont  `Boolean` value to indicate whether according feature is continuous
 *  @param k       the number of classes
 *  @param cn      the names for all classes
 *  @param vc      the value count array indicating number of distinct values per feature
 */
class DecisionTreeC45 (val x: MatriD, val y: VectoI, fn: Array [String], isCont: Array [Boolean],
                       k: Int, cn: Array [String], private var vc: Array [Int] = null)
      extends ClassifierReal (x, y, fn, k, cn)
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Class that contains information for a tree node.
     *  @param f         feature of the node, if it is leaf, contains the feature of its parent
     *  @param value     branch value
     *  @param theshold  threshold for continuous feature
     *  @param leaf      `Boolean` value indicate whether is leaf node
     *  @param decision  decision if it is leaf node
     */
    class Node (val f: Int, var value: Double, val threshold: Double = -1,
                val leaf: Boolean = false, val decision: Int = -1)
    {
        var next = new MutableList [DecisionTreeC45#Node] ()
        
        override def toString (): String =
        {
            if (leaf) {
                val s="Node (LeafOf: " + fn(f) + "\t" + "BranchValue: "  + value + "\tclass: " + decision + ")"
                println(s)
                s
            } else if (isCont (f)) {
                if (value == -1) "Node (feature: " + fn(f) + "\t BranchValue: ROOT" + "\tThreshold: " + threshold + ")"
                else             "Node (feature: " + fn(f) + "\t BranchValue: " + value + "\tThreshold: " + threshold + ")"
            } else {
                if (value == -1) "Node (feature: " + fn(f) + "\t BranchValue: ROOT" + ")"
                else             "Node (feature: " + fn(f) + "\t BranchValue: " + value + ")"
            } // if
        } // toString

    } // Node class

    private val DEBUG  = false                       // debug flag
    private val y_prob = new VectorD (k)             // probability that class c occurs

    if (vc == null) vc = vc_default                  // set value count (vs) to default for binary data (2)
    for (i <- 0 until m) y_prob(y(i)) += 1
    y_prob /= md
    private val entropy_0 = entropy (y_prob)         // the initial entropy
    private var root: DecisionTreeC45#Node = null    // decision tree, store according to layers of tree
    private var threshold = new Array [Double] (n)   // threshold for continuous features (below <=, above >)
    
    for (i <- 0 until n if isCont(i)) vc(i) = 2      // for continuous features set vc to 2 (below, above)
      
    if (DEBUG) println ("Constructing a C45 Decision Tree: initial entropy = " + entropy_0)
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a feature column (e.g., 2 (Humidity)) and a value (e.g., 1 (High))
     *  use the frequency of occurrence the value for each classification
     *  (e.g., 0 (no), 1 (yes)) to estimate k probabilities.  Also, determine
     *  the fraction of training cases where the feature has this value
     *  (e.g., fraction where Humidity is High = 7/14).
     *  @param fCol   a feature column to consider (e.g., Humidity)
     *  @param value  one of the possible values for this feature (e.g., 1 (High))
     *  @param cont   indicates whether is calculating continuous feature
     *  @param thres  threshold for continuous feature
     */
    def frequency (fCol: VectoD, value: Double, cont: Boolean = false, thres: Double = 0):
                   Tuple2 [Double, VectorD] =
    {
        val prob  = new VectorD (k)        // probability vector for a given feature and value
        var count = 0.0
        if (cont) {                        // feature with continuous values
            if (value == 0) {
                for (i <- 0 until m if fCol(i) <= thres) {    // below threshold
                    count      += 1.0
                    prob(y(i)) += 1
                } // for
             } else {
                for (i <- 0 until m if fCol(i) > thres) {      // above threshold
                    count      += 1.0
                    prob(y(i)) += 1
                } // for
            } // if
        } else {                           // feature with discrete values
            for (i <- 0 until m if fCol(i) == value) {
                count      += 1.0
                prob(y(i)) += 1
            } // for
        } // if
        (count / md, prob /= count)        // return the fraction and the probability vector 
    } // frequency

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the information gain due to using the values of a feature/attribute
     *  to distinguish the training cases (e.g., how well does Humidity with its
     *  values Normal and High indicate whether one will play tennis).
     *  @param f  the feature to consider (e.g., 2 (Humidity))
     */
    def gain (f: Int): Double =
    {
        val fCol = x.col(f)            // extract column f from data matrix x
        val vals = vc(f)               // the number of distinct values for feature f
        var sum  = 0.0
        for (i <- 0 until vals) {
            val (coun_fi, prob_fi) = frequency (fCol, i, isCont(f), threshold(f))
            val entr_fi = entropy (prob_fi)           // entropy for feature f value i
            sum += coun_fi * entr_fi
        } // for
        val igain = entropy_0 - sum                   // the drop in entropy
        igain                                         // return the information gain
    } // gain

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a continuous feature, adjust its threshold to improve gain.
     *  @param f  the feature index to consider
     */
    def calThreshold (f: Int)
    {
        var thres    =  0.0                          // start with a threshold of 0
        var tmpThres =  0.0                          // try other thresholds
        var maxGain  = -1.0                          // maximum gain
        var tmpGain  =  0.0                          // gain with current threshold
        var fCol     = x.col(f)                      // feature column
        var values   = new MutableList [Double] ()   // values for feature
        for (i <- 0 until m if ! values.contains (fCol(i))) values += fCol(i)
        values = values.sorted

        if (DEBUG) {
            println("\n ************ Threshold calculation for feature = " + f)
            println("possible value for feature = " + f + "  are: " + values)
        } // if

        for (i <- 0 until values.length - 1) {
            tmpThres     = (values(i) + values(i+1)) / 2.0
            threshold(f) = tmpThres                  // tmp change for gain calculation
            tmpGain      = gain (f)                  // compute gain with new threshold
            if (DEBUG) println ("for threshold " + tmpThres + " the gain is " + tmpGain)
            if (tmpGain > maxGain) {
                thres   = tmpThres                   // found a better threshold
                maxGain = tmpGain                    // save better gain
            } // if
        } // for

        threshold(f) = thres                         // save best threshold for this feature
        if (DEBUG) println ("for feature "+ f + " threshold = " + thres)
    } // calThreshold

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return new x matrix and y array for next step of constructing decision tree.
     *  @param f      the feature index
     *  @param value  one of the features values
     */
    def nextXY (f: Int, value: Double): Tuple2 [MatrixD, VectorI] =      
    {
        var count = 0
        if (isCont(f)) {                       // feature with continuous values
            if (value == 0) {
                for (i <- 0 until m if x(i, f) <= threshold(f)) count += 1
            } else {
                for (i <- 0 until m if x(i, f) > threshold(f)) count += 1
            } // if
        } else {                               // feature with discrete values
            for (i <- 0 until m if x(i, f) == value) count += 1
        } // if

        val nx = new MatrixD (count, n)        // new x matrix
        val ny = new VectorI (count)           // new y array

        var x_index = 0
        if (isCont(f)) {                       // feature with continuous values
            if (value == 0) {
                for (i <- 0 until m if x(i, f) <= threshold(f)) {
                    ny(x_index) = y(i)
                    nx(x_index) = x(i)
                    x_index += 1
                } // for
            } else {
                for (i <- 0 until m if x(i, f) > threshold(f)) {
                    ny(x_index) = y(i)
                    nx(x_index) = x(i)
                    x_index += 1
                } // for
            } // if
        } else {                               // feature with discrete values
            for (i <- 0 until m if x(i, f) == value) {
                ny(x_index) = y(i)
                nx(x_index) = x(i)
                x_index += 1
            } // for
        } // if
        (nx, ny)
    } // nextXY
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifier, i.e., determine which feature provides the most
     *  information gain and select it as the root of the decision tree.
     *  @param interval train the tree using the features index within 
     *  
     */
    def train (interval: IndexedSeq [Int]): DecisionTreeC45 =    // FIX - use these parameters
    {
        if (DEBUG) {
            println ("train: inputs:")
            println ("\t x      = " + x)
            println ("\t y      = " + y)
            println ("\t vc     = " + vc.deep)
            println ("\t isCont = " + isCont.deep)
        } // if

        for (f <- 0 until n if isCont(f)) calThreshold (f)    // set threshold for cont. features
        var opt = (0, gain (0))                               // compute gain for feature 0
        
        if (DEBUG) println ("train: for feature " + opt._1 + " the gain is " + opt._2)
        
        for (f <- interval) {
            val fgain = gain (f)                              // compute gain for feature f
            if (DEBUG) println ("train: for feature " + f + " the gain is " + fgain)
            if (fgain > opt._2) opt = (f, fgain)              // save feature giving best gain
        } // for
        
        if (DEBUG) println ("train: \noptimal feature is " + opt._1 + " with a gain of " + opt._2)
        
        buildTree (opt)
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the next most distinguishing feature/attribute, extend the
     *  decision tree.
     *  @param opt  the optimal feature and its gain
     */
    def buildTree (opt: Tuple2 [Int, Double])
    {
        root = if (isCont(opt._1)) new Node (opt._1, -1, threshold(opt._1)) else new Node (opt._1, -1)

        for (i <- 0 until vc(opt._1)) {
            var next = nextXY(opt._1, i)
            if (next._2.size != 0) {  //if additionial spit doesn't cause empty nodes
                var nodeContainSameClass = true
                for (j <- 0 until next._2.dim - 1 if next._2(j) != next._2(j + 1)) nodeContainSameClass = false
                if (nodeContainSameClass) {
                    var t = next._1
                    root.next += new Node (opt._1, root.next.length, -1, true, next._2(0))
                    if (DEBUG) {
                        println (" --> Leaf = " + root.next)
                        println ("\t x      = " + next._1)
                        println ("\t y      = " + next._2)
                        println ("\t vc     = " + vc.deep)
                        println ("\t isCont = " + isCont.deep)
                    } // if
                } else {
                    var nodeContainSameFeatures = true    // used to examine if features are identical, then no need to build trees
                    for (i <- 0 until next._1.dim1-1) if (next._1(i) != next._1(i+1)) nodeContainSameFeatures = false
                    if (! nodeContainSameFeatures) {
                        val subtree = new DecisionTreeC45 (next._1, next._2, fn, isCont, k, cn, vc)
                        subtree.train ()
                        subtree.root.value = root.next.length
                        root.next += subtree.root
                    } // if
                } // if
            } // if
        } // for
    } // buildTree
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print out the decision tree using Breadth First Search (BFS).
     */
    def printTree ()
    {
        println("\n*********************")
        println("  DecisionTree:\n")
        var queue = new Queue [DecisionTreeC45#Node] ()
        queue += root

        do {
            var nd = queue.dequeue
            for (i <- 0 until nd.next.length) queue += nd.next(i)
            if (isCont (nd.f) && ! nd.leaf) {
                println (nd + " --> (" + nd.next.length + " )")
            } else {
                println (nd + " --> (" + nd.next.length + " )")
            } // if
        } while (! queue.isEmpty)
    } // printTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.
     *  Return the best class, its name and its FIX.
     *  @param z  the data vector to classify (purely discrete features)
     *
    def classify (z: VectoD): (Int, String, Double) =
    {
        if(DEBUG) println ("classify: purely discrete features\n")

        for (i <- 0 until n if z(i) >= vc(i)) {
            println("classify: the " + i + "th value is too large")
            break
        } // for

        var nd   = root         // current node
        var step = 0
        while ( ! nd.leaf) {
            println ("classify: step-" + step + ": " + nd)
            nd = nd.next(z(nd.f).toInt)
            step += 1
        } // while

        println ("classify step-" + step + ": " + nd + "\n")
        val best = nd.decision
        (best, cn(best), -1.0)               // FIX - need ranking metric
    } // classify
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.
     *  Return the best class, it ane and FIX.
     *  @param z  the data vector to classify (some continuous features)
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        if(DEBUG) println ("classify: some continuous features\n")

        var nd   = root         // current node
        var step = 0
        while ( ! nd.leaf) {
            if (DEBUG) println ("classify: step-" + step + ": " + nd)
            if (isCont (nd.f)) {
                nd = if (z(nd.f) <= nd.threshold) nd.next(0) else nd.next(1)
            } else {
                nd = nd.next(z(nd.f).toInt)
            } // if
            step += 1
        } // while

        if (DEBUG) println ("classify: step-" + step + ": " + nd + "\n")
        val best = nd.decision
        (best, cn(best), -1.0)          // FIX - need metric
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        // FIX: to be implemented
    } // reset

} // DecisionTreeC45 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `DecisionTreeC45` is the companion object for the `DecisionTreeC45` class.
 */
object DecisionTreeC45
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `DecisionTreeID3` object, passing 'x' and 'y' together in one table.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn      the names for all features/variables
     *  @param isCont  `Boolean` value to indicate whether according feature is continuous
     *  @param k       the number of classes
     *  @param cn      the names for all classes
     *  @param vc      the value count array indicating number of distinct values per feature
     */
    def apply (xy: MatriD, fn: Array [String], isCont: Array [Boolean], k: Int, cn: Array [String],
               vc: Array [Int] = null) =
    {
        new DecisionTreeC45 (xy(0 until xy.dim1, 0 until xy.dim2-1), xy.col(xy.dim2-1).toInt, fn, isCont, k, cn, vc)
    } // apply

} // DecisionTreeC45 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45Test` object is used to test the `DecisionTreeC45` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured
 *  features.
 *  @see http://www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 *  > runMain scalation.analytics.classifier.DecisionTreeC45_Test
 */
object DecisionTreeC45_Test extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: Cold (0), Mild (1), Hot (2)
    // Humidity:    Normal (0), High (1)
    // Wind:        Weak (0), Strong (1)
    // features:    Outlook Temp Humidity Wind
    
    val x  = new MatrixD ((14, 4),   2,     2,     1,     0,      // day  1 - data matrix
                                     2,     2,     1,     1,      // day  2
                                     1,     2,     1,     0,      // day  3
                                     0,     1,     1,     0,      // day  4
                                     0,     0,     0,     0,      // day  5
                                     0,     0,     0,     1,      // day  6
                                     1,     0,     0,     1,      // day  7
                                     2,     1,     1,     0,      // day  8
                                     2,     0,     0,     0,      // day  9
                                     0,     1,     0,     0,      // day 10
                                     2,     1,     0,     1,      // day 11
                                     1,     1,     1,     1,      // day 12
                                     1,     2,     0,     0,      // day 13
                                     0,     1,     1,     1)      // day 14
    // day:           1  2  3  4  5  6  7  8  9 10 11 12 13 14
    val y  = VectorI (0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0)   // classification vector: 0(No), 1(Yes))
    val vc = Array (3, 3, 2, 2)                                   // distinct values for each feature
    val fn = Array ("Outlook", "Temp", "Humidity", "Wind")        // feature names
    val flag = Array (false, false, false, false)                 // is continuous
    val cn = Array("No","Yes")
//  val x = new MatrixI ((14, 4),  0,     85,     85,    0,    
//                                 0,     80,     90,    1,    
//                                 1,     83,     78,    0,    
//                                 2,     70,     96,    0,    
//                                 2,     68,     80,    0,    
//                                 2,     65,     70,    1,    
//                                 1,     64,     65,    1,    
//                                 0,     72,     95,    0,    
//                                 0,     69,     70,    0,    
//                                 2,     75,     80,    0,    
//                                 0,     75,     70,    1,    
//                                 1,     72,     90,    1,
//                                 1,     81,     75,    0,    
//                                 2,     71,     80,    1)    
//                                 
//  val y  = VectorI (0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0)     // classification vector: 0(No), 1(Yes))
//  val vc = Array (3, 3, 2, 2)                                     // distinct values for each feature
//  val flag = Array (false, true, true, false)

    // train the classifier ---------------------------------------------------
    val cl = new DecisionTreeC45 (x, y, fn, flag, 2, vc = vc, cn = cn)      // create the classifier
    cl.train ()
    cl.printTree ()
    val t = VectorD(1,2,1,0)
    println("result" + cl.classify(t))
//  test sample ------------------------------------------------------------
//  val z = VectorD (2, 100, 77.5, 0)                               // new data vector to classify
//  println ("--- classify " + z + " = " + cl.classify (z) + "\n")
//  val z2 = VectorD (2, 100, 77.6, 1)                              // new data vector to classify
//  println ("--- classify " + z2 + " = " + cl.classify (z2) + "\n")
    
} // DecisionTreeC45Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeC45_Test2` object is used to test the `DecisionTreeC45` class
 *  using the well-known winequality dataset.
 *  > runMain scalation.analytics.classifier.DecisionTreeC45_Test2
 */
object DecisionTreeC45_Test2 extends App {

    val file        = BASE_DIR + "winequality-white.csv"
    val numberClass = 7

    val data   = MatrixD (file)
    val target = data.col (data.dim2-1).-=(3)                        // regularize the target value
    val sample = data.selectCols (Range(0, data.dim2 - 1).toArray)
    var fn     = new Array[String] (data.dim2-1)
    val cn     = new Array[String] (numberClass)
    for (i <- 0 until data.dim2-1) fn(i) = s"feature$i"
    for (i <- 0 until numberClass) cn(i) = s"class$i"
    val isC  = Array.fill (sample.dim2)(true)
    val tree = new DecisionTreeC45 (sample, target.toInt, fn, isCont = isC, k = numberClass, cn = cn)
    tree.train ()
    tree.printTree ()

} // DecisionTreeC45_Test2 object

