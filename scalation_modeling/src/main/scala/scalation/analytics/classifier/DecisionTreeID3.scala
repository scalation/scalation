
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Kevin Warrick, John Miller
 *  @version 1.4
 *  @date    Wed Jan  9 15:07:13 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.{ceil, floor}
import scala.collection.mutable.HashMap

import scalation.analytics.Probability.entropy
import scalation.linalgebra.{MatriI, MatrixI, VectorD, VectoI, VectorI}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3` class implements a Decision Tree classifier using the
 *  ID3 algorithm.  The classifier is trained using a data matrix 'x' and a
 *  classification vector 'y'.  Each data vector in the matrix is classified into
 *  one of 'k' classes numbered '0, ..., k-1'.  Each column in the matrix represents
 *  a feature (e.g., Humidity).  The 'vc' array gives the number of distinct values
 *  per feature (e.g., 2 for Humidity).
 *  @param x   the data vectors stored as rows of a matrix
 *  @param y   the class array, where y_i = class for row i of the matrix x
 *  @param fn  the names for all features/variables
 *  @param k   the number of classes
 *  @param cn  the names for all classes
 *  @param vc  the value count array indicating number of distinct values per feature
 */
class DecisionTreeID3 (x: MatriI, y: VectoI, fn: Array [String], k: Int, cn: Array [String],
                      private var vc: VectoI = null)
      extends ClassifierInt (x, y, fn, k, cn)
{
    abstract class Node
    case class FeatureNode (f: Int, branches: HashMap [Int, Node]) extends Node
    case class LeafNode (y: Int) extends Node
    type Path = List [Tuple2 [Int, Int]]
    
    private val DEBUG    = false                  // debug flag
    private val y_prob   = new VectorD (k)        // probability that class c occurs

    if (vc == null) vc = vc_default               // set value count (vs) to default for binary data (2)
    for (i <- 0 until m) y_prob(y(i)) += 1
    y_prob /= md
    private val entropy_0 = entropy (y_prob)      // the initial entropy
    println ("the initial entropy entropy_0 = " + entropy_0)
    private var tree: Node = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a feature column (e.g., 2 (Humidity)) and a value (e.g., 1 (High))
     *  use the frequency of occurrence of the value for each classification
     *  (e.g., 0 (no), 1 (yes)) to estimate k probabilities.  Also, determine
     *  the fraction of training cases where the feature has this value
     *  (e.g., fraction where Humidity is High = 7/14).
     *  @param dset   the list of data set tuples to consider (e.g. value, row index)
     *  @param value  one of the possible values for this feature (e.g., 1 (High))
     */
    def frequency (dset: Array [Tuple2 [Int, Int]], value: Int): Tuple2 [Double, VectorD] =
    {
        val prob  = new VectorD (k)     // probability vector for a given feature and value
        var count = 0.0
        for ((i, j) <- dset if i == value) {
            count   += 1.0
            prob(j) += 1
        } // for
        (count / dset.size, prob /= count)     // return the fraction and the probability vector 
    } // frequency

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract column from matrix, filtering out values rows that are not on path.
     *  @param f  the feature to consider (e.g., 2 (Humidity))
     *  @param p  the path
     */
    def dataset (f: Int, path: Path): Array [Tuple2 [Int, Int]] =
    {
        val col = x.col(f).apply.zipWithIndex 
        col.filter (t => path.forall (tt => x(t._2, tt._1) == tt._2)).map (t => (t._1, y(t._2))).toArray
    } // dataset
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the most frequent classification.
     *  @param a  array of discrete classifications
     */ 
    def mode (a: Array [Int]): Int =
    {
        a.groupBy (i => i).map (t => (t._1, t._2.size)).maxBy (_._2)._1
    } // mode 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the information gain due to using the values of a feature/attribute
     *  to distinguish the training cases (e.g., how well does Humidity with its
     *  values Normal and High indicate whether one will play tennis).
     *  @param f  the feature to consider (e.g., 2 (Humidity))
     *  @param p  the path
     */
    def gain (f: Int, path: Path): Double =
    {
        val dset = dataset (f, path)                  // extract values from column f indata matrix x
//      val vals = dset.map (_._1).toSet.size
        val vals = vc(f)                              // number of distinct values for feature f
        var sum  = 0.0
        for (i <- 0 until vals) {
            val (coun_fi, prob_fi) = frequency (dset, i)
            val entr_fi = entropy (prob_fi)           // entropy for feature f value i
            println ("gain from feature " + f + " for value " + i + " is " + entr_fi)
            sum += coun_fi * entr_fi
        } // for
        val igain = entropy_0 - sum                   // the drop in entropy
        println ("entropy = " + sum + ": overall gain from feature " + f + " is " + igain)
        igain                                         // return the information gain
    } // gain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the decision tree.
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)    // FIX - use these parameters
    {
        tree = buildTree (List [Tuple2 [Int, Int]] ())
        println (tree)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extend the tree given a path e.g. ((outlook, sunny), ...).
     *  @param path an existing path in the tree ((feature, value), ...)
     */
    def buildTree (path: Path): Node = 
    {
        val features = ((0 until x.dim2) diff path.map (_._1))
                
        var opt = (0, gain (0, path))
        for (f <- features) {
            val fgain = gain (f, path)
            println ("for feature " + f + " the gain is " + fgain)
            if (fgain > opt._2) opt = (f, fgain)
        } // for
        println ("optimal feature is " + opt._1 + " with a gain of " + opt._2)
        
        val f = opt._1
        val node = new FeatureNode(f, new HashMap[Int, Node])
        for (b <- 0 until vc(f)) {    // build subtree or leaf for each branch value
            // base case
            val dset = dataset (f, (f, b) :: path)
            if (dset.size == 0 || features.size == 0 || dset.map (_._2).toSet.size == 1) {
                node.branches += b -> new LeafNode (mode (dset.map (_._2)))
            } else {
                node.branches += b -> buildTree((f, b) :: path)
            } // if
        } // for
        node
    } // buildTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.
     *  Return the best class, its name and FIX.
     *  @param z  the data vector to classify
     */
    def classify (z: VectoI): (Int, String, Double) =
    {
        var n = tree
        for (i <- 0 until z.dim) {
            n match { 
                case FeatureNode (f, branches) => n = branches (z(f))
                case LeafNode (y) => { val best = y; return (best, cn(best), -1.0) }   // FIX - need metric
            } // match
        } // for
        (-1, "?", -1.0)          // FIX - need metric
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        // FIX: to be implemented
    } // reset

} // DecisionTreeID3 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** `DecisionTreeID3` is the companion object for the `DecisionTreeID3` class.
 */
object DecisionTreeID3
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `DecisionTreeID3` object, passing 'x' and 'y' together in one table.
     *  @param xy  the data vectors along with their classifications stored as rows of a matrix
     *  @param fn  the names for all features/variables
     *  @param k   the number of classes
     *  @param cn  the names for all classes
     *  @param vc  the value count array indicating number of distinct values per feature
     */
    def apply (xy: MatriI, fn: Array [String], k: Int, cn: Array [String], vc: VectoI = null) =
    {
        new DecisionTreeID3 (xy(0 until xy.dim1, 0 until xy.dim2-1), xy.col(xy.dim2-1), fn, k, cn, vc)
    } // apply

} // DecisionTreeID3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTreeID3Test` object is used to test the `DecisionTreeID3` class.
 *  Ex: Classify (No/Yes) whether a person will play tennis based on the measured
 *  features.
 *  @see http://www.cise.ufl.edu/~ddd/cap6635/Fall-97/Short-papers/2.htm
 */
object DecisionTreeID3Test extends App
{
    // training-set -----------------------------------------------------------
    // Outlook:     Rain (0), Overcast (1), Sunny (2)
    // Temperature: Cold (0), Mild (1), Hot (2)
    // Humidity:    Normal (0), High (1)
    // Wind:        Weak (0), Strong (1)
    // features:                Outlook Temp Humidity Wind
    val x  = new MatrixI ((14, 4),  2,     2,     1,     0,       // day  1 - data matrix
                                    2,     2,     1,     1,       // day  2
                                    1,     2,     1,     0,       // day  3
                                    0,     1,     1,     0,       // day  4
                                    0,     0,     0,     0,       // day  5
                                    0,     0,     0,     1,       // day  6
                                    1,     0,     0,     1,       // day  7
                                    2,     1,     1,     0,       // day  8
                                    2,     0,     0,     0,       // day  9
                                    0,     1,     0,     0,       // day 10
                                    2,     1,     0,     1,       // day 11
                                    1,     1,     1,     1,       // day 12
                                    1,     2,     0,     0,       // day 13
                                    0,     1,     1,     1)       // day 14
    // day:           1  2  3  4  5  6  7  8  9 10 11 12 13 14
    val y  = VectorI (0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0)   // classification vector: 0(No), 1(Yes))
    val vc = VectorI (3, 3, 2, 2)                                 // distinct values for each feature
    val fn = Array ("Outlook", "Temp", "Humidity", "Wind")
    println ("x  = " + x)
    println ("y  = " + y)
    println ("vc = " + vc)
    println ("---------------------------------------------------------------")

    // train the classifier ---------------------------------------------------
    val id3 = new DecisionTreeID3 (x, y, fn, 2, null, vc)        // create the classifier            
    id3.train ()

    val z = VectorI (2, 2, 1, 1)                                 // new data vector to classify
    println ("classify (" + z + ") = " + id3.classify (z))

} // DecisionTreeID3Test object

