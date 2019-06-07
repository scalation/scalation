
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed May 22 14:17:49 EDT 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.collection.mutable.{ArrayBuffer, SortedMap}

import scalation.linalgebra.{MatriI, VectoD, VectoI, VectorI}
import scalation.util.Error

import Probability.entropy

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2` trait provides common capabilities for all types of
 *  decision trees.
 */
trait DecisionTree2 extends Error
{
    private var root: Node = null                                // the root node
    private [classifier] val leaves = ArrayBuffer [Node] ()      // array buffer of leaf nodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the root node to the tree.
     *  @param r  the root node of the tree
     */
    def addRoot (r: Node) { root = r }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add child node 'c' to the tree via branch 'v' from node 'n'.
     *  @param n  the parent node
     *  @param v  the branch value from the parent node
     *  @param c  the child node
     */
    def add (n: Node, v: Int, c: Node)
    {
        c.pv = v                                                 // branch value from parent to child
        n.branch += v -> c                                       // add to parent's branch map
        if (c.isLeaf) leaves += c                                // if leaf, add to leaves
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add multiple child nodes to the tree via branchs from node 'n'.
     *  @param n   the parent node
     *  @param vc  the branch value and child node, repeatable
     */
    def add (n: Node, vc: (Int, Node)*)
    {
        for ((v, c) <- vc) add (n, v, c)
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** As part of tree pruning, turn an internal node into a leaf.
     *  @param n  the node to turn into a leaf (pruning all nodes below it)
     */
    def makeLeaf (n: Node)
    {
        if (! n.isLeaf) {
            for (c <- n.branch.values) leaves -= c               // remove children from leaves
            n.branch.clear ()                                    // clear branch map
            n.isLeaf = true                                      // set leaf flag
            leaves += n                                          // add n to leaves
        } else {
            println (s"makeLeaf: node $n already is a leaf")
        } // if
    } // makeLeaf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the entropy of the tree as the weighted average over the list
     *  of nodes (defualts to leaves).
     *  @param nodes  the nodes to compute the weighted entropy over
     */
    def calcEntropy (nodes: ArrayBuffer [Node] = leaves): Double =
    {
        var sum, ent = 0.0
        for (n <- nodes) {
            sum += n.nu_sum                                      // add number of counts for node n
            ent += n.nu_sum * entropy (n.nu)                     // unnormalized weighted entropy
        } // for
        ent / sum                                                // normalized weighted entropy
    } // calcEntropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.  If no branch found,
     *  give maximal decision of current node.
     *  Return the best class and its name.
     *  @param z  the data vector to classify
     */
    def classify2 (z: VectoI): Int = classify (z, root)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxilliary classify method facilitating recursion.
     *  @param z  the data vector to classify
     *  @param n  the current node in the tree
     */
    private def classify (z: VectoI, n: Node): Int =
    {
        if (n.isLeaf) n.y
        else {
            val zf = z(n.f)
            try classify (z, n.branch(zf))
            catch { case ex: NoSuchElementException =>
                flaw ("classify", s"unknown value z(${n.f}) = $zf not in ${n.branch.keys}")
                throw ex
            } // try
       } // if
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector z, classify it returning the class number (0, ..., k-1)
     *  by following a decision path from the root to a leaf.  If no branch found,
     *  give maximal decision of current node.
     *  Return the best class and its name.
     *  @param z  the data vector to classify
     */
    def classify2 (z: VectoD): Int = classify (z, root)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxilliary classify method facilitating recursion.
     *  @param z  the data vector to classify
     *  @param n  the current node in the tree
     */
    private def classify (z: VectoD, n: Node): Int =
    {
        if (n.isLeaf) n.y
        else {
            val zf = z(n.f)
            try {
                val cont = n.thres > Double.NegativeInfinity
                if (cont) classify (z, if (zf <= n.thres) n.branch(0) else n.branch(1))
                else      classify (z, n.branch(zf.toInt))
            } catch { case ex: NoSuchElementException =>
                flaw ("classify", s"unknown value z(${n.f}) = $zf not in ${n.branch.keys}")
                throw ex
            } // try
       } // if
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the decision tree using 'prinT' method from `Node` class.
     */
    def printTree ()
    {
        println ("Decision Tree:")
        Node.printT (root, 0)
        println ()
    } // printTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize counters, if needed.
     */
    def reset () { /* NA */ }

} // trait class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Node` class is used to hold information about a node in the decision tree.
 *  @param f       the feature/variable number used for splitting (negative => leaf)
 *  @param gn      the information gain recorded at this node
 *  @param nu      the frequency count
 *  @param parent  the parent node (null for root)
 *  @param y       the response/decision value
 *  @param isLeaf  whether the node is a leaf (terminal node)
 */
case class Node (f: Int, gn: Double, nu: VectoI, parent: Node = null, y: Int,
                 private [classifier] var isLeaf: Boolean = false)
     extends Cloneable
{
    private [classifier] var pv = -1                                   // the branch value from the parent node to this node
    private [classifier] var thres: Double = Double.NegativeInfinity   // threshold for continuous/ordinal features
    private [classifier] val nu_sum = nu.sum                           // sum of frequency counts
    private [classifier] val branch = SortedMap [Int, Node] ()         // maps the branch value, e.g., f2 has values 0, 1, 3,
                                                                       //                              for nodes n0, n1, n2

    override def toString: String = 
       if (! isLeaf && thres > Double.NegativeInfinity)
           s"$pv -> \t Node (f = $f, nu = $nu, y = $y, isLeaf = $isLeaf, thres = $thres)"
       else
           s"$pv -> \t Node (f = $f, nu = $nu, y = $y, isLeaf = $isLeaf)"

} // Node class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Node` companion object provides helper functions.
 */
object Node
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Recursively print the decision tree nodes, indenting each level.
     *  @param n       the current node
     *  @param level   the level of node in the tree
     */
    def printT (n: Node, level: Int)
    {
        if (n.isLeaf) {
            println ("\t" * level + "[ " + n + " ]")
        } else {
            println ("\t" * level + "[ " + n)
            for (c <- n.branch.values) printT (c, level + 1)
            println ("\t" * level + "]")
        } // if
    } // printT

} // Node object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree2Test` is used to test the `DecisionTree` class.
 *  > runMain scalation.analytics.classifier.DecisionTree2Test
 */
object DecisionTree2Test extends App
{
    object Tree extends DecisionTree2

    val n0 = Node (0, -0, VectorI (5, 9), null, 1)
    Tree.addRoot (n0)                                            // add root n0
        val n1 = Node (3, -0, VectorI (2, 3), n0, 1)
            val n2 = Node (-1, -0, VectorI (0, 3), n1, 1, true)
            val n3 = Node (-2, -0, VectorI (2, 0), n1, 0, true)
        Tree.add (n1, (0, n2), (1, n3))                          // add children of n1
        val n4 = Node (-3, -0, VectorI (0, 4), n0, 1, true)
        val n5 = Node (2, -0, VectorI (3, 2), n0, 0)
            val n6 = Node (-4, -0, VectorI (0, 2), n5, 1, true)
            val n7 = Node (-5, -0, VectorI (3, 0), n5, 0, true)
        Tree.add (n5, (0, n6), (1, n7))                          // add children of n5
    Tree.add (n0, (0, n1), (1, n4), (2, n5))                     // add children of n0

    Tree.printTree ()
    println (s"inital entropy = ${Tree.calcEntropy (ArrayBuffer (n0))}")
    println (s"final  entropy = ${Tree.calcEntropy ()}")

    println ("Classify New Data")
    val z = VectorI (2, 2, 1, 1)                                 // new data vector to classify
    println (s"classify2 ($z) = ${Tree.classify2 (z)}")

} // DecisionTree2Test

