
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Kevin Warrick, John Miller, Susan George
 *  @version 1.6
 *  @date    Tue Oct 16 17:03:00 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.collection.mutable.{ArrayBuffer, HashMap}

import scalation.linalgebra.{MatriD, VectoI}

import Probability.entropy

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DecisionTree` trait provides common capabilities for all types of
 *  decision trees.
 */
trait DecisionTree
{
    private [classifier] var root: Node2 = null                      // the root of the decision tree
    private [classifier] var leaves = ArrayBuffer [LeafNode] ()      // array buffer of leaf nodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the most frequent classification.
     *  @param y  the array of discrete classifications
     */
    def mode (y: Array [Int]): Int =
    {
        y.groupBy (yi => yi).map (g => (g._1, g._2.size)).maxBy (_._2)._1
    } // mode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the matrix 'x' is multivalued (>= 2 distinct rows).
     *  @param x  the given vector
     */
    def multivalued (x: MatriD): Boolean =
    {
        for (i <- 1 until x.dim1 if x(i) != x(i-1)) return true
        false
    } // multivalued

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the decision tree using 'prinT' method from `Node` class.
     *  @param vc  the value count array (number of values for each feature)
     */
    def printTree (vc: Array [Int])
    {
        println ("Decision Tree:")
        Node2.printT (root, 0, -1, vc)
        println ()
    } // printTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize counters, if needed.
     */
    def reset () { /* NA */ }

} // DecisionTree trait


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Node` class is used to hold information about a node in the decision tree.
 *  @param nu  the frequency count
 */
abstract class Node2 (nu: VectoI)
         extends Cloneable
{
    /** The sum of frequency counts
     */
    val nu_sum = nu.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Copy a node and all of its child nodes.
     *  @param vc  the value count
     */
    def copy (vc: Array [Int]): Node2 =
    {
        this match {
        case FeatureNode (f, path, nu, branch) => 
            deepCopy (FeatureNode (f, path, nu, branch.clone ()), vc)
        case LeafNode (y, nu) =>
            LeafNode (y, nu)
        } // match
    } // copy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method deep copies a node all the way down by creating new instances of feature node.
     *  This is required while pruning.
     *  @param curNode  the current node
     *  @param vc       the value count
     */
    def deepCopy (curNode: Node2, vc: Array [Int]): Node2 =
    {
        val fn = curNode.asInstanceOf [FeatureNode]
        for (i <- 0 until vc(fn.f)) {
            if (fn.branch.get(i) != None) {
                val node = fn.branch(i)
                if (node.isInstanceOf [FeatureNode]) {
                    val tFn    = node.asInstanceOf [FeatureNode]
                    val newFn  = new FeatureNode (tFn.f, tFn.path, tFn.nu, tFn.branch.clone ())
                    fn.branch += i -> newFn
                } // if
            } // if
        } // for
        fn
    } // deepCopy

} // Node2 abstract class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Node2` object provides helper functions.
 */
object Node2
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add leaf node to the decision tree and return the leaf.
     *  @param y       the output value of leaf node
     *  @param nu      frequency count of leaf
     *  @param parent  the parent node
     *  @param br      the branch identifier to add leaf
     */
    def addLeaf (y: Int, nu: VectoI, parent: FeatureNode, br: Int): LeafNode =
    {
        val leaf       = LeafNode (y, nu)
        leaf.parent    = parent
        parent.branch += br -> leaf
        leaf
    } // addLeaf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the entropy of the tree as the weighted average over the list of leaves.
     */
    def calcEntropy (leaves: ArrayBuffer [LeafNode]): Double =
    {
        var sum = 0.0
        for (nod <- leaves) sum += nod.nu_sum * entropy (nod.nu)
        sum / leaves.size
    } // calcEntropy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Recursively print the decision tree nodes.
     *  @param nod    the current node
     *  @param level  the level of node 'nod' in the tree
     *  @param branc  the branch of node 'nod'
     *  @param vc     the value count (vc) array
     */
    def printT (nod: Node2, level: Int, branc: Int, vc: Array [Int])
    {
        print ("\t" * level + "[ ")
        nod match {
        case FeatureNode (f, path, nu, branch) =>
            val thresh = nod.asInstanceOf [FeatureNode].threshold
            println (s"Node b$branc : f = x$f ( $nu ), threshold = $thresh ]")
            for (b <- 0 until vc(f)) {
                if (branch.get(b) != None) {
                    val node = branch.get(b).get
                    printT (node, level + 1, b, vc)
                } // if
            } // for
        case LeafNode (y, nu) =>
            println (s"Leaf b$branc : y = $y ( $nu ) ]")
        } // match
    } // printT

} // Node2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FeatureNode` class is for internal nodes.
 *  @param f       the feature/variable number used for splitting
 *  @param path    the path from the current node to the root {(parent node feature, branch)}
 *  @param nu      the frequency count
 *  @param branch  maps the branch value, e.g., f2 has values 0, 1, 3, for a node 
 */
case class FeatureNode (f: Int, path: List [(Int, Int)], nu: VectoI,
                        branch: HashMap [Int, Node2] = HashMap [Int, Node2] ())
     extends Node2 (nu) with Cloneable
{
    var threshold: Double = -1.0

} // FeatureNode class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LeafNode` class is for leaf nodes.
 *  @param y       the respone/decision value
 *  @param nu      the frequency count (count for each possible decision value for y)
 */
case class LeafNode (y: Int, nu: VectoI)
     extends Node2 (nu) with Cloneable
{
    var parent: FeatureNode = null

} // LeafNode class

