
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Apr  9 13:31:26 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  Toggle 'ANIMATE' flag in `Tree` object to turn animation of tree on/off.
 */

package scalation.graph_db

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.math.pow
import scala.reflect.ClassTag

//import scalation.animation.{AnimateCommand, DgAnimator}
//import scalation.animation.CommandType._
import scalation.random.{Randi, Variate}
import scalation.scala2d.Colors._
import scalation.scala2d.{Ellipse, QCurve, R2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeNode` class is for a node in a tree.
 *  @param nid    the unique identifier for the node
 *  @param lev    the level of the node in the tree
 *  @param label  the node/incoming edge label
 *  @param colr   the color of the node
 *  @param ord    the birth order
 */
class TreeNode [TLabel: ClassTag] (val nid: Int, val lev: Int, var label: TLabel,
                                   var colr: Color = null, var ord: Int = 0)
{
    private val DEBUG = true                                   // debug flag 

    if (colr == null) colr = randomColor (nid)
    val loc   = R2 (0.0, 0.0)
    val child = new ArrayBuffer [TreeNode [TLabel]] ()
    var parent: TreeNode [TLabel] = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if 'this' node is an ancestor of node 'n'.
     *  @param n  target node 
     */           
    def isAncestor (n: TreeNode [TLabel]): Boolean =
    {
        if (n == null) false else if (this == n) true else isAncestor (n.parent)
    } // isAncestor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' node is a leaf.
     */           
    def isLeaf: Boolean = child.size == 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the left sibling of 'this' node.
     */           
    def leftSibling: TreeNode [TLabel] =
    {
        val ls= if (parent != null && ord > 0) parent.child(ord - 1)
                else null
        if (DEBUG) println (s"leftSibling ($nid) = $ls") 
        ls
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the right sibling of 'this' node.
     */           
    def rightSibling: TreeNode [TLabel] =
    {
        val rs = if (parent != null && ord < parent.child.size - 1) parent.child(ord + 1)
                 else null
        if (DEBUG) println (s"rightSibling ($nid) = $rs") 
        rs
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a tree node to a string.
     */
    override def toString: String = s"[ $nid, $label, $colr ]"

} // TreeNode class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tree` companion object provides methods for building trees.
 *  Toggle 'ANIMATE' flag to turn animation on/off.
 */
object Tree
{
    private val DEBUG        = true                          // debug flag
    private val ANIMATE      = false                         // animation flag
    private var rng: Variate = null                          // random generator for # children

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly generate a tree.
     *  @param depth   the depth of the tree
     *  @param minOut  the minimum number of children allowed (0 => binary tree)
     *  @param maxOut  the maximum number of children allowed (2 => binary tree)
     *  @param stream  the random number stream
     */
    def apply [TLabel: ClassTag] (depth: Double, minOut: Int = 0, maxOut: Int = 2, stream: Int = 0): Tree  [TLabel]=
    {
        rng = new Randi (minOut, maxOut, stream)             // random generator for # children
        val root = new TreeNode [TLabel] (0, 0, null.asInstanceOf [TLabel])           // make the root node of tree
        val tree = new Tree [TLabel] (root, depth)           // make a tree from root
        if (depth > 0.0) {
            val imax = rng.igen
            for (i <- 0 until imax) genPre (depth, root, 1, i, imax)         // add root's children
        } // if

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Recursive helper method for generating a tree using a pre-order traversal.
         *  @param depth  the depth of the tree
         *  @param p      the parent node
         *  @param lev    the level of the node
         *  @param ord    the birth order of the node
         *  @param sibs   the number of siblings
         */
        def genPre (depth: Double, p: TreeNode [TLabel], lev: Int, ord: Int, sibs: Int)
        {
            val n = tree.add (p)                                             // add node n to tree
//          if (ANIMATE) tree.aniStep (n, p, ord, sibs)
            if (lev < depth) {
                val imax = rng.igen
                for (i <- 0 until imax) genPre (depth, n, lev+1, i, imax)    // add n's children
            } // if
        } // genPre

        tree
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tree from an inverted tree, i.e., a predecessor/parent list.
     *  @see `TreeTest4` for an example.
     *  @param pred   the predecessor/parent list
     *  @param labl   the node labels
     *  @param depth  the estimated average depth of the tree (used by animation)
     *  @param name   the name of tree
     */
    def apply [TLabel: ClassTag] (pred: Array [Int], labl: Array [TLabel], depth: Double, name: String): Tree [TLabel] =
    {
        if (DEBUG) println ("apply: pred = " + pred.deep + ", name = " + name)

        val lab   = if (labl == null) Array.fill (pred.length)(null.asInstanceOf [TLabel]) else labl
        val root  = new TreeNode [TLabel] (0, 0, lab(0))       // for vertex 0 in g, create a root node
        val tree  = new Tree [TLabel] (root, depth, name)      // make a tree based on this root, est. depth
        val n_map = Map [Int, TreeNode [TLabel]] ()            // node map from node id to tree node
        n_map += 0 -> root                            // put the root node in the tree map

        for (ni <- 1 until pred.length) {
            val pi = pred(ni)
            if (pi >= 0) {
                if (DEBUG) println (s"apply: pi = $pi, ni = $ni")
                val p = n_map.getOrElse (pi, { val pp = new TreeNode [TLabel] (pi, 1, lab(pi)); n_map += pi -> pp; pp })
                val n = n_map.getOrElse (ni, { val nn = new TreeNode [TLabel] (ni, p.lev+1, lab(ni)); n_map += ni -> nn; nn })
                tree.add (p, n)
            } // if
        } // for
        tree
    } // apply

} // Tree object

import Tree._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tree` class provides a data structure for multi-way trees with
 *  colored nodes.
 *  @param root   the root node of the tree
 *  @param depth  the estimated average depth of the tree (used by animation)
 */
class Tree [TLabel: ClassTag] (val root: TreeNode [TLabel], depth: Double, val name: String = "tree")
{
    private val DEBUG = true                          // debug flag
    private val TAB   = "    "                        // spaces for TAB
    private val MID   = 600.0                         // x coordinate for root
    private val TOP   = 100.0                         // y coordinate for root
    private val DIA   = 15.0                          // diameter for circles
    private val nodes  = ArrayBuffer (root)           // list of all nodes
    private var nCount = 0                            // node counter for nid auto-increment

    /** Tree animator
     */
//  private val ani = if (ANIMATE) new DgAnimator ("Tree") else null

    /** Tree command queue
     */
//  private val cq = if (ANIMATE) ani.getCommandQueue else null

    root.loc.x = MID
    root.loc.y = TOP
//  if (ANIMATE) cq.add (AnimateCommand (CreateNode, 0, Ellipse (), " n-0", true, root.colr,
//                                Array (root.loc.x, root.loc.y, DIA, DIA), 0.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'i'th node.
     *  @param  the index of the node to return
     */
    def apply (i: Int): TreeNode [TLabel] = nodes(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create and add a new node, link it in the tree and return it.
     *  @param p  the parent node (null for root)
     */
    def add (p: TreeNode [TLabel]): TreeNode [TLabel] =
    {
        nCount  += 1
        val n    = new TreeNode [TLabel] (nCount, p.lev+1, null.asInstanceOf [TLabel])   // add node n
        nodes   += n                                                                     // add node n to nodes list
        n.parent = p                                                                     // comment out, if parent references not needed
        if (p != null) {
            p.child += n                                                                 // add n as child of p
            n.ord = p.child.size - 1                                                     // record n's birth order
        } // if
        n                                                                                // return node n
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a new node, link it in the tree and return it.
     *  @param p  the parent node (null for root)
     *  @param n  the new node to be added
     */
    def add (p: TreeNode [TLabel], n: TreeNode [TLabel]): TreeNode [TLabel] =
    {
        if (DEBUG) println (s"add: p = $p, n = $n")
        nCount  += 1
        nodes   += n                                  // add node n to nodes list
        n.parent = p                                  // comment out, if parent references not needed
        if (p != null) {
            p.child += n                              // add n as child of p
            n.ord = p.child.size - 1                  // record n's birth order
        } // if
        n                                             // return node n
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of nodes in the tree.
     */
    def size: Int = nodes.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a map of labels for nodes with incoming edges in the tree.
     */
    def labelMap: Map [Pair, TLabel] =
    {
        val labMap = Map [Pair, TLabel] ()
        for (n <- nodes if n != root) labMap += (n.parent.nid, n.nid) -> n.label
        labMap
    } // labelMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the tree.
     */
    def printTree () { printPre (root, 0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for printing the tree using a preorder traversal.
     *  @param n    the current node to print
     *  @param lev  the level of the node => amount of indentation
     */
    private def printPre (n: TreeNode [TLabel], lev: Int)
    {
        print (TAB * lev)                             // indent
        println (n)                                   // print node n
        for (c <- n.child) printPre (c, lev+1)        // print subtrees
    } // printPre

    private var t = 0.0                               // node display time

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prepare the animation step for adding node 'n' to the tree.
     *  @param n     the new node to add to the tree
     *  @param p     the parent node
     *  @param ord   the birth order of the node
     *  @param sibs  the number of siblings
     *
    def aniStep (n: TreeNode [TLabel], p: TreeNode [TLabel], ord: Int, sibs: Int)
    {
        if (ANIMATE) {
            val k   = n.nid                                                       // node's id
            val f   = depth + 1 - n.lev                                           // width factor
            n.loc.x = p.loc.x + pow (3.0, f) * (1 + 2 * ord - sibs) * DIA / 2.0   // node's x coordinate
            n.loc.y = TOP + 4 * n.lev * DIA                                       // node's y coordinate
            t  += 500.0                                                           // node's display time
            cq.add (AnimateCommand (CreateNode, k, Ellipse (), " n-" + k, true, n.colr,
                                    Array (n.loc.x, n.loc.y, DIA, DIA), t))
            cq.add (AnimateCommand (CreateEdge, -k, QCurve (), "", true, black, null, t+100.0, p.nid, k))
        } // if
    } // aniStep
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build up the animation step by traversing the tree.
     *
    def aniTree ()
    {
        if (ANIMATE) {
            for (j <- root.child.indices) aniPre (root.child(j), root, j, root.child.length)
            showAnimation ()
        } // if
    } // aniTree
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for animating the tree using a preorder traversal.
     *  @param n     the current node to animate
     *  @param p     the parent node
     *  @param ord   the birth order of the node
     *  @param sibs  the number of siblings
     *
    private def aniPre (n: TreeNode [TLabel], p: TreeNode [TLabel], ord: Int, sibs: Int) 
    {
        aniStep (n, p, ord, sibs)                        // animation step for node n
        for (j <- n.child.indices) aniPre (n.child(j), n, j, n.child.length)
    } // aniPre
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate the generation of the tree.
     */
//  def showAnimation () { if (ANIMATE) ani.animate (0, 100000) }

} // Tree class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeTest` object is used to test the `Tree` class by randomly building
 *  a tree.
 *  > runMain scalation.graphalytics.TreeTest
 */
object TreeTest extends App
{
    println ("--------------------------------------------------------------")

    val ct = Tree [Int] (3, 2, 3)
    ct.printTree ()
//  ct.showAnimation ()

    println ("--------------------------------------------------------------")

} // TreeTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeTest2` object is used to test the `Tree` class by manually building
 *  a tree.
 *  > runMain scalation.graphalytics.TreeTest2
 */
object TreeTest2 extends App
{
    println ("--------------------------------------------------------------")

    val FANOUT = 3
    val root = new TreeNode (0, 0, null.asInstanceOf [Double])     // nid = 0, lev = 0
    val ct = new Tree (root, 2.0)                                  // root, depth = 2
    for (i <- 0 until FANOUT) {
        val n = ct.add (ct.root)
//      ct.aniStep (n, root, i, FANOUT) 
        for (j <- 0 until FANOUT) {
            val m = ct.add (n)
//          ct.aniStep (m, n, j, FANOUT) 
        } // for
    } // for
    ct.printTree ()
//  ct.showAnimation ()

    println ("--------------------------------------------------------------")

} // TreeTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeTest3` object is used to test the `Tree` class by manually building
 *  a tree.
 *  > runMain scalation.graphalytics.TreeTest3
 */
object TreeTest3 extends App
{
    println ("--------------------------------------------------------------")

    val FANOUT = 3
    val root = new TreeNode (0, 0, null.asInstanceOf [Double])     // nid = 0, lev = 0
    val tree = new Tree (root, 2.0)                                // root, depth = 2
    for (i <- 0 until FANOUT) {
        val n = tree.add (tree.root)
        for (j <- 0 until FANOUT) {
            val m = tree.add (n)
        } // for
    } // for
    tree.printTree ()
//  tree.aniTree ()

    println ("--------------------------------------------------------------")

} // TreeTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeTest4` object is used to test the `Tree` class by manually building
 *  a tree.  No animation.
 *  > runMain scalation.graphalytics.TreeTest4
 */
object TreeTest4 extends App
{
    println ("--------------------------------------------------------------")

    val pred = Array (-1, 0, 0, 1, 1, 2, 2)
    val labl = Array (10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
    val tree = Tree (pred, labl, 3.0, "t")
    tree.printTree ()

    println ("--------------------------------------------------------------")

} // TreeTest4 object

