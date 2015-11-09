
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Apr  9 13:31:26 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.mutable.ListBuffer
import scala.math.pow

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.random.{Randi, Variate}
import scalation.scala2d.Colors._
import scalation.scala2d.{Ellipse, QCurve, R2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeNode` class is for a node in the color tree.
 *  @param id    the unique identifier for the node
 *  @param lev   the level of the node in the tree
 *  @param colr  the color of the node
 */
class TreeNode (val id: Int, val lev: Int, var colr: Color = null)
{
     if (colr == null) colr = randomColor (id)
     val loc   = R2 (0.0, 0.0)
     val child = new ListBuffer [TreeNode] ()
     var parent: TreeNode = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if this node is an ancestor of node n.
     *  @param n  target node 
     */           
    def isAncestor (n: TreeNode): Boolean =
    {
        if (n == null) false else if (this == n) true else isAncestor (n.parent)
    } // isAncestor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a tree node to a string.
     */
    override def toString: String = "[ " + id + ", " + colr + " ]"

} // TreeNode class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorTree` companion object provides methods for building color trees.
 */
object ColorTree
{
    private var rng: Variate = null                    // random generator for # children

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly generate a color tree.
     *  @param depth   the depth of the tree
     *  @param minOut  the minimum number of children allowed (0 => binary tree)
     *  @param maxOut  the maximum number of children allowed (2 => binary tree)
     */
    def apply (depth: Double, minOut: Int = 0, maxOut: Int = 2, stream: Int = 0): ColorTree =
    {
        rng = new Randi (minOut, maxOut, stream)        // random generator for # children
        val root = new TreeNode (0, 0)                  // make the root node of tree
        val tree = new ColorTree (root, depth)          // make a color tree from root
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
        def genPre (depth: Double, p: TreeNode, lev: Int, ord: Int, sibs: Int)
        {
            val n = tree.add (p)                                             // add node n to tree
            tree.aniStep (n, p, ord, sibs)
            if (lev < depth) {
                val imax = rng.igen
                for (i <- 0 until imax) genPre (depth, n, lev+1, i, imax)    // add n's children
            } // if
        } // genPre

        tree
    } // apply

} // ColorTree object

import ColorTree._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorTree` class provides a data structure for multi-way trees with
 *  colored nodes.
 *  @param root   the root node of the tree
 *  @param depth  the estimated average depth of the tree (used by animation)
 */
class ColorTree (val root: TreeNode, depth: Double)
{
    private val TAB  = "    "                          // spaces for TAB
    private val MID  = 600.0                           // x coordinate for root
    private val TOP  = 100.0                           // y coordinate for root
    private val DIA  = 15.0                            // diameter for circles

    private val ani    = new DgAnimator ("ColorTree")  // tree animator
    private val cq     = ani.getCommandQueue           // tree animator command queue
    private val nodes  = ListBuffer [TreeNode] ()      // list of all nodes
    private var nCount = 0                             // node counter for id auto-increment

    root.loc.x = MID
    root.loc.y = TOP
    cq.add (AnimateCommand (CreateNode, 0, Ellipse (), " n-0", true, root.colr,
                            Array (root.loc.x, root.loc.y, DIA, DIA), 0.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'i'th node.
     *  @param  the index of the node to return
     */
    def apply (i: Int): TreeNode = nodes(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create and add a new node, link it in the tree and return it.
     *  @param p  the parent node (null for root)
     */
    def add (p: TreeNode): TreeNode =
    {
        nCount  += 1
        val n    = new TreeNode (nCount, p.lev+1)      // add node n
        nodes   += n                                   // add node n to nodes list
        n.parent = p                                   // comment out, if parent references not needed
        if (p != null) p.child += n                    // add n as child of p
        n                                              // return node n
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a new node, link it in the tree and return it.
     *  @param p  the parent node (null for root)
     *  @param n  the new node to be added
     */
    def add (p: TreeNode, n: TreeNode): TreeNode =
    {
        nCount  += 1
        nodes   += n                         // add node n to nodes list
        n.parent = p                         // comment out, if parent references not needed
        if (p != null) p.child += n          // add n as child of p
        n                                    // return node n
    } // add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of nodes in the color tree.
     */
    def size: Int = nodes.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the tree.
     */
    def printTree { printPre (root, 0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for printing the tree using a pre-order traversal.
     *  @param n    the current node to print
     *  @param lev  the level of the node => amount of indentation
     */
    private def printPre (n: TreeNode, lev: Int)
    {
        print (TAB * lev)                              // indent
        println (n)                                    // print node n
        for (c <- n.child) printPre (c, lev+1)         // print subtrees
    } // printPre

    private var t = 0.0                                // node display time

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prepare the animation step for adding node 'n' to the color tree.
     *  @param n     the new node to add to the color tree
     *  @param p     the parent node
     *  @param ord   the birth order of the node
     *  @param sibs  the number of siblings
     */
    def aniStep (n: TreeNode, p: TreeNode, ord: Int, sibs: Int)
    {
        val k   = n.id                                                        // node's id
        val f   = depth + 1 - n.lev                                           // width factor
        n.loc.x = p.loc.x + pow (3.0, f) * (1 + 2 * ord - sibs) * DIA / 2.0   // node's x coordinate
        n.loc.y = TOP + 4 * n.lev * DIA                                       // node's y coordinate
        t  += 500.0                                                           // node's display time
        cq.add (AnimateCommand (CreateNode, k, Ellipse (), " n-" + k, true, n.colr,
                                Array (n.loc.x, n.loc.y, DIA, DIA), t))
        cq.add (AnimateCommand (CreateEdge, -k, QCurve (), "", true, black, null, t+100.0, p.id, k))
    } // aniStep

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build up the animation step by traversing the tree.
     */
    def aniTree ()
    {
        for (j <- root.child.indices) aniPre (root.child(j), root, j, root.child.length)
        showAnimation ()
    } // aniTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for animating the tree using a pre-order traversal.
     *  @param n     the current node to animate
     *  @param p     the parent node
     *  @param ord   the birth order of the node
     *  @param sibs  the number of siblings
     */
    private def aniPre (n: TreeNode, p: TreeNode, ord: Int, sibs: Int) 
    {
        aniStep (n, p, ord, sibs)                        // animation step for node n
        for (j <- n.child.indices) aniPre (n.child(j), n, j, n.child.length)
    } // aniPre

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate the generation of the color tree.
     */
    def showAnimation () { ani.animate (0, 100000) }

} // ColorTree class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorTreeTest` object is used to test the `ColorTree` class.
 *  by randomly building a color tree.
 *  > run-main scalation.graphalytics.ColorTreeTest
 */
object ColorTreeTest extends App
{
    println ("--------------------------------------------------------------")

    val ct = ColorTree (3, 2, 3)
    ct.printTree
    ct.showAnimation ()

    println ("--------------------------------------------------------------")

} // ColorTreeTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorTreeTest2` object is used to test the `ColorTree` class.
 *  by manually building a color tree.
 *  > run-main scalation.graphalytics.ColorTreeTest2
 */
object ColorTreeTest2 extends App
{
    println ("--------------------------------------------------------------")

    val FANOUT = 3
    val root = new TreeNode (0, 0)                          // id = 0, lev = 0
    val ct = new ColorTree (root, 2.0)                      // root, depth = 2
    for (i <- 0 until FANOUT) {
        val n = ct.add (ct.root)
        ct.aniStep (n, root, i, FANOUT) 
        for (j <- 0 until FANOUT) {
            val m = ct.add (n)
            ct.aniStep (m, n, j, FANOUT) 
        } // for
    } // for
    ct.printTree
    ct.showAnimation ()

    println ("--------------------------------------------------------------")

} // ColorTreeTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorTreeTest3` object is used to test the `ColorTree` class.
 *  by manually building a color tree.
 *  > run-main scalation.graphalytics.ColorTreeTest3
 */
object ColorTreeTest3 extends App
{
    println ("--------------------------------------------------------------")

    val FANOUT = 3
    val root = new TreeNode (0, 0)                          // id = 0, lev = 0
    val ct = new ColorTree (root, 2.0)                      // root, depth = 2
    for (i <- 0 until FANOUT) {
        val n = ct.add (ct.root)
        for (j <- 0 until FANOUT) {
            val m = ct.add (n)
        } // for
    } // for
    ct.printTree
    ct.aniTree ()

    println ("--------------------------------------------------------------")

} // ColorTreeTest3 object

