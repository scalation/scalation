
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Apr  9 13:31:26 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.ListBuffer
import math.pow

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.random.Randi
import scalation.scala2d.Colors._
import scalation.scala2d.{Ellipse, QCurve, R2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TreeNode` class is for a node in the color tree.
 *  @param id   the unique identifier for the node
 *  @param col  the color of the node
 */
class TreeNode (val id: Int, var colr: Color = null)
{
     if (colr == null) colr = randomColor (id)
     val loc   = R2 (0.0, 0.0)
     var child = new ListBuffer [TreeNode] ()
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
/** The `ColorTree` class provides a data structure for multi-way trees with
 *  colored nodes.
 *  @param minOut  the minimum number of children allowed (0 => binary tree)
 *  @param maxOut  the maximum number of children allowed (2 => binary tree)
 */
class ColorTree (minOut: Int = 0, maxOut: Int = 2)
{
    private val TAB    = "    "                        // spaces for TAB
    private val MID    = 600.0                         // x coordinate for root
    private val TOP    = 100.0                         // y coordinate for root
    private val DIA    = 15.0                          // diameter for circles
    private val rng    = new Randi (minOut, maxOut)    // random generator for # children
    private val ani    = new DgAnimator ("ColorTree")  // tree animator
    private val cq     = ani.getCommandQueue           // tree animator command queue
            val root   = new TreeNode (0)              // the root node of the tree
    private val nodes  = ListBuffer [TreeNode] ()      // list of all nodes
    private var nCount = 0                             // node counter for id auto-increment

    root.loc.x = MID
    root.loc.y = TOP
    cq.add (AnimateCommand (CreateNode, 0, Ellipse (), "no-0", true, root.colr,
                            Array (root.loc.x, root.loc.y, DIA, DIA), 0.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the ith node.
     *  @param  the index of the node to return
     */
    def apply (i: Int): TreeNode = nodes(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a new node, link it in the tree and return it.
     *  @param p  the parent node (null for root)
     */
    def add (p: TreeNode): TreeNode =
    {
        nCount  += 1
        val n    = new TreeNode (nCount)     // add node n
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
    /** Randomly generate a color tree.
     *  @param depth  the depth of the tree
     */
    def genTree (depth: Int)
    {
        if (depth > 0) {
            val imax = rng.igen
            for (i <- 0 until imax) genPre (depth, root, 1, i, imax)      // add root's children
        } // if
    } // genTree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for generating a tree using a pre-order traversal.
     *  @param depth  the depth of the tree
     *  @param p      the parent node
     *  @param lev    the level of the node
     *  @param ord    the birth order of the node
     *  @param sibs   the number of siblings
     */
    def genPre (depth: Int, p: TreeNode, lev: Int, ord: Int, sibs: Int)
    {
        val n   = add (p)                                                // add node n to tree
        val k   = n.id                                                   // node's id
        val f   = depth + 1 - lev                                        // width factor
        n.loc.x = p.loc.x + pow (3.0, f) * (1 + 2 * ord - sibs) * DIA / 2.0    // node's x coordinate
        n.loc.y = TOP + 4 * lev * DIA                                    // node's y coordinate
        val t   = 500.0 * k                                              // node's display time
        cq.add (AnimateCommand (CreateNode, k, Ellipse (), "n-" + k, true, n.colr,
                                Array (n.loc.x, n.loc.y, DIA, DIA), t))
        cq.add (AnimateCommand (CreateEdge, -k, QCurve (), "", true, black, null, t+100.0, p.id, k))
        if (lev < depth) {
            val imax = rng.igen
            for (i <- 0 until imax) genPre (depth, n, lev+1, i, imax)   // add n's children
        } // if
    } // genPre

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for printing the tree using a pre-order traversal.
     *  @param n    the current node to print
     *  @param lev  the level of the node => amount of indentation
     */
    private def printPre (n: TreeNode, lev: Int)
    {
        for (i <- 0 until lev) print (TAB)             // indent
        println (n)                                    // print node n
        for (c <- n.child) printPre (c, lev + 1)       // print subtrees
    } // printPre

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the tree.
     */
    def printTree { printPre (root, 0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate the generation of the color tree.
     */
    def showAnimation { ani.animate (0, 100000) }

} // ColorTree class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorTreeTest` object is used to test the `'ColorTree` class.
 */
object ColorTreeTest extends App
{
/*
    println ("--------------------------------------------------------------")

    val ct = new ColorTree (1, 4)
    for (i <- 0 until 3) ct.add (ct.root)
    ct.printTree
*/

    println ("--------------------------------------------------------------")

    val ct2 = new ColorTree (2, 3)
    ct2.genTree (3)
    ct2.printTree
    ct2.showAnimation

    println ("--------------------------------------------------------------")

} // ColorTreeTest object

