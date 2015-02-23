
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Tue Apr  9 13:31:26 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.ListBuffer

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.math.DoubleWithExp._
import scalation.random.Randi
import scalation.scala2d.Colors._
import scalation.scala2d.{Ellipse, QCurve, R2}
import scalation.util.Count

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is for a node in the color tree.
 *  @param col  the color of the node
 */
case class Node (val id: Int, var colr: Color = null)
{
     if (colr == null) colr = randomColor (id)
     val loc   = R2 (0., 0.)
     var child = new ListBuffer [Node] ()
     var parent: Node = null

     override def toString: String = "[ " + id + ", " + colr + " ]"

} // Node class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class provides a data structure for multi-way trees with colored nodes.
 *  @param  minOut  the minimum number of children allowed (0 => binary tree)
 *  @param  maxOut  the maximum number of children allowed (2 => binary tree)
 */
class ColorTree (minOut: Int = 0, maxOut: Int = 2)
{
    private val TAB    = "    "                        // spaces for TAB
    private val MID    = 600.                          // x coordinate for root
    private val TOP    = 100.                          // y coordinate for root
    private val DIA    = 15.                           // diameter for circles
    private val rng    = new Randi (minOut, maxOut)    // random generator for # children
    private val nCount = Count ()                      // node counter for id auto-increment
    private val ani    = new DgAnimator ("ColorTree")  // tree animator
    private val cq     = ani.getCommandQueue           // tree animator command queue
            val root   = Node (0)                      // the root of the tree

    root.loc.x = MID
    root.loc.y = TOP
    cq += AnimateCommand (CreateNode, 0, Ellipse (), "no-0", true, root.colr,
                          Array (root.loc.x, root.loc.y, DIA, DIA), 0.)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a new node, link it in the tree and return it.
     *  @param p  the parent node (null for root)
     */
    def add (p: Node): Node =
    {
        val n    = Node (nCount++)         // add node n
        n.parent = p                       // comment out, if parent references not needed
        if (p != null) p.child += n        // add n as child of p
        n                                  // return node n
    } // add

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
    def genPre (depth: Int, p: Node, lev: Int, ord: Int, sibs: Int)
    {
        val n   = add (p)                                                    // add node n to tree
        val k   = n.id                                                       // node's id
        val f   = depth + 1 - lev                                            // width factor
        n.loc.x = p.loc.x + 3.~^f * (1 + 2 * ord - sibs) * DIA / 2.          // node's x coordinate
        n.loc.y = TOP + 4 * lev * DIA                                        // node's y coordinate
        val t   = 500. * k                                                   // node's display time
        cq += AnimateCommand (CreateNode, k, Ellipse (), "n-" + k, true, n.colr,
                              Array (n.loc.x, n.loc.y, DIA, DIA), t)
        cq += AnimateCommand (CreateEdge, -k, QCurve (), "", true, black, null, t+100., p.id, k)
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
    private def printPre (n: Node, lev: Int)
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
/** This object is used to test the ColorTree class.
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

