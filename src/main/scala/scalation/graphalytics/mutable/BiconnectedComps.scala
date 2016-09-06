
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Nov 15 21:44:37 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see algs4.cs.princeton.edu/41graph/Biconnected.java.html
 *  @see seclab.cs.sunysb.edu/sekar/cse548/ln/graph4.pdf
 *  @see pluto.huji.ac.il/~galelidan/papers/ElidanGouldJMLR.pdf
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.collection.mutable.{Set => SET}
import scala.math.min

import scalation.graphalytics.Pair

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Block` class is used record information about biconnected
 *  components (blocks).
 */
case class Block (_id: Int)
{
    val id = _id                                   // the block identifier
    val vertices = SET [Int] ()                    // set of vertices contained in
    var parent: Block = null                       // the parent block in block tree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a block to a `String`.
     */
    override def toString: String =
    {
        s"Block ($id, $vertices, " + (if (parent == null) "null)" else s"${parent.id})")
    } // toString

} // Block class

 
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BiconnectedComp` class provides methods for finding the cut points and
 *  blocks in undirected graphs.  Removal of a cut point will make the graph
 *  disconnected.  Cut points and blocks are also referred to as articulation
 *  points and biconnected components, respectively.
 *  @param g_  the graph whose cut points/blocks are sought
 */
case class BiconnectedComp [TLabel] (g_ : Graph [TLabel])
{
    private val DEBUG  = true                           // debug flag
    private val g      = g_.clone.makeUndirected ()     // the undirected version of g_
    private val n      = g.size                         // the number of vertices
    private val cp     = ArrayBuffer [Int] ()           // the cut points in graph g
    private val blocks = ArrayBuffer [Block] ()         // the array buffer of blocks in g

    private var bcount = 0                              // counter for assigning id's to blocks

    if (DEBUG) g.printG ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find all the cut points in undirected graph 'g_'.  Translated from Java code.
     *  Use when only cut points (not blocks) are sought.
     *  @see algs4.cs.princeton.edu/41graph/Biconnected.java.html
     */
    def cutPoints (): ArrayBuffer [Int] =
    {
        val pre = Array.fill (n)(-1)                    // array of vertex pre-order numbers
        val low = Array.fill (n)(-1)                    // array of vertex low-points
        var cnt = 0

        for (v <- 0 until n if pre(v) == -1) dfs (v, v)

        /*  Use Depth First Search (DFS) to find cut points.
         *  @param u  a parent vertex
         *  @param v  a child vertex
         */
        def dfs (u: Int, v: Int)
        {
            var children = 0
            pre(v) = cnt; cnt += 1
            low(v) = pre(v)
    
            for (w <- g.ch(v)) {
                if (pre(w) == -1) {
                    children += 1
                    dfs (v, w)
                    low(v) = min (low(v), low(w))              // update low number
                    if (low(w) >= pre(v) && u != v) cp += v    // non-root of DFS is an cut point if low(w) >= pre(v)
                } else if (w != u) {
                    low(v) = min (low(v), pre(w))              // update low number - ignore reverse of edge leading to v
                } // if
            } // for
    
            // root of DFS is an cut point if it has more than 1 child
            if (u == v && children > 1) cp += v
        } // dfs

        cp
    } // cutPoints

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find all the blocks (and cut points) in undirected graph 'g_'.
     *  The parent block is also determined.  Translated from pseudo-code.
     *  @see www.cs.umd.edu/class/fall2005/cmsc451/biconcomps.pdf
     */
    def findBlocks (): ArrayBuffer [Block] =
    {
        val stack  = Stack [Pair] ()                    // edge stack
        val depth  = Array.ofDim [Int](n)               // vertex depth
        val low    = Array.fill (n)(-1)                 // array of vertex low-points
        val parent = Array.fill(n)(-1)                  // array of vertex parents
        val go     = Array.fill (n)(true)               // go => not yet visited

        var pblock: Block = null                        // previous block
        var count  = 0                                  // counter for assigning block id's
        var root   = 0                                  // vertex of current root  

        for (v <- 0 until n if go(v)) { root = v; dfs (v) }

        /*  Use Depth First Search (DFS) to find blocks.
         *  @param u  a parent vertex
         *  @param v  a child vertex
         */
        def dfs (u: Int)
        {
            go(u)    = false
            count   += 1
            depth(u) = count
            low(u)   = count

            for (v <- g.ch(u)) {
                if (v == root) pblock = null
                if (go(v)) {
                    stack push ((u,v))
                    parent(v) = u
                    dfs (v)
                    if (low(v) >= depth(u)) makeBlock (u, v)
                    low(u) = min (low(u), low(v))
                } else if (v != parent(u) && depth(v) < depth(u)) {
                    stack push ((u,v))
                    low(u) = min (low(u), depth(v))
                } // if
            } // for
        } // dfs

        /*  Make a new block and add it to the array buffer.
         *  @param u  the from vertex in edge
         *  @param v  the to vertex in edge
         */
        def makeBlock (u: Int, v: Int)
        {
            if (DEBUG) println ("-" * 40)
            val block = Block (bcount); bcount += 1       // make a new block
            blocks   += block                             // add new block to array buffer
            if (pblock != null) pblock.parent = block     // make it the parent for previous block
            var cont = true

            while (cont) {
                val e = stack.pop ()
                if (DEBUG) println ("makeBlock: e = " + e)
                blocks(blocks.size-1).vertices ++= Seq (e._1, e._2)
                if (e == (u, v)) { cp += u; cont = false }
            } // while

            pblock = block                                // save as the previous block
        } // makeBlock

        blocks                                            // return all the blocks
    } // findblocks

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assemble the cut points and blocks in undirected graph 'g_' into a block tree.
     *  See Algorithm 7 and Figure 7.b in
     *  @see pluto.huji.ac.il/~galelidan/papers/ElidanGouldJMLR.pdf
     */
    def assembleBlockTree ()
    {
        if (blocks.size == 0) findBlocks ()
        if (DEBUG) { println ("cut points " + cp); println ("blocks = " + blocks) }

        // FIX - to be implemented
    } // assembleBlockTree

} // BiconnectedComp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BiconnectedCompTest` object tests the `BiconnectedComp` class.
 *  @see www.geeksforgeeks.org/biconnected-components
 *  > run-main scalation.graphalytics.mutable.BiconnectedCompTest
 */
object BiconnectedCompTest extends App
{
    val g = new Graph (Array (SET (1, 6),        // edges from 0
                              SET (2, 3),        // edges from 1
                              SET (3, 4),        // edges from 2
                              SET (4),           // edges from 3
                              SET (),            // edges from 4
                              SET (6, 7, 8),     // edges from 5
                              SET (),            // edges from 6
                              SET (8),           // edges from 7
                              SET (9),           // edges from 8
                              SET ()),           // edges from 9
                      Array.fill (10)(0.0))      // vertex labels
    g.printG ()
    println ("-" * 60)

    val bcc = BiconnectedComp (g)
    println ("-" * 60)
    println ("cut points = " + bcc.cutPoints ())
    println ("-" * 60)
    println ("blocks     = " + bcc.findBlocks ())
    println ("-" * 60)
    
} // BiconnectedCompTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BiconnectedCompTest2` object tests the `BiconnectedComp` class.
 *  See Figure 7.a in
 *  @see pluto.huji.ac.il/~galelidan/papers/ElidanGouldJMLR.pdf
 *  > run-main scalation.graphalytics.mutable.BiconnectedCompTest2
 */
object BiconnectedCompTest2 extends App
{
    val g1 = new Graph (Array (SET (1, 8),          // ch(0) - children of vertex 0
                               SET (2, 3),          // ch(1)
                               SET (4),             // ch(2)
                               SET (5),             // ch(3)
                               SET (6),             // ch(4)
                               SET (6),             // ch(5)
                               SET (7),             // ch(6)
                               SET (),              // ch(7)
                               SET (9),             // ch(8)
                               SET (10, 12),        // ch(9)
                               SET (11),            // ch(10)
                               SET (),              // ch(11)
                               SET (13),            // ch(12)
                               SET (14),            // ch(13)
                               SET ()),             // ch(14)
                        Array (0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0,
                               8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0),        // vertex labels
                        false, "g1")                                           // inverse, name

//  val mg1 = MGraph (g1, Map ((0, 1)   -> 2.0,     // edge labels
//                             (0, 8)   -> 2.0,
//                             (1, 2)   -> 9.0,
//                             (1, 3)   -> 9.0,
//                             (2, 4)   -> 1.0,
//                             (3, 5)   -> 1.0,
//                             (4, 6)   -> 3.0,
//                             (5, 6)   -> 3.0,
//                             (6, 7)   -> 7.0,
//                             (8, 9)   -> 5.0,
//                             (9, 10)  -> 9.0,
//                             (9, 12)  -> 2.0,
//                             (10, 11) -> 6.0,
//                             (12, 13) -> 9.0,
//                             (13, 14) -> 1.0),
//                    "mg1")

    g1.printG ()
    println ("-" * 60)

    val bcc = BiconnectedComp (g1)
    println ("-" * 60)
    println ("block tree = " + bcc.assembleBlockTree ())
    println ("-" * 60)

} // BiconnectedCompTest2 object

