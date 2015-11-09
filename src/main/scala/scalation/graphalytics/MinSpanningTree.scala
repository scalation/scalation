
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Nov  7 21:01:31 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

//  U N D E R   D E V E L O P M E N T

package scalation.graphalytics

import scala.collection.mutable.Map
//import scala.collection.mutable.PriorityQueue

import scalation.math.ExtremeD.MAX_VALUE
import scalation.util.PriorityQueue

import LabelType._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinSpanningTree` class is used to build minimum cost spanning trees
 *  from graphs.  Edge cost/weights are given by edge labels. `MinSpanningTree`
 *  implements Prim's algorithm.
 *  @see www.cse.ust.hk/~dekai/271/notes/L07/L07.pdf
 *  @param g  the multi-digraph to build the spanning tree from
 */
class MinSpanningTree (g: MDigraph)
      extends Error
{
    private val DEBUG = true                           // debug flag
    private val size  = g.size                         // the number of nodes for the spanning tree
    private val root  = new TreeNode (0, 0)            // for vertex 0 in g, create a root node
    private val stree = new ColorTree (root, 3.5)      // make a tree based on this root, est. depth 
    private val n_map = Map [Int, TreeNode] ()         // node map from node id to tree node

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the spanning tree.
     */
    def printSTree { stree.printTree }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the spanning tree.
     */
    def aniSTree { stree.aniTree }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a minimum cost spanning tree for the given graph, returning true
     *  if a complete spanning tree connecting all of g's vertices can be created.
     */
    def span (): Boolean =
    {
        val pred = makeITree ()                        // make an inverted tree
        for (ni <- 1 until size) {
            val pi = pred(ni)
            if (DEBUG) println (s"pi = $pi, ni = $ni")
            val p = n_map.getOrElse (pi, { val pp = new TreeNode (pi, 1); n_map += pi -> pp; pp })
            val n = n_map.getOrElse (ni, { val nn = new TreeNode (ni, p.lev+1); n_map += ni -> nn; nn })
            stree.add (p, n)
        } // for
        true                      // FIX - check for completeness
    } // span

    private val key  = Array.fill (size)(MAX_VALUE)    // cost/key array

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `NodeOrder` object defines the order of node indices based on
     *  their 'key' value.  Using -key to get "smallest first" in priority queue.
     */
    object NodeOrder extends Ordering [Int]
    {
        def compare (ni1: Int, ni2: Int): Int = -key(ni1) compare -key(ni2)
    } // NodeOrder

    private val qu   = PriorityQueue (Seq.range (0, size): _*)(NodeOrder)   // priority queue of vertices
    private val out  = Array.fill (size)(true)                              // status of outside spanning tree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make an inverted tree by recording the predecessor/parent array.
     *  Each note except the root will have one parent.  See pseudocode on p. 28
     *  @see www.cse.ust.hk/~dekai/271/notes/L07/L07.pdf
     */
    def makeITree (): Array [Int] =
    {
        val pred = Array.ofDim [Int] (size)            // predecessor node array
        key(0)   = TLabel_DEFAULT                      // start at the root (node index 0)
        pred(0)  = -1                                  // it has no predecessor/parent
        while (qu.nonEmpty) {                          // until all vertices in spanning tree
            val pi = qu.dequeue                        // return and remove least cost vertex
            for (ni <- g.ch (pi)) {                    // iterate through its children
                val cost = g.elabel (pi, ni)           // get cost from edge label
                if (out(ni) && cost < key(ni)) {
                   key(ni) = cost                      // lower the cost for node index ni
                   qu.increaseKey (ni, ni)             // reposition ni toward front in priority queue
                   pred(ni) = pi                       // set pred of ni to parent pi
                } // if
            } // for
            out(pi) = false                            // now finished with pi
        } // while
        if (DEBUG) println ("pred = " + pred.deep)
        pred
    } // makeITree

} // MinSpanningTree class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinSpanningTreeTest` object is used to test the `MinSpanningTree` class.
 *  > run-main scalation.graphalytics.MinSpanningTreeTest
 */
object MinSpanningTreeTest extends App
{
    val g = MDigraph.g2
    g.print ()

    val st = new MinSpanningTree (g)
    if (! st.span ()) print ("un")
    println ("able to complete a spanning tree for graph " + g.name)
    println ("-" * 60)
    st.printSTree
    st.aniSTree

} // MinSpanningTreeTest object

