
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Nov  7 21:01:31 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db

import scala.collection.mutable.{Map, Queue}

import scalation.graph_db.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SpanningTree` class is used to build spanning trees from graphs.
 *  @param g  the digraph to build the spanning tree from
 */
class SpanningTree (g: Graph [Double])
      extends Error
{
    private val DEBUG = true                                  // debug flag
    private val size  = g.size                                // the number of nodes for the spanning tree
    private val root  = new TreeNode [Double] (0, 0, 0.0)     // for vertex 0 in g, create a root node
    private val stree = new Tree [Double] (root, 3.5)         // make a tree based on this root, est. depth 
    private val n_map = Map [Int, TreeNode [Double]] ()       // node map from node id to tree node

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the spanning tree.
     */
    def printSTree () { stree.printTree () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate the spanning tree.
     */
//  def aniSTree () { stree.aniTree () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a spanning tree for the given graph, returning true if a complete
     *  spanning tree connecting all of g's vertices can be created.
     */
    def span (): Boolean =
    {
        n_map += 0 -> root                                    // put the root in the map   
        for (i <- 1 until size) {
            val (pi, ni) = findNext                           // find connection to a out node
            if (DEBUG) println (s"pi = $pi, ni = $ni")
            if (pi == -1) return false
            val p = n_map (pi)                                // parent node from parent index
            val n = new TreeNode [Double] (ni, p.lev+1, 0.0)  // make a tree node for ni
            n_map += ni -> n                                  // put node n in the map
            stree.add (p, n)                                  // connect parent p to node n
        } // for
        true
    } // span

    private val qu  = Queue (0)                        // queue of vertices in the spanning tree
    private val out = Array.fill (size)(true)          // status of outside spanning tree
    out(0) = false                                     // root is in (not outside)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the indices of a node pair, where 'pi' is an index in the spanning
     *  tree and 'ni' is 'out' (i.e, not yet in the spanning tree).
     */
    private def findNext: Tuple2 [Int, Int] =
    {
        while (qu.nonEmpty) {
            val pi = qu.front
            for (ni <- g.ch (pi) if out(ni)) { out(ni) = false; qu.enqueue (ni); return (pi, ni) }
            qu.dequeue ()                              // no connections left for pi
        } // while
        (-1, -1)
    } // findNext

} // SpanningTree class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SpanningTreeTest` object is used to test the `SpanningTree` class.
 *  > runMain scalation.graph_db.SpanningTreeTest
 */
object SpanningTreeTest extends App
{
    val g = EX_GRAPH.g2
    g.printG ()

    val st = new SpanningTree (g)
    if (! st.span ()) print ("un")
    println ("able to complete a spanning tree for graph " + g.name)
    println ("-" * 60)
    st.printSTree ()
//  st.aniSTree ()

} // SpanningTreeTest object

