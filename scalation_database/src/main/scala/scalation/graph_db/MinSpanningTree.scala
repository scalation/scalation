
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Peng Hao
 *  @version 1.4
 *  @date    Sat Nov  7 21:01:31 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db

import scala.collection.mutable.{Map, Set => SET}
//import scala.collection.mutable.PriorityQueue

import scalation.math.ExtremeD.{MAX_VALUE, MIN_VALUE}
import scalation.graph_db.{ExampleMGraphD => EX_GRAPH}
import scalation.util.PriorityQueue

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinSpanningTree` class is used to build minimum cost spanning trees
 *  from graphs.  Edge cost/weights are given by edge labels. `MinSpanningTree`
 *  implements Prim's algorithm.
 *  @see www.cse.ust.hk/~dekai/271/notes/L07/L07.pdf
 *  @param g           the multi-digraph to build the spanning tree from
 *  @param min         whether to to create a minimum (true) or maximum (false) spanning tree
 *  @param undirected  whether the graph is already undirected
 */
class MinSpanningTree (g: MGraph [Double], min: Boolean = true, undirected: Boolean = true)
      extends Error
{
    private val DEBUG = false                                   // debug flag
    private val size  = g.size                                  // the number of nodes for the spanning tree
    private val root  = new TreeNode [Double] (0, 0, 0.0)       // for vertex 0 in g, create a root node
    private var stree: Tree [Double] = null                     // spanning tree built by calling span

    if (! undirected) g.makeUndirected ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the spanning tree.
     */
    def printSTree () { stree.printTree () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate the display of the spanning tree.
     */
//    def aniSTree () { stree.aniTree () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a minimum cost spanning tree for the given graph, returning true
     *  if a complete spanning tree connecting all of g's vertices can be created.
     */
    def span (): Tree [Double] =
    {
        val pred = makeITree ()                                     // make an inverted tree
        val el   = Array.ofDim [Double] (pred.length)               // copy elabel value from g into a pred elabel array
        for (i <- 1 until el.length) el(i) = g.elabel(pred(i), i)   // skipping root node (0)
        stree = Tree (pred, el, 3.5, "st")                          // build spanning tree from pred array
        stree
    } // span

    private val key = if (min) Array.fill (size)(MAX_VALUE) else Array.fill (size)(MIN_VALUE)  // cost/key array

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Elem` class is used for ordering elements on a priority queue.
     *  @param idx  the index of a node
     *  @param key  the ordering key (based on cost) for a node
     */
    case class Elem (idx: Int, key: Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `NodeOrder` object defines the order of node indices based on
     *  their 'key' value.  Using -key to get "smallest first" in priority queue.
     *  This is for minimum spanning trees ('min' = true)
     */
    object NodeOrder extends Ordering [Elem]
    {
        def compare (e1: Elem, e2: Elem): Int = -e1.key compare -e2.key
    } // NodeOrder

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `NodeOrder` object defines the order of node indices based on
     *  their 'key' value.  Using +key to get "largest first" in priority queue.
     *  This is for maximum spanning trees ('min' = false)
     */
    object NodeOrder2 extends Ordering [Elem]
    {
        def compare (e1: Elem, e2: Elem): Int = e1.key compare e2.key
    } // NodeOrder2

    private val qu = PriorityQueue ()(if (min) NodeOrder else NodeOrder2)   // priority queue of vertices
    for (i <- 0 until size) { qu.enqueue (Elem (i, key(i))) }               // put all vertices in priority queue
    private val out = Array.fill (size)(true)                               // status of outside spanning tree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make an inverted tree by recording the predecessor/parent array.
     *  Each note except the root will have one parent.  See pseudo-code on p. 28
     *  @see www.cse.ust.hk/~dekai/271/notes/L07/L07.pdf
     */
    def makeITree (): Array [Int] =
    {
        val pred = Array.fill (size)(-1)                                 // predecessor node array
        key(0)   = null.asInstanceOf [Double]                            // start at the root (node index 0)
        pred(0)  = -1                                                    // it has no predecessor/parent
        while (qu.nonEmpty) {                                            // until all vertices in spanning tree
            if (DEBUG) qu.printInOrder
            val pi = qu.dequeue ().idx                                   // return and remove least cost vertex
            if (DEBUG) println ("makeITree: dequeued pi = " + pi)
            for (ni <- g.ch(pi)) {                                       // iterate through its children
                val cost = g.elabel (pi, ni)                             // get cost from edge label
                if (out(ni) && (min && cost < key(ni) || !min && cost > key(ni))) {
                   qu.increaseKey (Elem (ni, key(ni)), Elem (ni, cost))  // reposition ni toward front in priority queue
                   key(ni) = cost                                        // lower the cost for node index ni
                   pred(ni) = pi                                         // set pred of ni to parent pi
                } // if
            } // for
            out(pi) = false                                              // now finished with pi
        } // while
        if (DEBUG) println ("pred = " + pred.deep)
        pred
    } // makeITree

} // MinSpanningTree class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinSpanningTreeTest` object is used to test the `MinSpanningTree` class.
 *  > runMain scalation.graph_db.MinSpanningTreeTest
 */
object MinSpanningTreeTest extends App
{
    val g = EX_GRAPH.g2
    g.printG ()

    val st = new MinSpanningTree (g)
    st.span ()
    println ("spanning tree for graph " + g.name)
    println ("-" * 60)
    st.printSTree ()
//  st.aniSTree

} // MinSpanningTreeTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinSpanningTreeTest2` object is used to test the `MinSpanningTree` class.
 *  @see www.cse.ust.hk/~dekai/271/notes/L07/L07.pdf
 *  > runMain scalation.graph_db.MinSpanningTreeTest2
 */
object MinSpanningTreeTest2 extends App
{
    val g = new MGraph (Array (SET (1, 3, 4),               // ch(0)
                               SET (2, 3),                  // ch(1)
                               SET (3, 5),                  // ch(2)
                               SET (4, 5),                  // ch(3)
                               SET (),                      // ch(4)
                               SET ()),                     // ch(5)
                               Array.fill (6)(-1.0),        // vertex labels
                        Map ((0, 1) -> 1.0,                 // edge labels
                             (0, 3) -> 10.0,
                             (0, 4) -> 3.0,
                             (1, 2) -> 2.0,
                             (1, 3) -> 3.0,
                             (2, 3) -> 4.0,
                             (2, 5) -> 5.0,
                             (3, 4) -> 4.0,
                             (3, 5) -> 1.0))
    g.printG ()

    val st = new MinSpanningTree (g)
    st.span ()
    println ("spanning tree for graph " + g.name)
    println ("-" * 60)
    st.printSTree ()
//  st.aniSTree ()

} // MinSpanningTreeTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MinSpanningTreeTest3` object is used to test the `MinSpanningTree` class.
 *  This test the Maximum Spanning Tree option.
 *  @see www.cse.ust.hk/~dekai/271/notes/L07/L07.pdf
 *  > runMain scalation.graph_db.MinSpanningTreeTest3
 */
object MinSpanningTreeTest3 extends App
{
    val g = new MGraph (Array (SET (1, 3, 4),               // ch(0)
                               SET (2, 3),                  // ch(1)
                               SET (3, 5),                  // ch(2)
                               SET (4, 5),                  // ch(3)
                               SET (),                      // ch(4)
                               SET ()),                     // ch(5)
                               Array.fill (6)(-1.0),        // vertex labels
                        Map ((0, 1) -> 1.0,                 // edge labels
                             (0, 3) -> 10.0,
                             (0, 4) -> 3.0,
                             (1, 2) -> 2.0,
                             (1, 3) -> 3.0,
                             (2, 3) -> 4.0,
                             (2, 5) -> 5.0,
                             (3, 4) -> 4.0,
                             (3, 5) -> 1.0))
    g.printG ()

    val st = new MinSpanningTree (g, false)
    st.span ()
    println ("spanning tree for graph " + g.name)
    println ("-" * 60)
    st.printSTree ()
//  st.aniSTree ()

} // MinSpanningTreeTest3 object

