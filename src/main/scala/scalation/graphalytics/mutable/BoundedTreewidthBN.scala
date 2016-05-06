
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
 *  @version 1.2
 *  @date    Sat Nov 14 20:44:53 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Developed from pseudo-code in the following paper:
 *  "Learning Bounded Treewidth Bayesian Networks"
 *  @see pluto.huji.ac.il/~galelidan/papers/ElidanGouldJMLR.pdf
 */

//  U N D E R   D E V E L O P M E N T

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scalation.graphalytics.Pair
import scalation.util.Error

import BoundedTreeWidthChains._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BoundedTreeWidthBN` class provides bounded treewidth Bayesian Networks.
 *  @param g_in  the input directed graph (FIX: training data)
 *  @param kMax  the maximum allowed treewidth
 */
class BoundedTreeWidthBN (g_in: MGraph, kMax: Int)
      extends Error
{
    val DEBUG = true                                        // debug flag

    if (Cycle.hasCycle (g_in)) {
        flaw ("constructor", "graph g_in must be acyclic")
    } // if

    val stree = new MinSpanningTree (g_in, false, false)    // false, false => max spanning tree, not undirected

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Learn a Bayesian Network with Bounded Treewidth.  Starting with a spanning
     *  tree, add score improving edges, so long as the 'kMax' bound on the graph's
     *  treewidth is not exceeded.  This corresponds to Algorithm 1 in the paper.
     */
    def learnBN (): MGraph =
    {
        val tr = stree.span ()                              // create a maximum scoring spanning tree
        if (DEBUG) { tr.printTree; tr.aniTree }
        val g  = MGraph (tr)                                // create a graph from the maximum scoring spanning tree
        val mp = g.clone.makeUndirected ()                  // undirected skeleton of g (triangulated moralized graph)
        var k  = 1                                          // current treewidth

        while (k < kMax && positiveEdges) {
            val order = orderVertices (g, mp)               // determine vertex order - Algorithm 7
            val c = learnChain (g, order)                   // find max scoring chain - Algorithm 6

            for ((i, j) <- c) {                             // g union c - by adding c's edges to g
                g.ch(i)  += j                               // add edge to g
                g.elabel += (i, j) -> g_in.elabel (i, j)    // add edge label
            } // for

            for ((i, j) <- c) edgeUpdate (g, mp, (i, j))    // add additional edges - Algorithm 3
            k = maxCliqueSize (mp)                          // recompute the tree width
        } // while

        addEdgesGreedily (g, mp, k)                         // add edges that d'nt increase treewidth beyond kMax
        g
    } // learnBN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Greedily add edges to 'g' that do not increase treewidth beyond 'kMax'. 
     *  @param g   the maximum scoring graph of bounded treewidth
     *  @param mp  the triangulated moralized graph
     *  @param k   the current treewidth
     */
    def addEdgesGreedily (g: MGraph, mp: MGraph, k: Int)
    {
        while (k < kMax) {
            val (i, j) = findbestEdge (g, mp, k)            // find best remaining edge
            if (i < 0) return                               // unable to find
            g.ch(i) += j                                    // add edge to g
            g.elabel += (i, j) -> g_in.elabel (i, j)        // add edge label
            // adjust mp as well                            // FIX - to be implemented
        } // while
    } // addEdgesGreedily

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return the next best remaining edge.  Return (-1, -1) if there
     *  are none.
     *  @param g   the maximum scoring graph of bounded treewidth
     *  @param mp  the triangulated moralized graph
     *  @param k   the current treewidth
     */
    def findbestEdge (g: MGraph, mp: MGraph, k: Int): Pair =
    {
        (-1, -1)                                   // FIX - to be implemented
    } // findbestEdge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether positive scoring edges still exist.
     */
    def positiveEdges: Boolean = true               // FIX - to be implemented

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order the vertices and blocks in the graph.
     *  @see Algorithm 7
     *  @param g   the graph built up from the spanning tree
     *  @param mp  the triangulated moralized graph
     */
    def orderVertices (g: MGraph, mp: MGraph): (Array [Int], Array [Int]) =
    {
        (TopSort.topSort (g), blockSort (mp))
    } // orderVertices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order the blocks in the graph.
     *  @param mp  the triangulated moralized graph
     */
    def blockSort (mp: MGraph): Array [Int] =
    {
        null
    } // blockSort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the size of the largest clique in the graph.
     *  @param mp  the triangulated moralized graph whose maximum clique size is sought
     */
    def maxCliqueSize (mp: MGraph): Int =
    {
        1                                            // FIX - to be implemented
    } // maxCliqueSize

} // BoundedTreeWidthBN class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BoundedTreeWidthBNTest` is used to test the `BoundedTreeWidthBN` class.
 *  > run-main scalation.graphalytics.mutable.BoundedTreeWidthBNTest
 */
object BoundedTreeWidthBNTest extends App
{
    val g_in = new MGraph (Array (SET (1, 3, 4),               // ch(0)
                                  SET (2, 3),                  // ch(1)
                                  SET (3, 5),                  // ch(2)
                                  SET (4, 5),                  // ch(3)
                                  SET (),                      // ch(4)
                                  SET ()),                     // ch(5)
                                  Array.fill (6)(-1),          // vertex labels
                           Map ((0, 1) -> 1.0,                 // edge labels
                                (0, 3) -> 10.0,
                                (0, 4) -> 3.0,
                                (1, 2) -> 2.0,
                                (1, 3) -> 3.0,
                                (2, 3) -> 4.0,
                                (2, 5) -> 5.0,
                                (3, 4) -> 4.0,
                                (3, 5) -> 1.0))
    g_in.printG ()

    val bbn = new BoundedTreeWidthBN (g_in, 2)
    val g_out = bbn.learnBN ()
    g_out.printG ()

} // BoundedTreeWidthBNTest object

