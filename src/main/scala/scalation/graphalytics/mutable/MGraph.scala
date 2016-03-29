
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Graph (MGraph) Data Structure Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scalation.graphalytics.{Pair, Tree}

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraph` class stores vertex/edge-labeled multi-directed graphs using
 *  an adjacency set ('ch') representation, e.g., ch = { {1, 2}, {0}, {1} } means
 *  that the graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *----------------------------------------------------------------------------
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of vertex labels
 *  @param elabel   the map of edge labels
 *  @param inverse  whether to store inverse adjacency sets (parents)
 *  @param name     the name of the multi-digraph
 */
class MGraph (ch:      Array [SET [Int]],
                label:   Array [TLabel] = Array.ofDim (0),
            val elabel:  Map [Pair, TLabel] = Map (),
                inverse: Boolean = false,
                name: String = "g")
      extends Graph (ch, label, inverse, name) with Cloneable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of 'this' multi-digraph.
     */
    override def clone: MGraph =
    {
        val ch2    = Array.ofDim [SET [Int]] (ch.length)
        val label2 = Array.ofDim [TLabel] (ch.length)
        for (i <- ch2.indices) {
            ch2(i) = SET (ch(i).toArray: _*)
            label2(i) = label(i)
        } // for
        new MGraph (ch2, label2, elabel.clone, inverse, name)
    } // clone

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the edges in the elabel map correspond to edges in the
     *  the adjacency list.
     */
    def checkElabels: Boolean =
    {
        for ((u, v) <- elabel.keys if ! (ch(u) contains v)) {
            println (s"checkElabels: no such edge from $u to $v")
            return false
        } // for
        true
    } // checkElabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make this multi-directed graph work like an undirected graph by making
     *  sure that for every edge 'u -> v', there is a 'v -> u' edge and that
     *  they have same edge label.
     */
    override def makeUndirected (): MGraph =
    {
        super.makeUndirected ()
        val edges = elabel.clone.keys
        for ((u, v) <- edges) elabel += (v, u) -> elabel(u, v)
        this
    } // makeUndirected

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' multi-digraph to a string in a shallow sense.
     *  Large arrays are not converted.  Use 'print' to show all information.
     */
    override def toString: String =
    {
        s"MGraph (ch.length = ${ch.length}, label.length = ${label.length}" +
        s"elabel.size = ${elabel.size}, inverse = $inverse, name = $name)"
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'i'th row/line of 'this' multi-digraph to a string.
     *  @param i     the ith row/line
     *  @param clip  whether to clip out "Set(" and ")"
     */
    override def toLine (i: Int, clip: Boolean = true): String =
    {
        val ch_i = ch(i).toString.replace ("Set(", "").replace (")", "")
        s"$i, ${label(i)}, $ch_i"
    } // toLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' multi-digraph in a deep sense with all the information.
     *  @param clip  whether to clip out "Set(" and ")"
     */
    override def printG (clip: Boolean = true)
    {
        println (s"MGraph ($name, $inverse, $size")
        for (i <- ch.indices) println (toLine (i))
        for ((k, v) <- elabel) println (s"$k -> $v")
        println (")")
    } // printG

} // MGraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraph` companion object provides builder methods and example query
 *  multi-digraphs.
 */
object MGraph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MGraph` from a `Graph`.
     *  @param g     the base `Graph` for building the `MGraph`
     *  @param eLab  the edge labels
     *  @param name  the name for the new multi-digraph
     */
    def apply (g: Graph, eLab: Map [Pair, TLabel], name: String): MGraph =
    {
        val g2 = g.clone ()
        new MGraph (g2.ch, g2.label, eLab, g2.inverse, name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MGraph` from a `Tree`.
     *  @param tree     the base `Tree` for building the `Graph`
     *  @param name     the name for the new digraph
     *  @param inverse  whether to add parent references
     */
    def apply (tree: Tree, name: String = "t", inverse: Boolean = false): MGraph =
    {
        val dg = Graph (tree, name, inverse)
        MGraph (dg, tree.labelMap, name)
    } // apply

    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    // data multi-digraph g1 -------------------------------------------------

    val g1 = MGraph (Graph.g1,
                       Map [Pair, TLabel] ((1, 0) -> -1,
                                           (1, 2) -> -1,
                                           (1, 3) -> -1,
                                           (1, 4) -> -1,
                                           (2, 0) -> -1,
                                           (3, 4) -> -2),                // change from -1 to -2 filter out vertices
                       "g1")

     // query multi-digraph q1 ------------------------------------------------

     val q1 = MGraph (Graph.q1,
                        Map [Pair, TLabel] ((0, 1) -> -1,
                                            (0, 2) -> -1,
                                            (2, 1) -> -1),
                        "q1")

    val g1p = new MGraph (g1.ch, g1.label, g1.elabel, true, g1.name)    // with parents
    val q1p = new MGraph (q1.ch, q1.label, q1.elabel, true, q1.name)    // with parents


    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data multi-digraph g2 -------------------------------------------------

     val g2 = MGraph (Graph.g2,
                        Map [Pair, TLabel] ((0, 1) -> 1,
                                            (1, 0) -> 1,
                                            (1, 2) -> 1,
                                            (1, 3) -> 1,
                                            (1, 4) -> 1,                   // 2
                                            (1, 5) -> 1,
                                            (5, 6) -> 1,
                                            (5, 10) -> 1,
                                            (6, 7) -> 1,
                                            (6, 4) -> 1,                   // 2
                                            (6, 8) -> 1,
                                            (6, 9) -> 1,
                                            (7, 1) -> 1,
                                            (10, 11) -> 1,
                                            (11, 12) -> 1,
                                            (12, 11) -> 1,
                                            (12, 13) -> 1,
                                            (14, 13) -> 1,
                                            (14, 15) -> 1,
                                            (15, 16) -> 1,
                                            (16, 17) -> 1,
                                            (16, 18) -> 1,
                                            (17, 14) -> 1,
                                            (17, 19) -> 1,
                                            (18, 20) -> 1,
                                            (19, 14) -> 1,
                                            (20, 19) -> 1,
                                            (20, 21) -> 1,
                                            (22, 21) -> 1,
                                            (22, 23) -> 1,
                                            (23, 25) -> 1,
                                            (25, 24) -> 1,
                                            (25, 26) -> 1,
                                            (26, 28) -> 1,
                                            (28, 27) -> 1,
                                            (28, 29) -> 1,
                                            (29, 22) -> 1),
                        "g2")

     // query multi-digraph q2 ------------------------------------------------

     val q2 = MGraph (Graph.q2,
                        Map [Pair, TLabel] ((0, 1) -> 1,
                                            (1, 0) -> 1,
                                            (1, 2) -> 1,
                                            (1, 3) -> 1),
                        "q2")
                         
    val g2p = new MGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // MGraph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphTest` object is used to test the `MGraph` class using the
 *  graphs given in the `MGraph` companion object.
 *  > run-main scalation.graphalytics.mutable.MGraphTest
 */
object MGraphTest extends App
{
    import MGraph._
    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MGraphTest

import MGraphGen._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphTest2` object is used to test the `MGraph` class using a
 *  randomly generated multi-digraph.
 *  > run-main scalation.graphalytics.mutable.MGraphTest2
 */
object MGraphTest2 extends App
{
    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct vertex labels
    private val eLabels   = 3          // number of distinct edge labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the graph

    genRandomGraph (nVertices, nLabels, eLabels, outDegree, inverse, name).printG ()

} // MGraphTest2

