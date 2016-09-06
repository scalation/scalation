
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Graph 'MGraph' Data Structure Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.{Pair, Tree}
import scalation.graphalytics.mutable.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraph` class stores vertex/edge-labeled multi-directed graphs using
 *  an adjacency set 'ch' representation, e.g., 'ch = { {1, 2}, {0}, {1} }' means
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
class MGraph [TLabel: ClassTag] (ch:      Array [SET [Int]],
                                 label:   Array [TLabel] = Array.ofDim (0),
                             val elabel:  Map [Pair, TLabel] = Map (),
                                 inverse: Boolean = false,
                                 name: String = "g")
      extends Graph [TLabel] (ch, label, inverse, name) with Cloneable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of 'this' multi-digraph.
     */
    override def clone: MGraph [TLabel] =
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
    /** Check whether the edges in the 'elabel' map correspond to edges in the
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
    override def makeUndirected (): MGraph [TLabel] =
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
     *  @param i     the 'i'th row/line
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
     *  @param gr    the base `Graph` for building the `MGraph`
     *  @param eLab  the edge labels
     *  @param name  the name for the new multi-digraph
     */
    def apply [TLabel: ClassTag] (gr: Graph [TLabel], eLab: Map [Pair, TLabel], name: String): MGraph [TLabel] =
    {
        new MGraph (gr.ch, gr.label, eLab, gr.inverse, name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MGraph` from a `Tree`.
     *  @param tree     the base `Tree` for building the `MGraph`
     *  @param name     the name for the new multi-digraph
     *  @param inverse  whether to add parent references
     */
    def apply (tree: Tree, name: String = "t", inverse: Boolean = false): MGraph [Double] =
    {
        MGraph (Graph (tree, name, inverse), tree.labelMap, name)
    } // apply

} // MGraph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphTest` object is used to test the `MGraph` class using examples
 *  from the `ExampleMGraphI` object, which contains multi-digraphs whose vertex
 *  and edge labels are of type `Double`.
 *  > run-main scalation.graphalytics.mutable.MGraphTest
 */
object MGraphTest extends App
{
    import ExampleMGraphI._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MGraphTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphTest2` object is used to test the `MGraph` class using examples
 *  from the `ExampleMGraphD` object, which contains multi-digraphs whose vertex
 *  and edge labels are of type `Double`.
 *  > run-main scalation.graphalytics.mutable.MGraphTest2
 */
object MGraphTest2 extends App
{
    import ExampleMGraphD._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MGraphTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphTest3` object is used to test the `MGraph` class using a
 *  randomly generated multi-digraph.
 *  > run-main scalation.graphalytics.mutable.MGraphTest3
 */
object MGraphTest3 extends App
{
    val mgGen = new MGraphGen [Double]

    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct vertex labels
    private val eLabels   = 3          // number of distinct edge labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the graph

    mgGen.genRandomGraph (nVertices, nLabels, eLabels, outDegree, inverse, name).printG ()

} // MGraphTest3

