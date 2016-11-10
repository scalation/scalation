
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Tue Nov  1 19:12:16 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Graph 'MuGraph' Data Structure Using Mutable Sets
 */

package scalation.graphalytics.multi

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.{Pair, Tree}
import scalation.graphalytics.mutable.{Graph, MGraph, MGraphGen}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraph` class stores vertex/edge-labeled multi-directed graphs using
 *  an adjacency set 'ch' representation, e.g., 'ch = { {1, 2}, {0}, {1} }' means
 *  that the graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *----------------------------------------------------------------------------
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of vertex labels: v -> vertex label
 *  @param elabel   the map of edge labels: (u, v) -> edge label
 *  @param inverse  whether to store inverse adjacency sets (parents)
 *  @param name     the name of the multi-digraph
 *  @param schema   optional schema: map from label to label type
 */
class MuGraph [TLabel: ClassTag] (ch:     Array [SET [Int]],
                                 label:   Array [TLabel],
                             val elabel:  Map [Pair, SET [TLabel]],
                                 inverse: Boolean = false,
                                 name:    String = "g",
                                 schema:  Map [TLabel, String] = null)
      extends Graph [TLabel] (ch, label, inverse, name) with Cloneable
{
    /** Map from schema label type to set of labels
     */
    val schemaMap = if (schema == null) null else buildSchemaMap (schema)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the schema map from label type to set of labels.
     *  @param schema  the schema - map of label types: label -> label type
     */
    def buildSchemaMap (schema: Map [TLabel, String]): Map [String, SET [TLabel]] =
    {
        val schMap = Map [String, SET [TLabel]] ()
        for ((lab, typ) <- schema) schMap += typ -> (schMap.getOrElse (typ, SET ()) + lab)
        schMap
    } // buildSchemaMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of 'this' multi-digraph.
     */
    override def clone: MuGraph [TLabel] =
    {
        val ch2    = Array.ofDim [SET [Int]] (ch.length)
        val label2 = Array.ofDim [TLabel] (ch.length)
        for (i <- ch2.indices) {
            ch2(i) = SET (ch(i).toArray: _*)
            label2(i) = label(i)
        } // for
        new MuGraph (ch2, label2, elabel.clone, inverse, name)
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
    override def makeUndirected (): MuGraph [TLabel] =
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
        s"MuGraph (ch.length = ${ch.length}, label.length = ${label.length}" +
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
        println (s"MuGraph ($name, $inverse, $size")
        for (i <- ch.indices) println (toLine (i))
        for ((k, v) <- elabel) println (s"$k -> $v")
        println (")")
    } // printG

} // MuGraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraph` companion object provides builder methods and example query
 *  multi-digraphs.
 */
object MuGraph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MuGraph` from a `Graph`.
     *  @param gr    the base `Graph` for building the `MuGraph`
     *  @param eLab  the edge labels
     *  @param name  the name for the new multi-digraph
     */
    def apply [TLabel: ClassTag] (gr: Graph [TLabel], eLab: Map [Pair, SET [TLabel]], name: String): MuGraph [TLabel] =
    {
        new MuGraph (gr.ch, gr.label, eLab, gr.inverse, name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MuGraph` from a `MGraph`.
     *  @param mgr   the base `MGraph` for building the `MuGraph`
     *  @param name  the name for the new multi-digraph
     */
    def apply [TLabel: ClassTag] (mgr: MGraph [TLabel], name: String): MuGraph [TLabel] =
    {
        new MuGraph (mgr.ch, mgr.label, ν(mgr.elabel), mgr.inverse, name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an `MuGraph` from a `Tree`.
     *  @param tree     the base `Tree` for building the `MuGraph`
     *  @param name     the name for the new multi-digraph
     *  @param inverse  whether to add parent references
     */
    def apply (tree: Tree, name: String = "t", inverse: Boolean = false): MuGraph [Double] =
    {
        MuGraph (Graph (tree, name, inverse), ν (tree.labelMap), name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect the label(s) into a set.
     *  @param label  the given label(s)
     */
    def ν [TLabel: ClassTag] (label: TLabel*): SET [TLabel] = SET (label:_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect the labels in the map into sets.
     *  @param labelMap  the initial label map
     */
    def ν [TLabel: ClassTag] (labelMap: Map [Pair, TLabel]): Map [Pair, SET [TLabel]] =
    {
        val vmap = Map [Pair, SET [TLabel]] ()
        for ((k, v) <- labelMap) vmap += k -> SET (v)
        vmap
    } // ν

} // MuGraph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphTest` object is used to test the `MuGraph` class using examples
 *  from the `ExampleMuGraphI` object, which contains multi-digraphs whose vertex
 *  and edge labels are of type `Int`.
 *  > run-main scalation.graphalytics.mutable.MuGraphTest
 *
object MuGraphTest extends App
{
    import ExampleMuGraphI._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MuGraphTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphTest2` object is used to test the `MuGraph` class using examples
 *  from the `ExampleMuGraphD` object, which contains multi-digraphs whose vertex
 *  and edge labels are of type `Double`.
 *  > run-main scalation.graphalytics.mutable.MuGraphTest2
 */
object MuGraphTest2 extends App
{
    import ExampleMuGraphD._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MuGraphTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphTest3` object is used to test the `MuGraph` class using a
 *  randomly generated multi-digraph.
 *  > run-main scalation.graphalytics.mutable.MuGraphTest3
 */
object MuGraphTest3 extends App
{
    val mgGen = new MGraphGen [Double]

    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct vertex labels
    private val eLabels   = 3          // number of distinct edge labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the graph

    val mGraph = mgGen.genRandomGraph (nVertices, nLabels, eLabels, outDegree, inverse, name)
    MuGraph (mGraph, "mu" + name).printG ()

} // MuGraphTest3

