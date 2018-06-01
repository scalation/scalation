
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz, Supriya Ramireddy
 *  @version 1.5
 *  @date    Tue Nov  1 19:12:16 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Graph 'MuGraph' Data Structure Using Mutable Sets
 */

package scalation.graph_db.graph_algebra

import java.io._

import com.sun.xml.internal.ws.api.server.SDDocument.Schema

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, SortedMap, HashMap => MAP, Set => SET}
import scala.reflect.ClassTag
import scalation.graph_db.graph_algebra.MuGraph.ν
import scalation.graph_db.{Pair, Tree}
import scalation.graph_db.{Graph, MGraph, MGraphGen}
import scalation.util.{ReArray, Wildcard}
import util.control.Breaks._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PathType` object provides type definitions for paths and rows.
 */
object PathType
{
    type Path     = ArrayBuffer [Int]
    type Rows     = ArrayBuffer [Path]
    type Path_Lab = ArrayBuffer [String]
    type Rows_lab = ArrayBuffer [Path_Lab]

} // PathType object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HashMap_NE` is used to override the 'default' method to avoid unnecessary
 *  exceptions (NE means no exception).
 */
class HashMap_NE [K] extends MAP [K, SET [Int]]
{
    override def default (key: K): SET [Int] = SET [Int] ()

} // HashMap_NE class


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
 *  @param id       the array of vertex id's
 *  @param schema   optional schema: map from label to label type
 */
class MuGraph [TLabel: ClassTag] (ch:      Array [SET [Int]],
                                  label:   Array [TLabel],
                              val elabel:  Map [Pair, SET [TLabel]],
                                  inverse: Boolean = false,
                                  name:    String = "g",
                              val id:      Array [Int] = Array (),
                              val schema:  Array [String] = Array ())
      extends Graph [TLabel] (ch, label, inverse, name) with Cloneable
{
    if (id == null) buildId 

    /** index from (source node, edge) to the destination node
    */
    val indexMap  = new HashMap_NE [(TLabel, TLabel)] ()

    /** count of no. of occurences of a pair of vertices with a particular edge
    */
    val count = new MAP[(TLabel, TLabel, Int), Int]()

    /** index from edges the set of pairs of nodes
    */
    val edgeMap = new MAP[TLabel, SET[Pair]]()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an array [1, 2, ..., size] for default values for 'id's.
     */
    def buildId: Array [Int] = Array.range (1, ch.size + 1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of graph in terms of the number of vertices.
     */
    override def size: Int  = ch.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the children of vertex 'u' that are connected via an edge labeled
     *  'elab'.
     *  @param u     the source vertex
     *  @param elab  the edge label
     */
    def children (u: Int, elab: TLabel): SET [Int] =
    {
        for (v <- ch (u) if elabel((u, v)) contains elab) yield v
    } // children

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parents of vertex 'v' that are connected via an edge labeled
     *  'elab'.  Requires the parents 'pa' to be added (@see `Graph`).
     *  @param v    the destination vertex
     *  @param elab the edge label
     */
    def parents (v: Int, elab: TLabel): SET [Int] =
    {
        for (u <- pa (v) if elabel((u, v)) contains elab) yield u
    } // parents

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of 'this' multi-digraph.
     */
    override def clone: MuGraph [TLabel] =
    {
        val ch2 = Array.ofDim[SET[Int]](ch.length)
        val label2 = Array.ofDim[TLabel](ch.length)
        for (i <- ch2.indices) {
            ch2(i) = SET(ch(i).toArray: _*)
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
            println(s"checkElabels: no such edge from $u to $v")
            return false
        } // for
        true
    } // checkElabels

    // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Builds index from (start vertex, edge) -> end vertex.
     */
    def buildIndex
    {
        for (k <- elabel.keys) {
            for (e <- elabel(k)) {
                count    += (label(k._1), e.asInstanceOf[TLabel], k._2) ->
                            (count.getOrElse((label(k._1), e.asInstanceOf[TLabel], k._2), 0) + 1)
                indexMap += (label(k._1), e.asInstanceOf[TLabel]) ->
                            (indexMap.getOrElse((label(k._1), e.asInstanceOf[TLabel]), SET()) + k._2)
            } // for
        } // for
        println ("count of triples")
        for ((k, v) <- count) println((k, v))
        println ("-" * 50)
        println("indexMap")
        for ((k, v) <- indexMap) println((k, v))
        println ("-" * 60)
    } // buildIndex

    // ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build and index from edge -> (Vertex pair).
     */
    def indexEdges
    {
        for (k <- elabel.keys; e <- elabel(k)) edgeMap += e -> (edgeMap.getOrElse (e, SET()) + k)
        println ("edgeMap" + edgeMap)
        println ("-" * 50)
    } // indexEdges

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
     *  @param i    the 'i'th row/line
     *  @param clip whether to clip out "Set(" and ")"
     */
    override def toLine (i: Int, clip: Boolean = true): String =
    {
        val ch_i = ch(i).toString.replace ("Set(", "").replace(")", "")
        s"$i, " +
        s"${i+1}, " +
        s"${label(i)}, " +
        s"$ch_i"
    } // toLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' multi-digraph in a deep sense with all the information.
     *  @param clip whether to clip out "Set(" and ")"
     */
    override def printG (clip: Boolean = true)
    {
        println (s"MuGraph ($name, $inverse, $size")
        for (i <- ch.indices) println (toLine(i))
        for ((k, v) <- elabel) println (s"$k -> $v")
        println (")")
    } // printG

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an array vertices from selected vertices from graph 'g', those in 'vset'.
     *  If 'makeMap', record the mapping between g's vertices and the new vertices.
     *  @param g        graph to be used for adding vertex labels
     *  @param vset     selected vertices from graph g
     *  @param makeMap  whether to make a map from new vertex id's to old vertex id's
     */
    def addVertices (g: MuGraph [TLabel], vset: SET [Int], makeMap: Boolean = false):
                    (Array [TLabel], Map [Int, Int]) =
    {
        val lv   = Array.ofDim [TLabel] (vset.size)
        val vmap = Map [Int, Int] ()

        if (makeMap) {
            var n = 0
            for (i <- vset) {
                lv(n) = g.label(i)                  // nth vertex gets g's ith label
                vmap += n -> i                      // records mapping from n to i
                n    += 1
            } // for
        } else {
            for (i <- vset) lv(i) = g.label(i)      // direct correspondence for labels 
        } // if
        (lv, vmap)
    } //addVertices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** To check if two graphs are equal.
     *  @param g2_ the graph to be compared with this graph
     */
    override def equals (g2_ : Any): Boolean =
    {
        g2_ match {
        case _: MuGraph [TLabel] =>
                val g2 = g2_.asInstanceOf[MuGraph[TLabel]]
                if (ch.deep != g2.ch.deep) { println ("failed on ch"); return false }
                if (id.deep != g2.id.deep) { println ("failed on id"); return false }
                if (label.deep != g2.label.deep) { println("failed on label"); return false }
                if (elabel != g2.elabel) { println("failed on elabel"); return false }
                true
        case _ => false
        } // match
    } // equals

} // MuGraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraph` companion object provides builder methods and example query
 *  multi-digraphs.
 */
object MuGraph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a 'MuGraph' from the given vertex labels and edge mappings.
     *  @param label    the vertex labels
     *  @param le       the map of edges, vertex pair -> set of edge labels
     *  @param id       the vertex id's
     *  @param inverse  whether to store inverse adjacency sets (parents)
     *  @param schema   the type of the vertices
     */
    def apply [TLabel: ClassTag] (label: Array [TLabel], le: Map [Pair, SET[TLabel]], id: Array [Int],
                                  inverse: Boolean, schema: Array [String]): MuGraph[ TLabel] =
    {
        val n  = label.length
        val ch = Array.fill (n)(SET [Int] ())
        for ((e, l) <- le) ch(e._1) += e._2
        val old_ids = if (id == null) Array.range (1, n + 1) else id
        new MuGraph (ch, label, le, inverse, "g", old_ids, schema)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a `MuGraph` from a `Graph`.
     *  @param gr    the base `Graph` for building the `MuGraph`
     *  @param eLab  the edge labels
     *  @param name  the name for the new multi-digraph
     */
    def apply [TLabel: ClassTag] (gr: Graph [TLabel], eLab: Map [Pair, SET [TLabel]], name: String): MuGraph [TLabel] =
    {
        new MuGraph (gr.ch, gr.label, eLab, gr.inverse, name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a `MuGraph` from a `MGraph`.
     *  @param mgr   the base `MGraph` for building the `MuGraph`
     *  @param name  the name for the new multi-digraph
     */
    def apply [TLabel: ClassTag] (mgr: MGraph [TLabel], name: String): MuGraph [TLabel] =
    {
        new MuGraph (mgr.ch, mgr.label, ν(mgr.elabel), mgr.inverse, name)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a `MuGraph` from a `Tree`.
     *  @param tree     the base `Tree` for building the `MuGraph`
     *  @param name     the name for the new multi-digraph
     *  @param inverse  whether to add parent references
     */
    def apply (tree: Tree[Double], name: String = "t", inverse: Boolean = false): MuGraph [Double] =
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
 *  from the `ExampleMuGraphD` object, which contains multi-digraphs whose vertex
 *  and edge labels are of type `Double`.
 *  > runMain scalation.graph_db.graph_algebra.MuGraphTest
 */
object MuGraphTest extends App
{
    import ExampleMuGraphD._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MuGraphTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphTest2` object is used to test the `MuGraph` class using examples
 *  from the `ExampleMuGraphD` object, which contains multi-digraphs whose vertex
 *  and edge labels are of type `String`.
 *  > runMain scalation.graph_db.graph_algebra.MuGraphTest2
 */
object MuGraphTest2 extends App
{
    import ExampleMuGraphS._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // MuGraphTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphTest3` object is used to test the `MuGraph` class using a
 *  randomly generated multi-digraph.
 *  > runMain scalation.graph_db.graph_algebra.MuGraphTest3
 */
object MuGraphTest3 extends App
{
    val mgGen = new MGraphGen (0.0)

    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct vertex labels
    private val eLabels   = 3          // number of distinct edge labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the graph

    val mGraph = mgGen.genRandomGraph (nVertices, nLabels, eLabels, outDegree, inverse, name)
    MuGraph (mGraph, "mu" + name).printG ()

} // MuGraphTest3

