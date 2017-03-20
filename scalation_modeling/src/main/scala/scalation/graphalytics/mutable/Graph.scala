
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.3
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.{Tree, TreeNode}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` class stores vertex-labeled directed graphs using an adjacency
 *  set 'ch' representation, e.g., 'ch = { {1, 2}, {0}, {1} }' means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *----------------------------------------------------------------------------
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of vertex labels
 *  @param inverse  whether to store inverse adjacency sets (parents)
 *  @param name     the name of the digraph
 *  @param vid      the vertex id (facilitates partitioning)
 */
class Graph [TLabel: ClassTag] (val ch:      Array [SET [Int]],
                                val label:   Array [TLabel] = Array.ofDim (0),
                                val inverse: Boolean = false,
                                val name:    String = "g",
                                val vid:     Array [Int] = null)
      extends Cloneable
{
    /** Debug flag
     */
    private val DEBUG = true

    /** The map from label to the set of vertices with the label
     */
    val labelMap = buildLabelMap (label)

    /** The optional array of vertex inverse (parent) adjacency sets (incoming edges)
     */
    val pa = Array.ofDim [SET [Int]] (if (inverse) ch.size else 0)

    if (inverse) addPar ()                       // by default, don't use 'pa'

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of 'this' digraph.
     */
    override def clone: Graph [TLabel] = 
    {
        val ch2    = Array.ofDim [SET [Int]] (ch.length)
        val label2 = Array.ofDim [TLabel] (ch.length)
        for (i <- ch2.indices) {
            ch2(i) = SET (ch(i).toArray: _*)
            label2(i) = label(i)
        } // for
        new Graph [TLabel] (ch2, label2, inverse, name)
    } // clone

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the inverse adjacency sets for rapid accesses to parent vertices.
     */
    def addPar ()
    {
        for (j <- pa.indices) pa(j) = SET [Int] ()
        for (i <- ch.indices; j <- ch(i)) pa(j) += i
    } // addPar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of vertices in 'this' digraph.
     */
    def size = ch.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of edges in 'this' digraph.
     */
    def nEdges = ch.foldLeft (0) { (n, i) => n + i.size }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an array of labels, return an index from labels to the sets of
     *  vertices containing those labels.
     *  @param label  the array of vertex labels of type `TLabel`
     */
    def buildLabelMap (label: Array [TLabel]): Map [TLabel, SET [Int]] =
    {
        val labelMap = Map [TLabel, SET [Int]] ()
        for (i <- label.indices) {                      // for each vertex i
            val lab  = label(i)                         // label for vertex i
            labelMap += lab -> (labelMap.getOrElse (lab, SET ()) + i)
        } // for
        labelMap
    } // buildLabelMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum label value.
     */ 
//  def nLabels = labelMap.keys.max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the number of vertices in the digraph that have outgoing edges
     *  to themselves.
     */ 
    def nSelfLoops: Int =
    {
        ch.indices.foldLeft (0) { (sum, i) => if (ch(i) contains i) sum + 1 else sum }
    } // nSelfLoops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' digraph is (weakly) connected.
     */
    def isConnected: Boolean = (new GraphDFS (this)).weakComps == 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the union 'this' digraph and 'g2'.
     *  @param g2  the other digraph
     */
    def union (g2: Graph [TLabel]): Graph [TLabel] =
    {
        val vv     = if (vid == null)    Array.range (0, ch.length) else vid
        val uu     = if (g2.vid == null) Array.range (0, g2.ch.length) else g2.vid
        val vmap   = Map [Int, Int] ()
        val umap   = Map [Int, Int] ()
        val _vid   = ArrayBuffer [Int] ()
        val _label = ArrayBuffer [TLabel] ()
        var (i, j, k) = (0, 0, 0)
        var (more_i, more_j) = (true, true)

        while (more_i || more_j) {
           if (i >= vv.length) more_i = false
           if (j >= uu.length) more_j = false
           if (more_i && (! more_i || vv(i) < uu(j))) {
               _vid += vv(i)
               vmap += (k -> i)
               _label += label(i)
               i += 1
           } else if (more_j && (! more_i || vv(i) > uu(j))) {
               _vid += uu(j)
               umap += (k -> j)
               _label += g2.label(j)
               j += 1
           } else if (more_i && more_j) {
               _vid += vv(i)
               vmap += (k -> i)
               umap += (k -> j)
               _label += label(i)
               i += 1; j += 1
           } // if
           k += 1
        } // while

        if (DEBUG) println ("_vid = " + _vid)

        val _ch = Array.ofDim [SET [Int]] (_vid.size)

        for (k <- _vid.indices) {
            val vi = vmap.getOrElse (k, -1)
            val uj = umap.getOrElse (k, -1)
            _ch(k) = if (vi >= 0 && uj >= 0) ch(vi) union g2.ch(uj)
                     else if (vi >= 0) ch(vi)
                     else g2.ch(uj)
        } // for
        new Graph [TLabel] (_ch, _label.toArray, inverse, name + "_" + g2.name, _vid.toArray)
    } // union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the end-point vertex id of each edge is within bounds:
     *  '0 .. maxId'.
     */
    def checkEdges: Boolean =
    {
        val maxId = ch.size - 1
        for (u <- ch.indices; u_c <- ch(u) if u_c < 0 || u_c > maxId) {
            println (s"checkEdges: child of $u, with vertex id $u_c not in bounds 0..$maxId")
            return false
        } // for
        true
    } // checkEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' digraph and digraph 'g' have the same vertices
     *  and edges.  Note, this is more strict than graph isomorphism which allows
     *  vertices to be renumbered.
     *  @param g  the other digraph
     */
    def same (g: Graph [TLabel]): Boolean =
    {
        if (size != g.size) return false
        for (u <- ch.indices; u_c <- ch(u) if ! (g.ch(u) contains u_c)) return false
        true
    } // same

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the set of vertices in 'this' digraph with label l.
     */
    def getVerticesWithLabel (l: TLabel) = labelMap.getOrElse (l, SET [Int] ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make this directed graph work like an undirected graph by making sure that
     *  for every edge 'u -> v', there is a 'v -> u' edge.
     */
    def makeUndirected (): Graph [TLabel] =
    {
        for (u <- 0 until size; v <- ch(u)) ch(v) += u
        this
    } // makeUndirected

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' digraph to a string in a shallow sense.  Large arrays are
     *  not converted.  Use 'print' to show all information.
     */
    override def toString: String =
    {
        s"Graph (ch.length = ${ch.length}, label.length = ${label.length}, inverse = $inverse, name = $name)"
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'i'th row/line of 'this' digraph to a string.
     *  @param i     the 'i'th row/line
     *  @param clip  whether to clip out "Set(" and ")"
     */
    def toLine (i: Int, clip: Boolean = true): String =
    {
        var ch_i = ch(i).toString
        if (clip) ch_i = ch_i.replace ("Set(", "").replace (")", "")
        if (vid == null) if (i < label.length) s"$i, ${label(i)}, $ch_i"
                         else                  s"$i, $ch_i"
        else             if (i < label.length) s"$i, [${vid(i)}], ${label(i)}, $ch_i"
                         else                  s"$i, [${vid(i)}], $ch_i"
    } // toLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' digraph in a deep sense with all the information.
     *  @param clip  whether to clip out "Set(" and ")"
     */
    def printG (clip: Boolean = true)
    {
        println (s"Graph ($name, $inverse, $size")
        for (i <- ch.indices) println (toLine (i, clip))
        println (")")
    } // printG

} // Graph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` companion object contains build methods and example query digraphs.
 */
object Graph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a `Graph` from a `Tree`.
     *  @param tree     the base `Tree` for building the `Graph`
     *  @param name     the name for the new digraph
     *  @param inverse  whether to add parent references
     */
    def apply (tree: Tree, name: String = "t", inverse: Boolean = false): Graph [Double] =
    {
        val g = new Graph (Array.ofDim [SET [Int]] (tree.size),        // children
                           Array.ofDim [Double] (tree.size),           // labels
                           inverse, name)

        def traverse (n: TreeNode)
        {
            g.ch(n.nid) = SET [Int] ()
            g.label(n.nid) += n.label
            for (c <- n.child) {
                g.ch(n.nid) += c.nid
                traverse (c)
            } // for
        } // traverse

        traverse (tree.root)
        g
    } // apply

} // Graph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest` object is used to test the `Graph` class using example
 *  digraphs from the `ExampleGraphI` object, which contains graph whose
 *  vertex labels are of type `Int`.
 *  > run-main scalation.graphalytics.mutable.GraphTest
 */
object GraphTest extends App
{
    import ExampleGraphI._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // GraphTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest2` object is used to test the `Graph` class using example
 *  digraphs from the `ExampleGraphD` object, which contains graph whose
 *  vertex labels are of type `Double`.
 *  > run-main scalation.graphalytics.mutable.GraphTest2
 */
object GraphTest2 extends App
{
    import ExampleGraphD._

    g1.printG ()
    q1.printG ()
    g2.printG ()
    q2.printG ()

} // GraphTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest3` object is used to test the `Graph` class using the
 *  digraphs given in the `Graph` companion object.
 *  > run-main scalation.graphalytics.mutable.GraphTest3
 */
object GraphTest3 extends App
{
    import ExampleGraphD._

    q1.printG ()
    q2.printG ()
    val q3 = q1 union q2
    q3.printG ()

} // GraphTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest4` object is used to test the `Graph` class using a
 *  randomly generated digraph.
 *  > run-main scalation.graphalytics.mutable.GraphTest4
 */
object GraphTest4 extends App
{
    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the digraph

    val gGen = new GraphGen [Double] (0.0)
    gGen.genRandomGraph (nVertices, nLabels, outDegree, inverse, name).printG ()

} // GraphTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest5` object is used to test the `Graph` class by calling
 *  the apply in the `Graph` companion object.
 *  > run-main scalation.graphalytics.mutable.GraphTest5
 */
object GraphTest5 extends App
{
    val pred = Array (-1, 0, 0, 1, 1, 2, 2)
    val labl = Array (10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
    val t = Tree (pred, labl, 3.0, "t")
    t.printTree
    t.aniTree
//  val g = new Graph [Double] (t)
//  g.printG ()

} // GraphTest5 object

