
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Immutable Sets
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` class stores vertex-labeled directed graphs using an adjacency
 *  set ('ch') representation, e.g., ch = { {1, 2}, {0}, {1} } means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *----------------------------------------------------------------------------
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of vertex labels
 *  @param inverse  whether to store inverse adjacency sets (parents)
 *  @param name     the name of graph
 */
class Graph (val ch:      Array [SET [Int]],
             val label:   Array [TLabel] = Array.ofDim (0),
             val inverse: Boolean = false,
             val name: String = "g")
      extends Cloneable 
{
    /** The map from label to the set of vertices with the label
     */
    val labelMap = buildLabelMap (label)

    /** The optional array of vertex inverse (parent) adjacency sets (incoming edges)
     */
    val pa = Array.ofDim [SET [Int]] (if (inverse) ch.size else 0)

    if (inverse) addPar ()                       // by default, don't use 'pa'

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of this graph.
     */
    override def clone: Graph = new Graph (ch.clone, label.clone, inverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the inverse adjacency sets for rapid accesses to parent vertices.
     */
    def addPar ()
    {
        for (j <- pa.indices) pa(j) = SET [Int] ()
        for (i <- ch.indices; j <- ch(i)) pa(j) += i
    } // addPar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of vertices in the graph.
     */
    def size = ch.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of edges in the graph.
     */
    def nEdges = ch.foldLeft (0) { (n, i) => n + i.size }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an array of labels, return an index from labels to the sets of
     *  vertices containing those labels.
     *  @param label  the array of vertex labels of type TLabel
     */
    def buildLabelMap (label: Array [TLabel]): Map [TLabel, SET [Int]] =
    {
        var labelMap = Map [TLabel, SET [Int]] ()
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
    /** Determine the number of vertices in the graph that have outgoing edges
     *  to themselves (self loops).
     */ 
    def nSelfLoops: Int =
    {
        ch.indices.foldLeft (0) { (sum, i) => if (ch(i) contains i) sum + 1 else sum }
    } // nSelfLoops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' graph is (weakly) connected.
     */
    def isConnected: Boolean = (new GraphDFS (this)).weakComps == 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the endpoint vertex id of each edge is within bounds: 0..maxId.
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
    /** Determine whether 'this' graph and graph 'g' have the same vertices
     *  and edges.  Note, this is more strict than graph isomorphism which allows
     *  vertices to be renumbered.
     *  @param g  the other digraph
     */
    def same (g: Digraph): Boolean =
    {
        if (size != g.size) return false
        for (u <- ch.indices; u_c <- ch(u) if ! (g.ch(u) contains u_c)) return false
        true
    } // same

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the set of vertices in the graph with label l.
     */
    def getVerticesWithLabel (l: TLabel) = labelMap.getOrElse (l, SET [Int] ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' graph to a string in a shallow sense.  Large arrays are
     *  not converted.  Use 'print' to show all information.
     */
    override def toString: String =
    {
        s"Graph (ch.length = ${ch.length}, label.length = ${label.length}, inverse = $inverse, name = $name)"
    } // toString

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the 'i'th row/line of 'this' graph to a string.
     *  @param i     the ith row/line
     *  @param clip  whether to clip out "Set(" and ")"
     */
    def toLine (i: Int, clip: Boolean = true): String =
    {
        var ch_i = ch(i).toString
        if (clip) ch_i = ch_i.replace ("Set(", "").replace (")", "")
        if (i < label.length) s"$i, ${label(i)}, $ch_i"
        else                  s"$i, $ch_i"
    } // toLine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' graph in a deep sense with all the information.
     *  @param clip  whether to clip out "Set(" and ")"
     */
    def print (clip: Boolean = true)
    {
        println (s"Graph ($name, $inverse, $size")
        for (i <- ch.indices) println (toLine (i, clip))
        println (")")
    } // print

} // Graph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` companion object contains the example query graphs.
 */
object Graph
{
    // -----------------------------------------------------------------------
    // Simple data and query graphs.
    // -----------------------------------------------------------------------

    // data graph g1 ---------------------------------------------------------

    val g1 = new Graph (Array (SET (),                      // ch(0)
                               SET (0, 2, 3, 4),            // ch(1)
                               SET (0),                     // ch(2)
                               SET (4),                     // ch(3)
                               SET ()),                     // ch(4)
                        Array (11, 10, 11, 11, 11),         // vertex labels
                        false, "g1")                        // inverse, name

    // query graph q1 --------------------------------------------------------

    val q1 = new Graph (Array (SET (1, 2),                  // ch(0)
                               SET (),                      // ch(1)
                               SET (1)),                    // ch(2)
                        Array (10, 11, 11),
                        false, "q1")

    val g1p = new Graph (g1.ch, g1.label, true, g1.name)    // with parents
    val q1p = new Graph (q1.ch, q1.label, true, q1.name)    // with parents

    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data graph g2 ---------------------------------------------------------

    val g2 = new Graph (Array (SET (1),                     // ch(0)
                               SET (0, 2, 3, 4, 5),         // ch(1)
                               SET (),                      // ch(2)
                               SET (),                      // ch(3)
                               SET (),                      // ch(4)
                               SET (6, 10),                 // ch(5)
                               SET (7, 4, 8, 9),            // ch(6)
                               SET (1),                     // ch(7)
                               SET (),                      // ch(8)
                               SET (),                      // ch(9)
                               SET (11),                    // ch(10)
                               SET (12),                    // ch(11)
                               SET (11, 13),                // ch(12)
                               SET (),                      // ch(13)
                               SET (13, 15),                // ch(14)
                               SET (16),                    // ch(15)
                               SET (17, 18),                // ch(16)
                               SET (14, 19),                // ch(17)
                               SET (20),                    // ch(18)
                               SET (14),                    // ch(19)
                               SET (19, 21),                // ch(20)
                               SET (),                      // ch(21)
                               SET (21, 23),                // ch(22)
                               SET (25),                    // ch(23)
                               SET (),                      // ch(24)
                               SET (24, 26),                // ch(25)
                               SET (28),                    // ch(26)
                               SET (),                      // ch(27)
                               SET (27, 29),                // ch(28)
                               SET (22)),                   // ch(29)
                       Array (10, 11, 12, 12, 12, 10, 11, 10, 12, 15, 12, 10, 11, 12, 11,
                              10, 11, 12, 10, 10, 11, 12, 11, 10, 12, 11, 10, 12, 11, 10),
                              false, "g2")

    // query graph q2 --------------------------------------------------------

    val q2 = new Graph (Array (SET (1),                     // ch(0)
                               SET (0, 2, 3),               // ch(1)
                               SET (),                      // ch(2)
                               SET ()),                     // ch(3)
                        Array (10, 11, 12, 12),
                               false, "q2")

    val g2p = new Graph (g2.ch, g2.label, true, g2.name)    // with parents
    val q2p = new Graph (q2.ch, q2.label, true, q2.name)    // with parents

} // Graph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest` object is used to test the `Graph` class using the graphs
 *  given in the `Graph` companion object.
 *  > run-main scalation.graphalytics.GraphTest
 */
object GraphTest extends App
{
    import Graph._
    g1.print ()
    q1.print ()
    g2.print ()
    q2.print ()

} // GraphTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest2` object is used to test the `Graph` class using a randomly
 *  generated graph.
 *  > run-main scalation.graphalytics.GraphTest2
 */
object GraphTest2 extends App
{
    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the graph

    GraphGen.genRandomGraph (nVertices, nLabels, outDegree, inverse, name).print ()

} // GraphTest2 object

