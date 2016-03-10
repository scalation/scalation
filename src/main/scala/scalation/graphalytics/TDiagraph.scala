
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Aug 11 12:17:19 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Digraph (TDigraph) Data Structure Using Mutable Sets
 */

package scalation.graphalytics

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import collection.mutable.{HashSet => SET}

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TripleType` provides a type definition for triples.
 */
object TripleType
{
   type Triple = Tuple3 [Int, TLabel, Int]                              // V x L X V

} // TripleType

import TripleType.Triple


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TDigraph` class stores vertex/edge-labeled multi-directed graphs using
 *  an adjacency set ('ch') representation, e.g., ch = { {1, 2}, {0}, {1} } means
 *  that the graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'pa' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *----------------------------------------------------------------------------
 *  @param edge   the array of edge triples
 *  @param label  the array of vertex labels
 *  @param name   the name of the multi-digraph
 */
class TDigraph (edge:  Array [Triple],
                label: Array [TLabel] = Array.ofDim (0),
                name:  String = "g")
      extends Cloneable
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of 'this' multi-digraph.
     */
    override def clone: TDigraph =
    {
        val edge2  = Array.ofDim [Triple] (edge.length)
        val label2 = Array.ofDim [TLabel] (label.length)
        for (e <- edge2.indices)  edge2(e)  = edge(e)
        for (v <- label2.indices) label2(v) = label(v)
        new TDigraph (edge2, label2, name)
    } // clone

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of vertices in 'this' digraph.
     */
    def size = label.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of edges in 'this' digraph.
     */
    def nEdges = edge.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' multi-digraph to a string in a shallow sense.
     *  Large arrays are not converted.  Use 'print' to show all information.
     */
    override def toString: String =
    {
        s"TDigraph (edge.length = ${edge.length}, label.length = ${label.length}, name = $name)"
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print 'this' multi-digraph in a deep sense with all the information.
     */
    def print ()
    {
        println (s"TDigraph ($name, $size")
        for (e <- edge) println (e)
        for (l <- label) println (l)
        println (")")
    } // print

} // TDigraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TDigraph` companion object provides builder methods.
 */
object TDigraph
{
    def apply (edge: Array [Triple], label: Array [TLabel] = Array.ofDim (0), name: String = "g"): TDigraph =
    { 
        new TDigraph (edge, label, name)
    } // apply

    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    // data multi-digraph g1 -------------------------------------------------

    val g1 = TDigraph (Array ((1, -1, 0),                  // edge 0
                              (1, -1, 2),                  // edge 1
                              (1, -1, 3),                  // edge 2
                              (1, -1, 4),                  // edge 3
                              (2, -1, 0),                  // edge 4
                              (3, -2, 4)),                 // edge 5, change from -1 to -2 filter out vertices
                       Array (11, 10, 11, 11, 11),         // vertex labels
                       "g1")                               // name

     // query multi-digraph q1 ------------------------------------------------

     val q1 = TDigraph (Array ((0, -1, 1),                 // edge 0
                               (0, -1, 2),                 // edge 1
                               (2, -1, 1)),                // edge 2
                        Array (10, 11, 11),                // vertex labels
                        "q1")                              // name

    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data multi-digraph g2 -------------------------------------------------

    val g2 = TDigraph (Array ((0, 1, 1),
                              (1, 1, 0),
                              (1, 1, 2),
                              (1, 1, 3),
                              (1, 1, 4),                   // 2
                              (1, 1, 5),
                              (5, 1, 6),
                              (5, 1, 10),
                              (6, 1, 7),
                              (6, 1, 4),                   // 2
                              (6, 1, 8),
                              (6, 1, 9),
                              (7, 1, 1),
                              (10, 1, 11),
                              (11, 1, 12),
                              (12, 1, 11),
                              (12, 1, 13),
                              (14, 1, 13),
                              (14, 1, 15),
                              (15, 1, 16),
                              (16, 1, 17),
                              (16, 1, 18),
                              (17, 1, 14),
                              (17, 1, 19),
                              (18, 1, 20),
                              (19, 1, 14),
                              (20, 1, 19),
                              (20, 1, 21),
                              (22, 1, 21),
                              (22, 1, 23),
                              (23, 1, 25),
                              (25, 1, 24),
                              (25, 1, 26),
                              (26, 1, 28),
                              (28, 1, 27),
                              (28, 1, 29),
                              (29, 1, 22)),
                       Array (10, 11, 12, 12, 12, 10, 11, 10, 12, 15, 12, 10, 11, 12, 11,
                               10, 11, 12, 10, 10, 11, 12, 11, 10, 12, 11, 10, 12, 11, 10),
                       "g2")

    // query multi-digraph q2 ------------------------------------------------

    val q2 = TDigraph (Array ((0, 1, 1),
                              (1, 1, 0),
                              (1, 1, 2),
                              (1, 1, 3)),
                       Array (10, 11, 12, 12),
                       "q2")
                         
} // TDigraph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TDigraphTest` object is used to test the `TDigraph` class using the
 *  graphs given in the `TDigraph` companion object.
 *  > run-main scalation.graphalytics.TDigraphTest
 */
object TDigraphTest extends App
{
    import TDigraph._
    g1.print ()
    q1.print ()
    g2.print ()
    q2.print ()

} // TDigraphTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TDigraphTest2` object is used to test the `TDigraph` class using a
 *  randomly generated multi-digraph.
 *  > run-main scalation.graphalytics.TDigraphTest2
 *
object TDigraphTest2 extends App
{
    private val nVertices = 20         // number of vertices
    private val nLabels   = 5          // number of distinct vertex labels
    private val eLabels   = 3          // number of distinct edge labels
    private val outDegree = 2          // average out degree
    private val inverse   = false      // whether inverse adjacency is used (parents)
    private val name      = "gr"       // name of the graph

    MDigGen.genRandomGraph (nVertices, nLabels, eLabels, outDegree, inverse, name).print ()

} // TDigraphTest2
 */

