
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Aug 10 14:26:34 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{Map, PriorityQueue}
import scala.collection.mutable.{Set => SET}
//import scala.language.implicitConversions

import scalation.linalgebra.{MatriD, MatrixD, SparseMatrixD, VectoD, VectorD, VectorI}
import scalation.linalgebra.SparseMatrixD.RowMap

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPath` class is used to solve shortest path problems for graphs
 *  stored in matrices.  It solves the Single-Source Shortest Path 'SSSP' problem
 *  for directed graphs (both digraphs and multi-digraphs).  The `SSShortestPath`
 *  companion object is used to form a matrix from an `MGraph`.
 *----------------------------------------------------------------------------
 *  The edge cost/distance (must be non-negative) can be stored in either a dense
 *  or sparse matrix.  Dijkstra's Algorithm is used.
 *  @see en.wikipedia.org/wiki/Dijkstra%27s_algorithm
 *----------------------------------------------------------------------------
 *  For multi-digraphs, each multi-edge between a pair vertices has it own edge
 *  weight (`TLabel` = `VectorD` in this case).  The minimum is taking when forming
 *  the corresponding matrix.
 *  @see thescipub.com/PDF/jcssp.2013.377.382.pdf
 *----------------------------------------------------------------------------
 *  @param c  the cost/distance matrix, where a value of zero implies no connection.
 *            If the actual distance is zero, use a very small number instead.
 *  @param s  the single-source vertex
 */
class SSShortestPath (c: MatriD, s: Int)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Item` inner case class has two fields, vertex id and distance from
     *  vertex s (the source) as well as a compare method based on distance.
     *  @param id  the id of the vertex
     *  @param dd  the vextex's distance from vertex s
     */
    case class Item (id: Int, dd: Double) extends Ordered [Item]
    {
        def compare (v: Item) = v.dd compare dd
    } // Item class

    private val DEBUG = false                           // debug flag
    private val MAX   = Double.MaxValue                 // infinity (indicates no path so far)
    private val n     = c.dim1                          // the number of vertices
    private val rang  = 0 until n                       // index range
    private val q     = PriorityQueue.empty [Item]      // priority queue ordered by distance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the shortest path from vertex 's' to each vertex 'j' returning
     *  the vector 'd' giving the distance from 's' to all other vertices and
     *  the vector 'p' of predecessor vertices.  The path from 's' to each vertex
     *  can be deduced from the 'p' vector.
     */
    def spath (): Tuple2 [VectoD, VectorI] =
    {
        val d = c(s)                                    // the distance from vertex s to each vertex j
        for (j <- rang if c(s, j) == 0) d(j) = MAX      // set distance to infinity if no direct edge from s
        d(s)  = 0.0                                     // zero distance from s to s
        val p = new VectorI (n)                         // create predecessor vector
        for (j <- rang) {
            p(j) = if (d(j) != MAX) s else -1           // initialize predecessor vertices
            q += Item (j, d(j))                         // add each vertex j incl. its distance to q
        } // for

        var go = true                                   // set the go flag to true
        while (go && q.nonEmpty) {                      // iteratively, try to find a shortcut

            val v = q.dequeue ()                        // vertex v in q with least distance dd from s
            if (v.dd == MAX) go = false                 // no shortcuts left, so quit
            else {
                for (j <- rang if c(v.id, j) > 0.0) {   // check vertex v's neighbors
                    val alt = v.dd + c(v.id, j)         // compute alternate distance from s to j
                    if (alt < d(j)) {
                        p(j) = v.id; d(j) = alt; q += Item (j, d(j))
                    } // if
                } // for
                if (DEBUG) println ("distance from " + s + ": d = " + d)
            } // if
            
        } // while
        (d, p)                      // return the distance and predecessor vectors
    } // spath

} // SSShortestPath class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPath` companion object provides factory methods for the 
 *  `SSShortestPath` class.
 */
object SSShortestPath
{
    private val DEBUG = false                           // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert `VectorD` to `Double` by taking the minimum.  This is used
     *  to handle labels that are vectors.
     */
//  implicit def vectorD2Double (x: VectorD): Double = x.min ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `SSShortestPath` object from a multi-digraph.  First convert
     *  the graph's adjacency set representation to an adjacency matrix with
     *  edge weights (which optionally include initial vertex weight).
     *  @param g            the multi-digraph to use
     *  @param s            the source vertex for the multi-digraph
     *  @param hasV_Weight  whether vertices carry weights or not (0.0)
     *  @param hasE_Weight  whether edges carry specific weights or just unit weights (1.0)
     */
    def apply (g: MGraph, s: Int, hasV_Weight: Boolean = false, hasE_Weight: Boolean = true): SSShortestPath =
    {
        val n = g.size
        val c = new MatrixD (n, n)                             // cost/weight matrix
        for (i <- 0 until n; j <- g.ch(i)) {
            val v_i  = g.label(i)                              // vertex i
            val e_ij = g.elabel((i, j))                        // edge(s) i -> j

            val w_edge: Double = e_ij
            if (DEBUG) println (s"w ($i, $j) = $w_edge")
            val v_weight = if (hasV_Weight) v_i else 0.0
            val e_weight = if (hasE_Weight) w_edge else 1.0
            c(i, j) = v_weight + e_weight                      // overall weight for edge i -> j
        } // for
        new SSShortestPath (c, s)
    } // apply

} // SSShortestPath object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPathTest` object is used to test the `SSShortestPath` class.
 *  Input is in the form of matrices (`MatrixD` or `SparseMatrixD`).
 *  > run-main scalation.graphalytics.mutable.SSShortestPathTest
 */
object SSShortestPathTest extends App
{
    // dense matrix representation for the graph, where d_ij = distance from i to j

    val c = new MatrixD ((3, 3),   0.0,   2.0, 100.0,
                                 100.0,   0.0,   3.0,
                                   4.0, 100.0,   0.0)
    println (c)
    val sp = new SSShortestPath (c, 0)
    println ("(d, p) = " + sp.spath ())          // shortest distance from s to all vertices)

    // sparse matrix representation for the graph, where d_ij = distance from i to j

    val b = new SparseMatrixD (3, 3, Array (new RowMap ((1, 2.0),   (2, 100.0)),
                                            new RowMap ((0, 100.0), (2, 3.0)),
                                            new RowMap ((0, 4.0),   (1, 100.0)) ))
    println (b)
    val sp2 = new SSShortestPath (b, 0)
    println ("(d, p) = " + sp2.spath ())        // shortest distance from s to all vertices

} // SSShortestPathTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPathTest2` object is used to test the `SSShortestPath` class.
 *  Input is in the form of graphs (`MGraph`).
 *  @see http://thescipub.com/PDF/jcssp.2013.377.382.pdf (Fig. 1)
 *  > run-main scalation.graphalytics.mutable.SSShortestPathTest2
 */
object SSShortestPathTest2 extends App
{
    val g = new MGraph (Array (SET (1, 2),                       // ch(0)
                               SET (0, 3),                       // ch(1)
                               SET (0, 1, 3),                    // ch(2)
                               SET (1, 2)),                      // ch(3)
                        Array (10, 11, 12, 13),                  // for A, B, C, D
                        Map ((0, 1) -> 65,
                             (0, 2) -> 46,
                             (1, 0) -> 39,
                             (1, 3) -> 14,
                             (2, 0) -> 46,
                             (2, 1) -> 37,
                             (2, 3) -> 21,
                             (3, 1) -> 19,
                             (3, 2) -> 15),
                        false, "g")

    g.checkEdges
    g.checkElabels
    g.printG ()

    val s  = 0
    val sp = SSShortestPath (g, s)
    println ("(d, p) = " + sp.spath ())        // shortest distance from s to all vertices

} // SSShortestPathTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPathTest3` object is used to test the `SSShortestPath` class.
 *  Input is in the form of graphs (`MGraph`).  This test case requires
 *  `Tlabel` in `LabelType` to be `VectorD`.  Should be commented out otherwise.
 *  @see thescipub.com/PDF/jcssp.2013.377.382.pdf (Fig. 1)
 *  > run-main scalation.graphalytics.mutable.SSShortestPathTest3
 */
//object SSShortestPathTest3 extends App
//{
//    val g = new MGraph (Array (SET (1, 2),                       // ch(0)
//                               SET (0, 3),                       // ch(1)
//                               SET (0, 1, 3),                    // ch(2)
//                               SET (1, 2)),                      // ch(3)
//                        Array (10, 11, 12, 13),                  // for A, B, C, D
//                        Map ((0, 1) -> VectorD (65, 72),
//                             (0, 2) -> VectorD (46),
//                             (1, 0) -> VectorD (39),
//                             (1, 3) -> VectorD (14),
//                             (2, 0) -> VectorD (46),
//                             (2, 1) -> VectorD (37),
//                             (2, 3) -> VectorD (21),
//                             (3, 1) -> VectorD (19),
//                             (3, 2) -> VectorD (15)),
//                        false, "g")
//
//    g.checkEdges
//    g.checkElabels
//    g.printG ()
//
//    //val s  = 0
//    val sp = SSShortestPath (g, s)
//    println ("(d, p) = " + sp.spath ())        // shortest distance from s to all vertices
//
//} // SSShortestPathTest3 object

