
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Fri Jul 20 16:47:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.math.min

import scalation.linalgebra.{MatriD, MatrixD, SparseMatrixD}
import scalation.linalgebra.SparseMatrixD.RowMap
import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `APShortestPath` class is used to solve shortest path problems for graphs
 *  stored in matrices.  It solves the All-Pairs Shortest Path 'APSP' problem
 *  for directed graphs.  The edge cost/distance (must be non-negative) can be
 *  stored in either a dense or sparse matrix.  The Floyd-Warshall Algorithm is used.
 *  @see http://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
 *  'APSP' can be used to determine a graph's eccentricities, radius and diameter.
 *  These can also be computed using a BFS-based algorithm.
 *  @see math.stackexchange.com/questions/240556/radius-diameter-and-center-of-graph
 *  @param c  the cost/distance matrix.
 */
class APShortestPath (c: MatriD)
{
    val rang = 0 until c.dim1     // index range

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the shortest from vertex i to j for all pairs of vertices.
     *  The matrix 'c' is changed in-place from edge length to least distance.
     */
    def spath ()
    {
        for (k <- rang) {
            for (i <- rang; j <- rang) c(i,j) = min (c(i,j), c(i,k) + c(k,j))
        } // for
    } // spath

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the eccentricity of vertex 'i'.  Must call 'spath' first.
     *  @param i  the vertex whose eccentricity is sought
     */
    def ecc (i: Int): Double = c(i).max ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the radius (minimum eccentricity) of the graph.  Must call 'spath'
     *  first.
     */
    def rad: Double = 
    {
        var emin = ecc (0)
        for (i <- 1 until c.dim1) {
            val ec = ecc (i)
            if (ec < emin) emin = ec
        } // for
        emin
    } // radius

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the diameter (longest shortest path) of the graph.  Must call
     *  'spath' first.
     */
    def diam: Double = c.max ()

} // APShortestPath class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `APShortestPath` companion object provides factory methods for the 
 *  `APShortestPath` class.
 */
object APShortestPath
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `APShortestPath` object from a graph.  First convert the graph's
     *  adjacency set representation to an adjacency matrix with unit edge lengths.
     *  @param g  the graph to use
     */
    def apply (g: Graph): APShortestPath =
    {
        val n = g.size
        val c = new MatrixD (n, n)
        c.set (Double.PositiveInfinity)
        for (i <- 0 until n; j <- g.ch(i)) c(i, j) = 1.0
        new APShortestPath (c)
    } // apply

} // APShortestPath object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `APShortestPathTest` object is used to test the `APShortestPath` class.
 */
object APShortestPathTest extends App
{
    // dense matrix representation for the graph, where d_ij = distance from i to j

    val d = new MatrixD ((3, 3),   0.0,   2.0, 100.0,
                                 100.0,   0.0,   3.0,
                                   4.0, 100.0,   0.0)
    println (d)
    val sp = new APShortestPath (d)
    sp.spath ()
    println ("d        = " + d)
    println ("radius   = " + sp.rad)
    println ("diameter = " + sp.diam)

    // sparse matrix representation for the graph, where d_ij = distance from i to j

    val d2 = new SparseMatrixD (3, 3, Array (new RowMap ((1, 2.0),   (2, 100.0)),
                                             new RowMap ((0, 100.0), (2, 3.0)),
                                             new RowMap ((0, 4.0),   (1, 100.0)) ))
    println (d2)
    val sp2 = new APShortestPath (d2)
    sp2.spath ()
    println ("d2       = " + d2)
    println ("radius   = " + sp2.rad)
    println ("diameter = " + sp2.diam)

    // graph adjacency set representation with unit edge lengths (distance is 1.0)
    // @see math.stackexchange.com/questions/240556/radius-diameter-and-center-of-graph

    val g = new Graph (Array (Set (4, 5),                // 0
                              Set (5),                   // 1
                              Set (6, 7),                // 2
                              Set (7, 8),                // 3
                              Set (0, 5, 9),             // 4
                              Set (0, 1, 4, 6, 10),      // 5
                              Set (2, 5, 7, 10, 11),     // 6
                              Set (2, 3, 6, 8),          // 7
                              Set (3, 7, 12),            // 8
                              Set (4),                   // 9
                              Set (5, 6),                // 10
                              Set (6),                   // 11
                              Set (8)))                  // 12

    println (g)
    var dia = 0.0
    var sp3: APShortestPath = null
    print ("build matrix:  "); time { sp3 = APShortestPath (g) }
    print ("shortest path: "); time { sp3.spath () } 
    print ("calc diameter: "); time { dia = sp3.diam }
    println ("diameter = " + dia)
    println ("radius   = " + sp3.rad)

} // APShortestPathTest object

