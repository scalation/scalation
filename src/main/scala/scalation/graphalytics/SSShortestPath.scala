
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sat Aug 10 14:26:34 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.PriorityQueue

import scalation.linalgebra.{Matrix, MatrixD, SparseMatrixD, VectorD, VectorI}
import scalation.linalgebra.SparseMatrixD.RowMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPath` class is used to solve shortest path problems for graphs
 *  stored in matrices.  It solves the Single-Source Shortest Path (SSSP) problem
 *  for directed graphs.  The edge cost/distance (must be non-negative) can be
 *  stored in either a dense or sparse matrix.  Dijkstra's Algorithm is used.
 *  @see http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
 *  @param c  the cost/distance matrix, where aA value of zero implies no connection.
 *            If the actual distance is zero, use a very small number instead.
 *  @param s  the single-source vertex
 */
class SSShortestPath (c: Matrix, s: Int)
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

    private val DEBUG = true                            // debug flag
    private val MAX   = Double.MaxValue                 // infinity (indicates no path so far)
    private val n     = c.dim1                          // the number of vertices
    private val rang  = 0 until n                       // index range
    private val q = PriorityQueue.empty [Item]          // priority queue ordered by distance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the shortest path from vertex 's' to each vertex 'j' returning
     *  the vector 'd' giving the distance from 's' to all other vertices and
     *  the vector 'p' of predecessor vertices.  The path from 's' to each vertex
     *  can be deduced from the 'p' vector.
     */
    def spath (): Tuple2 [VectorD, VectorI] =
    {
        val d = c(s)                                    // the distance from vertex s to each vertex j
        for (j <- rang if c(s, j) == 0) d(j) = MAX      // set distance to infinity if no direct edge from s
        d(s)  = 0.0                                     // zero distance from s to s
        val p = new VectorI (n)                         // create predecessor vertor
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
 *  `APShortestPath` class.
 */
object SSShortestPath
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `SSShortestPath` object from a graph.  First convert the graph's
     *  adjaceny set representation to an adjacency matrix with unit edge lengths.
     *  @param g  the graph to use
     *  @param s  the source for the graph
     */
    def apply (g: Graph2, s: Int, hasNodeWeight: Boolean = false): SSShortestPath =
    {
        val n = g.size
        val c = new MatrixD (n, n)
        for (i <- 0 until n; j <- g.adj(i)) {
            c(i, j) = if (hasNodeWeight) g.elabel((i, j)) + g.label(i) 
                      else               g.elabel((i, j))
        } // for
        new SSShortestPath (c, s)
    } // apply

} // APShortestPath object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SSShortestPathTest` object is used to test the `SSShortestPath` class.
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

