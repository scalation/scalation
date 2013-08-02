
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Jul 31 16:41:13 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.PriorityQueue

import scalation.linalgebra.{Matrix, MatrixD, SparseMatrixD}
import scalation.linalgebra.SparseMatrixD.RowMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'SSShortestPath' class is used to solve shortest path problems for graphs
 *  stored in matrices.  It solves the Single-Source Shortest Path (SSSP) problem
 *  for directed graphs.  The edge cost/distance (must be non-negative) can be
 *  stored in either a dense or sparse matrix.  Dijkstra's Algorithm is used.
 *  @see http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
 *  @param c  the cost/distance matrix
 *  @param s  the single-source vertex
 */
class SSShortestPath (c: Matrix, s: Int)
{
    type Item = Tuple2 [Int, Double]                       // vertex id and its distance to vertex s
    private val n     = c.dim1                             // the number of vertices
    private val rang  = 0 until n                          // index range
    private val p = Array.ofDim [Int] (n)                  // the predecessor to each vertex j
    private val d = Array.ofDim [Double] (n)               // the distance from vertex s to each vertex j
    private val q = PriorityQueue.empty [Item]             // priority queue ordered by distance
            (new Ordering [Item] { def compare (it1: Item, it2: Item) = it1._2 compare it2._2 })
    private val INF = Double.PositiveInfinity

    for (j <- rang) { p(j) = -1; d(j) = INF }              // init. predecessor and distance
    d(s) = 0.0                                             // no distance from s to s
    for (j <- rang) q += ( (j, d(j)) )                     // add each vertex j incl. distance to q

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the shortest path from vertex s to each vertex j.
     */
    def spath() =
    {
        var go = true                                      // set the go flag to true
        while (go && q.nonEmpty) {

            val (l, d_l) = q.dequeue ()                    // vertex l in q with least distance d_l from s
            if (d_l == INF) go = false                     // no shortcuts left, so quit
            else {
                for (j <- rang if c(l, j) > 0.0) {         // check vertex l's neighbors
                    val alt = d_l + c(l, j)                // compute alternate distance from s to j
                    if (alt < d(j)) {
                        p(j) = l;  d(j) = alt; q += ( (j, d(j)) )
                    } // if
                } // for
            } // if

        } // while
        d                                                  // return the array of distances
    } // spath

} // SSShortestPath class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'SSShortestPathTest' object is used to test the 'SSShortestPath' class.
 */
object SSShortestPathTest extends App
{
    // dense matrix representation for the graph, where d_ij = distance from i to j

    val d = new MatrixD ((3, 3),   0.0,   2.0, 100.0,
                                 100.0,   0.0,   3.0,
                                   4.0, 100.0,   0.0)
    println (d)
    val sp = new SSShortestPath (d, 0)
    sp.spath
    println ("d = " + d)

    // sparse matrix representation for the graph, where d_ij = distance from i to j

    val d2 = new SparseMatrixD (3, 3, Array (new RowMap ((1, 2.0),   (2, 100.0)),
                                             new RowMap ((0, 100.0), (2, 3.0)),
                                             new RowMap ((0, 4.0),   (1, 100.0)) ))
    println (d2)
    val sp2 = new SSShortestPath (d2, 0)
    sp2.spath
    println ("d2 = " + d2)

} // SSShortestPathTest object

