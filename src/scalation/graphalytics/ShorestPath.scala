
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Fri Jul 20 16:47:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import math.min

import scalation.linalgebra.{Matrix, MatrixD, SparseMatrixD}
import scalation.linalgebra.SparseMatrixD.RowMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to solve shortest path problems for graphs stored in matrices.
 *  It solves the All-Pairs Shortest Paths (APSP) problem for directed graphs.
 *  The edge cost/distance (must be non-negative) can be stored in either a
 *  dense or sparse matrix.  The Floyd­Warshall Algorithm is used.
 *  @param c  the cost/distance matrix.
 */
class ShortestPath (c: Matrix)
{
    val rang = 0 until c.dim1     // index range

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the shortest from node i to j for all pairs of nodes.
     */
    def spath ()
    {
        for (k <- rang) {
            for (i <- rang; j <- rang) c(i,j) = min (c(i,j), c(i,k) + c(k,j))
        } // for
    } // spath

} // ShortestPath class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the ShortestPath class.
 */
object ShortestPathTest extends App
{
    // dense matrix representation for the graph, where d_ij = distance from i to j

    val d = new MatrixD ((3, 3),   0.0,   2.0, 100.0,
                                 100.0,   0.0,   3.0,
                                   4.0, 100.0,   0.0)
    println (d)
    val sp = new ShortestPath (d)
    sp.spath
    println ("d = " + d)

    // sparse matrix representation for the graph, where d_ij = distance from i to j

    val d2 = new SparseMatrixD (3, 3, Array (new RowMap (List ((1, 2.0),   (2, 100.0))),
                                             new RowMap (List ((0, 100.0), (2, 3.0))),
                                             new RowMap (List ((0, 4.0),   (1, 100.0)))))
    println (d2)
    val sp2 = new ShortestPath (d2)
    sp2.spath
    println ("d2 = " + d2)

} // ShortestPathTest object

