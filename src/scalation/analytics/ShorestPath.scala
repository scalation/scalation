
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Fri Jul 20 16:47:16 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.min

import scalation.linalgebra.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to solve shortest path problems for graphs stored in matrices.
 *  @param c  the cost/distance matrix.
 */
class ShortestPath (c: MatrixD)
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
    val d = new MatrixD ((3, 3),   0.,   2., 100.,
                                 100.,   0.,   3.,
                                   4., 100.,   0.)
    println (d)
    val sp = new ShortestPath (d)
    sp.spath
    println (d)

} // ShortestPathTest object

