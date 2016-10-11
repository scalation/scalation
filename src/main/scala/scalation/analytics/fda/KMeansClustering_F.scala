
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Oct 10 16:42:21 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see romisatriawahono.net/lecture/dm/paper/clustering/
 *       Garcia%20-%20K-means%20algorithms%20for%20functional%20data%20-%202015.pdf
 */

package scalation.analytics.fda

import scalation.analytics.{Clusterer, KMeansClustering}
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustering_F` class provides a simple form of k-means clustering
 *  that simply smoothes the data and then appliers `KMeansClustering`.
 *  @param x  the vectors/points to be clustered stored as rows of a matrix
 *  @param k  the number of clusters to make
 *  @param s  the random number stream (to vary the clusters made)
 */
class KMeansClustering_F (x: MatrixD, t: VectorD, k: Int, s: Int = 0)
      extends Clusterer
{
    private val DEBUG = true                          // debug flag
    private val xs    = new MatrixD (x.dim1, x.dim2)  // smoothed version of data matrix
    private var cl: KMeansClustering = null           // holder to clustering algorithm

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create 'k' clusters consisting of points/rows that are closest to each other.
     */
    def cluster (): Array [Int] =
    {
        smooth ()                                     // smooth the data
        if (DEBUG) println ("xs = " + xs)
        cl = new KMeansClustering (xs, k, s)          // use classical k-means
        cl.cluster ()                                 // create the clsuters
    } // cluster

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'y', determine which cluster it belongs to,
     *  i.e., the cluster whose centroid it is closest to.
     *  @param y  the vector to classify
     */
    def classify (y: VectorD): Int =
    {
        cl.classify (y)
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the data:  For each row of matrix 'x', create a smoother version
     *  and store it in matrix 'xs'.
     */
    private def smooth ()
    {
        for (i <- x.range1) {                         // for each vector/row in matrix x
            val frg = new Smoothing_F (x(i), t, t.dim-3)
            val c = frg.train ()
            if (DEBUG) println ("c = " + c)
            for (j <- x.range2) xs(i, j) = frg.predict (t(j))
        } // for
    } // smooth

} // KMeansClustering_F class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustering_FTest` object is used to test the `KMeansClustering_F`
 *  class.
 *  > run-main scalation.analytics.fda.KMeansClustering_FTest
 */
object KMeansClustering_FTest extends App
{
    val x = new MatrixD ((6, 6), 1.0, 2.0, 2.0, 1.0, 2.0, 1.0,    // FIX: need larger example
                                 2.0, 1.0, 1.0, 2.0, 1.0, 2.0,
                                 5.0, 4.0, 4.0, 5.0, 4.0, 5.0,
                                 4.0, 5.0, 5.0, 4.0, 5.0, 4.0,
                                 9.0, 8.0, 8.0, 9.0, 8.0, 9.0,
                                 8.0, 9.0, 9.0, 8.0, 9.0, 8.0)

    val t = VectorD.range (0, 6) / 2.0
    val y = new VectorD (6); y.set (10.0)

    println ("x = " + x)
    println ("t = " + t)
    println ("y = " + y)
    println ("----------------------------------------------------")

    for (s <- 0 to 4) {                         // test with different random streams
        banner ("KMeansClustering_F for stream s = " + s)
        val cl = new KMeansClustering_F (x, t, 3, s)                 
        println ("--- final cluster = " + cl.cluster ().deep + "\n")
        println ("--- classify " + y + " = " + cl.classify (y) + "\n")
    } // for

} // KMeansClustering_FTest object

