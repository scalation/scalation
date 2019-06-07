
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Oct 10 16:42:21 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see romisatriawahono.net/lecture/dm/paper/clustering/
 *       Garcia%20-%20K-means%20algorithms%20for%20functional%20data%20-%202015.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics.fda

import scalation.analytics.clusterer.{Clusterer, KMeansClusterer}
import scalation.calculus.DB_Spline
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustering_F` class provides a simple form of k-means clustering
 *  that simply smoothes the data and then appliers `KMeansClustering`.
 *  @param x  the vectors/points to be clustered stored as rows of a matrix
 *  @param t  the time points
 *  @param τ  the time points for knots
 *  @param k  the number of clusters to make
 */
class KMeansClustering_F (x: MatrixD, t: VectorD, τ: VectorD, k: Int)
      extends Clusterer
{
    private val DEBUG = true                          // debug flag
    private val xs    = new MatrixD (x.dim1, x.dim2)  // smoothed version of data matrix
    private var cl: KMeansClusterer = null            // holder to clustering algorithm

   /** As seen from class KMeansClustering_F, the missing signatures are as follows.
    *  For convenience, these are usable as stub implementations.
    */
    def centroids: scalation.linalgebra.MatrixD = ???
    def csize: scalation.linalgebra.VectorI = ???
    def classify(y: scalation.linalgebra.VectoD): Int = ???
    def train(): scalation.analytics.clusterer.Clusterer = ???

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create 'k' clusters consisting of points/rows that are closest to each other.
     */
    def cluster (): Array [Int] =
    {
        smooth ()                                     // smooth the data
        if (DEBUG) println ("xs = " + xs)
        cl = new KMeansClusterer (xs, k)              // use classical k-means
        cl.train ()
        cl.cluster                                    // create the clsuters
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
        val bf = new DB_Spline (t)
        for (i <- x.range1) {                         // for each vector/row in matrix x
//          val moo = new Smoothing_F (x(i), t, τ)
            val moo = new Smoothing_F (x(i), t, bf)
            val c   = moo.train ()
            if (DEBUG) println ("c = " + c)
            for (j <- x.range2) xs(i, j) = moo.predict (t(j))
        } // for
    } // smooth

} // KMeansClustering_F class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustering_FTest` object is used to test the `KMeansClustering_F`
 *  class.
 *  > runMain scalation.analytics.fda.KMeansClustering_FTest
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
        val cl = new KMeansClustering_F (x, t, t, 3)                 
        cl.setStream (s)
        println ("--- final cluster = " + cl.cluster ().deep + "\n")
        println ("--- classify " + y + " = " + cl.classify (y) + "\n")
    } // for

} // KMeansClustering_FTest object

