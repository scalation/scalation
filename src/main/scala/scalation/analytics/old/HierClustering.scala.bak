
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Cluster several vectors/points using hierarhical clustering.  Start with
 *  each point forming its own cluster and merge clusters until there are only k.
 *  @param x  the vectors/points to be clustered stored as rows of a matrix
 *  @param k  stop when the number of clusters equals k
 */
class HierClustering (x: MatrixD, k: Int = 1)
      extends Clustering with Error
{
    if (k >= x.dim1) flaw ("constructor", "k must be less than the number of vectors")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a distance metric between vectors/points u and v.
     *  @param u  the first vector/point
     *  @param v  the second vector/point
     */
    def distance (u: VectorD, v: VectorD): Double =
    {
        (u - v).normSq       // squared Euclidean norm used for efficiency, may use other norms
    } // distance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create initial clusters where each point forms its own cluster.
     */
    def initClusters (): Array [Int] =
    {
        null
    } // cluster

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively merge clusters until until the number of clusters equals k.
     */
    def cluster (): Array [Int] =
    {
        null
    } // cluster

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector y, determine which cluster it belongs to.
     *  @param y  the vector to classify
     */
    def classify (y: VectorD): Int =
    {
        -1
    } // classify

} // HierClustering class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the HierClustering class.
 */
object HierClusteringTest extends App
{
    val v = new MatrixD ((6, 2), 1., 2.,
                                 2., 1.,
                                 5., 4.,
                                 4., 5.,
                                 9., 8.,
                                 8., 9.)
    val y = new VectorD (10., 10.)
    println ("v = " + v)
    println ("y = " + y)
    println ("----------------------------------------------------")

    for (s <- 0 to 4) {                         // test with different random streams
        val cl = new HierClustering (v)                 
        println ("--- final cluster = " + cl.cluster () + "\n")
        println ("--- classify " + y + " = " + cl.classify (y) + "\n")
    } // for

} // HierClusteringTest object

