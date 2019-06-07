
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Karen Gilmer
 *  @version 1.6
 *  @date    Wed Jan  9 15:38:04 EST 2013
 *  @see     LICENSE (MIT style license file).
 */
 
package scalation.analytics.clusterer

import scala.collection.mutable.{Set, ArrayBuffer}
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Cluster several vectors/points using hierarchical clustering.  Start with
 *  each point forming its own cluster and merge clusters until there are only 'k'.
 *  @param x  the vectors/points to be clustered stored as rows of a matrix
 *  @param k  stop when the number of clusters equals k
 */
class HierClusterer (x: MatriD, k: Int = 2)
      extends Clusterer with Error
{
    if (k >= x.dim1) flaw ("constructor", "k must be less than the number of vectors")

    private val DEBUG = false                             // debug flag
    private val cent  = new MatrixD (k, x.dim2)           // the k centroids of clusters
    private val to_c  = Array.ofDim [Int] (x.dim1)        // assignment of vectors to clusters
    private val clust = ArrayBuffer [Set [Int]] ()        // the list of clusters
    private val sz    = new VectorI (k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively merge clusters until until the number of clusters equals 'k'.
     */
    def train (): HierClusterer =
    {
        sz.set (0)                                        // initialize cluster sizes to zero
        var set_i: Set [Int] = null
        var set_j: Set [Int] = null
        initClusters ()

        for (kk <- x.dim1 until k by -1) {
            var minDist = Double.PositiveInfinity
            for (i <- 0 until kk-1; j <- i+1 until kk) {
                val d_ij = clustDist (clust(i), clust(j))
                if (d_ij < minDist) {
                    minDist = d_ij                        // update minDistance
                    set_i   = clust(i)                    // remember point sets i and j
                    set_j   = clust(j)
                } // if
            } // for

            clust += (set_i | set_j)                      // add the union of sets i and j
            clust -= set_i                                // remove set i
            clust -= set_j                                // remove set j
            if (DEBUG) println (s"train: for cluster (${kk-1}), clust = $clust")
        } // for

        finalClusters ()                                  // make final cluster assignments
        calcCentroids (x, to_c, sz, cent)                 // calculate centroids for clusters
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cluster assignment vector.  Should only be called after `train`.
     */
    def cluster: Array [Int] = to_c

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids.  Should only be called after `train`. 
     */
    def centroids: MatriD = cent

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes of the centroids.  Should only be called after `train`. 
     */
    def csize: VectoI = sz

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum distance between cluster 'setA' and 'setB'.
     *  @param setA  the first set
     *  @param setB  the second set
     */
    def clustDist (setA: Set [Int], setB: Set [Int]): Double =
    {
        var dm = Double.PositiveInfinity
        for (i <- setA; j <- setB) {
            val d = dist (x(i), x(j))
            if (d < dm) dm = d
        } // for
        dm
    } // clustDist

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create initial clusters where each point forms its own cluster.
     */
    def initClusters () { for (i <- x.range1) clust += Set (i) }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For each data point, determine its final cluster assignment, finalize
     *  the 'to_c' array of cluster assignments.
     */
    def finalClusters ()
    {
        for (c <- 0 until k; i <- clust(c)) { to_c(i) = c; sz(c) += 1 }
    } // finalClusters

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector y, determine which cluster it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Int = distance (z, cent).argmin ()

} // HierClusterer class

import Clusterer.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HierClusterer` object is used to test the `HierClusterer` class.
 *  > runMain scalation.analytics.clusterer.HierClustererTest
 */
object HierClustererTest extends App
{
    import Clusterer.x

    val y = VectorD (10.0, 10.0)
    val z = VectorD ( 2.0,  4.0)
    println ("x = " + x)
    println ("y = " + y)
    println ("z = " + z)
    println ("----------------------------------------------------")

    val k   = 3
    val opt = 3.0
    val cl = new HierClusterer (x, k)                 
    test (x, Array (false), cl, opt)

//  cl.train ()
    println ("--- final cluster = " + cl.cluster.deep + "\n")
    println ("--- classify " + y + " = " + cl.classify (y) + "\n")
    println ("--- classify " + z + " = " + cl.classify (z) + "\n")
    println (s"sse = ${cl.sse (x, cl.cluster)}")

} // HierClustererTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HierClustererTest2` object is used to test the `HierClusterer` class.
 *  > runMain scalation.analytics.clusterer.HierClustererTest2
 */
object HierClustererTest2 extends App
{
    import scalation.random.{Bernoulli, Normal}
    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val v     = new MatrixD (50, 2)
    for (i <- v.range1) v(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)

    println ("v = " + v)
    println ("----------------------------------------------------")

    val cl = new HierClusterer (v, 4)                 
    cl.train ()
    println ("--- final cluster = " + cl.cluster.deep + "\n")
    println (s"sse = ${cl.sse(v, cl.cluster)}")

} // HierClustererTest2 object

