
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Karen Gilmer
 *  @version 1.5
 *  @date    Wed Jan  9 15:38:04 EST 2013
 *  @see     LICENSE (MIT style license file).
 */
 
package scalation.analytics.clusterer

import scala.collection.mutable.{Set, ListBuffer}
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectorD, VectorI}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Cluster several vectors/points using hierarchical clustering.  Start with
 *  each point forming its own cluster and merge clusters until there are only 'k'.
 *  @param x  the vectors/points to be clustered stored as rows of a matrix
 *  @param k  stop when the number of clusters equals k
 */
class HierClusterer (x: MatrixD, k: Int = 2)
      extends Clusterer with Error
{
    if (k >= x.dim1) flaw ("constructor", "k must be less than the number of vectors")

    private val cent   = new MatrixD (k, x.dim2)           // the k centroids of clusters
    private val clustr = Array.ofDim [Int] (x.dim1)        // assignment of vectors to clusters
    private val clust  = ListBuffer [Set [VectorD]] ()     // the list of clusters
    private val sizes  = new VectorI (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids. Should only be called after `cluster ()`. 
     */
    def centroids (): MatrixD = cent

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes of the centroids. Should only be called after 
     *  `cluster ()`. 
     */
    def csize (): VectorI =
    {
        for (i <- x.range1) sizes(clustr(i)) += 1
        sizes
    } // csize

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create initial clusters where each point forms its own cluster.
     *  @param setA  the first set
     *  @param setB  the second set
     */
    def clustDist (setA: Set [VectorD], setB: Set [VectorD]): Double =
    {
        var d = Double.PositiveInfinity
        for (a <- setA; b <- setB if distance (a, b) < d) d = distance (a, b)
        d
    } // clustDist

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create initial clusters where each point forms its own cluster.
     */
    def initClusters ()
    {
        for (i <- 0 until x.dim1) clust += Set [VectorD] (x(i))            
    } // initCluster

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively merge clusters until until the number of clusters equals k.
     */
    def cluster (): Array [Int] =
    {
        var set_i: Set [VectorD] = null
        var set_j: Set [VectorD] = null
        initClusters ()

        for (kk <- x.dim1 until k by -1) {
            var minDistance = Double.PositiveInfinity
            for (i <- 0 until kk-1; j <- i+1 until kk) {
                val d_ij = clustDist (clust(i), clust(j))
                if (d_ij < minDistance) {
                    minDistance = d_ij               // update minDistance
                    set_i = clust(i)                 // remember point sets i and j
                    set_j = clust(j)
                } // if
            } // for

            clust += (set_i | set_j)                 // add the union of sets i and j
            clust -= set_i                           // remove set i
            clust -= set_j                           // remove set j
            println ("cluster: (" + (kk-1) + ") clust = " + clust)
        } // for

        finalClusters ()                             // make final cluster assignments
        calcCentroids ()                             // calculate centroids for clusters
        clustr
    } // cluster

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For each data point, determine its cluster assignment.
     */
    def finalClusters ()
    {
        for (i <- 0 until x.dim1) {                  // for each data point
            breakable { for (c <- 0 until k) {       // find its cluster
                if (clust(c) contains x(i)) { clustr(i) = c; break }
            }} // for
        } // for
    } // finalClusters

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the centroids based on current assignment of points to clusters.
     */
    def calcCentroids ()
    {
        val cx = new MatrixD (k, x.dim2)             // to hold sum of vectors for each cluster
        val cs = new VectorD (k)                     // to hold number of vectors in each cluster
        for (i <- 0 until x.dim1) {
            val ci = clustr(i)
            cx(ci) = cx(ci) + x(i)                   // add the next vector in cluster
            cs(ci) += 1.0                            // add 1 to number in cluster
        } // for
        for (c <- 0 until k) cent(c) = cx(c) / cs(c)   // divide to get averages/means
        println(s"cent = $cent")        
    } // calcCentroids

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector y, determine which cluster it belongs to.
     *  @param y  the vector to classify
     */
    def classify (y: VectorD): Int =
    {
        var dist = distance (y, cent(0))             // calc distance to centroid 0
        var clus = 0                                 // assign y to cluster 0
        for (c <- 1 until k) {
            val newDist = distance (y, cent(c))      // calc distance to centroid c
            if (newDist < dist) {                    // is it closer than old distance
                dist = newDist                       // make it the new distance
                clus = c                             // assign y to cluster c
            } // if
        } // for
        clus             
    } // classify

} // HierClusterer class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HierClusterer` object is used to test the `HierClusterer` class.
 *  > runMain scalation.analytics.clusterer.HierClustererTest
 */
object HierClusterer extends App
{
    val v = new MatrixD ((6, 2), 1.0, 2.0,
                                 2.0, 1.0,
                                 5.0, 4.0,
                                 4.0, 5.0,
                                 9.0, 8.0,
                                 8.0, 9.0)
    val y = VectorD (10.0, 10.0)
    val z = VectorD ( 2.0,  4.0)
    println ("v = " + v)
    println ("y = " + y)
    println ("z = " + z)
    println ("----------------------------------------------------")

    val cl = new HierClusterer (v, 3)                 
    println ("--- final cluster = " + cl.cluster ().deep + "\n")
    println ("--- classify " + y + " = " + cl.classify (y) + "\n")
    println ("--- classify " + z + " = " + cl.classify (z) + "\n")
    println (s"sse = ${cl.sse(v)}")

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
    println ("--- final cluster = " + cl.cluster ().deep + "\n")
    println (s"sse = ${cl.sse(v)}")

} // HierClustererTest2 object

