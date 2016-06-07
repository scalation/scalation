
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.random.Randi
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustering` class cluster several vectors/points using k-means
 *  clustering.  Either (1) randomly assign points to 'k' clusters or (2) randomly
 *  pick 'k' points as initial centroids (technique (1) to work better and is the
 *  primary technique).  Iteratively, reassign each point to the cluster containing
 *  the closest centroid.  Stop when there are no changes to the clusters.
 *  @param x        the vectors/points to be clustered stored as rows of a matrix
 *  @param k        the number of clusters to make
 *  @param s        the random number stream (to vary the clusters made)
 *  @param primary  true indicates use the primary technique for initiating the clustering
 */
class KMeansClustering (x: MatrixD, k: Int, s: Int = 0, primary: Boolean = true)
      extends Clusterer with Error
{
    if (k >= x.dim1) flaw ("constructor", "k must be less than the number of vectors")

    private val MAX_ITER = 200                           // the maximum number of iterations
    private val cent     = new MatrixD (k, x.dim2)       // the k centroids of clusters
    private val clustr   = Array.ofDim [Int] (x.dim1)    // assignment of vectors to clusters
    private val dist     = new VectorD (x.dim1)          // distance to closest centroid
    dist.set (Double.PositiveInfinity)

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
    /** Assign each vector/point to a random cluster.  Primary technique for
     *  initiating the clustering.
     */
    def assign ()
    {
        val ran = new Randi (0, k - 1, s)                // for random integers: 0, ..., k-1
        for (i <- 0 until x.dim1) clustr(i) = ran.igen   // randomly assign to a cluster
    } // assign

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reassign each vector/point to the cluster with the closest centroid.
     *  Indicate done, if no points changed clusters (for stopping rule).
     */
    def reassign (): Boolean =
    {
        var done = true                                // done indicates no changes
        for (i <- 0 until x.dim1) {
            val v = x(i)                               // let v be the ith vector
            for (c <- 0 until k) {
                val newDist = distance (v, cent(c))    // calc distance to centroid c
                if (newDist < dist(i)) {               // is it closer than old distance
                    dist(i)  = newDist                 // make it the new distance
                    clustr(i) = c                      // assign vector i to cluster c
                    done = false                       // changed clusters => not done
                } // if
            } // for
        } // for
        done                                           // return whether there were no changes
    } // reassign

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly pick vectors/points to serve as the initial k centroids (cent).
     *  Secondary technique for initiating the clustering.
     */
    def pickCentroids ()
    {
        val ran  = new Randi (0, x.dim1 - 1, s)     // for random integers: 0, ..., x.dim1-1
        val iSet = Set [Int] ()                     // set of integers already generated
        for (c <- 0 until k) {
            var i = ran.igen                        // generate a random integer
            while (iSet contains i) i = ran.igen    // do not allow repeats
            iSet   += i                             // add to set of generated integers
            cent(c) = x(i)                          // let centroid c be the ith vector
        } // for
    } // pickCentroids

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the centroids based on current assignment of points to clusters.
     */
    def calcCentroids ()
    {
        val cx = new MatrixD (k, x.dim2)    // to hold sum of vectors for each cluster
        val cs = new VectorD (k)            // to hold number of vectors in each cluster
        for (i <- 0 until x.dim1) {
            cx(clustr(i)) += x(i)           // add the next vector in cluster
            cs(clustr(i)) += 1.0            // add 1 to number in cluster
        } // for
        for (c <- 0 until k) cent(c) = cx(c) / cs(c)   // divide to get averages/means
    } // calcCentroids

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively recompute clusters until the assignment of points does not
     *  change, returning the final cluster assignment vector.
     */
    def cluster (): Array [Int] =
    {
        if (primary) {
            assign ()                                // randomly assign points to clusters
            calcCentroids ()                         // calculate the initial centroids
        } else {
            pickCentroids ()                         // alt., pick points for initial centroids 
        } // if
        println ("(" + 0 + ") clustr = " + clustr)
        println ("(" + 0 + ") cent  = " + cent)

        breakable { for (l <- 1 to MAX_ITER) {
            if (reassign ()) break                   // reassign points to clusters (no change => break)
            calcCentroids ()                         // re-calculate the centroids
            println ("(" + l + ") clustr = " + clustr)
            println ("(" + l + ") cent  = " + cent)
        }} // for   
        clustr                                       // return the cluster assignment vector
    } // cluster

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector y, determine which cluster it belongs to (i.e.,
     *  the cluster whose centroid it is closest to.
     *  @param y  the vector to classify
     */
    def classify (y: VectorD): Int =
    {
        var dist = distance (y, cent(0))          // calc distance to centroid 0
        var clus = 0                              // assign y to cluster 0
        for (c <- 1 until k) {
            val newDist = distance (y, cent(c))   // calc distance to centroid c
            if (newDist < dist) {                 // is it closer than old distance
                dist = newDist                    // make it the new distance
                clus = c                          // assign y to cluster c
            } // if
        } // for
        clus                                      // return cluster y belongs to
    } // classify

} // KMeansClustering class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusteringTest` object is used to test the `KMeansClustering` class.
 */
object KMeansClusteringTest extends App
{
    val v = new MatrixD ((6, 2), 1.0, 2.0,
                                 2.0, 1.0,
                                 5.0, 4.0,
                                 4.0, 5.0,
                                 9.0, 8.0,
                                 8.0, 9.0)
    val y = VectorD (10.0, 10.0)
    println ("v = " + v)
    println ("y = " + y)
    println ("----------------------------------------------------")

    for (s <- 0 to 4) {                         // test with different random streams
        val cl = new KMeansClustering (v, 3, s)                 
        println ("--- final cluster = " + cl.cluster () + "\n")
        println ("--- classify " + y + " = " + cl.classify (y) + "\n")
    } // for

} // KMeansClusteringTest object

