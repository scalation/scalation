
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package clusterer

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Clusterer` provides a simple dataset (matrix of data points) for
 *  initial testing of clustering algorithms.
 */
object Clusterer
{
//                                x0   x1
    val x = new MatrixD ((6, 2), 1.0, 2.0,                               // data matrix (6 points)
                                 2.0, 1.0,
                                 4.0, 5.0,
                                 5.0, 4.0,
                                 8.0, 9.0,
                                 9.0, 8.0)

    import scalation.stat.Statistic

    private val NTESTS = 1000                                            // number of tests/streams

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the quality of the variants of the `KMeansClusterer` class.
     *  @param x    the data matrix holding the points/vectors
     *  @param fls  the array of flags 
     *  @param alg  the clustering algorithm to test
     *  @param opt  the known optimum for see (ignore if not known)
     */
    def test (x: MatriD, fls: Array [Boolean], alg: Clusterer, opt: Double = -1.0)
    {
        banner (s"test (flags = ${fls.deep}, opt = $opt)")
        val stat_sst = new Statistic ("sst")
        val stat_sse = new Statistic ("sse")
        val stat_rSq = new Statistic ("rSq")
        var ok = 0
        for (s <- 0 until NTESTS) {                                      // test with different random streams
            alg.setStream (s)
            alg.train ()
            val (sst, sse) = (alg.sst (x), alg.sse (x, alg.cluster))
            stat_sst.tally (sst)
            stat_sse.tally (sse)
            stat_rSq.tally (1.0 - sse / sst)
            if (opt >= 0.0 && alg.checkOpt (x, alg.cluster, opt)) ok += 1
        } // for
        if (opt != -1) println (s"ok = $ok of $NTESTS tests")
        println (Statistic.labels)
        println (stat_sst)
        println (stat_sse)
        println (stat_rSq)
    } // test

} // Clusterer object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Clusterer` trait provides a common framework for several clustering
 *  algorithms.
 *  @see `package.scala` for 'distance' function
 */
trait Clusterer
{
    private var _name: Strings = null                                // optional names for clusters

    protected var stream = 0                                         // the stream to use for random numbers

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the names for the clusters.
     *  @param nm  the array of names
     */
    def name_ (nm: Strings) { _name = nm }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the name of the 'c'-th cluster.
     *  @param c  the c-th cluster
     */
    def name (c: Int): String = 
    {
        if (_name != null && c < _name.length) _name(c) else "unknown"
    } // name

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the random stream to 's'.  Method must be called in implemeting classes
     *  before creating any random generators.
     *  @param s  the new value for the random number stream
     */
    def setStream (s: Int) { stream = s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of points/vectors, put them in clusters, returning the cluster
     *  assignments.  A basic goal is to minimize the sum of squared errors (sse)
     *  in terms of squared distances of points in the cluster to its centroid.
     */
    def train (): Clusterer

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cluster assignments.  Should only be called after 'train'.
     */
    def cluster: Array [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes (number of points within) of the clusters.  Should only
     *  be called after 'train'. 
     */
    def csize: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids (a centroid is the mean of points in a cluster).
     *  Should only be called after 'train'. 
     */
    def centroids: MatriD

    def initCentroids (): Boolean = false

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the centroids based on current assignment of points to clusters
     *  and update the 'cent' matrix that stores the centroids in its rows.
     *  @param x     the data matrix holding the points {x_i = x(i)} in its rows
     *  @param to_c  the cluster assignment array
     *  @param sz    the sizes of the clusters (number of points)
     *  @param cent  the matrix holding the centroids in its rows
     */
    def calcCentroids (x: MatriD, to_c: Array [Int], sz: VectoI, cent: MatriD)
    {
        cent.set (0.0)                                               // set cent matrix to all zeros
        for (i <- x.range1) {
            val c   = to_c(i)                                        // x_i currently assigned to cluster c
            cent(c) = cent(c) + x(i)                                 // add the next vector in cluster
        } // for
        for (c <- cent.range1) cent(c) = cent(c) / sz(c)             // divide to get averages/means
    } // calcCentroids

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector z, determine which cluster it belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Int

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the distances between vector/point 'u' and the points stored as
     *  rows in matrix 'cn'
     *  @param u    the given vector/point (u = x_i)
     *  @param cn   the matrix holding several centroids
     *  @param kc_  the number of centroids so far
     */
    def distance (u: VectoD, cn: MatriD, kc_ : Int = -1): VectoD =
    {
        val kc = if (kc_ < 0) cn.dim1 else kc_
        val du = new VectorD (kc)
        for (c <- 0 until kc) du(c) = dist (u, cn(c))
        du
    } // distance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squared errors within all clusters, where error is
     *  indicated by e.g., the distance from a point to its centroid.
     *  @param x     the data matrix holding the points
     *  @param to_c  the cluster assignments
     */
    def sse (x: MatriD, to_c: Array [Int]): Double =
    {
        val cent = centroids
        var sum  = 0.0
        for (i <- x.range1) sum += dist (x(i), cent(to_c(i)))
        sum
    } // sse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squared errors from the points in cluster 'c' to the
     *  cluster's centroid.
     *  @param x     the data matrix holding the points
     *  @param c     the current cluster
     *  @param to_c  the cluster assignments
     */
    def sse (x: MatriD, c: Int, to_c: Array [Int]): Double =
    {
        val cent = centroids
        var sum  = 0.0
        for (i <- x.range1 if to_c(i) == c) sum += dist (x(i), cent(c))
        sum
    } // sse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squares total for all the points from the mean.
     *  @param x  the data matrix holding the points
     */
    def sst (x: MatriD): Double =
    {
        val xmean = x.mean
        var sum = 0.0
        for (i <- x.range1) sum += dist (x(i), xmean)
        sum
    } // sst

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check to see if the sum of squared errors is optimum.
     *  @param x     the data matrix holding the points
     *  @param to_c  the cluster assignments
     *  @param opt   the known (from human/oracle) optimum
     */
    def checkOpt (x: MatriD, to_c: Array [Int], opt: Double): Boolean = sse (x, to_c) <= opt

} // Clusterer trait

