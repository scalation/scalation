
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Apr 27 12:55:24 EDT 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.clusterer

//  U N D E R   D E V E L O P M E N T 

import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.random.Randi
import scalation.util.{banner, Error}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererSSE` class cluster several vectors/points using k-means
 *  clustering.  Randomly assign points to 'k' clusters (primary technique).
 *  Iteratively, reassign each point to the cluster containing
 *  the closest centroid.  Stop when there are no changes to the clusters.
 *  @see `KMeansClusterer2` for secondary technique.
 *-----------------------------------------------------------------------------
 *  @param x      the vectors/points to be clustered stored as rows of a matrix
 *  @param k      the number of clusters to make
 *  @param flags  the flags used to adjust the algorithm
 */
class KMeansClustererSSE (x: MatriD, k: Int, flags: Array [Boolean] = Array (false, false))
      extends Clusterer with Error
{
    if (k >= x.dim1) flaw ("constructor", "k must be less than the number of vectors")

    private   val DEBUG     = false                                      // debug flag
    private   val IMMEDIATE = true                                       // reassign returns after first change flag
    protected val MAX_ITER  = 1000                                       // the maximum number of iterations
    protected val (m, n)    = (x.dim1, x.dim2)                           // number of rows and columns

    protected val to_c = Array.ofDim [Int] (m)                           // assignment of points to clusters
    protected val clu  = Array.fill (k)(Cluster ())
    Cluster.reset ()

    protected val (post, immediate) = (flags(0), flags(1))               // (post processing swapping, immediate return upon change)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squared errors within the clusters, where error is
     *  indicated by e.g., the distance from a point to its centroid.
     *  @param x     the data matrix
     *  @param to_c  the cluster assignments
     */
    def sse (x: MatriD, clu: Array [Cluster], to_c: Array [Int]): Double =
    {
        var sum  = 0.0
        for (i <- x.range1) sum += dist (x(i), clu(to_c(i)).cen)
        sum
    } // sse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the distances between vector/point 'u' and the centroids.
     *  @param u    the given vector/point
     *  @param clu  array of clusters
     */
    private def distance (u: VectoD, clu: Array [Cluster]): VectoD =
    {
        val du = new VectorD (clu.size)
        for (c <- clu.indices) du(c) = dist (u, clu(c).cen)
        du
    } // distance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively recompute clusters until the assignment of points does not
     *  change.  Initialize by randomly assigning points to 'k' clusters.
     */
    def train (): KMeansClustererSSE =
    {
        assign ()                                                        // randomly assign points to clusters
        fixEmptyClusters ()                                              // swap points into empty clusters
        calcCentroids ()                                                 // calculate the initial centroids
        if (DEBUG) println (s"train: l = 0, clu = ${clu.deep}")

        var it = 0
        breakable { for (l <- 1 to MAX_ITER) {
            if (reassign ()) break                                       // reassign points to clusters (no change => break)
            calcCentroids ()                                             // re-calculate the centroids
            if (DEBUG) println (s"train: l = $l, clu = ${clu.deep}")
            it += 1
        }} // for
        if (DEBUG) println (s"terminated after $it iterations")
        emptyClusters ()                                                 // should not have any empty clusters

        move2 ()
        swap ()
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cluster assignment vector.  Should only be called after `train`.
     */
    def cluster: Array [Int] = to_c

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids. Should only be called after `train`. 
     */
    def centroids: MatriD = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes of the centroids.  Should only be called after `train`. 
     */
    def csize: VectoI = null

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly assign each vector/point 'x(i)' to a random cluster.
     *  Primary technique for initiating the clustering.
     */
    protected def assign ()
    {
        val ran = new Randi (0, k-1, stream)                             // for random integers: 0, ..., k-1
        for (i <- x.range1) {
            to_c(i)          = ran.igen                                  // randomly assign x(i) to a cluster
            clu(to_c(i)).np += 1                                         // increment size of that cluster
        } // for
    } // assign

    def largest: Int =
    {
        var jm = 0
        for (j <- 1 until k) if (clu(j).np > clu(jm).np) jm = j
        jm
    } // largest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fix all empty clusters by taking a point from the largest cluster.
     */
    protected def fixEmptyClusters ()
    {
        if (DEBUG) println (s"fixEmptyClusters: to_c = ${to_c.deep}")

        for (c <- 0 until k if ! (to_c contains c)) {                    // for each empty cluster
            if (DEBUG) println (s"fixEmptyClusters: cluster c = $c is empty!")
            val biggest = largest                                        // biggest cluster
            val indices = to_c.indices.filter (to_c(_) == biggest)       // indices of elements in biggest cluster            

            val ran = new Randi (0, indices.size-1)                      // random integer generator
            val i   = indices(ran.igen)                                  // randomly pick one point from biggest cluster
            clu(to_c(i)).np -= 1                                         // decrement size of previous cluster
            clu(c).np       += 1                                         // increment size of cluster c
            to_c(i)          = c                                         // reassign vector x(i) to cluster c
            if (DEBUG) println (s"fixEmptyClusters: to_c = ${to_c.deep}")
        } // for
    } // fixEmptyClusters

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check for empty clusters and throw an execption if found.
     */
    def emptyClusters ()
    {
        for (c <- 0 until k if ! (to_c contains c)) throw new Exception (s"Empty cluster c = $c")
    } // emptyClusters

    private def distance (i: Int, c1: Int, clu: Array [Cluster]): VectoD =
    {
        val d = new VectorD (k)
        val sse_c1 = clu(c1).ssef (x, to_c)
        to_c(i)    = -1
//        val del_c1 = clu(c1).ssef (x, to_c, clu(c1).cenf (x, to_c)) - sse_c1
        val del_c1 = clu(c1).ssef (x, to_c) - sse_c1
        d(c1)      = 0
        for (c <- 0 until k if c != c1) {
            val sse_c = clu(c).ssef (x, to_c)
            to_c(i)   = c
//            val del_c = clu(c).ssef (x, to_c, clu(c).cenf (x, to_c)) - sse_c
            val del_c = clu(c).ssef (x, to_c) - sse_c
            d(c)      = del_c1 + del_c
        } // for
        to_c(i) = c1
        //println (s"distance = $d")
        d
    } // distance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reassign each vector/point to the cluster with the closest centroid.
     *  Indicate done, if no points changed clusters (for stopping rule).
     */
    protected def reassign (): Boolean =
    {
        var done = true                                                  // done indicates no changes
        for (i <- x.range1) {
            val c1 = to_c(i)
            if (clu(c1).np > 1) {
                val d  = distance (i, c1, clu)                            // distances to all centroid
                val c2 = d.argmin ()                                     // u's (current, closest) cluster
                if (d(c2) < 0.0) {                                      // closest closer than current
                    clu(c1).np -= 1                                      // decrement size of current cluster
                    clu(c2).np += 1                                      // increment size of new cluster
                    to_c(i)     = c2                                     // reassign vector x(i) to cluster c2
                    done        = false                                  // changed clusters => not done
//                  if (IMMEDIATE) return false                          // return after first change
                } // if
            } // if
        } // for
        done                                                             // return whether there were no changes
    } // reassign

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the centroids based on current assignment of points to clusters.
     */
    protected def calcCentroids ()
    {
        for (j <- 0 until k) {
            val cl = clu(j)
            cl.set_cen (cl.cenf (x, to_c))
            if (DEBUG) println (s"calcCentroids ($j): cen = ${cl.cen}")
        } // for
    } // calcCentroids

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', determine which cluster it belongs to,
     *  i.e., the cluster whose centroid it is closest to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Int = distance (z, clu).argmin ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check to see if the sum of squared errors is optimum.
     *  @param opt  the known (from human/oracle) optimum
     */
    def checkOpt (opt: Double): Boolean = sse (x, clu, to_c) <= opt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap clusters for points 'x(i)' and 'x(j)'.
     *  param i  the inded for point x(i)
     *  param j  the inded for point x(j)
     */
    private def swapPoints (i: Int, j: Int)
    {
        val temp = to_c(i)
        to_c(i)  = to_c(j)
        to_c(j)  = temp
        calcCentroids ()
    } // swapPoints

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Try all pairwise swaps and make them if 'sse' improves.
     */
    protected def swap ()
    {
        for (i <- 0 until x.dim1-1; j <- i+1 until x.dim1 if to_c(i) != to_c(j)) {
            val sum1 = sse (x, clu, to_c) + sse (x, clu, to_c)
            swapPoints (i, j)
            val sum2 = sse (x, clu, to_c) + sse (x, clu, to_c)
            if (DEBUG) println (s"sum1 = $sum1 vs. sum2 = $sum2")
            if (sum2 > sum1) swapPoints (i, j)                           // if not better, swap back
        }  // for
    } // swap

    def move2 ()
    {
        for (c <- 0 until k if clu(c).np > 2) {
            var min_sse = Double.MaxValue
            var iMin, ciMin, jMin, cjMin = -1
            val idx = to_c.indices.filter (to_c(_) == c)
            if (DEBUG) println (s"move2: to_c = ${to_c.deep}")
            if (DEBUG) println (s"move2: cluster $c has ${clu(c).np} points, idx = $idx")
            for (i <- idx; j <- idx if i != j) {
                var ci_min, cj_min  = -1
                var di_min, dj_min = Double.MaxValue
                for (c2 <- 0 until k if c2 != c) {
                     val di = dist (x(i), clu(c2).cen)
                     if (di < di_min) { di_min = di; ci_min = c2 }
                     val dj = dist (x(j), clu(c2).cen)
                     if (dj < dj_min) { dj_min = dj; cj_min = c2 }
                } // for
                val sse1 = sse (x, clu, to_c)
                to_c(i) = ci_min; to_c(j) = cj_min               // check move to other clusters
                clu(c).np -= 2; clu(ci_min).np += 1; clu(cj_min).np += 1
                if (DEBUG) println (s"move2: to_c = ${to_c.deep}")
                calcCentroids ()
                val sse2 = sse (x, clu, to_c)
                if (DEBUG) println (s"sse1 = $sse1")
                if (DEBUG) println (s"sse2 = $sse2")
                if (sse2 < min_sse) { min_sse = sse2; iMin = i; ciMin = ci_min; jMin = j; cjMin = cj_min }
                to_c(i) = c; to_c(j) = c                         // move back
                clu(c).np += 2; clu(ci_min).np -= 1; clu(cj_min).np -= 1
                calcCentroids ()
            } // for
            to_c(iMin) = ciMin; to_c(jMin) = cjMin               // best move to other clusters
            clu(c).np -= 2; clu(ciMin).np += 1; clu(cjMin).np += 1
        } // for
    } // move2

} // KMeansClustererSSE class

import Clusterer.test


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererSSETest` object is used to test the `KMeansClustererSSE` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererSSETest
 */
object KMeansClustererSSETest extends App
{
    import Clusterer.x

    val k   = 3
    val opt = 3.0

    println ("x = " + x)
    println ("k = " + k)
    println ("----------------------------------------------------")

    val tf = Array (true, false)
    for (fl0 <- tf; fl1 <- tf) {
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClustererSSE (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClustererSSETest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererSSETest2` object is used to test the `KMeansClustererSSE` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererSSETest2
 */
object KMeansClustererSSETest2 extends App
{
    //                             x0    x1
    val x = new MatrixD ((8, 2),  1.0,  1.0,
                                  1.0,  3.0,
                                  5.0, 18.0,
                                  5.0, 20.0,
                                  9.0, 10.0,
                                  9.0, 12.0,
                                 15.0, 30.0,
                                 15.0, 32.0)

    val k   = 4
    val opt = 8.0

    println ("x = " + x)
    println ("k = " + k)
    println ("----------------------------------------------------")

    val tf = Array (true, false)
    for (fl0 <- tf; fl1 <- tf) {
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClustererSSE (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClustererSSETest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererSSETest2` object is used to test the `KMeansClustererSSE` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererSSETest3
 */
object KMeansClustererSSETest3 extends App
{
    import scalation.random.{Bernoulli, Normal}

    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x    = new MatrixD (50, 2)
    val k    = 4
    val opt  = 76.0

    for (i <- x.range1) x(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)

    println ("x = " + x)
    println ("k = " + k)
    println ("----------------------------------------------------")

    val tf = Array (true, false)
    for (fl0 <- tf; fl1 <- tf) {
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClustererSSE (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))    

} // KMeansClustererSSETest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererSSETest4` object is used to test the `KMeansClustererSSE` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererSSETest4
 */
object KMeansClustererSSETest4 extends App
{
    import scalation.random.{Normal, Bernoulli}

    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x    = new MatrixD (100, 2)
    val k    = 4
    val opt  = 171.0

    for (i <- x.range1) x(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)

//  import org.apache.commons.math3.ml.clustering.KMeansPlusPlusClusterer
//  val cl = new KMeansPlusPlusClusterer (k)
    
    println ("x = " + x)
    println ("k = " + k)
    println ("----------------------------------------------------")

    val tf = Array (true, false)
    for (fl0 <- tf; fl1 <- tf) {
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClustererSSE (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))

} // KMeansClustererSSETest4 object

