
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.clusterer

import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.random.{PermutedVecI, Randi}
import scalation.util.{banner, Error}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer` class cluster several vectors/points using k-means
 *  clustering.  Randomly assign points to 'k' clusters (primary technique).
 *  Iteratively, reassign each point to the cluster containing
 *  the closest centroid.  Stop when there are no changes to the clusters.
 *  @see `KMeansClusterer2` for secondary technique.
 *-----------------------------------------------------------------------------
 *  @param x      the vectors/points to be clustered stored as rows of a matrix
 *  @param k      the number of clusters to make
 *  @param flags  the array of flags used to adjust the algorithm
 *                    default: no post processing, no immediate return upon change
 */
class KMeansClusterer (x: MatriD, k: Int, val flags: Array [Boolean] = Array (false, false))
      extends Clusterer with Error
{
    if (k >= x.dim1)       flaw ("constructor", "k must be less than the number of vectors")
    if (flags.length != 2) flaw ("constructor", "KMeansClusterer requires 2 flags")

    private   val DEBUG     = false                                      // debug flag
    protected val MAX_ITER  = 1000                                       // the maximum number of iterations
    protected val cent      = new MatrixD (k, x.dim2)                    // the k centroids of clusters
    protected val sz        = new VectorI (k)                            // the cluster sizes
    protected val to_c      = Array.ofDim [Int] (x.dim1)                 // assignment of vectors to clusters

    protected val (post, immediate) = (flags(0), flags(1))               // (post processing swapping, immediate return upon change)
    protected var raniv: PermutedVecI = null                             // generator of permutations

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively recompute clusters until the assignment of points does not
     *  change.  Initialize by randomly assigning points to 'k' clusters.
     */
    def train (): KMeansClusterer =
    {
        sz.set (0)                                                       // cluster sizes initialized to zero
        raniv = PermutedVecI (VectorI.range (0, x.dim1), stream)         // for randomizing index order
        assign ()                                                        // randomly assign points to clusters
        fixEmptyClusters ()                                              // move points into empty clusters
        if (! initCentroids ()) calcCentroids (x, to_c, sz, cent)        // pick points for initial centroids
        if (DEBUG) show (0)

        breakable { for (l <- 1 to MAX_ITER) {
            if (reassign ()) break                                       // reassign points to clusters (no change => break)
            calcCentroids (x, to_c, sz, cent)                            // re-calculate the centroids
            if (DEBUG) show (l)
        }} // for
        val ce = sz.indexOf (0)                                          // check for empty clusters
        if (ce != -1) throw new Exception (s"Empty cluster c = $ce")

        if (post) swap ()                                                // swap points to improve sse
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cluster assignment vector.  Should only be called after `train`.
     */
    def cluster: Array [Int] = to_c

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the centroids. Should only be called after `train`. 
     */
    def centroids: MatriD = cent

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sizes of the centroids.  Should only be called after `train`. 
     */
    def csize: VectoI = sz

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly assign each vector/point 'x(i)' to a random cluster.
     *  Primary technique for initiating the clustering.
     */
    protected def assign ()
    {
        val ran = new Randi (0, k-1, stream)                             // for random integers: 0, ..., k-1
        for (i <- x.range1) {
            to_c(i)      = ran.igen                                      // randomly assign x(i) to a cluster
            sz(to_c(i)) += 1                                             // increment size of that cluster
        } // for
    } // assign

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fix all empty clusters by taking a point from the largest cluster.
     */
    protected def fixEmptyClusters ()
    {
        for (c <- 0 until k if sz(c) < 1) {                              // for each empty cluster
            if (DEBUG) println (s"fixEmptyClusters: cluster c = $c is empty!")
            val largest = sz.argmax ()                                   // largest cluster
            val indices = to_c.indices.filter (to_c(_) == largest)       // indices of elements in largest cluster            

            val ran = new Randi (0, indices.size-1)                      // random integer generator
            val i   = indices(ran.igen)                                  // randomly pick one point from largest cluster
            sz(to_c(i)) -= 1                                             // decrement size of previous cluster
            to_c(i)      = c                                             // reassign vector x(i) to cluster c
            sz(c)       += 1                                             // increment size of cluster c
            if (DEBUG) println (s"fixEmptyClusters: to_c = ${to_c.deep}")
        } // for
    } // fixEmptyClusters

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reassign each vector/point to the cluster with the closest centroid.
     *  Indicate done, if no points changed clusters (for stopping rule).
     */
    protected def reassign (): Boolean =
    {
        var done = true                                                  // done indicates no changes
//      for (i <- raniv.igen) {                                          // randomize order of index i
        for (i <- x.range1) {                                            // standard order for index i
            val c1 = to_c(i)                                             // c1 = current cluster for point x_i
            if (sz(c1) > 1) {                                            // if size of c1 > 1
                val d  = distance (x(i), cent)                           // distances to all centroid
                val c2 = d.argmin ()                                     // c2 = cluster with closest centroid to x_i
                if (d(c2) < d(c1)) {                                     // if closest closer than current
                    sz(c1) -= 1                                          // decrement size of current cluster
                    sz(c2) += 1                                          // increment size of new cluster
                    to_c(i) = c2                                         // reassign point x_i to cluster c2
                    done    = false                                      // changed clusters => not done
                    if (immediate) return false                          // optionally return after first change
                } // if
            } // if
        } // for
        done                                                             // return whether there were no changes
    } // reassign

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap clusters for points 'x(i)' and 'x(j)'.
     *  param i  the index for point x(i)
     *  param j  the index for point x(j)
     */
    private def swapPoints (i: Int, j: Int)
    {
        val temp = to_c(i)
        to_c(i)  = to_c(j)
        to_c(j)  = temp
        calcCentroids (x, to_c, sz, cent)
    } // swapPoints

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Try all pairwise swaps and make them if 'sse' improves.
     */
    protected def swap ()
    {
        for (i <- 0 until x.dim1-1; j <- i+1 until x.dim1 if to_c(i) != to_c(j)) {
            val sum1 = sse (x, to_c(i), to_c) + sse (x, to_c(j), to_c)
            swapPoints (i, j)
            val sum2 = sse (x, to_c(i), to_c) + sse (x, to_c(j), to_c)
            if (DEBUG) println (s"sum1 = $sum1 vs. sum2 = $sum2")
            if (sum2 > sum1) swapPoints (i, j)                           // if not better, swap back
        }  // for
    } // swap

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', determine which cluster it belongs to,
     *  i.e., the cluster whose centroid it is closest to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Int = distance (z, cent).argmin ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the state of the algorithm at iteration 'l'.
     *  @param l  the current iteration
     */
    def show (l: Int) { println (s"($l) to_c = ${to_c.deep} \n($l) cent = $cent") }

} // KMeansClusterer class

import Clusterer.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererTest` object is used to test the `KMeansClusterer` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererTest
 */
object KMeansClustererTest extends App
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
         test (x, fls, new KMeansClusterer (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClustererTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererTest2` object is used to test the `KMeansClusterer` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererTest2
 */
object KMeansClustererTest2 extends App
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
         test (x, fls, new KMeansClusterer (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClustererTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererTest2` object is used to test the `KMeansClusterer` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererTest3
 */
object KMeansClustererTest3 extends App
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
         test (x, fls, new KMeansClusterer (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))    

} // KMeansClustererTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererTest4` object is used to test the `KMeansClusterer` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererTest4
 */
object KMeansClustererTest4 extends App
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
         test (x, fls, new KMeansClusterer (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))

} // KMeansClustererTest4 object

