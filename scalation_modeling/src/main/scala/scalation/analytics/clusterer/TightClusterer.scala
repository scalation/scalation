
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Mar 12 16:49:17 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.jstor.org/stable/3695642?seq=1#page_scan_tab_contents
 */

package scalation.analytics.clusterer

import scala.collection.mutable.{ArrayBuffer, Set}
import scala.math.min

import scalation.linalgebra.MatrixD
import scalation.random.RandomVecSample
import scalation.util.SortingI

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightClusterer` class uses tight clustering to eliminate points that
 *  do not not fit well in any cluster.
 *  @param x     the vectors/points to be clustered stored as rows of a matrix
 *  @param k0    the number of clusters to make
 *  @param kmin  the minimum number of clusters to make
 *  @param s     the random number stream (to vary the clusters made)
 */
class TightClusterer (x: MatrixD, k0: Int, kmin: Int, s: Int = 0)
//      extends Clusterer
{
    private val DEBUG = true                                  // debug flag
    private val ratio = 0.7                                   // subsampling ratio
    private val alpha = 0.5                                   // how far below 1 to set threshold
    private val thres = 1 - alpha                             // membership threshold for high scores
    private val beta  = 0.7                                   // similarity threshold
    private val b     = 10                                    // number of times to resample
    private val q     = 7                                     // number of candidates for each k
    private val n     = x.dim1                                // size of whole sample/population
    private val ns    = (x.dim1 * ratio).toInt                // size of a random subsample
    private val sr    = 0 until ns                            // sample range
    private val rsg   = RandomVecSample (x.dim1, ns, s)       // random sample generator

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a new random subsample.
     */
    def createSubsample (): (MatrixD, Array [Int]) =
    {
        val indexMap = rsg.igen ().toArray                    // select e.g., 5, 3, 7  // FIX - why toArray
        val subsamp  = x.selectRows (indexMap)                // generate random subsample
        println (s"subsamp = $subsamp")
        (subsamp, indexMap) 
    } // createSubsample

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean comembership matrix by averaging results from several subsamples.
     */
    def computeMeanComembership (k: Int): MatrixD =
    {
        val md = new MatrixD (n, n)                           // mean comembership matrix
        for (l <- 0 until b) {
            println (s"\n iteration l = $l")
            val (y, imap) = createSubsample ()                // create a new subsample
            val kmc    = new KMeansPPClusterer (y, k, s = s)  // apply clustering to this subsample
            val clustr = kmc.cluster ()                       // get the clusters
            println (s"clustr = ${clustr.deep}")

            val d = new MatrixD (n, n)                        // comembership matrix for current sample
            for (i <- sr; j <- sr if clustr(i) == clustr(j)) d(imap(i), imap(j)) = 1.0
            println (s"d = $d")
            md += d 
        } // for
        md /= ratio * b                                       // compute mean
        md                                                    // return result
    } // computeMeanComembership

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form candidate clusters by collecting points with high average comembership
     *  scores together in clusters (clubs).
     *  @param md  the mean comembership matrix
     */
    def formCandidateClusters (md: MatrixD): ArrayBuffer [Set [Int]] =
    {
        val avail = Array.fill (n)(true)                      // whether a point is available
        val clubs = new ArrayBuffer [Set [Int]] ()            // list of clubs
        for (i <- 0 until n if avail(i)) {
            val club = Set (i)                                // put i in a club
            avail(i) = false                                  // make i unavailable
            for (j <- i until n if md(i, j) >= thres) { club += j; avail(j) = false }
            clubs += club
        } // for
        clubs
    } // formCandidateClusters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order the clubs (candidate clusters) by size, returning the rank order
     *  (largest first).
     *  @param clubs  the candidate clusters
     */
    def orderBySize (clubs: ArrayBuffer [Set [Int]]): Array [Int] =
    {
        val sizes = clubs.map (_.size).toArray                // record sizes of clubs
        new SortingI (sizes).iselsort2 ()                     // indirectly sort by size
    } // orderBySize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select candidates for tight clusters in the K-means algorithm for a given
     *  number of clusters 'k'.  This corresponds to Algorithm A in the paper/URL.
     *  @param k  the number of clusters
     */
    def selectCandidateClusters (k: Int): (ArrayBuffer [Set [Int]], Array [Int]) =
    {
        val md    = computeMeanComembership (k)               // mean comembership
        val clubs = formCandidateClusters (md)                // form candidate clusters (clubs)
        val order = orderBySize (clubs)                       // determine rank order by club size
        if (DEBUG) {
            println (s"mean = $md")
            println (s"clubs = $clubs")
            println (s"order = ${order.deep}")
        } // if
        (clubs, order)
    } // selectCandidateClusters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pick the top q clubs based on club size.
     *  @param clubs  all the clubs (candidate clusters)
     *  @param order  the rank order (by club size) of all the clubs
     */
    def pickTopQ (clubs: ArrayBuffer [Set [Int]], order: Array [Int]):  ArrayBuffer [Set [Int]] =
    {
        val ml = ArrayBuffer [Set [Int]] ()
        for (i <- 0 until min (q, clubs.size)) ml += clubs(order (i))
        ml
    } // pickTopQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the similarity of two clubs as the ratio of the size of their
     *  intersection to their union.
     *  @param c1  the first club
     *  @param c2  the second club
     */
    def sim (c1: Set [Int], c2: Set [Int]): Double = (c1 & c1).size / (c1 union c2).size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find a the first tight and stable cluster from the top candidate clubs.
     *  To be stable, a club must have a similar club at the next level (next k value).
     *  @param topClubs  the top clubs for each level to be search for stable clusters
     */
    def findStable (topClubs: Array [ArrayBuffer [Set [Int]]]): (Int, Set [Int]) =
    {
        for (lev <- 0 until topClubs.length-1) {
            for (c1 <- topClubs (lev); c2 <- topClubs (lev+1)) {
                if (sim (c1, c2) >= beta) return (lev, c1)        // found a stable cluster
            } // for
        } // for
        return (-1, null)                                         // none found
    } // findStable

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of points/vectors, put them in clusters, returning the cluster
     *  assignment vector.  A basic goal is to minimize the sum of the distances
     *  between points within each cluster.
     */
    def cluster (): ArrayBuffer [Set [Int]] =
    {
        val levels   = 2                                          // number of levels to try
        val clusters = new ArrayBuffer [Set [Int]] ()
        val topClubs = Array.ofDim [ArrayBuffer [Set [Int]]] (levels)
        for (kc <- k0 to kmin by -1) {                            // iteratively decrement kc (k current value)
            for (k <- kc until kc + levels) {
                val (clubs, order) = selectCandidateClusters (k0)
                topClubs(k-kc) = pickTopQ (clubs, order)
            } // for
            println (s"topClubs = ${topClubs.deep}")
            val (lev, stable) = findStable (topClubs)             // next stable cluster
            println (s"(lev, stable) = ($lev, $stable)")
            if (lev >= 0) {
                clusters      += stable                           // add to stable clusters
                topClubs(lev) -= stable                           // remove from top clubs
            } else {
                println ("no stable cluster found for kc = $kc: $stable")
            } // if
        } // for
        println (s"clusters = $clusters")
        clusters
    } // cluster

} // TightClusterer class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightClustererTest` is used to test the `TightClusterer` class.
 *  > run-main scalation.analytics.clusterer.TightClustererTest
 */
object TightClustererTest extends App
{
    val v = new MatrixD ((6, 2), 1.0, 2.0,
                                 2.0, 1.0,
                                 5.0, 4.0,
                                 4.0, 5.0,
                                 9.0, 8.0,
                                 8.0, 9.0)

   val (k0, kmin) = (3, 2)
   for (s <- 0 until 5) {
       val tcl = new TightClusterer (v, k0, kmin, s)
       tcl.cluster ()
   } // for

} // TightClustererTest object

