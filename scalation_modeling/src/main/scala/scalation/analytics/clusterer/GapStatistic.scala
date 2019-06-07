
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Michael Cotterell, John Miller, Hao Peng
 *  @version 1.6
 *  @date    Thu Mar  9 15:08:30 2017
 *  @see     LICENSE (MIT style license file).
  */

package scalation.analytics.clusterer

import math.log

import scalation.linalgebra.{MatriD, MatrixD, SVD, VectoD, VectorD}
import scalation.random.RandomVecD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GapStatistic` object is used to help determine the optimal number
 *  of clusters for a clusterer by comparing results to a reference 
 *  distribution.
 *-----------------------------------------------------------------------------
 *  @see web.stanford.edu/~hastie/Papers/gap.pdf 
 */
object GapStatistic
{
    import Algorithm._

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a reference distribution based on a set of points.
     *  @param x        the vectors/points to be clustered stored as rows of a matrix
     *  @param useSVD   use SVD to account for the shape of the points (default = true)
     *  @param s        the random number stream (to vary the clusters made)
     */
    def reference (x: MatriD, useSVD: Boolean = true, stream: Int = 0): MatrixD =
    {
        var ref = new MatrixD (x.dim1, x.dim2)
        if (useSVD) {
            val mean  = x.mean
            val xzero = x - mean
            val svd   = new SVD (xzero)
            val (u, s, vt) = svd.factor123 ()
            val xp    = xzero * vt.t
            val zp    = new MatrixD (x.dim1, x.dim2)
            for (i <- zp.range2) {
                val ci = xp.col(i)
                zp.setCol (i, RandomVecD (zp.dim1, ci.max, ci.min, stream = (stream + i) % 1000).gen)
            } // for
            ref = (zp * vt) + mean
        } else {
            for (i <- ref.range2) {
                val ci = x.col(i)
                ref.setCol (i, RandomVecD (ref.dim1, ci.max, ci.min, stream = (stream + i) % 1000).gen)
            } // for
        } // if
        ref 
    } // reference

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a sum of pairwise distances between points in each cluster (in
     *  one direction). 
     *  @param x       the vectors/points to be clustered stored as rows of a matrix
     *  @param cl      the `Clusterer` use to compute the distance metric 
     *  @param clustr  the cluster assignments
     *  @param k       the number of clusters
     */
    def cumDistance (x: MatriD, cl: Clusterer, clustr: Array [Int], k: Int): VectorD =
    {
        val sums = new VectorD (k)
        for (i <- 0 until x.dim1-1; j <- i+1 until x.dim1 if clustr(i) == clustr(j)) {
            sums(clustr(j)) += dist (x(i), x(j))
        } // for
        sums
    } // cumDistance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the within sum of squared errors in terms of distances between
     *  between points within a cluster (in one direction).
     *  @param x       the vectors/points to be clustered stored as rows of a matrix
     *  @param cl      the `Clusterer` use to compute the distance metric 
     *  @param clustr  the cluster assignments
     *  @param k       the number of clusters
     */
    def withinSSE (x: MatriD, cl: Clusterer, clustr: Array [Int], k: Int): Double =
    {
        (cumDistance (x, cl, clustr, k) / cl.csize.toDouble).sum
    } // withinSSE

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `KMeansPPClusterer` clustering on the given points with
     *  an optimal number of clusters `k` chosen using the Gap statistic.
     *  @param x       the vectors/points to be clustered stored as rows of a matrix
     *  @param kMax    the upper bound on the number of clusters
     *  @param algo    the reassignment aslgorithm used by `KMeansPlusPlusClusterer`
     *  @param b       the number of reference distributions to create (default = 1)
     *  @param useSVD  use SVD to account for the shape of the points (default = true)
     *  @param plot    whether or not to plot the logs of the within-SSEs (default = false)
     */
    def kMeansPP (x: MatriD, kMax: Int, algo: Algorithm = HARTIGAN, b: Int = 1, useSVD: Boolean = true,
                  plot: Boolean = false): (KMeansPPClusterer, Array [Int], Int) =
    {
        val awk = new VectorD (kMax)
        val rwk = new VectorD (kMax)
        val gap = new VectorD (kMax)
        val kv  = VectorD.range (1, kMax+1)
        var opk = -1
//      var opcl: KMeansPlusPlusClusterer = null
//      var opcls: Array [Int] = null

        for (k <- 0 until kMax) {
            val ref = GapStatistic.reference (x, useSVD)
            val acl = KMeansPPClusterer (x,   k+1, algo)
            val rcl = KMeansPPClusterer (ref, k+1, algo)
            awk(k)  = log(GapStatistic.withinSSE (x,   acl, acl.cluster, k+1))
            rwk(k)  = log(GapStatistic.withinSSE (ref, rcl, rcl.cluster, k+1))
            gap(k)  = rwk(k) - awk(k)
            if ((k != 0) && (opk == -1) && (gap(k-1) >= gap(k) - gap(k)*0.1)) { // TODO use stddev instead of 0.01*gap
                opk = k
            } // if
        } // for

        if (plot) {
            import scalation.plot.Plot
            new Plot (kv, awk, rwk, "Actual wSSE and Reference wSSE vs. k") // , true)
            new Plot (kv, gap, null, "Gap vs. k") // , true)
        } // if

        val cl = KMeansPPClusterer (x, opk, algo)   // TODO used saved instead of reclustering
        (cl, cl.cluster, opk)
    } // kMeansPP

} // GapStatistic object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GapStatisticTest` object is used to test the `GapStatistic` object.
 *  > runMain scalation.analytics.clusterer.GapStatisticTest
 */
object GapStatisticTest extends App
{
    import Clusterer.x

    val maxK = 6

    val (cl, cls, k) = GapStatistic.kMeansPP (x, maxK, useSVD = false, plot = true)
    println (s"  k = $k")
    println (s"sse = ${cl.sse (x, cls)}")

} // GapStatisticTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GapStatisticTest2` object is used to test the `GapStatistic` object.
 *  > runMain scalation.analytics.clusterer.GapStatisticTest2
 */
object GapStatisticTest2 extends App
{
    import scalation.linalgebra.VectorD
    import scalation.random.{Normal, Bernoulli}

    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 0.1)
    val dist2 = Normal (8.0, 0.1)
    val x     = new MatrixD (50, 2)
    val maxK  = 10 

    for (i <- x.range1) x(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)

    val (cl, cls, k) = GapStatistic.kMeansPP (x, maxK, useSVD = false, plot = true)
    println (s"  k = $k")
    println (s"sse = ${cl.sse (x, cls)}")

} // GapStatisticTest2

