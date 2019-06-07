
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Michael Cotterell, John Miller, Hao Peng
 *  @version 1.6
 *  @date    Tue Mar  7 22:10:21 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see webee.technion.ac.il/people/koby/publications_all/pdfs//conf_ijcai_SlonimAC13.pdf
 *  @see cseweb.ucsd.edu/~avattani/papers/hartigan.pdf
 */

package scalation.analytics.clusterer

//  U N D E R   D E V E L O P M E N T

import scala.collection.mutable.Set
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.random.{Discrete, Randi, Uniform, PermutedVecI, RandomVecI}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Algorithm` object specifies which algorithm to use.
 */
object Algorithm extends Enumeration
{
    type Algorithm = Value
    val HARTIGAN, LLOYD = Value

} // Algorithm

import Algorithm._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClusterer` class cluster several vectors/points using 
 *  the k-means++ clustering technique.  
 *-----------------------------------------------------------------------------
 *  @see ilpubs.stanford.edu:8090/778/1/2006-13.pdf 
 *-----------------------------------------------------------------------------
 *  @param x      the vectors/points to be clustered stored as rows of a matrix
 *  @param k      the number of clusters to make
 *  @param algo   the clustering algorithm to use
 *  @param flags  the flags used to adjust the algorithm
 */
class KMeansPPClusterer (x: MatriD, k: Int, algo: Algorithm = HARTIGAN,
                         flags: Array [Boolean] = Array (false, false))
      extends KMeansClusterer (x, k, flags)
{
    private   val DEBUG   = false                               // debug flag
    protected var _k      = 0                                   // last centroid chosen 
    protected val pdf     = new VectorD (x.dim1)                // pdf for choosing centroids

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of points/vectors, put them in clusters, returning the cluster
     *  assignment vector.  A basic goal is to minimize the sum of the distances
     *  between points within each cluster.
     */
    override def train (): KMeansPPClusterer =
    {
        raniv   = PermutedVecI (VectorI.range (0, x.dim1), stream)
        initCentroids ()
        algo match {
            case LLOYD    => clusterLloyd ()
            case HARTIGAN => clusterHartigan ()
        } // match
        val ce = sz.indexOf (0)                                          // check for empty clusters
        if (ce != -1) throw new Exception (s"Empty cluster c = $ce")

        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the shortest distance from a data point 'u' to the closest 
     *  centroid we have already chosen.
     *  @param u  the vector/point
     */
    private def shortestDistance (u: VectoD): Double = 
    {
        var min = Double.PositiveInfinity
        for (i <- 0 to _k) {
            val d = dist (u, cent(i))
            if (d < min) min = d
        } // for
        min
    } // shortestDistance

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the shortest distances from each data point to its
     *  closest centroid.
     */
    private def pseudoSSE (): Double =
    {
        var sum = 0.0
        for (i <- x.range1) sum += shortestDistance (x(i))
        sum
    } // pseudoSSE

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the probability distribution for choosing the next centroid.
     */
    private def updatePDF ()
    {
        val psse = pseudoSSE ()
        for (i <- x.range1) pdf(i) = shortestDistance (x(i)) / psse
        if (DEBUG) println (s"updated pdf = $pdf")
    } // updatePDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize the centroids according to the k-means++ technique.
     */
    override def initCentroids (): Boolean =
    {
        val ran1 = new Randi (0, x.dim1-1, stream)               // uniform integer distribution 
        cent(0)  = x(ran1.igen)                                  // pick first centroid uniformly at random
        for (i <- 1 until k) {                                   // pick remaining centroids
            updatePDF ()                                         // update probability distribution
            val ran2 = Discrete (pdf, stream = (stream+i) % 1000)
            cent(i)  = x(ran2.igen)                              // pick next centroid according to pdf
            _k       = i                                         // update last centroid chosen
        } // for
        if (DEBUG) println (s"initial cent = $cent")
        true
    } // initCentroids

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reassign each vector/point to the cluster with the closest centroid.
     *  Indicate done, if no points changed clusters (for stopping rule).
     *  @param first  whether this is the first call to 'reassign'
     */
    private def reassign (first: Boolean = false): Boolean =
    {
        var done = true
        for (i <- x.range1) {
            if (first) {                                         // first call => no c0
                val cc    = classify (x(i))                      // find closest cluster for x(i)
                to_c(i) = cc                                     // assign x(i) to this cluster
                sz(cc) += 1                                      // increment the size of this cluster
                done    = false                                  // change => not done
            } else {
                val c0 = to_c(i)                                 // record current cluster assignment c0
                if (sz(c0) > 1) {                                // make sure cluster c0 has multiple points
                    val cc  = classify (x(i))
                    if (cc != c0) {                              // if closest is not the current cluster
                        sz(c0) -= 1                              // decrement the number of points in current cluster
                        to_c(i) = cc                             // reassign x(i) to the closest cluster
                        sz(cc) += 1                              // increment the size of this cluster
                        done    = false                          // change => not done
                    } // if
                } // if
            } // if
        } // for
        done                                                     // return whether there was a change during this pass
    } // reassign

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cluster the points using Lloyd's algorithm.
     */
    private def clusterLloyd (): Array [Int] =
    {
        breakable { for (l <- 1 to MAX_ITER) {
            if (reassign (l == 1)) break                         // reassign points to clusters (no change => break)
            calcCentroids (x, to_c, sz, cent)                    // re-calculate the centroids
            if (DEBUG) show (l)
        }} // for
        if (DEBUG) println (s"to_c = ${to_c.deep}")
        to_c
    } // clusterLloyd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cluster the points using a version of the Hartigan-Wong algorithm.
     *  @see www.tqmp.org/RegularArticles/vol09-1/p015/p015.pdf
     */
    def clusterHartigan (): Array [Int] =
    {
        reassign (true)
        calcCentroids (x, to_c, sz, cent)
        breakable { for (l <- 1 to MAX_ITER) {
            if (reassign2 ()) break                              // reassign points to clusters (no change => break)
            if (DEBUG) show (l)
        }} // for
        if (DEBUG) println (s"to_c = ${to_c.deep}")
        to_c
    } // clusterHartigan
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reassign each vector/point to the cluster with the closest centroid.
     *  This one follows a version of the Hartigan-Wong algorithm.
     *  Indicate done, if no points changed clusters (for stopping rule).
     *  @see www.tqmp.org/RegularArticles/vol09-1/p015/p015.pdf
     */
    private def reassign2 (): Boolean =
    {
        var done = true                                          // done indicates no changes
        val iv   = raniv.igen                                    // randomly order the points
        for (i <- iv) {
            val c0 = to_c(i)                                     // record point v's current cluster (c0)
            if (sz(c0) > 1) {                                    // make sure not to empty a cluster
                calcCentroids (x, to_c, sz, cent)                // re-calculate the centroids
                val c1 = closestByR2 (i)                         // find closest cluster to point v
                sz(c0) -= 1                                      // decrement the size of cluster c0
                to_c(i) = c1                                     // reassign point v to cluster c1
                sz(c1) += 1                                      // increment size of cluster c1
                if (c1 != c0) done = false                       // changed clusters => not done
            } // if
        } // for
        done                                                     // return whether there were no changes
    } // reassign2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the closest cluster to point 'u' according to the R2 value 
     *  described in the Hartigan-Wong algorithm.
     */
    private def closestByR2 (i: Int): Int =
    {
        val u    = x(i)
        var (cmin, min) = (0, dist (u, cent(0)))
        var r2   = 0.0
        val cc   = to_c(i)
        for (c <- 1 until k) {
            r2 = if (c == cc) (sz(c) * dist (u, cent(c))) / (sz(c) - 1)
                 else         (sz(c) * dist (u, cent(c))) / (sz(c) + 1)
            if (r2 < min) { cmin = c; min = r2 }
        } // for
        cmin
    } // closestByR2

} // KMeansPPClusterer


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClusterer` companion object supplies a factory function.
 */
object KMeansPPClusterer
{
    import scalation.random.PermutedVecI

    private var streams: VectoI = VectorI.range (0, 1000)               // vector of random number streams

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector that is a permutation of the current vector of random number streams.
     *  @param stream  the stream to use in this process
     */
    def permuteStreams (stream: Int = 0)
    {
        streams = PermutedVecI (streams, stream).igen
    } // permuteStreams

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `KMeansPPClusterer` with random retarts.  It will restart enough
     *  times to see a local minumum 'check' times.
     *  @param x      the vectors/points to be clustered stored as rows of a matrix
     *  @param k      the number of clusters to make
     *  @param algo   the clustering algorithm to use
     *  @param check  the number of times to see local minumum before stopping
     */
    def apply (x: MatriD, k: Int, algo: Algorithm = HARTIGAN, check: Int = 3): KMeansPPClusterer =
    {
        var seen   = 0
        var ssemin = Double.PositiveInfinity
        var kmpp_min: KMeansPPClusterer = null

        for (s <- streams) {
            val kmpp = new KMeansPPClusterer (x, k, algo)
            kmpp.setStream (s)
            val cls  = kmpp.train ()
            val sse  = kmpp.sse (x, kmpp.to_c)
//          println (s"sse = $sse")
            if (sse == ssemin) {
                if (seen == check) return kmpp_min
                else               seen += 1
            } // if
            if (sse < ssemin) { ssemin = sse; kmpp_min = kmpp; seen = 0 }
        } // for
        kmpp_min
    } // apply

} // KMeansPPClusterer object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClustererTester` trait includes a `test` function to aid in the
 *  testing of the `KMeansPPClusterer` class. 
 */
trait KMeansPPClustererTester
{
    import scalation.stat.Statistic
    import scalation.plot.Plot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test ... 
     *  @param x
     *  @param k
     *  @param opt
     *  @param plot
     *  @param nstreams
     */
    def test (x: MatriD, k: Int, opt: Double = -1, plot: Boolean = false, nstreams: Int = 1000)
    {
        banner (s"Testing KMeansPlusPlusCluster")
        println (s"x.dim1 = ${x.dim1}")
        println (s"x.dim2 = ${x.dim2}")
        println (s"     k = $k")
        if (plot) new Plot (x.col(0), x.col(1))
        for (algo <- Algorithm.values) {
            banner (s"test (algo = $algo)")
            val statSSE = new Statistic ("sse")
            var ok = 0
            for (s <- 0 until nstreams) {
                val cl = new KMeansPPClusterer (x, k, algo)
                cl.setStream (s)
                cl.train ()
                val sse = cl.sse (x, cl.cluster)
                // println (s"stream $s, sse = $sse")
                statSSE.tally (sse)
                if ((opt != -1) && (sse <= opt)) ok += 1
            } // for
            println (Statistic.labels)
            println (statSSE)
            println (s"min sse = ${statSSE.min}")
            if (opt != -1) println (s"optimal = $ok / $nstreams")            
        } // for 
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test ... 
     *  @param x
     *  @param k
     *  @param opt
     *  @param plot
     *  @param nstreams
     */
    def test2 (x: MatriD, k: Int, opt: Double = -1, plot: Boolean = false, nstreams: Int = 1000)
    {
        banner (s"Testing KMeansPlusPlusCluster object")
        println (s"x.dim1 = ${x.dim1}")
        println (s"x.dim2 = ${x.dim2}")
        println (s"     k = $k")
        if (plot) new Plot (x.col(0), x.col(1))
        for (algo <- Algorithm.values) {
            banner (s"test2 (algo = $algo)")
            val statSSE = new Statistic ("sse")
            var ok = 0
            for (s <- 0 until nstreams) {
                KMeansPPClusterer.permuteStreams (s)
                val cl = KMeansPPClusterer (x, k, algo)
                val sse = cl.sse (x, cl.cluster)
                // println (s"stream $s, sse = $sse")
                statSSE.tally (sse)
                if ((opt != -1) && (sse <= opt)) ok += 1
            } // for
            println (Statistic.labels)
            println (statSSE)
            println (s"min sse = ${statSSE.min}")
            if (opt != -1) println (s"optimal = $ok / $nstreams")            
        } // for 
    } // test2

} // KMeansPPClustererTester trait


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClustererTest` object is used to test the `KMeansPlusPlusClusterer`
 *  class.
 *  > runMain scalation.analytics.clusterer.KMeansPPClustererTest
 */
object KMeansPPClustererTest
       extends App with KMeansPPClustererTester
{
    import Clusterer.x

    val k   = 3
    val opt = 3.0
    test (x, k, opt)
    test2 (x, k, opt)

} // KMeansPPClustererTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClustererTest2` object is used to test the `KMeansPlusPlusClusterer`
 *  class.
 *  > runMain scalation.analytics.clusterer.KMeansPPClustererTest2
 */
object KMeansPPClustererTest2
       extends App with KMeansPPClustererTester
{
    import scalation.random.{Normal, Bernoulli}
    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x     = new MatrixD (50, 2)
    for (i <- x.range1) x(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)
    val k   = 4
    val opt = 76         // rounded up

    test (x, k, opt)
    test2 (x, k, opt)

} // KMeansPPClustererTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClustererTest3` object is used to test the `KMeansPlusPlusClusterer`
 *  class.
 *  > runMain scalation.analytics.clusterer.KMeansPPClustererTest3
 */
object KMeansPPClustererTest3
       extends App with KMeansPPClustererTester
{
    import scalation.random.{Normal, Bernoulli}
    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x     = new MatrixD (50, 3)
    for (i <- x.range1) x(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)

    val k   = 8
    val opt = 106         // rounded up

    test (x, k, opt)
    test2 (x, k, opt)

} // KMeansPPClustererTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansPPClustererTest4` object is used to test the `KMeansPlusPlusClusterer`
 *  class.
 *  > runMain scalation.analytics.clusterer.KMeansPPClustererTest4
 */
object KMeansPPClustererTest4
       extends App with KMeansPPClustererTester
{
    import scalation.random.{Normal, Bernoulli}
    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x     = new MatrixD (100, 4)
    for (i <- x.range1) x(i) = VectorD (if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen,
                                        if (coin.gen == 0) dist1.gen else dist2.gen)

    val k   = 16
    val opt = 290         // rounded up

    test (x, k, opt)
    test2 (x, k, opt)

} // KMeansPPClustererTest4 object

