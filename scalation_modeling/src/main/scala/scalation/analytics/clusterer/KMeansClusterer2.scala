
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.clusterer

import scala.collection.mutable.Set
import scala.Double.PositiveInfinity
import scala.util.control.Breaks.{breakable, break}

import scalation.math.double_exp
import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.random.{PermutedVecI, Randi, RandomVecI}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer2` class cluster several vectors/points using k-means
 *  clustering.  Randomly pick 'k' points as initial centroids (secondary technique).
 *  Iteratively, reassign each point to the cluster containing the closest centroid.
 *  Stop when there are no changes to the clusters.
 *  @see `KMeansClusterer` for primary technique.
 *-----------------------------------------------------------------------------
 *  @param x      the vectors/points to be clustered stored as rows of a matrix
 *  @param k      the number of clusters to make
 *  @param flags  the flags used to adjust the algorithm
 */
class KMeansClusterer2 (x: MatriD, k: Int, flags: Array [Boolean] = Array (false, false))
      extends KMeansClusterer (x, k, flags)
{
    private val DEBUG = false                                            // debug flag

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly pick vectors/points to serve as the initial 'k' centroids 'cent'.
     *  Secondary technique for initiating the clustering.
     */
    override def initCentroids (): Boolean =
    {
        val rvi = RandomVecI (k, x.dim1-1, 0, stream = stream).igen      // random vector of integers
        for (i <- 0 until k) cent(i) = x(rvi(i))                         // set the centroids
        true                                                             // yes, they are initialized
    } // initCentroids

} // KMeansClusterer2 class

import Clusterer.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer2Test2` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.analytics.clusterer.KMeansClusterer2Test
 */
object KMeansClusterer2Test extends App
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
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClusterer2Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer2Test2` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.analytics.clusterer.KMeansClusterer2Test2
 */
object KMeansClusterer2Test2 extends App
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
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClusterer2Test2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer2Test2` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.analytics.clusterer.KMeansClusterer2Test3
 */
object KMeansClusterer2Test3 extends App
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
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))    

} // KMeansClusterer2Test3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer2Test4` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.analytics.clusterer.KMeansClusterer2Test4
 */
object KMeansClusterer2Test4 extends App
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

//  import org.apache.commons.math3.ml.clustering.KMeansPlusPlusClusterer2
//  val cl = new KMeansPlusPlusClusterer2 (k)
    
    println ("x = " + x)
    println ("k = " + k)
    println ("----------------------------------------------------")

    val tf = Array (true, false)
    for (fl0 <- tf; fl1 <- tf) {
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))

} // KMeansClusterer2Test4 object

