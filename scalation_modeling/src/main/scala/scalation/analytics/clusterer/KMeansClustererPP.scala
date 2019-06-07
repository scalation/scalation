
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Michael Cotterell, John Miller
 *  @version 1.6
 *  @date    Mon Apr 22 14:53:24 EDT 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.clusterer

import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatriD, MatrixD, VectorD, VectorI}
import scalation.plot.Plot
import scalation.random.{Discrete, PermutedVecI, Randi, RandomSeeds}
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererPP` class cluster several vectors/points using
 *  the Hartigan-Wong algorithm.
 *  @param x      the vectors/points to be clustered stored as rows of a matrix
 *  @param k      the number of clusters to make
 *  @param flags  the flags used to adjust the algorithm
 */
class KMeansClustererPP (x: MatriD, k: Int, flags: Array [Boolean] = Array (false, false))
      extends KMeansClustererHW (x, k, flags)
{
    private   val DEBUG    = false                                       // debug flag
    private   val NSTREAMS = RandomSeeds.seeds.length                    // number of random number streams available
    protected val pmf      = new VectorD (x.dim1)                        // pmf for choosing centroids

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize the centroids according to the k-means++ technique.
     */
    override def initCentroids (): Boolean =
    {
        val ranI = new Randi (0, x.dim1-1, stream)                       // uniform random integer generator
        cent(0)  = x(ranI.igen)                                          // pick first centroid uniformly at random
        for (c <- 1 until k) {                                           // pick remaining centroids
            val ranD = update_pmf (c)                                    // update distance derived pmf
            cent(c)  = x(ranD.igen)                                      // pick next centroid according to pmf
        } // for
        if (DEBUG) println (s"initCentroids: cent = $cent")
        true
    } // initCentroids

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the probability mass function (pmf) used for picking the next centroid.
     *  The farther 'x_i' is from any existing centroid, the higher its probability. 
     *  Return the corresponding distance-derived random variate generator.
     *  @param c  the current centroid index
     */
    def update_pmf (c: Int): Discrete =
    {
        for (i <- x.range1) pmf(i) = distance (x(i), cent, c).min ()     // shortest distances to any centroid
        pmf /= pmf.sum                                                   // divide by sum to get probabilities
        Discrete (pmf, stream = (stream + c) % NSTREAMS)                 // distance-derived random variate generator
    } // update_pmf

} // KMeansClustererPP class

import Clusterer.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererTestPP` object is used to test the `KMeansClustererPP` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererPPTest
 */
object KMeansClustererPPTest extends App
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
        test (x, fls, new KMeansClustererPP (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClustererPPTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererPPTest` object is used to test the `KMeansClustererPP` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererPPTest2
 */
object KMeansClustererPPTest2 extends App
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
        test (x, fls, new KMeansClustererPP (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1), null, "x0 vs x1")

} // KMeansClustererPPTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClustererPPTest3` object is used to test the `KMeansClustererPP` class.
 *  > runMain scalation.analytics.clusterer.KMeansClustererPPTest3
 */
object KMeansClustererPPTest3 extends App
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
        test (x, fls, new KMeansClustererPP (x, k, fls), opt)
    } // for

    new Plot (x.col(0), x.col(1))

} // KMeansClustererPPTest3 object

