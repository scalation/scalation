
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Dec 21 14:38:32 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package clusterer

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.{Plot, PlotM}
import scalation.stat.Statistic
import scalation.random.Bernoulli
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClusteringPredictor` class is used to predict a response value for new vector 'z'.
 *  It works by finding the cluster that the point 'z' would belong to.
 *  The recorded response value for 'y' is then given as the predicted response.
 *  The per cluster recorded reponse value is the consensus (e.g., average) of
 *  the individual predictions for 'z' from the members of the cluster.
 *  Training involves clustering the points in data matrix 'x' and then computing
 *  each clusters reponse.
 *  @param x       the vectors/points of predictor data stored as rows of a matrix
 *  @param y       the response value for each vector in x
 *  @param fname_  the names for all features/variables
 *  @param hparam  the number of nearest neighbors to consider
 */
class ClusteringPredictor (x: MatriD, y: VectoD,
                           fname_ : Strings = null, hparam: HyperParameter = ClusteringPredictor.hp)
      extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG      = false                                // debug flag
    private val MAX_DOUBLE = Double.PositiveInfinity              // infinity
    private val kappa      = hparam ("kappa").toInt               // the number of nearest neighbors to consider
    private val topK       = Array.fill (kappa)(-1, MAX_DOUBLE)   // top-kappa nearest points (in reserve order)
    private val coin       = Bernoulli ()                         // use a fair coin for breaking ties

    private val clust = new KMeansClusterer (x, kappa)            // underlying clustering algorithm
    private val yclus = new VectorD (kappa)                       // consensus response per cluster

    if (DEBUG) println (s" x = $x \n y = $y")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Training involves resetting the data structures before each prediction.
     *  It uses lazy training, so most of it is done during prediction.
     *  @param yy  the response values
     */
    def train (yy: VectoD = y): ClusteringPredictor =
    {
        clust.train ()
        val clustr = clust.cluster
        if (DEBUG) println (s"train: clusters = ${clustr.deep}")
        assignResponse (clustr)
        if (DEBUG) println (s"train: yclus = $yclus")
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign a consenus (average) response value for each cluster.
     *  @param clustr  the cluster assignments
     */
    private def assignResponse (clustr: Array [Int])
    {
        for (i <- y.range) yclus(clustr(i)) += y(i)
        yclus /= clust.csize.toDouble
    } // assignResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.  Requires override to adjust
     *  degrees of freedom (df1, df2).
     *  @param xx  the data matrix used in prediction
     *  @param yy  the actual response vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): ClusteringPredictor =
    {
        val yp = predict (xx)                                            // y predicted for xx (test/full)
        e = yy - yp                                                      // compute residual/error vector e
        val df1 = kappa                                                  // degrees of freedom model = kappa
        val df2 = yy.dim - df1                                           // degrees of freedom error
        resetDF ((df1, df2))
        diagnose (e, yy)                                                 // compute diagnostics
        this
    } // eval

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', classify it according to the cluster it
     *  belongs to.
     *  @param z  the vector to classify
     */
    def classify (z: VectoD): Int = clust.classify (z)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', predict its response value based on the
     *  average response from its clsuter.
     *  @param z  the vector to predict
     */
    override def predict (z: VectoD): Double = yclus (clust.classify (z))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize 'topK' and counters.
     */
    def reset ()
    {
        for (i <- 0 until kappa) topK(i)  = (-1, MAX_DOUBLE)     // initialize top-kappa
    } // reset

    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("ClusteringPredictor does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("ClusteringPredictor does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation.
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new ClusteringPredictor (x, y, fname, hparam),
                                                 xx, k, rando)
    } // crossVal

} // ClusteringPredictor class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClusteringPredictor` companion object provides a factory functions.
 */
object ClusteringPredictor
{
    val hp = new HyperParameter; hp += ("kappa", 3, 3)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `ClusteringPredictor` object from a combined 'xy' data-response matrix.
     *  @param xy      the combined data-response matrix
     *  @param fname   the names for all features/variables
     *  @param hparam  the number of nearest neighbors to consider
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = hp): ClusteringPredictor =
    {
        val (x, y) = PredictorMat.pullResponse (xy)
        new ClusteringPredictor (x, y, fname, hparam)
    } // apply

} // ClusteringPredictor object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClusteringPredictorTest` object is used to test the `ClusteringPredictor` class.
 *  > runMain scalation.analytics.clusterer.ClusteringPredictorTest
 */
object ClusteringPredictorTest extends App
{
    //                            x0 x1  y
    val xy = new MatrixD ((10, 3), 1, 5, 1,       // joint data matrix
                                   2, 4, 1,
                                   3, 4, 1,
                                   4, 4, 1,
                                   5, 3, 0,
                                   6, 3, 1,
                                   7, 2, 0,
                                   8, 2, 0,
                                   9, 1, 0,
                                  10, 1, 0)

    println ("----------------------------------------------------")
    println ("xy = " + xy)

//  val fn = Array ("x1", "x2")                   // feature/variable names
//  val cp = ClusteringPredictor (xy, fn)
    val cp = ClusteringPredictor (xy)
    cp.train ().eval ()

    val z1 = VectorD (10.0, 10.0)
    println ("----------------------------------------------------")
    println ("z1 = " + z1)
    println ("yp = " + cp.predict (z1))

    val z2 = VectorD ( 3.0,  3.0)
    println ("----------------------------------------------------")
    println ("z2 = " + z2)
    println ("yp = " + cp.predict (z2))

    val (x, y) = PredictorMat.pullResponse (xy)
    val yp = cp.predict (x)
    println ("y  = " + y)
    println ("yp = " + yp)
    println (cp.report)

    new Plot (xy.col(0), y, yp, lines = true)

} // ClusteringPredictorTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClusteringPredictorTest2` object is used to test the `ClusteringPredictor` class.
 *  > runMain scalation.analytics.clusterer.ClusteringPredictorTest2
 */
object ClusteringPredictorTest2 extends App
{
    //                            x0 x1  y
    val xy = new MatrixD ((9, 3), 0, 0, 0,
                                  0, 1, 0,
                                  0, 2, 1,
                                  1, 0, 0,
                                  1, 1, 0,
                                  1, 2, 1,
                                  2, 0, 1,
                                  2, 1, 1,
                                  2, 2, 1)
    val x = xy.sliceCol (0, 2)
    val y = xy.col (2)

    val fn = Array ("x1", "x2")                   // feature/variable names

    println ("----------------------------------------------------")
    println ("xy = " + xy)
    println ("----------------------------------------------------")
    println ("x = " + x)

    val cp = ClusteringPredictor (xy, fn)
    cp.train ()

    // test samples -----------------------------------------------------------
    for (i <- y.range) {
        println (s"ClusteringPredictor ($i): predict (${x(i)}) = ${cp.predict (x(i))} =? ${y(i)}")
    } // for

} // ClusteringPredictorTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ClusteringPredictorTest3` object is used to test the `ClusteringPredictor` class.
 *  Test on AutoMPG dataset and compare with `KNN_Predictor`.
 *  > runMain scalation.analytics.clusterer.ClusteringPredictorTest3
 */
object ClusteringPredictorTest3 extends App
{
    import ExampleAutoMPG._
    import scalation.plot.Plot

    println ("----------------------------------------------------")
    println ("xy = " + xy)

    val cap = 50
    val kr  = VectorD.range (0, cap)

    banner ("Test ClusteringPredictor on AutoMPG")
    val rSq = new MatrixD (cap, 3)                              // R^2, R^2 Bar,  R^2 cv

    for (k <- 2 until cap) {
        ClusteringPredictor.hp("kappa") = k
        val cp = ClusteringPredictor (xy)
        cp.train ().eval ()
        println (cp.report)

        val result = cp.crossVal ()                             // cross-validation result
        val cv     = result(cp.index_rSq).mean                  // mean for R^2
        rSq(k)     = VectorD (100 * cp.fit(cp.index_rSq),       // R^2 percentage
                              100 * cp.fit(cp.index_rSqBar),    // R^2 Bar percentage
                              100 * cv)                         // R^2 cv percentage
    } // for

    new PlotM (kr, rSq.t, null, "ClusteringPredictor", lines = true)

    banner ("Test ClusteringPredictor on AutoMPG")
    val rSQ = new MatrixD (cap, 3)                              // R^2, R^2 Bar,  R^2 cv

    for (k <- 2 until cap) {
        KNN_Predictor.hp("kappa") = k
        val knn = KNN_Predictor (xy)
        knn.eval ()                                             // lazy/late training in KNN
        println (knn.report)

        val result = knn.crossVal ()                            // cross-validation result
        val cv     = result(knn.index_rSq).mean                 // mean for R^2
        rSQ(k)     = VectorD (100 * knn.fit(knn.index_rSq),     // R^2 percentage
                              100 * knn.fit(knn.index_rSqBar),  // R^2 Bar percentage
                              100 * cv)                         // R^2 cv percentage
    } // for

    new PlotM (kr, rSQ.t, null, "KNN_Predictor", lines = true)


} // ClusteringPredictorTest3 object

