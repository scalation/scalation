
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Dec 21 14:38:32 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.{Plot, PlotM}
import scalation.stat.Statistic
//import scalation.random.Bernoulli
import scalation.util.{banner, Sorting}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_Predictor` class is used to predict a response value for new vector 'z'.
 *  It works by finding its 'kappa' nearest neighbors.  These neighbors essentially
 *  vote according to their prediction.  The consensus is the average individual
 *  predictions for 'z'.  Using a distance metric, the 'kappa' vectors nearest
 *  to 'z' are found in the training data, which are stored row-wise in data
 *  matrix 'x'.  The corresponding response values are given in the vector 'y',
 *  such that the response value for vector 'x(i)' is given by 'y(i)'.
 *  @param x       the vectors/points of predictor data stored as rows of a matrix
 *  @param y       the response value for each vector in x
 *  @param fname_  the names for all features/variables
 *  @param hparam  the number of nearest neighbors to consider
 */
class KNN_Predictor (x: MatriD, y: VectoD,
                     fname_ : Strings = null, hparam: HyperParameter = KNN_Predictor.hp)
      extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG      = false                                // debug flag
    private val MAX_DOUBLE = Double.PositiveInfinity              // infinity
    private val kappa      = hparam ("kappa").toInt               // the number of nearest neighbors to consider
    private val topK       = Array.fill (kappa)(-1, MAX_DOUBLE)   // top-kappa nearest points (in reserve order)
    private val d          = Array.ofDim [Double] (x.dim1)        // array to hold distances
//  private val coin       = Bernoulli ()                         // use a fair coin for breaking ties

//  if (DEBUG) println (s" x = $x \n y = $y")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a distance metric between vectors/points 'x' and 'z'.
     *  The squared Euclidean norm used for efficiency, but may use other norms.
     *  @param x  the first vector/point
     *  @param z  the second vector/point
     */
    def distance (x: VectoD, z: VectoD): Double = (x - z).normSq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'kappa' nearest neighbors (top-'kappa') to vector 'z' and store in
     *  the 'topK' array.
     *  @param z  the vector used for prediction
     */
    private def kNearest (z: VectoD)
    {
        for (i <- x.range1) d(i) = distance (z, x(i))             // distance to all points      
        val srt = new Sorting (d)                                 // create sort object
        val top = srt.iselsort (kappa)                            // use partial indirect selsort
        for (j <- 0 until kappa) topK(j) = (top(j), d(top(j)))    // assign index and diatance
        if (DEBUG) println (s"z = $z: topK = ${topK.deep}")
    } // kNearest

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Training involves resetting the data structures before each prediction.
     *  It uses lazy training, so most of it is done during prediction.
     *  @param yy  the response values
     */
    def train (yy: VectoD = y): KNN_Predictor = this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.  Requires override to adjust
     *  degrees of freedom (df1, df2).
     *  @param xx  the data matrix used in prediction
     *  @param yy  the actual response vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): KNN_Predictor =
    {
        val yp = predict (xx)                                     // y predicted for xx (test/full)
        e = yy - yp                                               // compute residual/error vector e
        val m   = yy.dim
        val df1 = m / kappa                                       // degrees of freedom model, see ESL
        val df2 = m - df1                                         // degrees of freedom error
        resetDF ((df1, df2))
        diagnose (e, yy)                                          // compute diagnostics
        this
    } // eval

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new point/vector 'z', predict its response value based on the
     *  actual response values of its 'kappa' nearest neighbors.
     *  @param z  the vector to predict
     */
    override def predict (z: VectoD): Double =
    {
        kNearest (z)                                              // set top-kappa to kappa nearest
        var sum = 0.0
        for (i <- 0 until kappa) sum += y(topK(i)._1)             // sum the individual predictions
        sum / kappa                                               // divide to get average
    } // predict

    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("KNN_Predictor does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("KNN_Predictor does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation.
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new KNN_Predictor (x, y, fname, hparam),
                                                 xx, k, rando)
    } // crossVal

} // KNN_Predictor class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_Predictor` companion object provides a factory functions.
 */
object KNN_Predictor
{
    val hp = new HyperParameter; hp += ("kappa", 3, 3)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `KNN_Predictor` object from a combined 'xy' data-response matrix.
     *  @param xy      the combined data-response matrix
     *  @param fname   the names for all features/variables
     *  @param hparam  the number of nearest neighbors to consider
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = hp): KNN_Predictor =
    {
        val (x, y) = PredictorMat.pullResponse (xy)
        new KNN_Predictor (x, y, fname, hparam)
    } // apply

} // KNN_Predictor object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_PredictorTest` object is used to test the `KNN_Predictor` class.
 *  > runMain scalation.analytics.KNN_PredictorTest
 */
object KNN_PredictorTest extends App
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

//  val fn = Array ("x0", "x1")                   // feature/variable names
//  val knn = KNN_Predictor (xy, fn)
    val knn = KNN_Predictor (xy)

    val z1 = VectorD (10.0, 10.0)
    println ("----------------------------------------------------")
    println ("z1 = " + z1)
    println ("yp = " + knn.predict (z1))

    val z2 = VectorD ( 3.0,  3.0)
    println ("----------------------------------------------------")
    println ("z2 = " + z2)
    println ("yp = " + knn.predict (z2))

    val (x, y) = PredictorMat.pullResponse (xy)
    knn.eval (x, y)                               // no train, due to lazy/late training
    banner ("Compare y vs. yp")
    val yp = knn.predict (x)
    println ("y  = " + y)
    println ("yp = " + yp)
    println (knn.report)

    new Plot (xy.col(0), y, yp, lines = true)

} // KNN_PredictorTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_PredictorTest2` object is used to test the `KNN_Predictor` class.
 *  > runMain scalation.analytics.KNN_PredictorTest2
 */
object KNN_PredictorTest2 extends App
{
    //                            x1 x2  y
    val xy = new MatrixD ((9, 3), 0, 0, 0,
                                  0, 1, 0,
                                  0, 2, 1,
                                  1, 0, 0,
                                  1, 1, 0,
                                  1, 2, 1,
                                  2, 0, 1,
                                  2, 1, 1,
                                  2, 2, 1)

    val (x, y) = PredictorMat.pullResponse (xy)
    val t = VectorD.range (0, xy.dim1)

    val fn = Array ("x1", "x2")                   // feature/variable names

    println ("----------------------------------------------------")
    println ("xy = " + xy)
    println ("----------------------------------------------------")

    val knn = KNN_Predictor (xy, fn)

    knn.eval (x, y)                               // no train, due to lazy/late training
    banner ("Compare y vs. yp")
    val yp = knn.predict (x)
    println ("y  = " + y)
    println ("yp = " + yp)
    println (knn.report)

    // test samples -----------------------------------------------------------
    for (i <- y.range) {
        println (s"KNN ($i): predict (${x(i)}) = ${knn.predict (x(i))} =? ${y(i)}")
    } // for

    new Plot (t, y, yp, lines = true)

} // KNN_PredictorTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KNN_PredictorTest3` object is used to test the `KNN_predictor` class.
 *  > runMain scalation.analytics.KNN_PredictorTest3
 */
object KNN_PredictorTest3 extends App
{
    import ExampleAutoMPG._

    println ("----------------------------------------------------")
    println ("xy = " + xy)

    val cap = 50
    val kr  = VectorD.range (0, cap)
    val rSq = new MatrixD (cap, 3)                              // R^2, R^2 Bar,  R^2 cv

    for (k <- 2 until cap) {
        KNN_Predictor.hp("kappa") = k
        val knn = KNN_Predictor (xy)
        knn.eval ()                                             // lazy/late training in KNN
        println (knn.report)
        rSq(k) = Fit.qofVector (knn.fit, knn.crossVal ())       // use main model, knn
    } // for

    new PlotM (kr, rSq.t, lines = true)

} // KNN_PredictorTest3 object

