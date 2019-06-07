//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Dong Yu Yu, John Miller
  * @version 1.6
  * @date    Sun Dec 16 16:09:16 EST 2018
  * @see     LICENSE (MIT style license file).
  */

package scalation.analytics

import scala.collection.mutable.{ArrayBuffer, Set}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectorI}
import scalation.random.PermutedVecI
import scalation.stat.Statistic

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTree_GB` class uses Gradient Boosting on `RegressionTree`.
 *  One Tree is included in the model at a time wisely chosen for reducing gradient.
 *  @param x       the data vectors stored as rows of a matrix
 *  @param y       the response vector
 *  @param fname_  the feature/variable names
 *  @param hparam  the hyper-parameters for the model
 */
class RegressionTree_GB (x: MatriD, y: VectoD,
                         fname_ : Strings = null, hparam: HyperParameter = RegressionTree_GB.hp)
      extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG   = false                                         // debug flag
    private val depth   = hparam("maxDepth").toInt                      // the max_depth for the base (regression tree)
    private val iter    = hparam("iterations").toInt                    // the iterations for training
    private val stream  = 0                                             // the random rumber stream
    private val forest  = new ArrayBuffer [RegressionTree] ()           // forest is the esemble for regression trees
    private val permGen = PermutedVecI (VectorI.range (0, m), stream)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Using Gradient Boosting on Training, for every iteration, we evaluate the residual and form a
     *  Regression Tree where the residual is the depedent value(equal to the gradient if using SSE as loss function).
     *  @param yy  only the vector in yy will be used in training
     */
    override def train (yy: VectoD): RegressionTree_GB =
    {
        val yp = VectorD.fill (y.dim)(y.mean)                           // initial value for y-predicted

        for (i <- 0 until iter) {
            val yres = y - yp                                           // y-residual
            val tree = new RegressionTree (x, yres, fname, hparam)      // i-th tree in forest
            forest  += tree                                             // add to forest
            tree.train ()                                               // train the i-th tree
            yp += tree.predict (x)                                      // add to cumulative prediction

            if (DEBUG) {
                println (s"train: i = $i - ensembles trees")
                eval ()
                println ("fitMap = " + fitMap)
            } // if
        } // for
        this
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data vector 'z', predict the value by summing the predict for each tree.
     *  @param z  the data vector to predict
     */
    override def predict (z: VectoD): Double =
    {
        var yp = y.mean
        for (i <- forest.indices) yp += forest(i).predict (z)
        yp
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a data matrix 'z', predict the value by summing the predict for each tree,
     *  for each row of the matrix.
     *  @param z  the data matrix to predict
     */
    override def predict (z: MatriD): VectoD =
    {
        val yp = new VectorD (z.dim1)
        for (i <- z.range1) yp(i) = predict (z(i))
        yp
    }  // predict

    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("RegressionTree_GB does not have feature selection")
    } // forwardSel

    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD) =
    {
        throw new UnsupportedOperationException ("RegressionTree_GB does not have feature selection")
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation.
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new RegressionTree_GB (x, y, fname, hparam),
                                                 xx, k, rando)
    } // crossVal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  The 'crossVal' abstract method must be coded in implementing classes to
     *  call the above 'crossValidate' method.
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  flag for using randomized cross-validation
     *
    def crossVal (k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        val stats   = Array.fill (fitLabel.length) (new Statistic ())
        val indices = if (rando) permGen.igen.split (k)
                      else       VectorI (0 until m).split (k)

        for (idx <- indices) {
            val x_te = x(idx)                                        // test data matrix
            val y_te = y(idx)                                        // test response vector
            val x_tr = x.selectRowsEx (idx)                          // training data matrix
            val y_tr = y.selectEx (idx)                              // training response vector

            if (DEBUG) {
                println ("x_te = " + x_te)
                println ("y_te = " + y_te)
                println ("x_tr = " + x_tr)
                println ("y_tr = " + y_tr)
            } // if

            val model = new RegressionTree_GB (x_tr, y_tr, fname, hparam)   // construct next model using training dataset
            model.train ()                                           // train the model
            model.eval (x_te, y_te)                                  // evaluate model on test dataset
            val qm = model.fit                                       // get quality of fit measures
            for (q <- qm.indices) stats(q).tally (qm(q))             // tally these measures
        } // for

        if (DEBUG) println ("stats = " + stats.deep)
        stats
    } // crossVal
     */

} // RegressionTree_GB class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTree_GB` companion object defines hyper-parameters and provides
 *  a factory function.
 */
object RegressionTree_GB
{
    import PredictorMat.pullResponse

    val hp = new HyperParameter
    hp += ("maxDepth", 5, 5)
    hp += ("threshold", 0.1, 0.1)
    hp += ("iterations", 50, 50)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionTree_GB` object that uses Gradient Boosting on `RegressionTree`.
     *  One Tree is included in the model at a time wisely chosen for reducing gradient.
     *  @param xy       the combined data-response matrix
     *  @param fname_   the feature/variable names
     *  @param hparam   the hyper-parameters for the model
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = hp): RegressionTree_GB =
    {
        val (x, y) = pullResponse (xy)
        new RegressionTree_GB (x, y, fname, hparam) 
    } // apply

} // RegressionTree_GB object

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTree_GBTest` object is used to test the `RegressionTree_GB` class.
  *  It tests a simple case that does not require a file to be read.
  *  > runMain scalation.analytics.RegressionTree_GBTest
  */
object RegressionTree_GBTest extends App
{
    val x = new MatrixD ((5, 1), 750, 800, 850, 900, 950)
    val y = VectorD (1160, 1200, 1280, 1450, 2000)

    val rgb = new RegressionTree_GB (x, y)
    rgb.train ().eval ()
    println ("fitMap = " + rgb.fitMap)

} // RegressionTree_GBTest object

