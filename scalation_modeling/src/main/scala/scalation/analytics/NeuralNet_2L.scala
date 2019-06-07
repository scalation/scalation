
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{FunctionV_2V, MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.PlotM
import scalation.stat.Statistic
import scalation.util.banner

import ActivationFun._
import Initializer._
import MatrixTransform._
import Optimizer._                                  // Optimizer - configuration
//import Optimizer_SGD._                            // Stochastic Gradient Descent
import Optimizer_SGDM._                             // Stochastic Gradient Descent with Momentum
import StoppingRule._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2L` class supports multi-output, 2-layer (input and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the weights/parameters 'b' connecting the layers,
 *  so that for a new input vector 'z', the net can predict the output value, i.e.,
 *  <p>
 *      yp_j = f (b dot z)
 *  <p>
 *  where 'f' is the activation function and the parameters 'b' gives the
 *  weights between input and output layers.
 *  No batching is used for this algorithm.
 *  Note, 'b0' is treated as the bias, so 'x0' must be 1.0.
 *  @param x       the m-by-nx input matrix (training data consisting of m input vectors)
 *  @param y       the m-by-ny output matrix (training data consisting of m output vectors)
 *  @param fname_  the feature/variable names (if null, use x_j's)
 *  @param hparam  the hyper-parameters for the model/network
 *  @param f1      the activation function family for layers 1->2 (input to output)
 *  @param itran   the inverse transformation function returns responses to original scale
 */
class NeuralNet_2L (x: MatriD, y: MatriD,
                    fname_ : Strings = null, hparam: HyperParameter = Optimizer.hp,
                    f1: AFF = f_sigmoid, val itran: FunctionV_2V = null)
      extends PredictorMat2 (x, y, fname_, hparam)                       // sets eta in parent class
{
    private val DEBUG     = false                                        // debug flag
    private val bSize     = hp ("bSize").toInt                           // batch size
    private val maxEpochs = hp ("maxEpochs").toInt                       // maximum number of training epochs/iterations

    private var b = new NetParam (weightMat (nx, ny))                    // parameters (weights, bias implicit) in to out

    println (s"Create a NeuralNet_2L with $nx input and $ny output nodes")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameters 'b' (weight matrix 'b.w') (array of 1).
     */
    def parameters: NetParams = Array (b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameters 'b'.
     *  Minimize the error in the prediction by adjusting the parameters 'b'.
     *  The error 'ee' is simply the difference between the target value 'y' and the
     *  predicted value 'yp'.  Minimize the dot product of error with itself using
     *  gradient-descent. specifically move in the opposite direction of the gradient.
     *  Iterate over several epochs.  It does not use batching nor a sufficient stopping rule.
     *  In practice, use the 'train' or 'train2' methods that use better optimizers.
     *  @param yy  the response/output matrix
     */
    def train0 (yy: MatriD = y): NeuralNet_2L =
    {
        println (s"train0: eta = $eta")
        var sse0 = Double.MaxValue                                      // hold prior value of sse

        for (epoch <- 1 to maxEpochs) {                                 // iterate over each epoch
            val yp = f1.fM (b * x)                                      // Yp = f(XB)
            ee     = yp - yy                                            // negative of error matrix
            val d  = f1.dM (yp) ** ee                                   // delta matrix for yy
            b     -= x.t * d * eta                                      // update 'b' parameters

            val sse = sseF (y, f1.fM (b * x))                           // recompute sum of squared errors
            if (DEBUG) println (s"train0: parameters for $epoch th epoch: b = $b, sse = $sse")
            if (sse > sse0) return this                                 // return when sse increases
            sse0 = sse                                                  // save prior sse
        } // for
        this
    } // train0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameters 'b'.
     *  Minimize the error in the prediction by adjusting the parameters 'b'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights.
     *  @param yy  the response/output matrix
     */
    def train (yy: MatriD = y): NeuralNet_2L =
    {
        val epochs = optimize2 (x, y, b, eta, bSize, maxEpochs, f1)     // optimize parameters b
        println (s"ending epoch = $epochs")
        estat.tally (epochs._2)
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameters 'b'.
     *  Minimize the error in the prediction by adjusting the parameters 'b'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights.
     *  This version preforms an interval search for the best 'eta' value.
     *  @param yy  the response/output matrix
     */
    override def train2 (yy: MatriD = y): NeuralNet_2L =
    {
        val etaI = (0.25 * eta, 4.0 * eta)                               // quarter to four times eta
        val epochs = optimize2I (x, y, b, etaI, bSize, maxEpochs, f1)    // optimize parameters b
        println (s"ending epoch = $epochs")
        estat.tally (epochs._2)
        this
    } // train2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  Quality of Fit (QoF).  May be called repeatedly.
     *  Only selects based on the first response/output column.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean = true): (Int, NetParams, VectoD) =
    {
        val ft0   =  fitA(0)
        val ir    =  if (adjusted) ft0.index_rSqBar else ft0.index_rSq   // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                                   // index of variable to add
        var b_max =  b.w                                                 // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)          // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                        // try adding x_j
            val x_cols = x.selectCols (cols_j.toArray)                   // x projected on cols
            val nn_j   = new NeuralNet_2L (x_cols, y, null, hparam, f1, itran)  // regress with x_j added
            nn_j.train ().eval ()                                        // train model, evaluate QoF
            val bb  = nn_j.parameters(0).w
            val ft  = nn_j.fitA(0).fit
            val qof = ft(ir)                                             // rSqBar/rSq
            if (DEBUG) println (s"forwardSel: cols_$j = $cols_j, qof_$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) println (s"forwardSel: add variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        (j_max, Array (NetParam (b_max)), ft_max)
    } // forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response vector 'f(z)'.
     *  @param z  the new input vector
     */
    def predictV (z: VectoD): VectoD = f1.fV (b dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix 'z', predict the output/response matrix 'f(z)'.
     *  @param z  the input matrix
     */
    def predict (z: MatriD = x): MatriD = f1.fM (b * z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: MatriD) => new NeuralNet_2L (x, y, fname, hparam, f1), k, rando)
    } // crossVal

} // NeuralNet_2L class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2L` companion object provides factory functions for buidling two-layer
 *  neural nets.
 */
object NeuralNet_2L
{
    import PredictorMat.pullResponse

    private val DEBUG = false                                       // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_2L` for a combined data matrix.
     *  @param xy       the combined input and output matrix
     *  @param fname    the feature/variable names
     *  @param hparam   the hyper-parameters
     *  @param f1       the activation function family for layers 1->2 (input to output)
     *  @param rescale  the whether to rescale matrix xy to match activation function
     */
    def apply (xy: MatriD, fname: Strings = null,
               hparam: HyperParameter = Optimizer.hp,
               f1: AFF = f_sigmoid, rescale: Boolean = true): NeuralNet_2L =
    {
        var itran: FunctionV_2V = null                              // inverse transform -> orginal scale
        val cy   = xy.dim2 - 1                                      // response column

        val xy_s =
            if (rescale) {
                if (f1.bounds != null) {                            // scale to bounds
                   val (min_xy, max_xy) = (min (xy), max (xy))
                   itran = unscaleV ((min_xy(cy), max_xy(cy)), f1.bounds) _
                   scale (xy, (min_xy, max_xy), f1.bounds)
               } else {                                             // normalize
                  val (mu_xy, sig_xy) = (xy.mean, stddev (xy))
                  itran = denormalizeV ((mu_xy(cy), sig_xy(cy))) _
                  normalize (xy, (mu_xy, sig_xy))
               } // if
           } else {                                                 // do not rescale
               xy
           } // if

        val (x, y) = pullResponse (xy_s)
        setCol2One (x)
        if (DEBUG) println ("scaled: x = " + x + "\nscaled y = " + y)
        new NeuralNet_2L (x, MatrixD (Seq (y)), fname, hparam, f1, itran)
    } // apply

} // NeuralNet_2L object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2LTest` object is used to test the `NeuralNet_2L` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.NeuralNet_2LTest
 */
object NeuralNet_2LTest extends App
{
    val s = 0                                                      // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,                   // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = new MatrixD ((3, 2), 0.5, 0.4,                         // training data - output matrix (m vectors)
                                 0.3, 0.3,
                                 0.6, 0.5)

    println ("input  matrix x = " + x)
    println ("output matrix y = " + y)

    val nn = new NeuralNet_2L (x, y)                               // create a NeuralNet_2L model

//  for (eta <- 0.5 to 10.0 by 0.5) {
    for (i <- 1 to 20) {
        val eta = i * 0.5
        banner (s"NeuralNet_2LTest: Fit the parameters using optimization with learning rate $eta")

        nn.reset (eta_ = eta)
        nn.train0 ().eval ()                                       // fit the weights using training data
        println (nn.report)

        val yp = nn.predict (x)                                    // predicted output values
        println ("target output:    y   = " + y)
        println ("predicted output: yp  = " + yp)
        println ("yp = " + nn.predict (x(0)))                      // predicted output values for row 0
    } // for

    banner ("NeuralNet_2LTest: Compare with Linear Regression - first column of y")

    val y0  = y.col(0)                                             // use first column of matrix y
    val rg0 = new Regression (x, y0)                               // create a Regression model
    rg0.train ().eval ()
    println (nn.report)

    val y0p = rg0.predict (x)                                      // predicted output value
    println ("target output:    y0  = " + y0)
    println ("predicted output: y0p = " + y0p)

    banner ("NeuralNet_2LTest: Compare with Linear Regression - second column of y")

    val y1 = y.col(1)                                              // use second column of matrix y
    val rg1 = new Regression (x, y1)                               // create a Regression model
    rg1.train ().eval ()
    println (nn.report)

    val y1p = rg1.predict (x)                                      // predicted output value
    println ("target output:    y1  = " + y1)
    println ("predicted output: y1p = " + y1p)

} // NeuralNet_2LTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2LTest2` object trains a neural netowrk on the `ExampleBasketBall` dataset.
 *  > runMain scalation.analytics.NeuralNet_2LTest2
 */
object NeuralNet_2LTest2 extends App
{
    import ExampleBasketBall._
    banner ("NeuralNet_2L vs. Regression - ExampleBasketBall")

    val f_ = f_id                                                  // try different activation functions
//  val f_ = f_sigmoid                                             // try different activation functions
//  val f_ = f_lreLU                                               // try different activation functions

    println ("ox = " + ox)
    println ("y  = " + y)

    banner ("Regression")
    val rg = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)

    banner ("prediction")                                          // not currently rescaling
    val yq = rg.predict ()                                         // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yq = " + yq)
    println ("error:            e  = " + (y - yq))

    banner ("NeuralNet_2L with scaled y values")
//  hp("eta") = 0.01                                               // try several values for train0
    hp("eta") = 0.1                                                // try several values for train
    val nn  = NeuralNet_2L (oxy, f1 = f_)                          // factory function automatically rescales
//  val nn  = new NeuralNet_2L (ox, y, f1 = f_)                    // constructor does not automatically rescale

    nn.trainSwitch (2).eval ()                                     // fit the weights using training data: 0, 1, 2
    println (nn.report)

/*
    banner ("scaled prediction")
    val yp = nn.predict ().col (0)                                 // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yp = " + yp)
    println ("error:            e  = " + (y - yp))

    banner ("unscaled prediction")
//  val (ymu, ysig) = (y.mean, sqrt (y.variance))                  // should obtain from apply - see below
//  val ypu = denormalizeV ((ymu, ysig))(yp)                       // denormalize predicted output values for all x
    val ypu = nn.itran (yp)                                        // denormalize predicted output values for all x
    println ("target output:   y   = " + y)
    println ("unscaled output: ypu = " + ypu)
*/

} // NeuralNet_2LTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2LTest3` object trains a neural netowrk on the `ExampleAutoMPG` dataset.
 *  > runMain scalation.analytics.NeuralNet_2LTest3
 */
object NeuralNet_2LTest3 extends App
{
    import ExampleAutoMPG._
    banner ("NeuralNet_2L vs. Regression - ExampleAutoMPG")

    val f_ = f_id                                                  // try different activation functions
//  val f_ = f_sigmoid                                             // try different activation functions
//  val f_ = f_lreLU                                               // try different activation functions

    banner ("Regression")
    val rg = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)

/*
    banner ("prediction")                                          // not currently rescaling
    val yq = rg.predict ()                                         // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yq = " + yq)
    println ("error:            e  = " + (y - yq))
*/

    banner ("NeuralNet_2L with scaled y values")
//  hp("eta") = 0.001                                              // try several values for train0
    hp("eta") = 0.02                                               // try several values for train
    val nn = NeuralNet_2L (oxy, f1 = f_)                           // factory function automatically rescales
//  val nn = new NeuralNet_2L (ox, y, f1 = f_)                     // constructor does not automatically rescale

    nn.trainSwitch (2).eval ()                                     // fit the weights using training data: 0, 1, 2
    println (nn.report)

/*
    banner ("scaled prediction")
    val yp = nn.predict ().col (0)                                 // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yp = " + yp)
    println ("error:            e  = " + (y - yp))

    banner ("unscaled prediction")
//  val (ymu, ysig) = (y.mean, sqrt (y.variance))                  // should obtain from apply - see below
//  val ypu = denormalizeV ((ymu, ysig))(yp)                       // denormalize predicted output values for all x
    val ypu = nn.itran (yp)                                        // denormalize predicted output values for all x
    println ("target output:   y   = " + y)
    println ("unscaled output: ypu = " + ypu)
*/

} // NeuralNet_2LTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2LTest4` object trains a neural netowrk on the `ExampleAutoMPG` dataset.
 *  It test cross-validation.
 *  > runMain scalation.analytics.NeuralNet_2LTest4
 */
object NeuralNet_2LTest4 extends App
{
    import ExampleAutoMPG._
    banner ("NeuralNet_2L cross-validation - ExampleAutoMPG")

    banner ("NeuralNet_2L with scaled y values")
//  hp("eta") = 0.001                                               // try several values - train0
    hp("eta") = 0.02                                                // try several values - train

    val nn = NeuralNet_2L (xy)                                      // factory function automatically rescales
//  val nn = new NeuralNet_2L (x, MatrixD (Seq (y)))                // constructor does not automatically rescale

    nn.trainSwitch (1).eval ()                                      // fit the weights using training data (0, 1, 2)
    println (nn.report)

    banner ("cross-validation")
    nn.crossVal ()

} // NeuralNet_2LTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2LTest5` object trains a neural network on the `ExampleAutoMPG` dataset.
 *  This tests forward feature/variable selection.
 *  > runMain scalation.analytics.NeuralNet_2LTest5
 */
object NeuralNet_2LTest5 extends App
{
    import ExampleAutoMPG._
    val n = ox.dim2                                                // number of parameters/variables
    val rSq = new MatrixD (n-1, 3)                                 // hold:  R^2, R^2 Bar, R^2 cv
    banner ("NeuralNet_2L feature selection - ExampleAutoMPG")

//  val f_ = f_id                                                  // try different activation functions
    val f_ = f_sigmoid                                             // try different activation functions
//  val f_ = f_lreLU                                               // try different activation functions

    banner ("NeuralNet_2L with scaled y values")
    hp("eta") = 0.02                                               // learning rate hyoer-parameter (see Optimizer)
    val nn  = NeuralNet_2L (oxy, f1 = f_)                          // factory function automatically rescales
//  val nn  = new NeuralNet_2L (ox, y, f1 = f_)                    // constructor does not automatically rescale

    nn.train ().eval ()                                            // fit the weights using training data
    println (nn.report)                                            // report parameters and fit
    val ft  = nn.fitA(0)                                           // fit for first output variable

    banner ("Forward Selection Test")
    val fcols = Set (0)                                            // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = nn.forwardSel (fcols)              // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")

        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                           // add variable x_j
            val x_cols = ox.selectCols (fcols.toArray)             // ox projected onto cols_j columns
            estat.reset ()
            val nn_j   = NeuralNet_2L (x_cols :^+ y, f1 = f_)      // regress with x_j added
            rSq(l-1)   = Fit.qofVector (fit_j, nn_j.crossVal ())   // use new model, nn_j

            println ("-" * 88)
            println (estat)
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                   // instance index
    new PlotM (t, rSq.t, lines = true)

} // NeuralNet_2LTest5 object

