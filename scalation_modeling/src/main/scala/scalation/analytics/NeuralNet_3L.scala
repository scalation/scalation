
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
/** The `NeuralNet_3L` class supports multi-output, 3-layer (input, hidden and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the parameters 'a' and 'b' connecting the layers,
 *  so that for a new input vector 'v', the net can predict the output value, i.e.,
 *  <p>
 *      yp = f1 (b * f0 (a * v))
 *  <p>
 *  where 'f0' and 'f1' are the activation functions and the parameter 'a' and 'b'
 *  are the parameters between input-hidden and hidden-output layers.
 *  Unlike `NeuralNet_2L` which adds input 'x0 = 1' to account for the intercept/bias,
 *  `NeuralNet_3L` explicitly adds bias.
 *  @param x       the m-by-nx input matrix (training data consisting of m input vectors)
 *  @param y       the m-by-ny output matrix (training data consisting of m output vectors)
 *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
 *  @param fname_  the feature/variable names (if null, use x_j's)
 *  @param hparam  the hyper-parameters for the model/network
 *  @param f0      the activation function family for layers 1->2 (input to hidden)
 *  @param f1      the activation function family for layers 2->3 (hidden to output)
 *  @param itran   the inverse transformation function returns responses to original scale
 */
class NeuralNet_3L (x: MatriD, y: MatriD,
                    private var nz: Int = -1,
                    fname_ : Strings = null, hparam: HyperParameter = hp,
                    f0: AFF = f_sigmoid, f1: AFF = f_lreLU,
                    val itran: FunctionV_2V = null)
      extends PredictorMat2 (x, y, fname_, hparam)                        // sets eta in parent class
{
    private val DEBUG     = false                                         // debug flag
    private val bSize     = hp ("bSize").toInt                            // mini-batch size
    private val maxEpochs = hp ("maxEpochs").toInt                        // maximum number of training epochs/iterations

    // Guidelines for setting the number of nodes in hidden layer, e.g.,
    // [1] nx - 1 or [2] (nx + ny) / 2
//  if (nz < 1) nz = nx - 1                                               // [1] default number of nodes for hidden layer
    if (nz < 1) nz = (nx + ny) / 2                                        // [2] default number of nodes for hidden layer
    val df_m = compute_df_m (nz)                                          // degrees of freedom for model (first output only)
    resetDF (df_m, x.dim1 - df_m)                                         // degrees of freedom for (model, error)

    private var a = new NetParam (weightMat (nx, nz), new VectorD (nz))   // parameters (weights & biases) in to hid
    private var b = new NetParam (weightMat (nz, ny), new VectorD (ny))   // parameters (weights & biases) hid to out

    println (s"Create a NeuralNet_3L with $nx input, $nz hidden and $ny output nodes: df_m = $df_m")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the degrees of freedom for the model (based on nx, n, ny = 1).
     *  Rough extimate based on total number of parameters - 1.
     *  FIX: use better estimate
     *  @param n  the number of nodes in the hidden layer
     */
    def compute_df_m (n: Int): Int = (nx + 2) * n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameters 'a' and 'b'.
     */
    def parameters: NetParams = Array (a, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parametera 'a' and 'b'.
     *  This is a simple algorithm that iterates over several epochs using gradient descent.  
     *  It does not use batching nor a sufficient stopping rule.
     *  In practice, use the 'train' or 'train2' methods that use better optimizers.
     *  @param yy  the response/output matrix
     */
    def train0 (yy: MatriD = y): NeuralNet_3L =
    {
        println (s"train0: eta = $eta")
        var sse0 = Double.MaxValue                                        // hold prior value of sse

        for (epoch <- 1 to maxEpochs) {                                   // iterate over each epoch
            var z  = f0.fM (a * x)                                        // Z  = f(XA)
            var yp = f1.fM (b * z)                                        // Yp = f(ZB)
            ee     = yp - yy                                              // negative of the error matrix
            val d1 = f1.dM (yp) ** ee                                     // delta matrix for yy
            val d0 = f0.dM (z) ** (d1 * b.w.t)                            // delta matrix for z
            a -= (x.t * d0 * eta, d0.mean * eta)                          // update 'a' weights & biases
            b -= (z.t * d1 * eta, d1.mean * eta)                          // update 'b' weights & biases

            val sse = sseF (y, b * f1.fM (f0.fM (a * x)))
            if (DEBUG) println (s"train0: parameters for $epoch th epoch: b = $b, sse = $sse")
            if (sse > sse0) return this                                   // return early if moving up
            sse0 = sse                                                    // save prior sse
            this
        } // for
        this
    } // train0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameters 'a' and 'b'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights. 
     *  @param yy  the response/output matrix
     */
    def train (yy: MatriD = y): NeuralNet_3L =
    {
        val epochs = optimize3 (x, y, a, b, eta, bSize, maxEpochs, f0, f1)     // optimize parameters a, b
        println (s"ending epoch = $epochs")
        estat.tally (epochs._2)
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter 'a' and 'b'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights. 
     *  This version preforms an interval search for the best 'eta' value. 
     *  @param yy  the response/output matrix
     */
    override def train2 (yy: MatriD = y): NeuralNet_3L =
    {
        val etaI = (0.25 * eta, 4.0 * eta)                                     // quarter to four times eta
        val epochs = optimize3I (x, y, a, b, etaI, bSize, maxEpochs, f0, f1)   // optimize parameters a, b
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
        val ir    =  if (adjusted) ft0.index_rSqBar else ft0.index_rSq     // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                                     // index of variable to add
        var b_max: NetParams = Array (a, b)                                // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)            // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                          // try adding x_j
            val x_cols = x.selectCols (cols_j.toArray)                     // x projected on cols
            val nn_j   = new NeuralNet_3L (x_cols, y, -1, null, hparam, f0, f1, itran)  // regress with x_j added
            nn_j.train ().eval ()                                          // train model, evaluate QoF
            val ab  = nn_j.parameters
            val ft  = nn_j.fitA(0).fit
            val qof = ft(ir)                                               // rSqBar/rSq
            if (DEBUG) println (s"forwardSel: cols_$j = $cols_j, qof_$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = ab; ft_max = ft }
        } // for
        if (DEBUG) println (s"forwardSel: add variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        (j_max, b_max, ft_max)
    } // forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'v', predict the output/response vector 'f(v)'.
     *  @param v  the new input vector
     */
    def predictV (v: VectoD): VectoD = f1.fV (b dot f0.fV (a dot v))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix 'v', predict the output/response matrix 'f(v)'.
     *  @param v  the input matrix
     */
    def predict (v: MatriD = x): MatriD = f1.fM (b * f0.fM (a * v))

    def showEstat ()
    {
        println (Statistic.labels)
        println (estat)
    } // showEstat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: MatriD) => new NeuralNet_3L (x, y, nz, fname, hparam, f0, f1),
                                                 k, rando) 
    } // crossVal

} // NeuralNet_3L class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3L` companion object provides factory functions for buidling three-layer
 *  (one hidden layer) neural nets.
 */
object NeuralNet_3L
{
    import PredictorMat.pullResponse

    private val DEBUG = false                                          // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_3L` for a combined data matrix.
     *  @param xy       the combined input and output matrix
     *  @param nz       the number of nodes in hidden layer
     *  @param fname    the feature/variable names
     *  @param hparam   the hyper-parameters
     *  @param f0       the activation function family for layers 1->2 (input to output)
     *  @param f1       the activation function family for layers 2->3 (hidden to output)
     *  @param rescale  the whether to rescale matrix xy to match activation function
     */
    def apply (xy: MatriD, fname: Strings = null,
               nz: Int = -1,
               hparam: HyperParameter = hp,
               f0: AFF = f_sigmoid, f1: AFF = f_lreLU,
               rescale: Boolean = true): NeuralNet_3L =
    {
        var itran: FunctionV_2V = null                                 // inverse transform -> orginal scale
        val cy   = xy.dim2 - 1                                         // response column

        val xy_s =
            if (rescale) {
                if (f0.bounds != null) {                               // scale to bounds
                   val (min_xy, max_xy) = (min (xy), max (xy))
                   itran = unscaleV ((min_xy(cy), max_xy(cy)), f0.bounds) _
                   scale (xy, (min_xy, max_xy), f0.bounds)
               } else {                                                // normalize
                  val (mu_xy, sig_xy) = (xy.mean, stddev (xy))
                  itran = denormalizeV ((mu_xy(cy), sig_xy(cy))) _
                  normalize (xy, (mu_xy, sig_xy))
               } // if
           } else {                                                    // do not rescale
               xy
           } // if

        val (x, y) = pullResponse (xy_s)
//      setCol2One (x)
        if (DEBUG) println ("scaled: x = " + x + "\nscaled y = " + y)
        new NeuralNet_3L (x, MatrixD (Seq (y)), nz, fname, hparam, f0, f1, itran)
    } // apply

} // NeuralNet_3L object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3LTest` object is used to test the `NeuralNet_3L` class.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.NeuralNet_3LTest
 */
object NeuralNet_3LTest extends App
{
    val s = 0                                                       // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,                    // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = new MatrixD ((3, 2), 0.5, 0.4,                          // training data - output matrix (m vectors)
                                 0.3, 0.3,
                                 0.6, 0.5)

    println ("input  matrix x = " + x)
    println ("output matrix y = " + y)

    hp("bSize") = 1
    val nn = new NeuralNet_3L (x.sliceCol (1, 3), y, 3)             // create a NeuralNet_3L

    for (i <- 1 to 20) {
        val eta = i * 0.5
        banner (s"NeuralNet_3LTest: Fit the parameters a & b using optimization with learning rate $eta")

        nn.reset (eta_ = eta)
        nn.train ().eval ()                                         // fit the weights using training data
        println (nn.report)

//      yp = nn.predict (x)                                         // predicted output values
//      println ("target output:    y   = " + y)
//      println ("predicted output: yp  = " + yp)
        println ("yp0 = " + nn.predict (x(0)))                      // predicted output values for row 0
    } // for

    banner ("NeuralNet_3LTest: Compare with Linear Regression - first column of y")

    val y0  = y.col(0)                                              // use first column of matrix y
    val rg0 = new Regression (x, y0)                                // create a Regression model
    rg0.train ().eval ()
    println (rg0.report)

    val y0p = rg0.predict (x)                                       // predicted output value
    println ("target output:    y0  = " + y0)
    println ("predicted output: y0p = " + y0p)

    banner ("NeuralNet_3LTest: Compare with Linear Regression - second column of y")

    val y1 = y.col(1)                                               // use second column of matrix y
    val rg1 = new Regression (x, y1)                                // create a Regression model
    rg1.train ().eval ()
    println (rg1.report)

    val y1p = rg1.predict (x)                                       // predicted output value
    println ("target output:    y1  = " + y1)
    println ("predicted output: y1p = " + y1p)

} // NeuralNet_3LTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3LTest2` object trains a neural netowrk on the `ExampleBasketBall` dataset.
 *  > runMain scalation.analytics.NeuralNet_3LTest2
 */
object NeuralNet_3LTest2 extends App
{
    import ExampleBasketBall._
    banner ("NeuralNet_3L vs. Regession - ExampleBasketBall")

    println ("x = " + x)
    println ("y = " + y)

    banner ("Regression")
    val rg = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)

    banner ("prediction")                                           // not currently rescaling
    val yq = rg.predict ()                                          // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yq = " + yq)
    println ("error:            e  = " + (y - yq))

    banner ("NeuralNet_3L with scaled y values")
//  hp("eta") = 0.016                                               // try several values - train0
    hp("eta") = 0.1                                                 // try several values - train

    val nn = NeuralNet_3L (xy)                                      // factory function automatically rescales
//  val nn = new NeuralNet_3L (x, MatrixD (Seq (y)))                // constructor does not automatically rescale

    nn.trainSwitch (2).eval ()                                      // fit the weights using training data
    println (nn.report)

    banner ("scaled prediction")
    val yp = nn.predict ().col (0)                                  // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yp = " + yp)
    println ("error:            e  = " + (y - yp))

/*
    banner ("unscaled prediction")
//  val (ymu, ysig) = (y.mean, sqrt (y.variance))                   // should obtain from apply - see below
//  val ypu = denormalizeV ((ymu, ysig))(yp)                        // denormalize predicted output values for all x
    val ypu = nn.itran (yp)                                         // denormalize predicted output values for all x
    println ("target output:   y   = " + y)
    println ("unscaled output: ypu = " + ypu)
*/

} // NeuralNet_3LTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3LTest3` object trains a neural netowrk on the `ExampleAutoMPG` dataset.
 *  > runMain scalation.analytics.NeuralNet_3LTest3
 */
object NeuralNet_3LTest3 extends App
{
    import ExampleAutoMPG._
    banner ("NeuralNet_3L vs. Regession - ExampleAutoMPG")

    banner ("Regression")
    val rg = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)

/*
    banner ("prediction")                                           // not currently rescaling
    val yq = rg.predict ()                                          // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yq = " + yq)
    println ("error:            e  = " + (y - yq))
*/

    banner ("NeuralNet_3L with scaled y values")
//  hp("eta") = 0.0014                                              // try several values - train0
    hp("eta") = 0.3                                                 // try several values - train

    val nn = NeuralNet_3L (xy)                                      // factory function automatically rescales
//  val nn = new NeuralNet_3L (x, MatrixD (Seq (y)))                // constructor does not automatically rescale

    nn.trainSwitch (2).eval ()                                      // fit the weights using training data (0, 1, 2)
    println (nn.report)

/*
    banner ("scaled prediction")
    val yp = nn.predict ().col (0)                                  // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yp = " + yp)
    println ("error:            e  = " + (y - yp))

    banner ("unscaled prediction")
//  val (ymu, ysig) = (y.mean, sqrt (y.variance))                   // should obtain from apply - see below
//  val ypu = denormalizeV ((ymu, ysig))(yp)                        // denormalize predicted output values for all x
    val ypu = nn.itran (yp)                                         // denormalize predicted output values for all x
    println ("target output:   y   = " + y)
    println ("unscaled output: ypu = " + ypu)
*/

} // NeuralNet_3LTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3LTest4` object trains a neural netowrk on the `ExampleAutoMPG` dataset.
 *  It test cross-validation.
 *  > runMain scalation.analytics.NeuralNet_3LTest4
 */
object NeuralNet_3LTest4 extends App
{
    import ExampleAutoMPG._
    banner ("NeuralNet_3L cross-validation - ExampleAutoMPG")

    banner ("NeuralNet_3L with scaled y values")
//  hp("eta") = 0.0014                                              // try several values - train0
    hp("eta") = 0.3                                                 // try several values - train

    val nn = NeuralNet_3L (xy)                                      // factory function automatically rescales
//  val nn = new NeuralNet_3L (x, MatrixD (Seq (y)))                // constructor does not automatically rescale

    nn.trainSwitch (1).eval ()                                      // fit the weights using training data (0, 1, 2)
    println (nn.report)

    banner ("cross-validation")
    nn.crossVal ()

} // NeuralNet_3LTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3LTest5` object trains a neural network on the `ExampleAutoMPG` dataset.
 *  This tests forward feature/variable selection.
 *  > runMain scalation.analytics.NeuralNet_3LTest5
 */
object NeuralNet_3LTest5 extends App
{
    import ExampleAutoMPG._
    val n = x.dim2                                                  // number of parameters/variables
    val rSq = new MatrixD (n - 1, 3)                                // hold: R^2, R^2 Bar,  R^2 cv
    banner ("NeuralNet_3L feature selection - ExampleAutoMPG")

    banner ("NeuralNet_3L with scaled y values")
    hp("eta") = 0.3
    val nn = NeuralNet_3L (xy)                                      // factory function automatically rescales
//  val nn = new NeuralNet_3L (x, y)                                // constructor does not automatically rescale

    nn.train ().eval ()                                             // fit the weights using training data
    println (nn.report)                                             // parameters and quality of fit
    val ft = nn.fitA(0)                                             // quality of fit for first output

    banner ("Forward Selection Test")
    val fcols = Set (0)                                             // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = nn.forwardSel (fcols)               // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")

        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                            // add variable x_j
            val x_cols = x.selectCols (fcols.toArray)               // x projected onto cols_j columns
            estat.reset ()
            val nn_j   = NeuralNet_3L (x_cols :^+ y)                // regress with x_j added
            rSq(l-1)   = Fit.qofVector (fit_j, nn_j.crossVal ())    // use new model, nn_j

            println ("-" * 88)
            println (estat)
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                    // instance index
    new PlotM (t, rSq.t, lines = true)

} // NeuralNet_3LTest5 object

