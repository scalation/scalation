
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Sep  9 13:30:41 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.sqrt

import scalation.linalgebra.{FunctionV_2V, MatriD, MatrixD, VectoD, VectorD}
import scalation.plot.PlotM
import scalation.stat.Statistic
import scalation.util.banner

import ActivationFun._
import Initializer._
import MatrixTransform._
import Optimizer._                                  // Optimizer - configuration
import Optimizer_SGD._                              // Stochastic Gradient Descent
//import Optimizer_SGDM._                               // Stochastic Gradient Descent with Momentum
import StoppingRule._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Perceptron` class supports single-output, 2-layer (input and output)
 *  Neural-Networks.  Although perceptrons are typically used for classification,
 *  this class is used for prediction.  Given several input vectors and output
 *  values (training data), fit the weights/parameters 'b' connecting the layers,
 *  so that for a new input vector 'z', the net can predict the output value, i.e.,
 *  <p>
 *      z = f (b dot z)
 *  <p>
 *  The parameter vector 'b' (w) gives the weights between input and output layers.
 *  Note, 'b0' is treated as the bias, so 'x0' must be 1.0.
 *  @param x       the data/input m-by-n matrix (data consisting of m input vectors)
 *  @param y       the response/output m-vector (data consisting of m output values)
 *  @param fname_  the feature/variable names
 *  @param hparam  the hyper-parameters for the model/network
 *  @param f1      the activation function family for layers 1->2 (input to output)
 *  @param itran   the inverse transformation function returns responses to original scale
 */
class Perceptron (x: MatriD, y: VectoD,
                  fname_ : Strings = null, hparam: HyperParameter = Optimizer.hp,
                  f1: AFF = f_sigmoid, val itran: FunctionV_2V = null)
      extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG     = false                                   // debug flag
    private var eta       = hparam ("eta")                          // the learning/convergence rate (requires adjustment)
    private var bSize     = hparam ("bSize").toInt                  // the batch size
    private val maxEpochs = hparam ("maxEpochs").toInt              // the maximum number of training epcochs/iterations
    private val _1        = VectorD.one (m)                         // vector of all ones

    if (y.dim != m) flaw ("constructor", "dimensions of x and y are incompatible")

    println (s"Create a Perceptron with $n input nodes and 1 output node")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial parameter/weight vector 'b' manually before training.
     *  This is mainly for testing purposes.
     *  @param w0  the initial weights for b
     */
    def setWeights (w0: VectoD)
    {
        b = w0
        if (DEBUG) {
            eval ()
            println (report)
        } // if
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the learning rate 'eta'.
     *  @param eta  the learning rate
     */
    def reset (eta_ : Double) { eta = eta_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     *  Minimize the error in the prediction by adjusting the weight vector 'b'.
     *  The error 'e' is simply the difference between the target value 'yy' and the
     *  predicted value 'yp'.  Minimize the dot product of error with itself using
     *  gradient-descent (move in the opposite direction of the gradient).
     *  Iterate over several epochs (no batching).
     *  Use val d  = yp * (_1 - yp) * e                            // delta y (for sigmoid only)
     *  @param yy  the response/output vector
     */
    def train0 (yy: VectoD = y): Perceptron =
    {
        println (s"train0: eta = $eta")
        if (b == null) b = weightVec (n)                            // initialize parameters/weights
        var sse0 = Double.MaxValue

        for (epoch <- 1 until maxEpochs) {                          // epoch-th learning phase
            val yp = f1.fV (x * b)                                  // predicted output vector yp = f(Xb)
            e      = yy - yp                                        // error vector for y
            val d  = -f1.dV (yp) * e                                // delta vector for y
            b     -= x.t * d * eta                                  // update the parameters/weights

            val sse = sseF (y, f1.fV (x * b))                       // recompute sum of squared errors
            if (DEBUG) println (s"train0: parameters for $epoch th epoch: b = $b, sse = $sse")
            if (sse >= sse0) return this                            // return when sse increases
            sse0 = sse                                              // save prior sse
        } // for
        this
    } // train0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     *  Minimize the error in the prediction by adjusting the weight vector 'b'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights.
     *  @param yy  the response/output vector
     */
    def train (yy: VectoD = y): Perceptron =
    {
        if (yy.dim < 2 * bSize) flaw ("train", "not enough data for batching - use 'train0'")
        println (s"train: eta = $eta")
        if (b == null) b = weightVec (n)                            // initialize parameters/weights
        val result = optimize (x, y, b, eta, bSize, maxEpochs, f1)
        println (s"result = (sse, ending_epoch_) = $result")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     *  Minimize the error in the prediction by adjusting the weight vector 'b'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights.
     *  This version preforms an interval search for the best 'eta' value.
     *  @param yy  the response/output vector
     */
    override def train2 (yy: VectoD = y): Perceptron =
    {
        if (yy.dim < 2 * bSize) flaw ("train2", "not enough data for batching - use 'train0'")
        val etaI = (0.25 * eta, 4.0 * eta)                          // quarter to four times eta
        println (s"train2: etaI = $etaI")
        if (b == null) b = weightVec (n)                            // initialize parameters/weights
        val result = optimizeI (x, y, b, etaI, bSize, maxEpochs, f1)
        println (s"result = (sse, ending_epoch_) = $result")
        this
    } // train2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Switch between 'train' methods: simple (0), regular (1) and hyper-parameter
     *  optimizing (2).
     *  @param which  the kind of 'train' method to use
     *  @param yy     the vector of responses/outputs
     */
    def trainSwitch (which: Int, yy: VectoD = y): Perceptron =
    {
        which match {
        case 0 => train0 (yy)
        case 1 => train (yy)
        case 2 => train2 (yy)
        case _ => flaw ("trainSwitch", s"which = $which not in (0, 1, 2)"); null
        } // match
    } // trainSwitch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response value 'f(z)'.
     *  @param z  the new input vector
     */
    override def predict (z: VectoD): Double = f1.f (b dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input matrix 'z', predict the output/response value 'f(z)'.
     *  @param z  the new input matrix
     */
    override def predict (z: MatriD = x): VectoD = f1.fV (z* b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean = true): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq      // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                              // index of variable to add
        var b_max =  b                                              // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)     // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                   // try adding x_j
            val x_cols = x.selectCols (cols_j.toArray)              // x projected on cols
            val nn_j   = new Perceptron (x_cols, y, null, hparam, f1, itran)  // regress with x_j added
            nn_j.train ().eval ()                                   // train model, evaluate QoF
            val bb  = nn_j.parameter
            val ft  = nn_j.fit
            val qof = ft(ir)                                        // rSqBar/rSq
            if (DEBUG) println (s"forwardSel: cols_$j = $cols_j, qof_$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) println (s"forwardSel: add variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        (j_max, b_max, ft_max)
    } // forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable from
     *  the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     *  @param first     first variable to consider for elimination
     *                       (default (1) assume intercept x_0 will be in any model)
     */
    def backwardElim (cols: Set [Int], adjusted: Boolean = true,
                    first: Int = 1): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq      // fit(ir) is rSqBar/rSq
        var j_max = -1                                              // index of variable to eliminate
        var b_max =  b                                              // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)     // optimize on quality of fit

        for (j <- first until x.dim2 if cols contains j) {
            val cols_j = cols - j                                   // try removing x_j
            val x_cols = x.selectCols (cols_j.toArray)              // x projected on cols
            val nn_j   = new Perceptron (x_cols, y, null, hparam, f1, itran)  // regress with x_j removed
            nn_j.train ().eval ()                                   // train model, evaluate QoF
            val bb  = nn_j.parameter
            val ft  = nn_j.fit
            val qof = ft(ir)                                        // rSqBar/rSq
            if (DEBUG) println (s"backwardElim: cols_$j = $cols_j, qof$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) println (s"backwardElim: remove variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        (j_max, b_max, ft_max)
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  FIX - rescaled or not?
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new Perceptron (x, y, fname, hparam, f1, itran),
                                                 xx, k, rando)
    } // crossVal

} // Perceptron class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Perceptron` companion object provides factory methods for buidling perceptrons.
 */
object Perceptron
{
    import PredictorMat.pullResponse

    private val DEBUG = false                                       // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Perceptron` with automatic rescaling from a combined data matrix.
     *  @param xy       the combined data/input and response/output matrix
     *  @param fname    the feature/variable names
     *  @param hparam   the hyper-parameters
     *  @param f1       the activation function family for layers 1->2 (input to output)
     *  @param rescale  the whether to rescale matrix xy to match activation function
     */
    def apply (xy: MatriD, fname: Strings = null,
               hparam: HyperParameter = Optimizer.hp,
               f1: AFF = f_sigmoid, rescale: Boolean = true): Perceptron =
    {
        var itran: FunctionV_2V = null                              // inverse transform -> original scale
        val cy = xy.dim2 - 1                                        // response/output column

        val xy_s =                                                  // scaled version of xy
            if (rescale) {
                if (f1.bounds != null) {                            // scale to bounds
                   val extrem = extreme (xy)
                   itran = unscaleV ((extrem._1(cy), extrem._2(cy)), f1.bounds) _
                   scale (xy, extrem, f1.bounds)
               } else {                                             // normalize
                  val (mu_xy, sig_xy) = (xy.mean, stddev (xy))
                  itran = denormalizeV ((mu_xy(cy), sig_xy(cy))) _
                  normalize (xy, (mu_xy, sig_xy))
               } // if
           } else {                                                 // do not rescale
               xy
           } // if

        val (x, y) = pullResponse (xy_s)
        setCol2One (x)                                              // make sure first column is all ones
        if (DEBUG) println ("scaled: x = " + x + "\nscaled y = " + y)
        new Perceptron (x, y, fname, hparam, f1, itran)
    } // apply

} // Perceptron object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest` object is used to test the `Perceptron` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.PerceptronTest
 */
object PerceptronTest extends App
{
    val s = 0                                                       // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,                    // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = VectorD (0.5, 0.30, 0.6)                                // training data - output vector

    println ("input  matrix x = " + x)
    println ("output vector y = " + y)

    val nn = new Perceptron (x, y)                                  // create a Perceptron

    banner ("PerceptronTest: Set the parameter vector b manually")

    val b = VectorD (0.0, 0.5, 0.5)                                 // set weight vector b manually
    nn.setWeights (b)
    println (nn.report)

    var yp = nn.predict ()                                          // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    for (i <- 1 to 20) {
        val eta = i * 0.5
        banner (s"PerceptronTest: Fit the parameter vector b using optimization with learning rate $eta")

        nn.reset (eta_ = eta)
        nn.train0 ().eval ()                                        // fit the weights using training data
        println (nn.report)

        yp = nn.predict ()                                          // predicted output value
        println ("target output:    y   = " + y)
        println ("predicted output: yp  = " + yp)
    } // for

    banner ("PerceptronTest: Compare with Linear Regression")

    val rg = new Regression (x, y)                                  // create a Regression model
    rg.train ().eval ()
    println (nn.report)

    yp = rg.predict ()                                              // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

} // PerceptronTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest2` object trains a perceptron on a small dataset of
 *  temperatures from counties in Texas where the variables/factors to consider
 *  are Latitude (x1), Elevation (x2) and Longitude (x3).  The model equation
 *  is the following:
 *  <p>
 *      y  =  sigmoid (w dot x)  =  sigmoid (w0 + w1*x1 + w2*x2 + w3*x3)
 *  <p>
 *  This test case illustrates how to transform the columns of the matrix
 *  so that the 'sigmoid' activation function can work effectively.
 *  Since the dataset is very small, should use 'train0' which does no batching.
 *  > runMain scalation.analytics.PerceptronTest2
 */
object PerceptronTest2 extends App
{
    // 16 data points:        Constant      x1      x2       x3     y
    //                                     Lat    Elev     Long  Temp        County
    val xy = new  MatrixD ((16, 5), 1.0, 29.767,   41.0,  95.367, 56.0,    // Harris
                                    1.0, 32.850,  440.0,  96.850, 48.0,    // Dallas
                                    1.0, 26.933,   25.0,  97.800, 60.0,    // Kennedy
                                    1.0, 31.950, 2851.0, 102.183, 46.0,    // Midland
                                    1.0, 34.800, 3840.0, 102.467, 38.0,    // Deaf Smith
                                    1.0, 33.450, 1461.0,  99.633, 46.0,    // Knox
                                    1.0, 28.700,  815.0, 100.483, 53.0,    // Maverick
                                    1.0, 32.450, 2380.0, 100.533, 46.0,    // Nolan
                                    1.0, 31.800, 3918.0, 106.400, 44.0,    // El Paso
                                    1.0, 34.850, 2040.0, 100.217, 41.0,    // Collington
                                    1.0, 30.867, 3000.0, 102.900, 47.0,    // Pecos
                                    1.0, 36.350, 3693.0, 102.083, 36.0,    // Sherman
                                    1.0, 30.300,  597.0,  97.700, 52.0,    // Travis
                                    1.0, 26.900,  315.0,  99.283, 60.0,    // Zapata
                                    1.0, 28.450,  459.0,  99.217, 56.0,    // Lasalle
                                    1.0, 25.900,   19.0,  97.433, 62.0)    // Cameron

    val (x, y) = PredictorMat.pullResponse (xy)
    println ("x = " + x)

    banner ("Perceptron with scaled y values")
    val nn = Perceptron (xy)                                        // factory function automatically rescales
//  val nn = new Perceptron (x, y)                                  // constructor does not automatically rescale

    nn.reset (eta_ = 0.5)                                           // try several values
    nn.train0 ().eval ()                                            // fit the weights using training data - simple alg.

//  nn.reset (eta_ = 0.5)                                           // try several values
//  nn.train ().eval ()                                             // fit the weights using training data
    println (nn.report)

    banner ("scaled prediction")
    val yp = nn.predict ()                                          // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yp = " + yp)

    banner ("unscaled prediction")
    val (ymin, ymax) = (y.min (), y.max ())                         // FIX - obtain from apply
    val ypu = unscaleV ((ymin, ymax), (0, 1)) (yp)                  // unscaled predicted output values for all x
    println ("target output:   y   = " + y)
    println ("unscaled output: ypu = " + ypu)

} // PerceptronTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest3` object trains a perceptron on a small dataset with variables
 *  x1 and x2.  The model equation is the following:
 *  <p>
 *      y  =  sigmoid (b dot x)  =  sigmoid (b0 + b1*x1 + b2*x2)
 *  <p>
 *  Does not call the 'train' method; improvements steps for sigmoid are explicitly in code below.
 *  > runMain scalation.analytics.PerceptronTest3
 */
object PerceptronTest3 extends App
{
    import PredictorMat.pullResponse

    // 9 data points:        Constant    x1    x2     y
    val xy = new MatrixD ((9, 4), 1.0,  1.0,  1.0,  0.04,           // dataset 1
                                  1.0,  2.0,  1.0,  0.05,
                                  1.0,  3.0,  1.0,  0.06,

                                  1.0,  1.0,  2.0,  0.10,
                                  1.0,  2.0,  2.0,  0.11,
                                  1.0,  3.0,  2.0,  0.12,

                                  1.0,  1.0,  3.0,  0.20,
                                  1.0,  2.0,  3.0,  0.21,
                                  1.0,  3.0,  3.0,  0.22)

    val b = VectorD ( 4.0, 0.58, 4.0)                               // initial weights/parameters
//  val b = VectorD (-5.0, -0.5, 1.5)                               // initial weights/parameters, better

/*
    // 9 data points:        Constant    x1    x2     y
    val xy = new MatrixD ((9, 4), 1.0,  0.0,  0.0,  0.5,            // dataset 2
                                  1.0,  0.0,  0.5,  0.3,
                                  1.0,  0.0,  1.0,  0.2,

                                  1.0,  0.5,  0.0,  0.8,
                                  1.0,  0.5,  0.5,  0.5,
                                  1.0,  0.5,  1.0,  0.3,

                                  1.0,  1.0,  0.0,  1.0,
                                  1.0,  1.0,  0.5,  0.8,
                                  1.0,  1.0,  1.0,  0.5)

    val b = VectorD (0.1, 0.2, 0.1)                                 // initial weights/parameters
*/

    private val _1 = VectorD.one (xy.dim1)                          // vector of all ones

    println (s"xy = $xy")
    val (x, y) = pullResponse (xy)                                  // input matrix, output vector
    val sst = Fit.sstf (y)                                          // sum of squares total

    var eta = 1.0 
    val hp = Optimizer.hp.updateReturn ("eta", eta)                 // try several values
    val nn = new Perceptron (x, y, null, hp)                        // create a perceptron, user control
//  val nn = Perceptron (xy, null, hp)                              // create a perceptron, automatic scaling
//  println (nn.train0 ().eval ().report)                           // don't train, run step-by-step
    banner ("initialize")

    nn.setWeights (b)                                               // set the parameters/weights
    var xb  = x * b                                                 // pre-activation value
    var yp  = nn.predict ()                                         // predicted response from 'nn'
    var yp2 = sigmoidV (xb)                                         // predicted response from calculation
    assert (yp == yp2)
    var e   = y - yp                                                // error
    val g   = yp * (_1 - yp)                                        // derivative
    val d   = g * e                                                 // delta
    var sse = e dot e                                               // sum of squared errors
    println (s"b   = $b")
    println (s"xb  = $xb")
    println (s"y   = $y")
    println (s"yp  = $yp")
    println (s"yp2 = $yp2")
    println (s"e   = $e")
    println (s"d   = $d")
    println (s"sse = $sse")
    println (s"R^2 = ${1 - sse/sst}")
 
    for (epoch <- 1 to 20) {
        banner (s"improvement step $epoch")
        val bup = x.t * (e * yp * (_1 - yp)) * eta                  // adjust the parameters/weights (for sigmoid)
        b += bup
        nn.setWeights (b)
        xb  = x * b
        yp  = nn.predict ()
        yp2 = sigmoidV (xb)
        assert (yp == yp2)
        e   = y - yp
        sse = e dot e
        println (s"bup = $bup")
        println (s"b   = $b")
        println (s"xb  = $xb")
        println (s"y   = $y")
        println (s"yp  = $yp")
        println (s"yp2 = $yp2")
        println (s"e   = $e")
        println (s"sse = $sse")
        println (s"R^2 = ${1 - sse/sst}")
    } // for
 
} // PerceptronTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest4` object trains a perceptron on a small dataset with variables
 *  x1 and x2.  The model equation is the following:
 *  <p>
 *      y  =  sigmoid (b dot x)  =  sigmoid (b0 + b1*x1 + b2*x2)
 *  <p>
 *  This version calls the 'train0' method.
 *  > runMain scalation.analytics.PerceptronTest4
 */
object PerceptronTest4 extends App
{
    // 16 data points:        Constant    x1    x2     y
    val xy = new  MatrixD ((9, 4), 1.0,  0.0,  0.0,  0.5,
                                   1.0,  0.0,  0.5,  0.3,
                                   1.0,  0.0,  1.0,  0.2,

                                   1.0,  0.5,  0.0,  0.8,
                                   1.0,  0.5,  0.5,  0.5,
                                   1.0,  0.5,  1.0,  0.3,

                                   1.0,  1.0,  0.0,  1.0,
                                   1.0,  1.0,  0.5,  0.8,
                                   1.0,  1.0,  1.0,  0.5)

    println (s"xy = $xy")

    val hp = Optimizer.hp.updateReturn ("eta", 2.0)                 // try several values
    val nn = Perceptron (xy, null, hp)                              // create a perceptron

    nn.train0 ().eval ()                                            // fit the weights using training data
    println (nn.report)

} // PerceptronTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest5` object trains a perceptron on a small dataset with 2 variables.
 *  > runMain scalation.analytics.PerceptronTest5
 */
object PerceptronTest5 extends App
{
    val x = new MatrixD ((6, 2), 1, 1, 1, 2, 1, 3, 1, 4, 1, 5, 1, 6)
    val y = VectorD (0, 0, 1, 0, 1, 1)

    val hp = Optimizer.hp.updateReturn ("eta", 0.3)                 // try several values
    val nn = new Perceptron (x, y, null, hp)                        // create a perceptron

    nn.setWeights (VectorD (-3.1, 0.9))                             // start search from this point

    nn.train0 ().eval ()                                            // fit the weights using training data
    println (nn.report)

    val yp = nn.predict ()                                          // predicted output value
    val ypr = yp.map (math.round (_))
    println ("target output:    y   = " + y)
    println ("predicted output: ypr = " + ypr)
    println ("predicted output: yp  = " + yp)

} // PerceptronTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest6` object trains a perceptron on a small dataset with 4 variables.
 *  > runMain scalation.analytics.PerceptronTest6
 */
object PerceptronTest6 extends App
{
    val xy = new MatrixD ((9, 4), 1, 0, 0, 0,
                                  1, 0, 1, 0,
                                  1, 0, 2, 0,
                                  1, 1, 0, 0,
                                  1, 1, 1, 1,
                                  1, 1, 2, 1,
                                  1, 2, 0, 0,
                                  1, 2, 1, 1,
                                  1, 2, 2, 1)

    val hp = Optimizer.hp.updateReturn ("eta", 0.1)                 // try several values
    val nn = Perceptron (xy, null, hp)                              // create a perceptron

//  nn.setWeights (VectorD (-3.1, 0.9))                             // start search from this point

    nn.train0 ().eval ()                                            // fit the weights using training data
    println (nn.report)

    val (x, y) = PredictorMat.pullResponse (xy)
    val yp = nn.predict ()                                          // predicted output value
    val ypr = yp.map (math.round (_))
    println ("target output:    y   = " + y)
    println ("predicted output: ypr = " + ypr)
    println ("predicted output: yp  = " + yp)

} // PerceptronTest6 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest7` object trains a perceptron on the `ExampleBasketBall` dataset.
 *  > runMain scalation.analytics.PerceptronTest7
 */
object PerceptronTest7 extends App
{
    import ExampleBasketBall._
    banner ("Perceptron vs. Regession - ExampleBasketBall")

    val f_ = f_id                                                   // try different activation function

    println ("ox = " + ox)
    println ("y  = " + y)

    banner ("Regression")
    val rg = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)

    banner ("prediction")                                           // not currently rescaling
    val yq = rg.predict ()                                          // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yq = " + yq)
    println ("error:            e  = " + (y - yq))

    banner ("Perceptron with scaled y values")
    val nn  = Perceptron (oxy, f1 = f_)                             // factory function automatically rescales
//  val nn  = new Perceptron (ox, y, f1 = f_)                       // constructor does not automatically rescale

    nn.reset (eta_ = 0.01)                                          // try several values
    nn.train0 ().eval ()                                            // fit the weights using training data - simple alg.

//  nn.reset (eta_ = 0.01)                                          // try several values
//  nn.train ().eval ()                                             // fit the weights using training data
    println (nn.report)

    banner ("scaled prediction")
    val yp = nn.predict ()                                          // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yp = " + yp)
    println ("error:            e  = " + (y - yp))

    banner ("unscaled prediction")
//  val (ymu, ysig) = (y.mean, sqrt (y.variance))                   // should obtain from apply - see below
//  val ypu = denormalizeV ((ymu, ysig))(yp)                        // denormalize predicted output values for all x
    val ypu = nn.itran (yp)                                         // denormalize predicted output values for all x
    println ("target output:   y   = " + y)
    println ("unscaled output: ypu = " + ypu)

} // PerceptronTest7 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest8` object trains a perceptron on the `ExampleAutoMPG` dataset.
 *  > runMain scalation.analytics.PerceptronTest8
 */
object PerceptronTest8 extends App
{
    import scala.math.{exp, log}
    import ExampleAutoMPG._
    banner ("Perceptron vs. Regession - ExampleAutoMPG")

//  val f_ = f_id                                                   // try different activation function
    val f_ = f_sigmoid                                              // try different activation function
/*
    println ("ox = " + ox)
    println ("y  = " + y)
*/
    banner ("Regression")
    val rg = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)

    banner ("prediction")                                           // not currently rescaling
    val yq = rg.predict ()                                          // scaled predicted output values for all x
    println ("target output:    y  = " + y)
    println ("predicted output: yq = " + yq)
    println ("error:            e  = " + (y - yq))

    banner ("Perceptron with scaled y values")
    val nn = Perceptron (oxy, f1 = f_)                              // factory function automatically rescales
//  val nn = new Perceptron (ox, y, f1 = f_)                        // constructor does not automatically rescale

//  nn.reset (eta_ = 0.001)                                         // try several values - for train0
    nn.reset (eta_ = 0.03)                                          // try several values - for train1, 2
    nn.trainSwitch (2).eval ()                                      // fit the parameters using the dataset
    println (nn.report)

//  banner ("scaled prediction")
    val yps = nn.predict ()                                         // scaled predicted output values for all x
//  println ("target output:    y   = " + y)
//  println ("predicted output: yps = " + yps)
//  println ("error:            e   = " + (y - yps))

    banner ("unscaled prediction")
    val yp = nn.itran (yps)                                         // unscaled predicted output values for all x
    println ("target output:   y  = " + y)
    println ("unscaled output: yp = " + yp)

    val rnk = y.rank
    val ry  = y.reorder (rnk)                                       // actual - red
    val ryq = yq.reorder (rnk)                                      // Regression - green
    val ryp = yp.reorder (rnk)                                      // Perceptron - blue

    val ys = MatrixD (Seq (ryp, ry, ryq))
    new PlotM (t, ys.t, null, "Perceptron")

    banner ("TranRegression")
    def f (u: Double): Double  = -log (1/u - 1)                     // transform
    def fi (t: Double): Double = 1 / (1 + exp (-t))                 // inverse transform
    val extrem = extreme (y)                                        // (min, max) for y
    val bounds = (0.01, 0.99)                                       // transform function domain bounds

    val trg = TranRegression (oxy, null, f _, fi _, RegTechnique.QR, bounds, true)
    trg.train ().eval ()
    val yt  = trg.predict ()//.map (fi _)
    val yt2 = scaleV (bounds, extrem)(yt)
    val ryt = yt2.reorder (rnk)                                      // TranRegression - green

    val ys2 = MatrixD (Seq (ryt, ry, ryq))
    new PlotM (t, ys2.t, null, "TranRegression")

} // PerceptronTest8 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest9` object trains a perceptron on the `ExampleAutoMPG` dataset.
 *  This tests forward feature/variable selection.
 *  > runMain scalation.analytics.PerceptronTest9
 */
object PerceptronTest9 extends App
{
    import ExampleAutoMPG._
    banner ("Perceptron feature selection - ExampleAutoMPG")

    val f_ = f_id                                                   // try different activation function
//  val f_ = f_sigmoid                                              // try different activation function
/*
    println ("ox = " + ox)
    println ("y  = " + y)
*/

    banner ("Perceptron with scaled y values")
    val hp2 = Optimizer.hp.updateReturn (("eta", 0.05), ("bSize", 10.0))
    val nn  = Perceptron (oxy, f1 = f_)                             // factory function automatically rescales
//  val nn  = new Perceptron (ox, y, f1 = f_)                       // constructor does not automatically rescale

    nn.train ().eval ()                                             // fit the weights using training data
    val n = ox.dim2                                                 // number of parameters/variables
    println (nn.report)
   
    banner ("Cross-Validation Test")
    nn.crossVal ()

    banner ("Forward Selection Test")
    val rSq = new MatrixD (ox.dim2-1, 3)                            // R^2, R^2 Bar,  R^2 cv

    val fcols = Set (0)                                             // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = nn.forwardSel (fcols)               // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")

        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                            // add variable x_j
            val x_cols = ox.selectCols (fcols.toArray)              // ox projected onto cols_j columns
//          rSq(l-1)   = Fit.qofVector (fit_j, nn.crossVal (x_cols))   // use main model, 'nn' - FIX
            val nn_j   = Perceptron (x_cols :^+ y, f1 = f_)         // regress with x_j added
            rSq(l-1)   = Fit.qofVector (fit_j, nn_j.crossVal ())    // use new model, nn_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                    // instance index
    new PlotM (t, rSq.t, lines = true)

} // PerceptronTest9 object

