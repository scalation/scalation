
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.5
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectorI}
import scalation.math._
import scalation.random.{PermutedVecI, RandomMatD}
import scalation.random.RNGStream.ranStream
import scalation.util.{banner, Error}

import ActivationFun._
import NeuralNet._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3L` class supports multi-output, 3-layer (input, hidden and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the weights/parameters 'aa' and 'bb' connecting the layers,
 *  so that for a new input vector 'v', the net can predict the output value, i.e.,
 *  <p>
 *      yp = f2 (bb * f1V (aa * v))
 *  <p>
 *  where 'f1' and 'f2' are the activation functions and the parameter matrices
 *  'aa' and 'bb' gives the  weights between input-hidden and hidden-output layers.
 *  Note, if 'a0' is to be treated as bias/intercept, 'x0' must be 1.0.
 *  @param x            the m-by-nx input matrix (training data consisting of m input vectors)
 *  @param y            the m-by-ny output matrix (training data consisting of m output vectors)
 *  @param nz           the number of nodes in hidden layer
 *  @param eta_         the learning/convergence rate
 *  @param max_epochs_  the maximum number of training epochs/iterations
 *  @param bsize        the mini-batch size
 *  @param f1           the input-hidden layer activation function (mapping scalar => scalar)
 *  @param f1D          the derivative of the input-hidden vector activation function 
 *  @param f2           the hidden-output layer activation function (mapping scalar => scalar)
 *  @param f2D          the derivative of the hidden-output vector activation function 
 */
class NeuralNet_3L (x: MatriD, y: MatriD,
                    private var nz: Int = -1,
                    eta_ : Double       = DEFAULT_ETA,
                    max_epochs_ : Int   = DEFAULT_EPOCHS,
                    bsize: Int          = 5,
                    f1:  FunctionS2S    = sigmoid _,
                    f1D: FunctionM_2M   = sigmoidDM,
                    f2:  FunctionS2S    = sigmoid _,
                    f2D: FunctionM_2M   = sigmoidDM)
      extends NeuralNet (x, y, eta_, max_epochs_)                  // sets eta and max_epochs in parent class
{
    private val DEBUG         = false                              // debug flag
    private val ADJUST_PERIOD = 100                                // number of epochs before adjusting learning rate
    private val ADJUST_FACTOR = 1.2                                // adjustment factor
    private val EPSILON       = 1E-7                               // number close to zero
    private val permGen       = PermutedVecI (VectorI.range(0, m), ranStream)

    private val f1V = vectorize (f1)                               // vector version of activation function 1
    private val f1M = matrixize (f1V)                              // matrix version of activation function 1
    private val f2V = vectorize (f2)                               // vector version of activation function 2
    private val f2M = matrixize (f2V)                              // matrix version of activation function 2

    private var aa: MatriD = null                                  // weight parameters (in to hid) - new MatrixD (nx, nz)
    private var bb: MatriD = null                                  // weight parameters (hid to out) - new MatrixD (nz, ny)

    // Guidelines for setting the number of nodes in hidden layer, e.g.,
    // 2 nx + 1, nx + 1, (nx + ny) / 2, sqrt (nx ny)
    if (nz < 1) nz = nx + 1

    println (s"Create a NeuralNet_3L with $nx input, $nz hidden and $ny output nodes")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the weight matrices 'aa' and 'bb'.
     */
    def weights: Array [MatriD] = Array (aa, bb)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrices 'aa' and 'bb' with values in (0, limit) before
     *  training.
     *  @param stream  the random number stream to use
     *  @param limit   the maximum value for any weight
     */
    def setWeights (stream: Int = 0, limit: Double = 1.0 / sqrt (nx))
    {
        val rmg1 = new RandomMatD (nx, nz, limit, stream = stream)      // change stream to get different random numbers
        aa       = rmg1.gen
        val rmg2 = new RandomMatD (nz, ny, limit, stream = stream)      // change stream to get different random numbers
        bb       = rmg2.gen
        if (DEBUG) println (s"setWeights: aa = $aa \n bb = $bb")
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter/weight matrices 'aa' and 'bb'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights. 
     */
    def train (): NeuralNet_3L =
    {
        if (bb == null || aa == null) setWeights ()                     // initialize parameters/weights
        val nBat = m / bsize                                            // the number of batches
        println (s"minimizeError: bsize = $bsize, nBat = $nBat")
        var up = 0                                                      // counter for number of times moving up

        for (epoch <- 1 to max_epochs) {                                // iterate over each epoch
            var sse  = 0.0                                              // hold sum of squared errors
            var sse0 = Double.MaxValue                                  // hold prior value of sse
            val batches = permGen.igen.split (nBat)                     // permute indices and split into nBat batches 

            for (ib <- batches) {                                       // iterate over each batch
                sse += updateWeights (x(ib), y(ib))                     // update weight matrices aa and bb
            } // for

            if (DEBUG) println (s"weights for $epoch th epoch: sse = $sse")
            if (sse > sse0 + EPSILON) up += 1 else up = 0
            if (up > 5) { println (s"ending epoch = $epoch"); return this }  // return early if moving up for too long
            sse0 = sse                                                  // save prior sse
            if (epoch % ADJUST_PERIOD == 0) eta *= ADJUST_FACTOR        // adjust the learning rate
        } // for

        println (s"max_epochs = $max_epochs")
        if (DEBUG) println (s"train: aa = $aa \n bb = $bb")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the parameter/weight matrices 'aa' and 'bb' based on the current batch.
     *  Take a step in the direction opposite to the gradient.
     *  @param x  the input matrix for the current batch
     *  @param y  the output matrix for the current batch
     */
    private def updateWeights (x: MatriD, y: MatriD): Double =
    {
        val z  = f1M (x * aa); /* z.setCol (0, _1) */                   // Z  = f (X A) (and opt. set hidden bias)
        val yp = f2M (z * bb)                                           // Yp = f (Z B)
        val en = yp - y                                                 // -E  where E is the error matrix
        val dy = f2D (yp) ** en                                         // delta y
        val dz = f1D (z) ** (dy * bb.t)                                 // delta z

        val eta_by_sz = eta / x.dim1                                    // eta over the current batch size
        bb -= z.t * dy * eta_by_sz                                      // update hidden-output weights
        aa -= x.t * dz * eta_by_sz                                      // update input-hidden weights
        en.normF ~^ 2                                                   // return see for this batch
    } // updateWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'v', predict the output/response vector 'f(v)'.
     *  @param v  the new input vector
     */
    def predictV (v: VectoD): VectoD = f2V (bb dot f1V (aa dot v))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix 'x', predict the output/response matrix 'f(x)'.
     *  @param x  the input matrix
     */
    def predict (x: MatriD): MatriD = f2M (f1M (x * aa) * bb)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)
    {
        crossValidate ((x: MatriD, y: MatriD) => new NeuralNet_3L (x, y), k, rando) 
    } // crossVal

} // NeuralNet_3L class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3LTest` object is used to test the `NeuralNet_3L` class.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.NeuralNet_3LTest
 */
object NeuralNet_3LTest extends App
{
    val s = 0                                          // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,       // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = new MatrixD ((3, 2), 0.5, 0.4,             // training data - output matrix (m vectors)
                                 0.3, 0.3,
                                 0.6, 0.5)

    println ("input  matrix x = " + x)
    println ("output matrix y = " + y)

    val nn = new NeuralNet_3L (x, y, 3, bsize = 1)      // create a NeuralNet_3L

    banner ("NeuralNet_3LTest: Set the parameter matrix bb randomly")

    nn.setWeights (s)                                   // set weights randomly
    println ("(aa, bb) = " + nn.weights.deep)
    nn.eval ()
    nn.fitMap ()

    var yp = nn.predict (x)                             // predicted output values
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    for (eta <- 0.5 to 10.0 by 0.5) {
        banner (s"NeuralNet_3LTest: Fit the parameter matrix bb using optimization with learning rate $eta")

        nn.reset (eta)
        nn.train ().eval ()                             // fit the weights using training data
        println ("(aa, bb) = " + nn.weights.deep)
        nn.fitMap ()

//      yp = nn.predict (x)                             // predicted output values
//      println ("target output:    y   = " + y)
//      println ("predicted output: yp  = " + yp)
        println ("yp0 = " + nn.predict (x(0)))          // predicted output values for row 0
    } // for

    banner ("NeuralNet_3LTest: Compare with Linear Regression - first column of y")

    val y0  = y.col(0)                                  // use first column of matrix y
    val rg0 = new Regression (x, y0)                    // create a Regression model
    rg0.train ().eval ()
    println ("b      = " + rg0.coefficient)
    println ("fitMap = " +  rg0.fitMap)

    val y0p = rg0.predict (x)                           // predicted output value
    println ("target output:    y0  = " + y0)
    println ("predicted output: y0p = " + y0p)

    banner ("NeuralNet_3LTest: Compare with Linear Regression - second column of y")

    val y1 = y.col(1)                                   // use second column of matrix y
    val rg1 = new Regression (x, y1)                    // create a Regression model
    rg1.train ().eval ()
    println ("b      = " + rg1.coefficient)
    println ("fitMap = " + rg1.fitMap)

    val y1p = rg1.predict (x)                           // predicted output value
    println ("target output:    y1  = " + y1)
    println ("predicted output: y1p = " + y1p)

} // NeuralNet_3LTest object

