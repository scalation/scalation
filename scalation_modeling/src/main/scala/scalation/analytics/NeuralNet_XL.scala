
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.5
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 *  @see     http://neuralnetworksanddeeplearning.com/
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectorI}
import scalation.math._
import scalation.random.{Normal, PermutedVecI, RandomMatD}
import scalation.random.RNGStream.ranStream
import scalation.util.{Error, banner}

import ActivationFun._
import NeuralNet._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_XL` class supports multi-output, multi-layer (input, multiple hidden and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the weight and bias parameters connecting the layers,
 *  so that for a new input vector 'v', the net can predict the output value
 *  This implementation is partially adapted from Michael Nielsen's Python implementation found in
 *  @see  github.com/mnielsen/neural-networks-and-deep-learning/blob/master/src/network2.py
 *  @see  github.com/MichalDanielDobrzanski/DeepLearningPython35/blob/master/network2.py
 *------------------------------------------------------------------------------
 *  @param x            the m-by-nx input matrix (training data consisting of m input vectors)
 *  @param y            the m-by-ny output matrix (training data consisting of m output vectors)
 *  @param nh           the number of nodes in each hidden layer, e.g., Array (5, 10) means 2 hidden with sizes 5 and 10
 *  @param eta_         the learning/convergence rate (typically less than 1.0)
 *  @param max_epochs_  the maximum number of training epochs/iterations
 *  @param bsize        the mini-batch size
 *  @param lambda       the regularization parameter
 *  @param actfV        the array of activation function (mapping vector => vector) between every pair of layers
 *  @param actfDM       the array of derivative of the matrix activation functions
 */
class NeuralNet_XL (x: MatriD, y: MatriD,
                    private var nh: Array [Int] = null,
                    eta_ : Double = DEFAULT_ETA,
                    max_epochs_ : Int = DEFAULT_EPOCHS,
                    private var bsize: Int = 5,
                    private var lambda: Double = 0.0,
                    actfV:  Array [FunctionV_2V] = Array (sigmoidV, sigmoidV),
                    actfDM: Array [FunctionM_2M] = Array (sigmoidDM, sigmoidDM))
      extends NeuralNet (x, y, eta_, max_epochs_)               // sets eta and max_epochs in parent class
{
    private val DEBUG   = false                                 // debug flag
    private val EPSILON = 1E-7                                  // number close to zero
    private val permGen = PermutedVecI (VectorI.range(0, m), ranStream)

    // Guidelines for setting the number of nodes in hidden layer, e.g.,
    // 2 nx + 1, nx + 1, (nx + ny) / 2, sqrt (nx ny)
    if (nh == null) nh = Array (nx + 1)
    if (actfV.length != actfDM.length || actfV.length != nh.length + 1) {
        flaw ("NeuralNet_XL Constructor", "Dimension mismatch among number of layers or activation functions")
    } // if

    private val sizes = nx +: nh :+ ny                          // sizes of all layers
    private val nl    = sizes.length                            // number of layers
    private val nl1   = nl - 1                                  // number of layers - 1

    private var ww: IndexedSeq [MatriD] = null                  // weight matrices for all layers
    private var bi: IndexedSeq [VectoD] = null                  // bias vectors for all layers

    private val actfM = for (f <- actfV) yield matrixize (f)    // matrixize activation functions

    println (s"Create a NeuralNet_XL with $nx input, ${nh.deep} hidden and $ny output nodes")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the weight matrices.
     */
    def weights: Array [MatriD] = ww.toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the bias vectors.
     */
    def biases: Array [VectoD] = bi.toArray

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrices 'aa' and 'bi' with values in (0, limit) before
     *  training.
     *  @param stream  the random number stream to use
     *  @param limit   the maximum value for any weight
     */
    def setWeights (stream: Int = 0, limit: Double = 1.0 / sqrt (nx))
    {
        val normal = Normal (0.0, 1.0, stream)

        bi = for (i <- 1 until nl)  yield VectorD (for (j <- 0 until sizes(i)) yield normal.gen)
        ww = for (i <- 0 until nl1) yield MatrixD (for (j <- 0 until sizes(i))
                  yield VectorD (for (k <- 0 until sizes(i+1)) yield normal.gen/sqrt(sizes(i))), false)

        if (DEBUG) println (s"setWeights: weights = $ww \n biases = $bi")
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the learning rate 'eta', batch size 'bsize', and the regularization parameter 'lambda'.
     *  @param eta_     the learning rate
     *  @param bsize_   the batch size
     *  @param lambda_  the regularization parameter
     */
    def reset (eta_ : Double = 0.0, bsize_ : Int = 0, lambda_ : Double = -1.0)
    {
        if (eta_ > 0.0)     eta    = eta_
        if (bsize_ > 0)     bsize  = bsize_
        if (lambda_ >= 0.0) lambda = lambda_
        ww = null
        bi = null
    } // reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter/weight matrices 'aa' and 'bi'.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  'nbat' batches.  Each batch is used to update the weights. 
     */
    def train (): NeuralNet_XL =
    {
        if (ww == null || bi == null) setWeights ()                     // initialize parameters/weights
        val nBat = m / bsize                                            // the number of batches
        if (DEBUG) println (s"train: bsize = $bsize, nBat = $nBat")
        var up = 0                                                      // counter for number of times moving up

        for (epoch <- 1 to max_epochs) {                                // iterate over each epoch
            var sse  = 0.0                                              // hold sum of squared errors
            var sse0 = Double.MaxValue                                  // hold prior value of sse
            val batches = permGen.igen.split (nBat)                     // permute indices and split into nBat batches 

            for (ib <- batches) {                                       // iterate over each batch
                sse += updateWeights (x(ib), y(ib))                     // update weight matrices aa and bi
            } // for

            if (DEBUG) println (s"weights for $epoch th epoch: sse = $sse")
            if (sse > sse0 + EPSILON) up += 1 else up = 0
            if (up > 5) { println (s"ending epoch = $epoch"); return this }  // return early if moving up for too long
            sse0 = sse                                                  // save prior sse
        } // for

        if (DEBUG) println (s"max_epochs = $max_epochs")
        if (DEBUG) println (s"train: weights = $ww \n biases = $bi")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the parameter/weight matrices 'aa' and 'bi' based on the current batch.
     *  Take a step in the direction opposite to the gradient.
     *  @param x  the input matrix for the current batch
     *  @param y  the output matrix for the current batch
     */
    private def updateWeights (x: MatriD, y: MatriD): Double =
    {
        val as = Array.ofDim [MatriD](nl)            // array to store all the activations, layer by layer
        as(0)  = x                                   // initial activation, which is the input matrix
        for (i <- 0 until nl1) as(i+1) = actfM(i)(as(i) * ww(i) + bi(i))                        // feedforward and store all activations

        val yp = as.last                             // predicted value of y
        val en = yp - y                              // -E  where E is the error matrix

        val δs    = Array.ofDim [MatriD](nl1)        // array to store all δ's
        δs(nl1-1) = actfDM.last(yp) ** en            // δ for the last layer
        for (l <- 2 until nl) δs(nl1-l) = actfDM(nl1-l)(as(nl-l)) ** (δs(nl-l) * ww(nl-l).t)    // δ's for all previous hidden layers

        val eta_by_sz = eta / x.dim1                 // learning rate divided by the size of this mini-batch
        for (i <- 0 until nl1) {
            ww(i) *= 1.0 - eta * (lambda / m)        // regularization factor, weight decay
            ww(i) -= as(i).t * δs(i) * eta_by_sz     // update weights
            bi(i) -= δs(i).mean * eta                // update biases
        } // for
        en.normF ~^ 2                                // return see for this batch
    } // updateWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'v', predict the output/response vector 'f(v)'.
     *  @param v  the new input vector
     */
    def predictV (v: VectoD): VectoD =
    {
        var a = v
        for (i <- 0 until nl1) a = actfV(i)((ww(i) dot a) + bi(i))
        a
    } // predictV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix 'x', predict the output/response matrix 'f(x)'.
     *  @param x  the input matrix
     */
    def predict (x: MatriD): MatriD =
    {
        var a = x
        for (i <- 0 until nl1) a = actfM(i)(a * ww(i) + bi(i))
        a
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)
    {
        crossValidate ((x: MatriD, y: MatriD) => new NeuralNet_XL (x, y), k, rando)
    } // crossVal

} // NeuralNet_XL class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_XLTest` object is used to test the `NeuralNet_XL` class.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.NeuralNet_XLTest
 */
object NeuralNet_XLTest extends App
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

    val nn = new NeuralNet_XL (x, y, Array (3), bsize = 1)    // create a NeuralNet_XL

    banner ("NeuralNet_XLTest: Set the parameter matrix bb randomly")

    nn.setWeights (s)                                   // set weights randomly
    println ("weights     = " + nn.weights)
    nn.eval ()
    nn.fitMap ()

    var yp = nn.predict (x)                             // predicted output values
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    for (eta <- 0.5 to 10.0 by 0.5) {
        banner (s"NeuralNet_XLTest: Fit the parameter matrix bi using optimization with learning rate $eta")

        nn.reset (eta)
        nn.train ().eval ()                             // fit the weights using training data
        println ("bb     = " + nn.weights.deep)
        nn.fitMap ()

        //yp = nn.predict (x)                           // predicted output values
        println ("target output:    y   = " + y)
        //println ("predicted output: yp  = " + yp)
        println ("yp = " + nn.predict (x(0)))           // predicted output values for row 0
    } // for

    banner ("NeuralNet_XLTest: Compare with Linear Regression - first column of y")

    val y0  = y.col(0)                                  // use first column of matrix y
    val rg0 = new Regression (x, y0)                    // create a Regression model
    rg0.train ().eval ()
    println ("b      = " + rg0.coefficient)
    println ("fitMap = " +  rg0.fitMap)

    val y0p = rg0.predict (x)                           // predicted output value
    println ("target output:    y0  = " + y0)
    println ("predicted output: y0p = " + y0p)

    banner ("NeuralNet_XLTest: Compare with Linear Regression - second column of y")

    val y1 = y.col(1)                                   // use second column of matrix y
    val rg1 = new Regression (x, y1)                    // create a Regression model
    rg1.train ().eval ()
    println ("b      = " + rg1.coefficient)
    println ("fitMap = " + rg1.fitMap)

    val y1p = rg1.predict (x)                           // predicted output value
    println ("target output:    y1  = " + y1)
    println ("predicted output: y1p = " + y1p)

} // NeuralNet_XLTest object

