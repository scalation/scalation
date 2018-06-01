
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math._
import scalation.random.RandomMatD
import scalation.util.{banner, Error}

import ActivationFun._
import NeuralNet._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2L` class supports multi-output, 2-layer (input and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the weights/parameters 'bb' connecting the layers,
 *  so that for a new input vector 'z', the net can predict the output value, i.e.,
 *  <p>
 *      yp_j = f (bb dot z)
 *  <p>
 *  where 'f' is the activation function and the parameter matrix 'bb' gives the
 *  weights between input and output layers.
 *  No batching is used for this algorithm.
 *  Note, 'b0' is treated as the bias, so 'x0' must be 1.0.
 *  @param x            the m-by-nx input matrix (training data consisting of m input vectors)
 *  @param y            the m-by-ny output matrix (training data consisting of m output vectors)
 *  @param eta_         the learning/convergence rate
 *  @param max_epochs_  the maximum number of training epochs/iterations
 *  @param f1           the activation function (mapping scalar => scalar)
 *  @param f1D          the derivative of the vector activation function 
 */
class NeuralNet_2L (x: MatriD, y: MatriD,
                    eta_ : Double = DEFAULT_ETA,
                    max_epochs_ : Int = DEFAULT_EPOCHS,
                    f1:  FunctionS2S  = sigmoid _,
                    f1D: FunctionV_2V = sigmoidDV _)
      extends NeuralNet (x, y, eta_, max_epochs_)                    // sets eta and max_epochs in parent class
{
    private val DEBUG   = false                                      // debug flag
    private val EPSILON = 1E-7                                       // number close to zero

    private val f1V = vectorize (f1)                                 // vector version of activation function 1
    private val f1M = matrixize (f1V)                                // matrix version of activation function 1

    private var bb: MatriD = null                                    // weight parameters - new MatrixD (nx, ny)
    private var yp: MatriD = null                                    // prediction matrix
    private var ee: MatriD = null                                    // error matrix

    println (s"Create a NeuralNet_2L with $nx input and $ny output nodes")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the weight matrix 'bb'.
     */
    def weights: Array [MatriD] = Array (bb)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrix 'bb' with values in (0, 1) before training.
     *  @param stream  the random number stream to use
     *  @param limit   the maximum value for any weight
     */
    def setWeights (stream: Int = 0, limit: Double = 1.0 / sqrt (nx))
    {
        val rmg = new RandomMatD (nx, ny, 1.0, stream = stream)      // change stream to get different random numbers
        bb      = rmg.gen
        if (DEBUG) println ("setWeights: bb = " + bb)
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter/weight matrix 'bb'.
     *  Minimize the error in the prediction by adjusting the weight matrix 'bb'.
     *  The error 'e' is simply the difference between the target value 'y' and the
     *  predicted value 'yp'.  Minimize the dot product of error with itself using
     *  gradient-descent. specifically move in the opposite direction of the gradient.
     */
    def train (): NeuralNet_2L =
    {
        if (bb == null) setWeights ()                                // initialize parameters/weights
        for (epoch <- 1 to max_epochs) {                             // epoch-th learning phase
            var sse0 = Double.MaxValue
            var sse  = 0.0
            for (k <- y.range2) {
                val ypk = f1V (x * bb.col(k))                        // yp for output k
                val ek  = y.col(k) - ypk                             // error vector for output k
                bb.setCol (k, bb.col(k) + x.t * (ek * f1D (ypk) * eta))    // adjust the weights
                sse += ek dot ek                                     // sum of squares for error
            } // for
            if (DEBUG) println (s"weights for $epoch th epoch: bb = $bb.deep, sse = $sse")
            if (sse0 - sse < EPSILON) return this                    // return when sse difference is small enough
            sse0 = sse                                               // save prior sse
        } // for
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response vector 'f(z)'.
     *  @param z  the new input vector
     */
    def predictV (z: VectoD): VectoD = f1V (bb dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix 'z', predict the output/response matrix 'f(z)'.
     *  @param z  the input matrix
     */
    def predict (z: MatriD): MatriD = f1M (z * bb)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)
    {
        crossValidate ((x: MatriD, y: MatriD) => new NeuralNet_2L (x, y), k, rando)
    } // crossVal

} // NeuralNet_2L class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_2LTest` object is used to test the `NeuralNet_2L` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.NeuralNet_2LTest
 */
object NeuralNet_2LTest extends App
{
    val s = 0                                         // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,      // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = new MatrixD ((3, 2), 0.5, 0.4,            // training data - output matrix (m vectors)
                                 0.3, 0.3,
                                 0.6, 0.5)

    println ("input  matrix x = " + x)
    println ("output matrix y = " + y)

    val nn = new NeuralNet_2L (x, y)                  // create a NeuralNet_2L

    banner ("NeuralNet_2LTest: Set the parameter matrix bb randomly")

    nn.setWeights (s)                                 // set weights randomly
    println ("bb     = " + nn.weights.deep)
    nn.eval ()
    nn.fitMap ()

    var yp = nn.predict (x)                           // predicted output values
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    for (eta <- 0.5 to 10.0 by 0.5) {
        banner (s"NeuralNet_2LTest: Fit the parameter matrix bb using optimization with learning rate $eta")

        nn.reset (eta)
        nn.train ().eval ()                           // fit the weights using training data
        println ("bb     = " + nn.weights.deep)
        nn.fitMap ()

        yp = nn.predict (x)                           // predicted output values
        println ("target output:    y   = " + y)
        println ("predicted output: yp  = " + yp)
        println ("yp = " + nn.predict (x(0)))         // predicted output values for row 0
    } // for

    banner ("NeuralNet_2LTest: Compare with Linear Regression - first column of y")

    val y0  = y.col(0)                                // use first column of matrix y
    val rg0 = new Regression (x, y0)                  // create a Regression model
    rg0.train ().eval ()
    println ("b      = " + rg0.coefficient)
    println ("fitMap = " +  rg0.fitMap)

    val y0p = rg0.predict (x)                         // predicted output value
    println ("target output:    y0  = " + y0)
    println ("predicted output: y0p = " + y0p)

    banner ("NeuralNet_2LTest: Compare with Linear Regression - second column of y")

    val y1 = y.col(1)                                 // use second column of matrix y
    val rg1 = new Regression (x, y1)                  // create a Regression model
    rg1.train ().eval ()
    println ("b      = " + rg1.coefficient)
    println ("fitMap = " + rg1.fitMap)

    val y1p = rg1.predict (x)                         // predicted output value
    println ("target output:    y1  = " + y1)
    println ("predicted output: y1p = " + y1p)

} // NeuralNet_2LTest object

