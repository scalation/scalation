
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Sep  9 13:30:41 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://en.wikipedia.org/wiki/Perceptron
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import math.exp
import util.control.Breaks.{break, breakable}

import scalation.linalgebra.{MatriD, MatrixD, VectorD}
import scalation.random.Random
import scalation.util.Error

import LogisticFunction.sigmoid

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Perceptron` class supports single-valued 2-layer (input and output)
 *  Neural-Networks.  Given several input vectors and output values (training data),
 *  fit the weights 'w' connecting the layers, so that for a new
 *  input vector 'zi',  the net can predict the output value 'zo', i.e.,
 *  'zi --> zo = f (w dot zi)'.
 *  Note, w0 is treated as the bias, so x0 must be 1.0.
 *  @param x    the input matrix (training data consisting of m input vectors)
 *  @param y    the output vector (training data consisting of m output values)
 *  @param eta  the learning/convergence rate
 */
class Perceptron (x: MatrixD, y: VectorD, eta: Double = 1.0)
      extends Predictor with Error
{
    private val MAX_ITER = 200          // maximum number of iterations
    private val EPSILON  = 1E-9         // number close to zero
    private val DEBUG    = true         // debug flag
    private val m        = x.dim1       // number of data points (input vectors)
    private val n        = x.dim2       // dimensionality of the input
    private val _1       = new VectorD (m); _1.set (1.0)

    if (y.dim != m) flaw ("constructor", "dimensions of x and y are incompatible")

    println ("Create a Perceptron with " + n + " input, " + 1 + " output nodes")

    private var w: VectorD = null       // weight vector between input and output layers

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrix w manually before training.
     *  @param w0   the initial weights for w
     */
    def setWeights (w0: VectorD) { w  = w0 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight vector 'w' with values in (0, 1) before training.
     *  @param i  the random number stream to use
     */
    def setWeights (i: Int = 0)
    {
        val rn = new Random (i)          // change i to get different random numbers
        w      = new VectorD (n)
        for (i <- 0 until n) w(i) = rn.gen
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x and y, fit the weight vector w.
     */
    def train () { if (w == null) setWeights (); minimizeError () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Minimize the error in the prediction by adjusting the weight vector 'w'.
     *  The error 'e' is simply the difference between the target value 'y' and the
     *  predicted value 'z'.  Mininize 1/2 of the dot product of error with itself
     *  using gradient-descent. The gradient is '-x.t * (e * z * (_1 - z))', so
     *  move in the opposite direction of the gradient.
     */
    def minimizeError ()
    {
        breakable { for (k <- 0 until MAX_ITER) {         // kth learning phase
            val z = sigmoid (x * w)                       // vector of predicted outputs
            val e = y - z                                 // vector of outputs from training data
            w += x.t * (e * z * (_1 - z)) * eta           // adjust the weights
            println ("weights for " + k + "th phase: w = " + w + ", error e = " + e)
            if ((e dot e) < 2.0 * EPSILON) break          // break when error is small enough
        }} // for
    } // minimizeError

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit, the weigth vector 'w'.
     */
    def fit: VectorD = w

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'zi', predict the output/response value 'zo'.
     *  @param zi  the new input vector
     */
    def predict (zi: VectorD): Double = sigmoid (w dot zi)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several new input vectors stored as rows in a matrix 'zi',
     *  predict all output/response vector 'zo'
     *  @param zi  the matrix containing row vectors to use for prediction
     */
    override def predict (zi: MatriD): VectorD = sigmoid (zi * w)

} // Perceptron class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest` object is used to test the `Perceptron` class.  For this
 *  test, the initial weights are used for used for prediction.
 */
object PerceptronTest extends App
{
    val x   = new MatrixD (1, 3)               // training data - input vectors (not used)
    val y   = new VectorD (1)                  // training data - output vectors (not used)
    val ann = new Perceptron (x, y)            // create a Perceptron

    val w  = VectorD (0.0, 0.5, 0.5)           // weight vector w (input to output layer)
    ann.setWeights (w)                         // set intial weights

    val z_i = VectorD (1.0, 1.0, 1.0)          // predict output z_o from input z_i
    println ("input vector:  z_i = " + z_i)
    println ("output vector: z_o = " + ann.predict (z_i))

} // PerceptronTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest2` object is used to test the `Perceptron` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see http://www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 */
object PerceptronTest2 extends App
{
    val x   = new MatrixD ((1, 3), 1.0, 0.35, 0.9)   // training data - input vectors
    val y   = VectorD (0.5)                          // training data - output vectors
    val ann = new Perceptron (x, y)                  // create a Perceptron

    val w  = VectorD (0.0, 0.5, 0.5)                 // weight vector w (input to output layer)
    ann.setWeights (w)                               // set intial weights

    println ("input vector:  x(0) = " + x(0))
    println ("=== target output value: y(0) = " + y(0))
    println ("--- initial output value: z = " + ann.predict (x(0)))

    ann.train ()                                     // fit the weights using training data

    println ("+++ trained output value: z = " + ann.predict (x(0)))

} // PerceptronTest2 object

