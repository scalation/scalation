
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Sep  9 13:30:41 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.math.exp
import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.random.RandomVecD
import scalation.util.{banner, Error}

import LogisticFunction.sigmoid

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
 *  Note, b0 is treated as the bias, so x0 must be 1.0.
 *  @param x    the input matrix (training data consisting of m input vectors)
 *  @param y    the output vector (training data consisting of m output values)
 *  @param eta  the learning/convergence rate (typically less than 1.0)
 */
class Perceptron (x: MatrixD, y: VectorD, private var eta: Double = 1.0)
      extends Predictor with Error
{
    private val DEBUG    = false                       // debug flag
    private val MAX_ITER = 200                         // maximum number of iterations
    private val EPSILON  = 1E-9                        // number close to zero
    private val m        = x.dim1                      // number of data points (input vectors)
    private val n        = x.dim2                      // dimensionality of the input
    private val _1       = VectorD.fill (m)(1.0)       // vector of all ones

    if (y.dim != m) flaw ("constructor", "dimensions of x and y are incompatible")

    println (s"Create a Perceptron with $n input and 1 output nodes")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight vector 'b' manually before training.
     *  @param w0  the initial weights for b
     */
    def setWeights (w0: VectorD) { b  = w0; check (y) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight vector 'b' with values in (0, 1) before training.
     *  @param stream  the random number stream to use
     */
    def setWeights (stream: Int = 0)
    {
        val rvg = new RandomVecD (n, 1.0, stream = stream)    // change i to get different random numbers
        b       = rvg.gen
        check (y)
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the leaning rate 'eta'.
     *  @param eta  the learning rate
     */
    def reset (eta_ : Double) { eta = eta_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     *  @param yy  the output vector
     */
    def train (yy: VectoD)
    {
        if (b == null) setWeights ()                          // initialize parameters/weights
        minimizeError (yy)                                    // adjust weights b to minimize sse
        check (yy)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter/weight vector 'b'.
     */
    def train () { train (y) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     *  @param yy  the output vector
     */
    def check (yy: VectoD)
    { 
        e = yy - sigmoid (x * b)                              // compute residual/error vector 
        diagnose (yy)                                         // compute diagonostics
    } // check

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Minimize the error in the prediction by adjusting the weight vector 'b'.
     *  The error 'e' is simply the difference between the target value 'yy' and the
     *  predicted value 'yp'.  Minimize 1/2 of the dot product of error with itself
     *  using gradient-descent.  The gradient is '-x.t * (e * yp * (_1 - yp))', so
     *  move in the opposite direction of the gradient.
     *  @param yy  the output vector
     */
    def minimizeError (yy: VectoD)
    {
        breakable { for (k <- 0 until MAX_ITER) {             // kth learning phase
            val yp = sigmoid (x * b)                          // vector of predicted outputs
            val e  = yy - yp                                  // residual/error vector
            b     += x.t * (e * yp * (_1 - yp)) * eta         // adjust the weights

            sse = e dot e                                     // sum of squared errors
            if (DEBUG) println (s"weights for $k th phase: b = $b, sse = $sse")
            if (sse < 2.0 * EPSILON) break                    // break when error is small enough
        }} // for
    } // minimizeError

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response value 'f(z)'.
     *  @param z  the new input vector
     */
    def predict (z: VectoD): Double = sigmoid (b dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response value 'f(z)'.
     *  @param z  the new input vector
     */
    def predict (z: MatriD): VectoD = VectorD (for (i <-z.range1) yield sigmoid (b dot z(i)))

} // Perceptron class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest` object is used to test the `Perceptron` class.  For this
 *  test, the initial weights are used for used for prediction.
 *  > run-main scalation.analytics.PerceptronTest
 */
object PerceptronTest extends App
{
    val x   = new MatrixD (1, 3)                     // training data - input vectors (not used)
    val y   = new VectorD (1)                        // training data - output vectors (not used)
    val ann = new Perceptron (x, y)                  // create a Perceptron

    val b = VectorD (0.0, 0.5, 0.5)                  // weight vector b (input to output layer)
    ann.setWeights (b)                               // set initial weights

    val z  = VectorD (1.0, 1.0, 1.0)                 // new input vector z
    val yp = ann.predict (z)                         // predicted output value
    println (s"input vector: z  = $z")
    println (s"output value: yp = $yp")

} // PerceptronTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest2` object is used to test the `Perceptron` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > run-main scalation.analytics.PerceptronTest
 */
object PerceptronTest2 extends App
{
    val s = 0                                        // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,     // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = VectorD (0.5, 0.30, 0.6)                 // training data - output vector

    println ("input  matrix x = " + x)
    println ("output vector y = " + y)

    val nn = new Perceptron (x, y)                   // create a Perceptron

    banner ("PerceptronTest2: Set the parameter vector b manually")

    val b = VectorD (0.0, 0.5, 0.5)                  // set weight vector b manually
    nn.setWeights (b)
    println ("b   = " + nn.coefficient)
    println (nn.fitLabels)
    println ("fit = " + nn.fit)

    var yp = nn.predict (x)                          // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    banner ("PerceptronTest2: Set the parameter vector b randomly")

    nn.setWeights (s)                                // set weights randomly
    println ("b   = " + nn.coefficient)
    println (nn.fitLabels)
    println ("fit = " + nn.fit)

    yp = nn.predict (x)                              // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    for (eta <- 0.5 to 10.0 by 0.5) {
        banner (s"PerceptronTest2: Fit the parameter vector b using optimization with learning rate $eta")

        nn.reset (eta)
        nn.train ()                                  // fit the weights using training data
        println ("b   = " + nn.coefficient)
        println (nn.fitLabels)
        println ("fit = " + nn.fit)

        yp = nn.predict (x)                          // predicted output value
        println ("target output:    y   = " + y)
        println ("predicted output: yp  = " + yp)
    } // for

    banner ("PerceptronTest2: Compare with Linear Regression")

    val rg = new Regression (x, y)                  // create a Rgression model
    rg.train ()
    println ("b   = " + rg.coefficient)
    println (rg.fitLabels)
    println ("fit = " + rg.fit)

    yp = rg.predict (x)                             // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

} // PerceptronTest2 object

