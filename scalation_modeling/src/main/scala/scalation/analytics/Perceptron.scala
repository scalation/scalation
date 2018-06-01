
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Mon Sep  9 13:30:41 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.collection.mutable.{ListMap, Map}
import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math._
import scalation.random.RandomVecD
import scalation.util.banner

import ActivationFun._
import MatrixTransform._
import NeuralNet._

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
 *  @param x           the input matrix (training data consisting of m input vectors)
 *  @param y           the output vector (training data consisting of m output values)
 *  @param eta         the learning/convergence rate (requires adjustment)
 *  @param max_epochs  the maximum number of training epcochs/iterations
 *  @param f1          the activation function (mapping scalar => scalar)
 *  @param f1DV        the derivative of the vector activation function
 */
class Perceptron (x: MatriD, y: VectoD,
                  private var eta: Double = DEFAULT_ETA,
                  private val max_epochs: Int = DEFAULT_EPOCHS,
                  f1:  FunctionS2S  = sigmoid _,
                  f1D: FunctionV_2V = sigmoidDV _)
      extends PredictorMat (x, y)
{
    private val DEBUG      = false                                 // debug flag
    private val EPSILON    = 1E-7                                  // number close to zero
    private val n          = x.dim2                                // dimensionality of the input
    private val _1         = VectorD.one (m)                       // vector of all ones

    private val f1V        = vectorize (f1)                        // vector version of activation function 1

    if (y.dim != m) flaw ("constructor", "dimensions of x and y are incompatible")

    println (s"Create a Perceptron with $n input nodes and 1 output node")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial parameter/weight vector 'b' manually before training.
     *  @param w0  the initial weights for b
     */
    def setWeights (w0: VectoD) { b = w0 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly initialize the parameter/weight vector 'b' with values in
     *  '(ymin, ymax)' before training.
     *  @param ymin    the minimum value to produce
     *  @param ymax    the maximum value to produce
     *  @param stream  the random number stream to use
     */
    def setWeights (ymin: Double = 0.0, ymax: Double = 1.0, stream: Int = 0)
    {
        val rvg = new RandomVecD (n, ymax, ymin, stream = stream)    // change stream to get different random numbers
        b       = rvg.gen
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
     *  @param yy  the output vector
     */
    def train (yy: VectoD = y): Perceptron =
    {
        if (b == null) setWeights ()                             // initialize parameters/weights
        var sse0 = Double.MaxValue
        for (epoch <- 1 until max_epochs) {                      // epoch-th learning phase
            val yp = f1V (x * b)                                 // vector of predicted outputs
            val e  = y - yp                                      // residual/error vector
//          b     += x.t * (e * yp * (_1 - yp)) * eta            // adjust the parameters/weights (for sigmoid)
            b     += x.t * (e * f1D (yp)) * eta                  // adjust the parameters/weights
            val sse = e dot e                                    // sum of squares for error
            if (DEBUG) println (s"weights for $epoch th phase: b = $b, sse = $sse")
            if (sse0 - sse < EPSILON) return this                // return when sse difference is small enough
            sse0 = sse                                           // save prior sse
        } // for
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     */
    override def eval ()
    { 
        val yp = f1V (x * b)                                     // yp for output j
        e = y - yp                                               // difference between actual and predicted
        diagnose (e)                                             // compute diagonostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response value 'f(z)'.
     *  @param z  the new input vector
     */
    override def predict (z: VectoD): Double = f1 (b dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input matrix 'z', predict the output/response value 'f(z)'.
     *  @param z  the new input matrix
     */
    override def predict (z: MatriD): VectoD = f1V (z * b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)
    {
        crossValidate ((x: MatriD, y: VectoD) => new Perceptron (x, y), k, rando)
    } // crossVal

} // Perceptron class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest` object is used to test the `Perceptron` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 *  > runMain scalation.analytics.PerceptronTest
 */
object PerceptronTest extends App
{
    val s = 0                                        // random number stream to use
    val x = new MatrixD ((3, 3), 1.0, 0.35, 0.9,     // training data - input matrix (m vectors)
                                 1.0, 0.20, 0.7,
                                 1.0, 0.40, 0.95)
    val y = VectorD (0.5, 0.30, 0.6)                 // training data - output vector

    println ("input  matrix x = " + x)
    println ("output vector y = " + y)

    val nn = new Perceptron (x, y)                   // create a Perceptron

    banner ("PerceptronTest: Set the parameter vector b manually")

    val b = VectorD (0.0, 0.5, 0.5)                  // set weight vector b manually
    nn.setWeights (b)
    println ("b      = " + nn.coefficient)
    println ("fitMap = " + nn.fitMap)

    var yp = nn.predict (x)                          // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    banner ("PerceptronTest: Set the parameter vector b randomly")

    nn.setWeights (s)                                // set weights randomly
    println ("b      = " + nn.coefficient)
    println ("fitMap = " + nn.fitMap)

    yp = nn.predict (x)                              // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

    for (eta <- 0.5 to 10.0 by 0.5) {
        banner (s"PerceptronTest: Fit the parameter vector b using optimization with learning rate $eta")

        nn.reset (eta)
        nn.train ().eval ()                          // fit the weights using training data
        println ("b      = " + nn.coefficient)
        println ("fitMap = " + nn.fitMap)

        yp = nn.predict (x)                          // predicted output value
        println ("target output:    y   = " + y)
        println ("predicted output: yp  = " + yp)
    } // for

    banner ("PerceptronTest: Compare with Linear Regression")

    val rg = new Regression (x, y)                  // create a Regression model
    rg.train ().eval ()
    println ("b      = " + rg.coefficient)
    println ("fitMap = " + rg.fitMap)

    yp = rg.predict (x)                             // predicted output value
    println ("target output:    y   = " + y)
    println ("predicted output: yp  = " + yp)

} // PerceptronTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PerceptronTest2` object trains a perceptron on a small dataset of
 *  temperatures from counties in Texas where the variables/factors to consider
 *  are Latitude (x1), Elevation (x2) and Longitude (x3).  The regression equation
 *  is the following:
 *  <p>
 *      y  =  sigmoid (w dot x)  =  sigmoid (w0 + w1*x1 + w2*x2 + w3*x3)
 *  <p>
 *  > runMain scalation.analytics.PerceptronTest2
 */
object PerceptronTest2 extends App
{
    // 16 data points:        Constant      x1      x2       x3
    //                                     Lat    Elev     Long        County
    val x = new  MatrixD ((16, 4), 1.0, 29.767,   41.0,  95.367,    // Harris
                                   1.0, 32.850,  440.0,  96.850,    // Dallas
                                   1.0, 26.933,   25.0,  97.800,    // Kennedy
                                   1.0, 31.950, 2851.0, 102.183,    // Midland
                                   1.0, 34.800, 3840.0, 102.467,    // Deaf Smith
                                   1.0, 33.450, 1461.0,  99.633,    // Knox
                                   1.0, 28.700,  815.0, 100.483,    // Maverick
                                   1.0, 32.450, 2380.0, 100.533,    // Nolan
                                   1.0, 31.800, 3918.0, 106.400,    // El Paso
                                   1.0, 34.850, 2040.0, 100.217,    // Collington
                                   1.0, 30.867, 3000.0, 102.900,    // Pecos
                                   1.0, 36.350, 3693.0, 102.083,    // Sherman
                                   1.0, 30.300,  597.0,  97.700,    // Travis
                                   1.0, 26.900,  315.0,  99.283,    // Zapata
                                   1.0, 28.450,  459.0,  99.217,    // Lasalle
                                   1.0, 25.900,   19.0,  97.433)    // Cameron

    val y = VectorD (56.0, 48.0, 60.0, 46.0, 38.0, 46.0, 53.0, 46.0,
                     44.0, 41.0, 47.0, 36.0, 52.0, 60.0, 56.0, 62.0)

    val yy = scaleV (y, y.min (), y.max ())

    println ("-------------------------------------------------")
    println ("Fit the parameter vector b using Steepest Descent")
    val nn = new Perceptron (x, yy)
    val eta = 2.0                                // try several values
    nn.reset (eta)
    nn.train ().eval ()                          // fit the weights using training data

    println ("b      = " + nn.coefficient)
    println ("fitMap = " + nn.fitMap)

    val yp = nn.predict (x)                      // predicted output value
    println ("target output:    yy = " + yy)
    println ("predicted output: yp = " + yp)

    val z = VectorD (1.0, 30.0, 1000.0, 100.0)
    println ("predict (" + z + ") = " + nn.predict (z))

} // PerceptronTest2

