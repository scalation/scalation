
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Jan 21 14:43:50 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://page.mi.fu-berlin.de/rojas/neural/chapter/K7.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import scala.math.exp
import scala.util.control.Breaks.{break, breakable}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.linalgebra.MatrixD.outer
import scalation.random.Random
import scalation.util.Error

import ActivationFunc._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet` class supports basic 3-layer (input, hidden and output) Neural
 *  Networks.  Given several input and output vectors (training data), fit the weights
 *  connecting the layers, so that for a new input vector 'zi', the net can predict
 *  the output vector 'zo' ('zh' is the intermediate value at the hidden layer), i.e.,
 *  <p>
 *      zi --> zh = f (w * zi) --> zo = g (v * zh)
 *  <p>
 *  Note, w_0 and v_0 are treated as biases, so zi_0 and zh_0 must be 1.0.
 *  @param x    the input matrix (training data consisting of m input vectors)
 *  @param y    the output matrix (training data consisting of m output vectors)
 *  @param h    the number of neurons in the hidden layer
 *  @param eta  the learning/convergence rate
 */
class NeuralNet (x: MatriD, y: MatriD, h: Int, eta: Double = 1.0)
      extends Predictor with Error
{
    private val MAX_ITER = 200                  // maximum number of iterations
    private val EPSILON  = 1E-6                 // number close to zero
    private val DEBUG    = true                 // debug flag
    private val m        = x.dim1               // number of data points
    private val n        = x.dim2               // dimensionality of the input
    private val p        = y.dim2               // dimensionality of the output
    private val hh       = new MatrixD (m, h)   // used to hold hidden layer values
    private val _11      = VectorD (1.0)

    if (y.dim1 != m) flaw ("constructor", "dimensions of x and y are incompatible")

    println ("Create a Neural Net with " + n + " input, " +
                                           h + " hidden, " + 
                                           p + " output nodes")

    private var w: MatriD = null          // weight matrix between input and hidden layers
    private var v: MatriD = null          // weight matrix between hidden and output layers

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrices 'w and 'v' manually before training.
     *  @param w0   the initial weights for w
     *  @param v0   the initial weights for v
     */
    def setWeights (w0: MatriD, v0: MatriD) { w  = w0; v  = v0 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrices 'w' and 'v' randomly with a value in (0, 1)
     *  before training.
     *  @param i  the random number stream to use
     */
    def setWeights (i: Int = 0)
    {
        val rn = new Random (i)           // change i to get different random numbers
        w      = new MatrixD (n, h)
        v      = new MatrixD (h, p)
        for (i <- 0 until n; j <- 0 until h) w(i)(j) = rn.gen
        for (i <- 0 until h; j <- 0 until p) v(i)(j) = rn.gen
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the weight matrices 'w' and 'v'.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y.col(0)): NeuralNet =
    {
        // FIX - yy currently not used
        if (w == null) setWeights ()
        backProp ()
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the weight matrices 'w' and 'v'.
     */
//    def train (): NeuralNet = train (y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.  FIX
     *  @param yy   the response vector
     */
    def eval (yy: VectoD = y(0))                               // FIX - extend to matrices
    {
        e = yy - x * b                                         // compute residual/error vector e
//      diagnose (yy)                                          // compute diagnostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use back propagation to adjust the weight matrices 'w' and 'v' to make the
     *  predictions more accurate.  First adjust the 'v' weights (hidden to output layer)
     *  and then move back to adjust the 'w' weights (input to hidden layer).
     *  @see http://www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
     *  @see http://ufldl.stanford.edu/wiki/index.php/Backpropagation_Algorithm
     */
    def backProp ()
    {
        breakable { for (k <- 0 until MAX_ITER) {               // kth learning phase
           var sse = 0.0                                        // sum error over layers
           for (i <- 0 until m) hh(i) = _11 ++ sigmoidV (w * x(i))      // values for hidden layer
           sse += minimizeError (hh, y, v)                      // adjust v weights (hidden -> output)
           sse += minimizeError (x, hh, w)                      // adjust w weights (input  -> hidden)
           println ("weights for " + k + "th phase: w = " + w + "\nv = " + v)
           if (sse < EPSILON) break                             // break when error is small enough
        }} // for
    }  // backProp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Minimize the error in the prediction by adjusting the weight vector 'w'.
     *  The error 'eo' is simply the difference between the target value 'yi' and the
     *  predicted value 'zo'.  Minimize 1/2 of the dot product of error with itself
     *  using gradient-descent.
     *  @param xx  the effective input layer training data/matrix
     *  @param yy  the effective output layer training data/matrix
     *  @param ww  the weights between these two layers
     */
    def minimizeError (xx: MatriD, yy: MatriD, ww: MatriD): Double =
    {
        val _1  = new VectorD (yy.dim2); _1.set (1.0)  // one vector
        var sse = 0.0                                  // 1/2 sum of squared errors

        for (i <- 0 until m) {                         // for each example in training set
            val xi = xx(i)                             // ith input value (vector)
            val yi = yy(i)                             // ith target output value (vector)
            val zo = sigmoidV (ww * xi)                // ith predicted output value (vector)
            val eo = yi - zo                           // error = target - predicted
            val gr = outer (xi, eo * zo * (_1 - zo))   // -gradients of 1/2 dot product of error
            println (gr.dim1 + ", " + gr.dim2)
            w   += gr * eta                            // adjust w weights (input -> output)
            sse += .5 * (eo dot eo)
            if (DEBUG) println ("error eo = " + eo + " = [ yi = " + yi + " - zo = " + zo + " ]")
        } // for
        sse
    } // minimizeError

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (weight matrix 'w' and weight matrix 'v').
     *  FIX - make compatible with `Predictor`
     */
    def fit2: (MatriD, MatriD) = (w, v)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    override def fit: VectoD = new VectorD (0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = Seq ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input vector 'zi', predict the output/response vector 'zo'.
     *  For the hidden to output layer bias, prepend the hidden values with a one (_11).
     *  @param zi  the new input vector
     */
    def predictAll (zi: VectoD): VectoD = 
    {
        val zh = _11 ++ sigmoidV (w * zi)    // augmented hidden values
        sigmoidV (v * zh)                    // predicted output values
    } // predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input vector 'zi', predict the output/response scalar 'zo(0)'.
     *  May use this method if the output is one dimensional or interested in 1st value.
     *  @param zi  the new input vector
     */
    def predict (zi: VectoD): Double = predictAll (zi)(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several input vectors 'zi', predict the output/response vectors 'zo'.
     *  @param zi  the new input vectors (stored as rows in a matrix)
     */
    def predictAll (zi: MatriD): MatriD =
    {
        null            // FIX: to be implemented
    } // predictAll

} // NeuralNet class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNetTest` object is used to test the `NeuralNet` class.  For this
 *  test, the initial weights are used for used for prediction.
 */
object NeuralNetTest extends App
{
    val x   = new MatrixD (1, 3)                   // training data - input vectors (not used)
    val y   = new MatrixD (1, 3)                   // training data - output vectors (not used)
    val ann = new NeuralNet (x, y, 2)              // create a Neural Net

    val w  = new MatrixD ((2, 3), 0.0, 0.5, 0.0,   // weight matrix w (input to hidden layer)
                                  0.0, 0.5, 0.5)
    val v  = new MatrixD ((2, 3), 0.0, 0.5, 0.5,   // weight matrix v (hidden to output layer)
                                  0.0, 0.0, 0.0)
    ann.setWeights (w, v)                          // set initial weights and biases

    val zi = VectorD (1.0, 1.0, 1.0)               // predict output zo from input zi
    println ("input vector:  zi = " + zi)
    println ("output vector: zo = " + ann.predictAll (zi))

} // NeuralNetTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNetTest2` object is used to test the `NeuralNet` class.  For this
 *  test, training data is used to fit the weights before using them for prediction.
 *  @see http://www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 */
object NeuralNetTest2 extends App
{
    val x   = new MatrixD ((1, 3), 1.0, 0.35, 0.9)   // training data - input vectors
    val y   = new MatrixD ((1, 1), .5)               // training data - output vectors
    val ann = new NeuralNet (x, y, 2)                // create a Neural Net

    val w  = new MatrixD ((2, 3), 0.0, 0.1, 0.4,     // weight matrix w (input to hidden layer)
                                  0.0, 0.8, 0.6)
    val v  = new MatrixD ((1, 3), 0,0, 0.3, 0.9)     // weight matrix v (hidden to output layer)

    ann.setWeights (w, v)                            // set initial weights

    println ("input vector:  x(0) = " + x(0))
    println ("=== target output vector: y(0) = " + y(0))
    println ("--- initial output vector: zo = " + ann.predictAll (x(0)))

    ann.train ().eval ()                             // fit the weights using training data

    println ("+++ trained output vector: zo = " + ann.predictAll (x(0)))

} // NeuralNetTest2 object

