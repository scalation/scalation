
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Feb  8 11:43:38 EST 2012
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import math.exp

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class supports basic 3-layer (input, hidden and output) Neural Networks.
 *  Given several input and output vectors (training data), fit the weights
 *  connecting the layers, so that for a new input vector z_i, the net can predict
 *  the output vector z_o, i.e., z_i --> z_h = f (w * z_i) --> z_o = g (v * z_h)
 *  @param x  the input matrix (training data consisting of m input vectors)
 *  @param y  the output matrix (training data consisting of m output vectors)
 *  @param h  the number of neurons in the hidden layer
 */
class NeuralNet (x: MatrixD, y: MatrixD, h: Int)
      extends Predictor with Error
{
    private val m = x.dim1            // number of data points
    private val n = x.dim2            // dimensionality of the input
    private val p = y.dim2            // dimensionality of the output

    if (y.dim1 != m) flaw ("constructor", "dimensions of x and y are incompatible")

    private var w:  MatrixD = null     // weight matrix between input and hidden layers
    private var v:  MatrixD = null     // weight matrix between hidden and output layers
    private var wb: VectorD = null     // bias vector between input and hidden layers
    private var vb: VectorD = null     // bias vector between hidden and output layers

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the sigmoid function at t.
     *  @param t  the sigmoid function argument
     */
    def sigmoid (t: Double): Double = 1. / (1. + exp (-t))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of values of the sigmoid function applied to vector t.
     *  @param t  the sigmoid function vector argument
     */
    def sigmoid (t: VectorD): VectorD =
    {
        for (i <- 0 until t.dim) t(i) = 1. / (1. + exp (-t(i)))
        t
    } // sigmoid

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrices w and v.  This may be useful before training.
     *  @param w0   the initial weights for w
     *  @param v0   the initial weights for v
     *  @param wb0  the initial bias for wb
     *  @param vb0  the initial bias for vb
     */
    def setWeights (w0: MatrixD, v0: MatrixD, wb0: VectorD, vb0: VectorD)
    {
        w  = w0
        v  = v0
        wb = wb0
        vb = vb0
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x and y, fit the weight matrices w and v.
     */
    def train ()
    {
        if (w == null) {
            w = new MatrixD (n, h)
            v = new MatrixD (h, p)
        } // if
        // FIX: perform back propogation  (to be implemented)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (weigth matrix w, weigth matrix v, bias vector wb, bias vector vb)
     */
    def fit: Tuple4 [MatrixD, MatrixD, VectorD, VectorD] = (w, v, wb, vb)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input vector z_i, predict the output/response vector z_o.
     *  @param z_i  the new input vector
     */
    def predictAll (z_i: VectorD): VectorD =
    {
        val z_h = sigmoid (w * z_i + wb)          // hidden layer
        sigmoid (v * z_h + vb)                    // output layer
    } // predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input vector z_i, predict the output/response scalar z_o(0).
     *  May use this method if the output is one dimensional or interested in 1st value.
     *  @param z_i  the new input vector
     */
    def predict (z_i: VectorD): Double = predictAll (z_i)(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several input vectors z_i, predict the output/response vectors z_o.
     *  @param z_i  the new input vectors (stored as rows in a matrix)
     */
    def predictAll (z_i: MatrixD): MatrixD =
    {
        null            // FIX: to be implemented
    } // predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given several input vectors z_i, predict the output/response vector z_o(0).
     *  May use this method if the output is one dimensional or interested in 1st value.
     *  @param z_i  the new input vectors (stored as rows in a matrix)
     */
    def predict (z_i: MatrixD): VectorD = predictAll (z_i)(0)

} // NeuralNet class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the NeuralNet class.  For this test, the initial
 *  weights are used for used for prediction.
 */
object NeuralNetTest extends App
{
    val x = new MatrixD (1, 2)                 // training data - input vectors (not used)
    val y = new MatrixD (1, 2)                 // training data - output vectors (not used)
    val ann = new NeuralNet (x, y, 2)          // create a Neural Net

    val w = new MatrixD ((2, 2), 0.5, 0.0,     // weight matrix w (input to hidden layer)
                                 0.5, 0.5)
    val v = new MatrixD ((2, 2), 0.5, 0.5,     // weight matrix v (hidden to output layer)
                                 0.0, 0.0)
    val wb = new VectorD (0., 0.)              // bias vector wb (input to hidden layer)
    val vb = new VectorD (0., 0.)              // bias vector vb (hidden to output layer)
    ann.setWeights (w, v, wb, vb)              // set intial weights and biases

    val z_i = new VectorD (1., 1.)             // predict output z_o from input z_i
    println ("input vector:  z_i = " + z_i)
    println ("output vector: z_o = " + ann.predictAll (z_i))

} // NeuralNetTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the NeuralNet class.  For this test, training
 *  data is used to fit the weights before using them for prediction.
 */
object NeuralNetTest2 extends App
{
    val x = new MatrixD (10, 2)                // FIX: training data - input vectors
    val y = new MatrixD (10, 2)                // FIX: training data - output vectors
    val ann = new NeuralNet (x, y, 2)          // create a Neural Net

    ann.train ()                               // fit the weights/biases using training data
    println ("fit = " + ann.fit)

    val z_i = new VectorD (1., 1.)             // predict output z_o from input z_i
    println ("input vector:  z_i = " + z_i)
    println ("output vector: z_o = " + ann.predictAll (z_i))

} // NeuralNetTest2 object

