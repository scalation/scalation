
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Jan 21 14:43:50 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://page.mi.fu-berlin.de/rojas/neural/chapter/K7.pdf
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics

import math.exp

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.random.Random
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

    println ("Create a Neural Net with " + n + " input, " +
                                           h + " hidden, " + 
                                           p + " output nodes")

    private var w:  MatrixD = null     // weight matrix between input and hidden layers
    private var v:  MatrixD = null     // weight matrix between hidden and output layers
    private var wb: VectorD = null     // bias vector between input and hidden layers
    private var vb: VectorD = null     // bias vector between hidden and output layers

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the sigmoid function at t.
     *  @param t  the sigmoid function argument
     */
    def sigmoid (t: Double): Double = 1.0 / (1.0 + exp (-t))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of values of the sigmoid function applied to vector t.
     *  @param t  the sigmoid function vector argument
     */
    def sigmoid (t: VectorD): VectorD =
    {
        for (i <- 0 until t.dim) t(i) = 1.0 / (1.0 + exp (-t(i)))
        t
    } // sigmoid

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrices w and v manually before training.
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
    /** Set the initial weight matrices w and v randomly with a value in (0, 1) before
     *  training.
     *  @param i  the random number stream to use
     */
    def setWeights (i: Int = 0)
    {
        val rn = new Random (i)          // change i to get different random numbers
        w  = new MatrixD (n, h)
        v  = new MatrixD (h, p)
        wb = new VectorD (h)
        vb = new VectorD (p)
        for (i <- 0 until n; j <- 0 until h) w(i)(j) = rn.gen
        for (i <- 0 until h; j <- 0 until p) v(i)(j) = rn.gen
//      for (i <- 0 until h) wb(i) = rn.gen
//      for (i <- 0 until p) vb(i) = rn.gen
    } // setWeights

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x and y, fit the weight matrices w and v.
     */
    def train ()
    {
        if (w == null) setWeights ()
        backProp ()
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use back propogation to adjust the weight matrices w and v to make the
     *  predictions more accurate.  The implementation uses vector operations.
     *  @see http://www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
     */
    def backProp ()
    {
        val eta = 1.0                                       // the learning rate
        val _1h = new VectorD (h); _1h.set (1.0)            // one vector for hidden layer
        val _1o = new VectorD (p); _1o.set (1.0)            // one vector for output layer
        val y_h = new VectorD (h)                          // predicted hidden layer value
        val y_o = new VectorD (p)                          // predicted output layer value
 
        for (i <- 0 until m) {                             // each example in training set
            val x_i = x(i)                                 // ith input value (vector)
            val y_i = y(i)                                 // ith target output value (vector)

            for (k <- 0 until h) y_h(k) = sigmoid ((w.col(k) dot x_i) + wb(k))  // predicted at hidden layer
            for (k <- 0 until p) y_o(k) = sigmoid ((v.col(k) dot y_h) + vb(k))  // predicted at output layer

            println ("y_h = " + y_h)
            println ("y_o = " + y_o)

            val e_o = y_i - y_o                            // error at output layer
            val d_o = y_o * (_1o - y_o) * e_o              // delta for output layer
            for (k <- 0 until h) v(k) += d_o * y_h * eta   // adjust v weights (hidden -> output)

            println ("e_o = " + e_o)
            println ("d_o = " + d_o)
            println ("v = " + v)

            val e_h = v * d_o                              // weighted error at hidden layer
            val d_h = y_h * (_1h - y_h) * e_h              // delta for hidden layer
            for (k <- 0 until h) w(k) += d_h * x_i * eta   // adjust w weights (input -> hidden)

            println ("e_h = " + e_h)
            println ("d_h = " + d_h)
            println ("w = " + w)
        } // for
    } // backProp

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
        val z_h = new VectorD (h)
        val z_o = new VectorD (p)

        for (k <- 0 until h) z_h(k) = sigmoid ((w.col(k) dot z_i) + wb(k))   // hidden layer
        for (k <- 0 until p) z_o(k) = sigmoid ((v.col(k) dot z_h) + vb(k))   // output layer
        z_o
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
    val x   = new MatrixD (1, 2)               // training data - input vectors (not used)
    val y   = new MatrixD (1, 2)               // training data - output vectors (not used)
    val ann = new NeuralNet (x, y, 2)          // create a Neural Net

    val w  = new MatrixD ((2, 2), 0.5, 0.0,     // weight matrix w (input to hidden layer)
                                  0.5, 0.5)
    val v  = new MatrixD ((2, 2), 0.5, 0.5,     // weight matrix v (hidden to output layer)
                                  0.0, 0.0)
    val wb = new VectorD (0.0, 0.0)              // bias vector wb (input to hidden layer)
    val vb = new VectorD (0.0, 0.0)              // bias vector vb (hidden to output layer)
    ann.setWeights (w, v, wb, vb)              // set intial weights and biases

    val z_i = new VectorD (1.0, 1.0)             // predict output z_o from input z_i
    println ("input vector:  z_i = " + z_i)
    println ("output vector: z_o = " + ann.predictAll (z_i))

} // NeuralNetTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the NeuralNet class.  For this test, training
 *  data is used to fit the weights before using them for prediction.
 *  @see http://www4.rgu.ac.uk/files/chapter3%20-%20bp.pdf
 */
object NeuralNetTest2 extends App
{
    val x   = new MatrixD ((1, 2), .35, .9)    // training data - input vectors
    val y   = new MatrixD ((1, 1), .5)         // training data - output vectors
    val ann = new NeuralNet (x, y, 2)          // create a Neural Net

    val w  = new MatrixD ((2, 2), 0.1, 0.4,    // weight matrix w (input to hidden layer)
                                  0.8, 0.6)
    val v  = new MatrixD ((2, 1), 0.3, 0.9)    // weight matrix v (hidden to output layer)

    val wb = new VectorD (0.0, 0.0)              // bias vector wb (input to hidden layer)
    val vb = new VectorD (0.0, 0.0)              // bias vector vb (hidden to output layer)
    ann.setWeights (w, v, wb, vb)              // set intial weights and biases

    println ("input vector:  x(0) = " + x(0))
    println ("=== target output vector: y(0) = " + y(0))
    println ("--- initial output vector: z_o = " + ann.predictAll (x(0)))

    ann.train ()                               // fit the weights/biases using training data

    println ("+++ trained output vector: z_o = " + ann.predictAll (x(0)))

} // NeuralNetTest2 object

