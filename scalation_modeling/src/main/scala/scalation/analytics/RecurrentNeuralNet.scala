
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Dong-Yu Yu, John Miller
 *  @version 1.6
 *  @date    Sat May  5 15:59:23 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.ListBuffer
import scala.io.Source.fromFile
import scala.io.StdIn
import scala.math.{exp, log, max, sqrt}

import scalation.linalgebra.{VectoD, VectorD, MatriD, MatrixD}
import scalation.random.RandomMatD

import ActivationFun._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RecurrentNeuralNet` class feeds input in sequential time into hidden layer.
 *  It uses parameter U, W, V in network.
 *  where U is parameter for input x, W is for hidden layer z, and V is for output y
 *  We have 'St = Activate (U dot x(t) + W dot x(t-1))' and
 *          'y(t) = softmax(V dot St)'
 *  @see github.com/pangolulu/rnn-from-scratch
 *----------------------------------------------------------------------------
 *  @param data_dim       the dimension of the data space
 *  @param hidden_dim     the dimension of the hidden layer
 *  @param bptt_truncate  truncate bptt, clip to constrain the dependcy to avoid gradient vanish/explode 
 */
class RecurrentNeuralNet (data_dim: Int, hidden_dim: Int, bptt_truncate: Int = 4)
      extends Error
{
     var rvm = RandomMatD (hidden_dim, data_dim, -sqrt (1.0 / data_dim), sqrt (1.0 / data_dim))
     val u = rvm.gen
     rvm = RandomMatD (hidden_dim, hidden_dim, -sqrt (1.0 / hidden_dim), sqrt (1.0 / hidden_dim))
     val w = rvm.gen
     rvm = RandomMatD (data_dim, hidden_dim, -sqrt (1.0 / data_dim), sqrt (1.0 / data_dim))
     val v = rvm.gen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward the input and generate several RNN layers. 
     *  @param x  the data input
     */
    def forward_propagation (x: VectoD) =
    {
        val layers = ListBuffer [RecurrentNeuralNetLayer] ()
        var prev_s = new VectorD (hidden_dim)

        for (t <- x.range) {
            val layer = new RecurrentNeuralNetLayer ()
            val in    = new VectorD (data_dim)
            in(x(t).toInt) = 1
            layer.forward (in, prev_s, u, w, v)
            prev_s  = new VectorD (layer.s)
            layers += layer
        } // for

        layers.toList
    } // forward_propagation

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the loss from the prediction of 'x' and 'label' by adding up the
     *  prediction loss among rnn layers.
     *  @param x      the input data
     *  @param label  the class labels (given ouput values) 
     */
    def calculate_loss (x: VectoD, label: VectoD): Double =
    {
        val output = new Softmax ()
        val layers = forward_propagation (x)
        var loss  = 0.0
        var count = 0

        for (layer <- layers) {
            loss  += output.loss (layer.mulv, label(count).toInt)
            count += 1
        } // for

       loss / label.dim.toDouble
    } // calculate_loss

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the total loss.
     *  @param x      the input data
     *  @param label  the class labels (given ouput values)
     */
    def calculate_total_loss (x: List [VectoD], label: List [VectoD]): Double =
    {
      var loss = 0.0
      for (i <- label.indices) loss += calculate_loss (x(i), label(i))
      loss / label.length.toDouble
    } // calculate_total_loss

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use back propogation through time 'bptt' to calculates dl/dV, dl/dU, dl/dW
     *  where l is the loss.
     *  @param x      the input data
     *  @param label  the class labels (given ouput values)
     */
    def bptt (x: VectoD, label: VectoD) =
    {
        val output   = new Softmax ()
        val layers   = forward_propagation (x)
        val dU       = new MatrixD (u.dim1, u.dim2)
        val dV       = new MatrixD (v.dim1, v.dim2)
        val dW       = new MatrixD (w.dim1, w.dim2)
        var prev_s_t = new VectorD (hidden_dim)
        var diff_s   = new VectorD (hidden_dim)

        for (t <- layers.indices) {
            var dmulv = output.diff (layers(t).mulv, label(t).toInt)
            var in    = new VectorD (data_dim)
            in (x(t).toInt) = 1
            var (dprev_s, dU_t, dW_t, dV_t) = layers(t).backward (in, prev_s_t, u, w, v, diff_s, dmulv)
            prev_s_t = new VectorD (layers(t).s)
            dmulv    = new VectorD (data_dim)

            for (i <- t-1 until max (-1, t-bptt_truncate-1)) {
                in = new VectorD (data_dim)
                in(x(t).toInt) = 1
                val prev_s_i   = if (i <= 0) new VectorD (hidden_dim) else new VectorD (layers(i - 1).s)
                val (dprev_st, dU_i, dW_i, dV_i) = layers(t).backward (in, prev_s_i, u, w, v, dprev_s, dmulv)
                dprev_s = dprev_st
                dU_t   += dU_i
                dW_t   += dW_i
            } // for

            dV += dV_t
            dU += dU_t
            dW += dW_t
        } // for

        (dU, dW, dV)
    } // bptt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Stochastic gradient descent step.
     *  @param x              the input data
     *  @param label          the class labels (given ouput values)
     *  @param learning rate  the learning rate (gradient multiplier)
     */
    def sgd_step (x: VectoD, label: VectoD, learning_rate: Double) =
    {
        val (dU, dW, dV) = bptt (x, label)
        u -= dU * learning_rate
        v -= dV * learning_rate
        w -= dW * learning_rate
    } // sgd_step

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model by iterating throught the training set by sgd and adjusting
     *  the learning rate.
     *  @param x                the input data
     *  @param label            the class labels (given ouput values)
     *  @param rate             the initial learning rate (gradient multiplier)
     *  @param nepoch           number of epoch
     *  @param eval_loss_after  number of epoch to conduct evaluation
     */
    def train (x: List [VectoD], label: List [VectoD], rate: Double = 500.0,
               nepoch: Int, eval_loss_after: Int = 5)
    {
        var num_examples_seen = 0
        var losses            = ListBuffer [(Int, Double)] ()
        var learning_rate     = rate

        for (epoch <- 0 until nepoch) {
            if (epoch % eval_loss_after == 0) {
                val loss = calculate_total_loss (x, label)
                losses  += Tuple2 (num_examples_seen, loss)
                println (s"Loss after num_examples_seen = $num_examples_seen, epoch = $epoch : $loss")

                if (losses.length > 1 && losses(epoch)._2 > losses(epoch-1)._2) {
                    learning_rate *= 0.5
                    println (s"Setting learning rate to $learning_rate")
                } // if
            } // if

            for (i <- label.indices) {
                sgd_step (x(i), label(i), learning_rate)
                num_examples_seen += 1
            } // for
        } // for
    } // train

} // RecurrentNeuralNet class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tanh` class implements Tanh and derivative for vector version
 */
class Tanh
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute tanh on the vector input.
     *  @param input  the input vector
     */
    def forward (input: VectoD): VectoD = ActivationFun.tanhV (input)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative correponding to the input.
     *  @param input     the input vector
     *  @param top_diff  dl/dz where l is the loss, z is the output
     */
    def backward (input: VectoD, top_diff: VectoD): VectoD =
    {
        val output = forward (input)
        top_diff * output.map ((a: Double) => 1.0 - a*a)
    } // backward

} // Tanh class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** MultiplyGate is to perfrom dot product from input and weights 'w'.
 */
case class MultiplyGate ()
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform dot product with matrix W and Vector input.
     *  @param w      the weight matrix
     *  @param input  the input vector
     */
    def forward (w: MatriD, input: VectoD): VectoD = w.t dot input

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate derivative corresponding to the W and input 
     *  @param w      the weight matrix
     *  @param input  the input vector
     *  @param dz     dl/dz where l is the loss
     */
    def backward (w: MatriD, input:VectoD, dz: VectoD): (MatriD, VectoD) =
    {
        val dW = (MatrixD ((1, dz.dim),    (for (i <- dz.range) yield dz(i)): _*).mdot
                 (MatrixD ((1, input.dim), (for (i <- input.range) yield input(i)): _*)))
        val dinput = w dot dz
        (dW, dinput)
    } // backward

} // MultiplyGate class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Perform Add for vectors.
 */
case class AddGate ()
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform add with vector 'x' and Vector 'y'.
     *  @param x  vector x for operation add 
     *  @param y  vector y for operation add 
     */
    def forward (x: VectoD, y: VectoD): VectoD = x + y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate derivative according to the input 'x' and 'y'.  
     *  @param x   vector x for operation add 
     *  @param y   vector y for operation add 
     *  @param dz  dl/dz where l is the loss
     */
    def backward (x: VectoD, y: VectoD, dz: VectoD): (VectoD, VectoD) =
    {
/*
        var dx = new MatrixD (x.dim1, x.dim2)
        for (i <- dx.range1; j <- dx.range2) dx(i,j) = 1
        var dy = new MatrixD(y.dim1, y.dim2)
        for (i <- dy.range1; j <- dy.range2) dy(i,j) = 1
        dx = dz * dx
        dy = dz * dy
        (dx, dy)
*/
        (dz, dz)                                   // FIX
    } // backward

} // AddGate class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Softmax class calculate softmax regularization for the input
 */
class Softmax
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate softmax for the input.
     *  @param input  the vector input for softmax (vector sum = 1)
     */
    def predict (input: VectoD): VectoD =
    {
        val exp_scores = input.map (exp (_))
        exp_scores / exp_scores.sum
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate loss between the vector input and the label index.
     *  @param input  the data input
     *  @param index  class labels for input
     */
    def loss (input: VectoD, index: Int): Double =
    {
        val probs = predict (input)
        - log (probs(index))    
    } // loss

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the difference by first predict the input and deduct the position of index.
     *  @param input  the data input
     *  @param index  class labels for input
     */
    def diff (input: VectoD, index: Int): VectoD =
    {
        var probs     = predict (input)
        probs(index) -= 1.0
        probs
    } // diff

} // Softmax class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RecurrentNeuralNetLayer` is a 3-layer where x denotes the input,
 *  'y 'denotes the output and 's' is the intermediate/hidden value.
 *  We have 'St   = Activate (U dot x(t) + W dot x(t-1))' and
 *          'y(t) = softmax(V dot St)'.
 */
class RecurrentNeuralNetLayer
{
    private val mulGate    = MultiplyGate ()
    private val addGate    = AddGate ()
    private val activation = new Tanh ()

    var mulu: VectoD = _                          // FIX - should make private
    var mulw: VectoD = _
    var add: VectoD  = _
    var s: VectoD    = _
    var mulv: VectoD = _

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward the x into the RecurrentNeuralNet layer.
     *  We have St   = Activate (U dot x(t) + W dot x(t-1))
     *          y(t) = softmax(V dot St)
     *  @param x       the input data
     *  @param prev_s  record the previous hidden layer value  
     *  @param u       parameter for input x 
     *  @param w       parameter for hidden layer z
     *  @param v       parameter for output
     */
    def forward (x: VectoD, prev_s: VectoD, u: MatriD, w: MatriD, v: MatriD)
   {
        mulu = mulGate.forward (u, x)
        mulw = mulGate.forward (w, prev_s)
        add  = addGate.forward (mulw, mulu)
        s    = activation.forward (add)
        mulv = mulGate.forward (v, s)
    } // forward

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the derivate regarding to prev_s , U, W, V by backward of each unit
     *  @param x       the input data
     *  @param prev_s  record the previous hidden layer value  
     *  @param u       parameter for input x 
     *  @param w       parameter for hidden layer z
     *  @param v       parameter for output
     *  @param diff_s  diff_s = ds(t+1)/ ds (t)
     *  @param dmulv   dl/dmulv where l is the loss, mulv = V dot s
     */
    def backward (x: VectoD, prev_s: VectoD, u: MatriD, w: MatriD, v: MatriD, diff_s: VectoD,
                  dmulv: VectoD) =
    {
        forward (x, prev_s, u, w, v)
        val (dV, dsv: VectoD) = mulGate.backward (v, s, dmulv)
        val ds                = dsv + diff_s
        val dadd              = activation.backward (add, ds)
        val (dmulw, dmulu)    = addGate.backward (mulw, mulu, dadd)
        val (dW, dprev_s)     = mulGate.backward (w, prev_s, dmulw)
        val (dU, dx)          = mulGate.backward (u, x, dmulu)
        (dprev_s, dU, dW, dV)
    } // backward

} // RecurrentNeuralNetLayer class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RecurrentNeuralNetTest` object is used to test the `RecurrentNeuralNet` class.
 *  > runMain scalation.analytics.RecurrentNeuralNetTest
 */
object RecurrentNeuralNetTest extends App
{
    // read the input file  
    val file           = BASE_DIR + "RNNtrain.csv"
    val sp             = ','                                             // character separating the values
    val linesInput     = fromFile (file).getLines.toArray                // get the lines from file
    val num_linesInput = linesInput.length

    var x = ListBuffer [VectoD] ()
    for (i <- 0 until num_linesInput) {
        var l = linesInput(i).split (sp)
        l     = l.filterNot (name => name.contains ("[") || name.contains ("]"))
        x    += VectorD (l.map (_.toDouble))
    } // for
    val xList = x.toList

    // read the label
    val fileY     = BASE_DIR + "RNNlabel.csv"
    val lines     = fromFile (fileY).getLines.toArray                    // get the lines from file
    val num_lines = lines.length

    var label = ListBuffer [VectoD] ()
    for (i <- 0 until num_lines) {
        var l = lines(i).split (sp)
        l = l.filterNot (name => (name contains "[") || (name contains "]"))
        label += VectorD (l.map (_.toDouble))
    } // for
    val labelList = label.toList

    val data_dim   = 8000
    val hidden_dim = 100

    var rnn    = new RecurrentNeuralNet (data_dim, hidden_dim)
    val losses = rnn.train (xList.slice (0, 10), labelList.slice (0, 10), nepoch = 10, eval_loss_after = 1)

} // RecurrentNeuralNetTest object

