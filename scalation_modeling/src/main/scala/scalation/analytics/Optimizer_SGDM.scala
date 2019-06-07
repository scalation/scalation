
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Sun Jan 27 15:34:08 EST 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectorI, VectoD, VectorD}
import scalation.random.PermutedVecI
import scalation.random.RNGStream.ranStream

import ActivationFun._
import Initializer._
import Optimizer._
import StoppingRule._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Optimizer_SGDM` object provides functions to optimize the parameters/weights
 *  of Neural Networks with various numbers of layers.
 *  This optimizer used Stochastic Gradient Descent with Monentum.
 */
object Optimizer_SGDM
{
    private val DEBUG = false                                            // debug flag
    private val BETA  = 0.9                                              // momentum decay hyper-parameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y' for a 2-layer, single output Neural Network, fit
     *  the parameter/weight vector 'b'.  Select the best learning rate within the
     *  interval 'etaI'.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m output vector (training data consisting of m output vectors)
     *  @param b          the nx parameter/weight vector for layer 1->2 (input to output)
     *  @param etaI       the learning/convergence rate interval
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f1         the activation function family for layers 1->2 (input to output)
     */
    def optimizeI (x: MatriD, y: VectoD,
                   b: VectoD,
                   etaI: PairD,
                   bSize: Int     = hp.default ("bSize").toInt,
                   maxEpochs: Int = hp.default ("maxEpochs").toInt,
                   f1: AFF = f_sigmoid): (Double, Int) =
    {
        println (s"optimizeI: etaI = $etaI")
        var best = (Double.MaxValue, -1)
        var b_best: VectoD = null

        for (i <- 0 to NSTEPS) {
            val step = (etaI._2 - etaI._1) / NSTEPS                      // compute step size
            val eta  = etaI._1 + i * step                                // current learning rate
            b.set (weightVec (b.dim)())                                  // randomly assign weights

            val result = optimize (x, y, b, eta, bSize, maxEpochs, f1)
            println (s"optimizeI: eta = $eta, result = $result")
            if (result._1 < best._1) {
                best = result
                b_best = b.copy                                          // save best weights
            } // if
        } // for

        b.set (b_best ())                                                // use best weights
        best
    } // optimizeI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y' for a 2-layer, single output Neural Network, fit
     *  the parameter/weight vector 'b'.  Iterate over several epochs, where each epoch divides
     *  the training set into 'nB' batches.  Each batch is used to update the weights.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m output vector (training data consisting of m output vectors)
     *  @param b          the nx parameter/weight vector for layer 1->2 (input to output)
     *  @param eta_       the initial learning/convergence rate
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f1         the activation function family for layers 1->2 (input to output)
     */
    def optimize (x: MatriD, y: VectoD,
                  b: VectoD,
                  eta_ : Double  = hp.default ("eta"),
                  bSize: Int     = hp.default ("bSize").toInt,
                  maxEpochs: Int = hp.default ("maxEpochs").toInt,
                  f1: AFF = f_sigmoid): (Double, Int) =
    {
        val idx     = VectorI.range (0, x.dim1)                          // instance index range
        val permGen = PermutedVecI (idx, ranStream)                      // permutation vector generator
        val nB      = x.dim1 / bSize                                     // the number of batches
        val stop    = new StoppingRule ()                                // rule for stopping early
        var eta     = eta_                                               // set initial learning rate
        var mo      = new VectorD (b.dim)                                // momentum vector

        println (s"optimize: bSize = $bSize, nB = $nB, eta = $eta")

        for (epoch <- 1 to maxEpochs) {                                  // iterate over each epoch
            val batches = permGen.igen.split (nB)                        // permute indices & split into nB batches

            for (ib <- batches) b -= updateWeight (x(ib), y(ib))         // iteratively update weight vector b

            val sse = sseF (y, f1.fV (x * b))                            // recompute sum of squared errors
            if (DEBUG) println (s"optimize: parameters for $epoch th epoch: sse = $sse")
            val (b_best, sse_best) = stop.stopWhen (b, sse)
            if (b_best != null) {
                b.set (b_best())
                return (sse_best, epoch - UP_LIMIT)
            } // if
            if (epoch % ADJUST_PERIOD == 0) eta *= ADJUST_FACTOR         // adjust the learning rate
        } // for

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Compute the parameter/weight vector 'b' update based on the current batch.
         *  A step in the direction opposite to the gradient.
         *  @param x  the input matrix for the current batch
         *  @param y  the output vector for the current batch
         */
        def updateWeight (x: MatriD, y: VectoD): VectoD =
        {
            val yp = f1.fV (x * b)                                       // yp = f(Xb)
            val e  = yp - y                                              // negative of the error vector
            val d  = f1.dV (yp) * e                                      // delta vector for y

            val eta_o_sz = eta / x.dim1                                  // eta over the current batch size
            val bup = x.t * d * eta_o_sz                                 // gradient-based change in input-output weights
            mo = mo * BETA + bup                                         // update momentum
            mo                                                           // return momentum
        } // updateWeight

        if (DEBUG) println (s"optimize: parameters b = $b")
        (sseF (y, f1.fV (x * b)), maxEpochs)                             // return sse and number of epochs
    } // optimize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y' for a 2-layer, multi-output Neural Network, fit
     *  the parameter/weight vector 'b'.  Select the best learning rate within the
     *  interval 'etaI'.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m-by-by output matrix (training data consisting of m output vectors)
     *  @param b          the parameters with nx-by-ny weight matrix for layer 1->2 (input to output)
     *  @param etaI       the learning/convergence rate interval
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f1         the activation function family for layers 1->2 (input to output)
     */
    def optimize2I (x: MatriD, y: MatriD,
                    b: NetParam,
                    etaI: PairD,
                    bSize: Int     = hp.default ("bSize").toInt,
                    maxEpochs: Int = hp.default ("maxEpochs").toInt,
                    f1: AFF = f_sigmoid): (Double, Int) =
    {
        println (s"optimize2I: etaI = $etaI")
        var best = (Double.MaxValue, -1)
        var b_best: NetParam = null

        for (i <- 0 to NSTEPS) {
            val step = (etaI._2 - etaI._1) / NSTEPS                      // compute step size
            val eta  = etaI._1 + i * step                                // current learning rate
            b.set (weightMat (b.w.dim1, b.w.dim2))                       // randomly assign weights to b

            val result = optimize2 (x, y, b, eta, bSize, maxEpochs, f1)
            println (s"optimize2I: eta = $eta, result = $result")
            if (result._1 < best._1) {
                best = result
                b_best = b.copy                                          // save best weights
            } // if
        } // for

        b.set (b_best)                                                   // use best weights
        best
    } // optimize2I

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y' for a 2-layer, multi-output Neural Network, fit
     *  the parameter/weight matrix 'b'.  Iterate over several epochs, where each epoch
     *  divides the training set into 'nB' batches.  Each batch is used to update the
     *  the parameter's weights.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param b          the parameters with nx-by-ny weight matrix for layer 1->2 (input to output)
     *  @param eta_       the initial learning/convergence rate
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f1         the activation function family for layers 1->2 (input to output)
     */
    def optimize2 (x: MatriD, y: MatriD,
                   b: NetParam,
                   eta_ : Double  = hp.default ("eta"),
                   bSize: Int     = hp.default ("bSize").toInt,
                   maxEpochs: Int = hp.default ("maxEpochs").toInt,
                   f1: AFF = f_sigmoid): (Double, Int) =
    {
        val idx     = VectorI.range (0, x.dim1)                          // instance index range
        val permGen = PermutedVecI (idx, ranStream)                      // permutation vector generator
        val nB      = x.dim1 / bSize                                     // the number of batches
        var eta     = eta_                                               // set initial learning rate
        val stop    = new StoppingRule ()                                // rule for stopping early
        var mo      = new MatrixD (b.w.dim1, b.w.dim2)                   // momentum matrix
        println (s"optimize2: bSize = $bSize, nB = $nB")

        for (epoch <- 1 to maxEpochs) {                                  // iterate over each epoch
            val batches = permGen.igen.split (nB)                        // permute indices & split into nB batches

            for (ib <- batches) b -= updateWeight (x(ib), y(ib))         // iteratively update weight matrix b

            val sse = sseF (y, f1.fM (b * x))                            // recompute sum of squared errors
            if (DEBUG) println (s"optimize2: parameters for $epoch th epoch: sse = $sse")
            val (b_best, sse_best) = stop.stopWhen (Array (b), sse)
            if (b_best != null) {
                b.set (b_best (0))
                return (sse_best, epoch - UP_LIMIT)
            } // if

            if (epoch % ADJUST_PERIOD == 0) eta *= ADJUST_FACTOR         // adjust the learning rate
        } // for

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Update the parameter/weight matrix 'b' based on the current batch.
         *  Take a step in the direction opposite to the gradient.
         *  @param x  the input matrix for the current batch
         *  @param y  the output matrix for the current batch
         */
        def updateWeight (x: MatriD, y: MatriD): MatriD =
        {
            val yp = f1.fM (b * x)                                       // Yp = f(XB)
            val ee = yp - y                                              // negative of the error matrix
            val d  = f1.dM (yp) ** ee                                    // delta matrix for y

            val eta_o_sz = eta / x.dim1                                  // eta over the current batch size
            val bup = x.t * d * eta_o_sz                                 // gradient-based change in input-output weights
            mo = mo * BETA + bup                                         // update momentum
            mo                                                           // return momentum
        } // updateWeight

        if (DEBUG) println (s"optimize2: parameters b = $b")
        (sseF (y, f1.fM (b * x)), maxEpochs)                             // return number of epochs
    } // optimize2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y' for a 3-layer Neural Network, fit the parameters
     *  (weights and biases) 'a' & 'b'.  Select the best learning rate within the interval 'etaI'.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param a          the parameters with nx-by-nz weight matrix for layer 1->2 (input to hidden)
     *  @param b          the parameters with nx-by-ny weight matrix for layer 1->2 (input to output)
     *  @param etaI       the learning/convergence rate interval
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f1         the activation function family for layers 0->1 (input to hidden)
     *  @param f2         the activation function family for layers 1->2 (hidden to output)
     */
    def optimize3I (x: MatriD, y: MatriD,
                    a: NetParam, b: NetParam,
                    etaI: PairD,
                    bSize: Int     = hp.default ("bSize").toInt,
                    maxEpochs: Int = hp.default ("maxEpochs").toInt,
                    f1: AFF = f_sigmoid, f2: AFF = f_lreLU): (Double, Int) =
    {
        println (s"optimize3I: etaI = $etaI")
        var best = (Double.MaxValue, -1)
        var a_best, b_best: NetParam = null

        for (i <- 0 to NSTEPS) {
            val step = (etaI._2 - etaI._1) / NSTEPS                      // compute step size
            val eta  = etaI._1 + i * step                                // current learning rate
            a.set (weightMat (a.w.dim1, a.w.dim2),                       // randomly assign weights to a.w
                   weightVec (a.b.dim))                                  // randomly assign biases to a.b
            b.set (weightMat (b.w.dim1, b.w.dim2),                       // randomly assign weights to b.w
                   weightVec (b.b.dim))                                  // randomly assign biases to b.b

            val result = optimize3 (x, y, a, b, eta, bSize, maxEpochs, f1, f2)
            println (s"optimize3I: eta = $eta, result = $result")
            if (result._1 < best._1) {
                best = result
                a_best = a.copy; b_best = b.copy                         // save best weights
            } // if
        } // for

        a.set (a_best); b.set (b_best)                                   // use best weights
        best
    } // optimize3I

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y' for a 3-layer Neural Network, fit the parameters
     *  (weights and biases) 'a' & 'b'.  Iterate over several epochs, where each epoch divides
     *  the training set into 'nB' batches.  Each batch is used to update the weights.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param a          the parameters with nx-by-nz weight matrix & nz bias vector for layer 0->1
     *  @param b          the parameters with nz-by-ny weight matrix & ny bias vector for layer 1->2
     *  @param eta_       the initial learning/convergence rate
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f1         the activation function family for layers 1->2 (input to hidden)
     *  @param f2         the activation function family for layers 2->3 (hidden to output)
     */
    def optimize3 (x: MatriD, y: MatriD,
                   a: NetParam, b: NetParam,
                   eta_ : Double  = hp.default ("eta"),
                   bSize: Int     = hp.default ("bSize").toInt,
                   maxEpochs: Int = hp.default ("maxEpochs").toInt,
                   f1: AFF = f_sigmoid, f2: AFF = f_lreLU): (Double, Int) =
    {
        val idx     = VectorI.range (0, x.dim1)                          // instance index range
        val permGen = PermutedVecI (idx, ranStream)                      // permutation vector generator
        val nB      = x.dim1 / bSize                                     // the number of batches
        val stop    = new StoppingRule ()                                // rule for stopping early
        var eta     = eta_                                               // counter for number of times moving up
        var moa     = new MatrixD (a.w.dim1, a.w.dim2)                   // momentum matrix a
        var mob     = new MatrixD (b.w.dim1, b.w.dim2)                   // momentum matrix b

        println (s"optimize3: bSize = $bSize, nB = $nB")

        for (epoch <- 1 to maxEpochs) {                                  // iterate over each epoch
            val batches = permGen.igen.split (nB)                        // permute indices & split into nB batches

            for (ib <- batches) {
                val ab = updateWeight (x(ib), y(ib))                     // iteratively update weight matrices a & b
                a -= ab._1; b -= ab._2
            } // for

            val sse = sseF (y, b * f2.fM (f1.fM (a * x)))
            if (DEBUG) println (s"optimize3: parameters for $epoch th epoch: sse = $sse")
            val (b_best, sse_best) = stop.stopWhen (Array (a, b), sse)
            if (b_best != null) {
                a.set (b_best(0))
                b.set (b_best(1))
                return (sse_best, epoch - UP_LIMIT)
            } // if

            if (epoch % ADJUST_PERIOD == 0) eta *= ADJUST_FACTOR         // adjust the learning rate
        } // for

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Compute the parameter/weight matrices 'a' and 'b' updates based on the current batch.
         *  A step in the direction opposite to the gradient.
         *  @param x  the input matrix for the current batch
         *  @param y  the output matrix for the current batch
         */
        def updateWeight (x: MatriD, y: MatriD): (NetParam, NetParam) =
        {
            var z  = f1.fM (a * x)                                       // Z  = f(XA)
            var yp = f2.fM (b * z)                                       // Yp = f(ZB)
            var ee = yp - y                                              // negative of the error matrix
            val d1 = f2.dM (yp) ** ee                                    // delta matrix for y
            val d0 = f1.dM (z) ** (d1 * b.w.t)                           // delta matrix for z
    
            val eta_o_sz = eta / x.dim1                                  // eta over current batch size
            moa = moa * BETA + x.t * d0 * eta_o_sz                       // update momentum a
            mob = mob * BETA + z.t * d1 * eta_o_sz                       // update momentum b
            (NetParam (moa, d0.mean * eta),                              // change to 'a' paramters (weights and biases)
             NetParam (mob, d1.mean * eta))                              // change to 'b' paramters (weights and biases)
        } // updateWeight

        if (DEBUG) println (s"optimize3: parameters a = $a \n b = $b")
        (sseF (y, b * f2.fM (f1.fM (a * x))), maxEpochs)                 // return sse and number of epochs
    } // optimize3

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', for a multi-hidden layer Neural Network, fit the
     *  parameter array 'b', where each 'b(l)' contains a weight matrix and bias vector.
     *  Select the best learning rate within the  interval 'etaI'.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param b          the array of parameters (weights & bias) between every two adjacent layers
     *  @param etaI       the lower and upper bounds of learning/convergence rate
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f          the array of activation function family for every two adjacent layers
     */
    def optimizeXI (x: MatriD, y: MatriD,
                    b: NetParams,
                    etaI: PairD,
                    bSize: Int     = hp.default ("bSize").toInt,
                    maxEpochs: Int = hp.default ("maxEpochs").toInt,
                    lambda: Double = 0.0,
                    f: Array [AFF] = Array (f_sigmoid, f_sigmoid, f_lreLU)): (Double, Int) =
    {
        println (s"optimizeXI: etaI = $etaI")
        var best = (Double.MaxValue, -1)
        var b_best: NetParams = null

        for (i <- 0 to NSTEPS) {
            val step = (etaI._2 - etaI._1) / NSTEPS                      // compute step size
            val eta  = etaI._1 + i * step                                // current learning rate
            for (b_l <- b) b_l.set (weightMat (b_l.w.dim1, b_l.w.dim2),  // randomly assign weights to b_l.w
                                    weightVec (b_l.b.dim))               // randomly assign biases to b_l.b


            val result = optimizeX (x, y, b, eta, bSize, maxEpochs, lambda, f)
            println (s"optimizeXI: eta = $eta, result = $result")
            if (result._1 < best._1) {
                best = result
                b_best = for (l <- b.indices) yield b(l).copy            // save best parameters
            } // if
        } // for

        for (l <- b.indices) b(l).set (b_best(l))                        // use best parameters
        best
    } // optimizeXI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter/weight matrices 'bw' and
     *  bias vectors 'bi'. Iterate over several epochs, where each epoch divides the
     *  training set into 'nB' batches. Each batch is used to update the weights.
     *  @param x          the m-by-nx input matrix (training data consisting of m input vectors)
     *  @param y          the m-by-ny output matrix (training data consisting of m output vectors)
     *  @param b          the array of parameters (weights & bias) between every two adjacent layers
     *  @param eta_       the initial learning/convergence rate
     *  @param bSize      the batch size
     *  @param maxEpochs  the maximum number of training epochs/iterations
     *  @param f          the array of activation function family for every two adjacent layers
     */
    def optimizeX (x: MatriD, y: MatriD,
                   b: NetParams,
                   eta_ : Double  = hp.default ("eta"),
                   bSize: Int     = hp.default ("bSize").toInt,
                   maxEpochs: Int = hp.default ("maxEpochs").toInt,
                   lambda: Double = 0.0,
                   f: Array [AFF] = Array (f_sigmoid, f_sigmoid, f_lreLU)): (Double, Int) =
    {
        val idx     = VectorI.range (0, x.dim1)                          // instance index range
        val permGen = PermutedVecI (idx, ranStream)                      // permutation vector generator
        val nB      = x.dim1 / bSize                                     // the number of batches
        val stop    = new StoppingRule ()                                // rule for stopping early
        var eta     = eta_                                               // counter for number of times moving up
        var sse     = 0.0                                                // stores accumulated sse over batches for epoch
        println (s"optimizeX: bSize = $bSize, nB = $nB")

        val nl     = f.size                                              // number of layers
        val layers = 0 until nl                                          // range for layers
        val z      = Array.ofDim [MatriD] (nl+1)                         // array to store activations, layer by layer
        val d      = Array.ofDim [MatriD] (nl)                           // array to store all deltas
        var mo     = Array.ofDim [MatriD] (nl)                           // momentum array
        for (l <- layers) mo(l) = new MatrixD (b(l).w.dim1, b(l).w.dim2)

        for (epoch <- 1 to maxEpochs) {                                  // iterate over each epoch
            sse         = 0.0
            val batches = permGen.igen.split (nB)                        // permute indices &split into nB batches

            for (ib <- batches) sse += updateWeight (x(ib), y(ib))       // update parameter array b

            if (DEBUG) println (s"optimizeX: parameters for $epoch th epoch: b = $b, sse = $sse")
            val (b_best, sse_best) = stop.stopWhen (b, sse)
            if (b_best != null) {
                for (l <- b.indices) b(l).set (b_best(l))
                return (sse_best, epoch - UP_LIMIT)
            } // if

            if (epoch % ADJUST_PERIOD == 0) eta *= ADJUST_FACTOR         // adjust the learning rate
        } // for

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Compute the parameter array 'b' updates based on the current batch.
         *  A step in the direction opposite to the gradient.
         *  @param x  the input matrix for the current batch
         *  @param y  the output matrix for the current batch
         */
        def updateWeight (x: MatriD, y: MatriD): Double =
        {
            z(0)   = x                                                   // initial activation, which is the input matrix
            for (l <- layers) z(l+1) = f(l).fM (b(l) * z(l))             // feedforward and store all activations

            val yp  = z.last                                             // predicted value of y
            val ee  = yp - y                                             // negative of the error matrix
            d(nl-1) = f.last.dM (yp) ** ee                               // delta for the last layer before output
            for (l <- nl-2 to 0 by -1)
                d(l) = f(l).dM (z(l+1)) ** (d(l+1) * b(l+1).w.t)         // deltas for all previous hidden layers

            val eta_o_sz = eta / x.dim1                                  // learning rate divided by size of mini-batch
            for (l <- layers) {
//              b(l).w *= 1.0 - eta * (lambda / x.dim1)                  // regularization factor, weight decay
                mo(l) = mo(l) * BETA + z(l).t * d(l) * eta_o_sz          // update l-th momentum
                b(l) -= (mo(l), d(l).mean * eta)                         // update l-th parameter (weights and biases)
            } // for

            ee.normFSq                                                  // return the sse of this batch
        } // updateWeight

        if (DEBUG) println (s"optimizeX: parameters b = $b")
        (sse, maxEpochs)                                                 // return sse and number of epochs
    } // optimizeX

} // Optimizer_SGDM object

