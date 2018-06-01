
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

import scalation.linalgebra._
import scalation.stat.Statistic
import scalation.random.PermutedVecI
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet` companion object provides default values for hyper-parameters.
 */
object NeuralNet
{
    val DEFAULT_ETA    = 0.1                                              // the learning rate
    val DEFAULT_EPOCHS = 1000                                             // the maximum number of training epochs
} // NeuralNet


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet` abstract class provides the basic structure and API for
 *  a variety of Neural Networks.
 *  @param x           the m-by-nx input matrix (training data consisting of m input vectors)
 *  @param y           the m-by-ny output matrix (training data consisting of m output vectors)
 *  @param eta         the learning/convergence rate (adjustable)
 *  @param max_epochs  the maximum number of training epochs/iterations (fixed)
 */
abstract class NeuralNet (x: MatriD, y: MatriD,
                          protected var eta: Double,
                          protected val max_epochs: Int)
         extends Predictor with Error
{
    private   val DEBUG   = true
    protected val m       = x.dim1                                        // number of data points (input vectors)
    protected val nx      = x.dim2                                        // dimensionality of the input
    protected val ny      = y.dim2                                        // dimensionality of the output
    protected val _1      = VectorD.one (m)                               // vector of all ones
    private   val stream  = 0                                             // random number stream to use
    private   val permGen = PermutedVecI (VectorI.range (0, m), stream)   // permutation generator

    if (y.dim1 != m) flaw ("constructor", "row dimensions of x and y are incompatible")

    protected val fitA = Array.ofDim [Fit] (ny)
    for (k <- fitA.indices) fitA(k) = new Fit (y.col(k), nx, (nx-1, m - nx))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the weight matrix.
     */
    def weights: Array [MatriD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the initial weight matrix (ces) with values in (0, limit) before training.
     *  @param stream  the random number stream to use
     *  @param limit   the maximum value for any weight
     */
    def setWeights (stream: Int = 0, limit: Double = 1.0 / sqrt (nx))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the learning rate 'eta'.  Also, randomly generates new weights.
     *  @param eta_  the learning rate
     */
    def reset (eta_ : Double) { eta = eta_; setWeights () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'y', fit the parameter/weight matrix.
     */
    def train (): NeuralNet

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight matrix.
     *  @param yy  the vector of outputs for the first variable (currently ignored)
     */
    def train (yy: VectoD): NeuralNet = train ()

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of the fit for the parameter weight matrices on the
     *  the entire dataset or the training dataset.
     */
    def eval ()
    {
        val yp = predict (x)                                             // predict output/responses
        val e  = y - yp                                                  // error matrix
        for (k <- e.range2) fitA(k).diagnose (e.col(k))                  // compute diagonostics, per column
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of the fit for the parameter/weight matrices on the
     *  test dataset.
     *  @param xx  the test input data matrix
     *  @param yy  the test output response matrix
     */
    def eval (xx: MatriD, yy: MatriD)
    {
        val yp = predict (xx)                                            // predict output/responses
        val e  = yy - yp                                                 // error matrix
        for (k <- e.range2) fitA(k).diagnose (e.col(k))                  // compute diagonostics, per column
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the quality of fit measures.
     */
    def fitLabel: Seq [String] = fitA(0).fitLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show 'fitMap' for each y-column.
     */
    def fitMap ()
    {
        var sst, sse = 0.0
        for (k <- fitA.indices) {
            val fit_k = fitA(k).fitMap
            println (s"For column $k: fitMap = \n $fit_k")
            sst += fit_k ("sst").toDouble
            sse += fit_k ("sse").toDouble
        } // for
        val rSq = (sst - sse) / sst
        println (s"overall: rSq = $rSq, sst = $sst, sse = $sse")
    } // fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response value 'f(z)'.
     *  Return only the first output variable's value.
     *  @param z  the new input vector
     */
    def predict (z: VectoD): Double = predictV (z)(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector 'z', predict the output/response vector 'f(z)'.
     *  @param z  the new input vector
     */
    def predictV (z: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input matrix 'z', predict the output/response matrix 'f(z)'.
     *  @param z  the new input matrix
     */
    def predict (z: MatriD): MatriD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use 'k'-fold cross-validation to compute test quality of fit measures by
     *  dividing the dataset into a test dataset and a training dataset.
     *  The test dataset is defined by 'tRange' and the rest of the data is training dataset".
     *  @param x      the input data matrix
     *  @param y      the output response matrix
     *  @param algor  the prediction algorithm being applied (e.g., `NeuralNet_3L`)
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  whether to use randomized cross-validation
     */
    def crossValidate (algor: (MatriD, MatriD) => NeuralNet, k: Int = 10,
                       rando: Boolean = true): Array [Statistic] =
    {
        val stats   = Array.fill (ny * fitLabel.length) (new Statistic ())
        val indices = if (rando) permGen.igen.split (k)
                      else       VectorI (0 until m).split (k)

        for (idx <- indices) {
            val idxa = idx.toArray
            val x_te = x(idx)                                    // test data matrix
            val y_te = y(idx)                                    // test response vector
            val x_tr = x.selectRowsEx (idxa)                     // training data matrix
            val y_tr = y.selectRowsEx (idxa)                     // training response vector

            if (DEBUG) {
                println ("x_te = " + x_te)
                println ("y_te = " + y_te)
                println ("x_tr = " + x_tr)
                println ("y_tr = " + y_tr)
            } // if

            val model = algor (x_tr, y_tr)                       // construct next model using training dataset
            model.train ()                                       // train the model
            model.eval (x_te, y_te)                              // evaluate model on test dataset
            for (j <- 0 until ny) {
                val qm = model.fitA(j).fit                       // get quality of fit measures
                val qs = qm.size
                for (q <- qm.indices) stats(j*qs + q).tally (qm(q))   // tally these measures
            } // for
        } // for

        if (DEBUG) println ("stats = " + stats.deep)
        stats
    } // crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'crossVal' abstract method must be coded in implementing classes to
     *  call the above 'crossValidate' method.  The 'algor' parameter may be
     *  specified as a lambda function to create the prediction algorithm.
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)

} // NeuralNet abstract class

