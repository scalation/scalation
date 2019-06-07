
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation.analytics

import scala.collection.mutable.{ArrayBuffer, Map, Set}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectorI}
import scalation.stat.Statistic
import scalation.random.PermutedVecI
import scalation.util.{banner, Error}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorMat2` abstract class provides the basic structure and API for
 *  a variety of modeling techniques with multiple outputs/responses, e.g., Neural Networks.
 *  @param x       the m-by-nx data/input matrix (data consisting of m input vectors)
 *  @param y       the m-by-ny response/output matrix (data consisting of m output vectors)
 *  @param fname   the feature/variable names (if null, use x_j's)
 *  @param hparam  the hyper-parameters for the model/network
 */
abstract class PredictorMat2 (x: MatriD, y: MatriD,
                              protected var fname: Strings, hparam: HyperParameter)
         extends Predictor with Error
{
    if (x.dim1 != y.dim1) flaw ("constructor", "row dimensions of x and y are incompatible")

    private   val DEBUG   = true
    protected val m       = x.dim1                                       // number of data points (input vectors)
    protected val nx      = x.dim2                                       // dimensionality of the input
    protected val ny      = y.dim2                                       // dimensionality of the output
    protected val _1      = VectorD.one (m)                              // vector of all ones
    protected var eta     = if (hparam == null) 0.0 else hparam ("eta")  // the learning/convergence rate (adjustable)

    private   val stream  = 0                                            // random number stream to use
    private   val permGen = PermutedVecI (VectorI.range (0, m), stream)  // permutation generator

    protected var ee: MatriD = null                                      // residual/error matrix

    if (fname == null) fname = x.range2.map ("x" + _).toArray            // default feature/variable names

    val fitA = Array.ofDim [Fit] (ny)
    for (k <- fitA.indices) fitA(k) = new Fit (y.col(k), nx, (nx-1, m - nx))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the learning rate 'eta'.  Since this hyper-parameter needs frequent tuning,
     *  this method is provided to facilitate that.
     *  @param eta_  the learning rate
     */
    def reset (eta_ : Double) { eta = eta_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given data matrix 'x' and response matrix 'yy', fit the parameters 'b' (weights and
     *  biases) using a simple, easy to follow algorithm.
     *  @param yy  the matrix of outputs
     */
    def train0 (yy: MatriD = y): PredictorMat2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given data matrix 'x' and response matrix 'yy', fit the parameters 'b' (weights and
     *  biases).
     *  @param yy  the matrix of outputs
     */
    def train (yy: MatriD = y): PredictorMat2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given data matrix 'x' and response vector 'yy', fit the parameter 'b' (weights and
     *  biases).
     *  @param yy  the vector of outputs, e.g., for the first variable/column
     */
    def train (yy: VectoD): PredictorMat2 = train (MatrixD (Seq (yy)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given data matrix 'x' and response matrix 'yy', fit the parameters 'b' (weights and
     *  biases).  Overriding implementations of this method should optimize hyper-parameters
     *  (e.g., eta).
     *  @param yy  the matrix of outputs
     */
    def train2 (yy: MatriD = y): PredictorMat2 = train (yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Switch between 'train' methods: simple (0), regular (1) and hyper-parameter
     *  optimizing (2).
     *  @param which  the kind of 'train' method to use
     *  @param yy     the matrix of responses/outputs
     */
    def trainSwitch (which: Int, yy: MatriD = y): PredictorMat2 =
    {
        which match {
        case 0 => train0 (yy)
        case 1 => train (yy)
        case 2 => train2 (yy)
        case _ => flaw ("trainSwitch", s"which = $which not in (0, 1, 2)"); null
        } // match
    } // trainSwitch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the degrees of freedom to the new updated values.  For some models,
     *  the degrees of freedom is not known until after the model is built.
     *  Caveat:  only applies to the first response/output variable.
     *  @param df_update  the updated degrees of freedom (model, error)
     */
    def resetDF (df_update: PairD)
    {
        fitA(0).resetDF (df_update)
        if (DEBUG) println (s"resetDF: df = $df_update")
    } // resetDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of the fit for the parameter/weight matrices on the
     *  entire dataset or the test dataset.  Only considers the first response/output
     *  variable/column.
     *  @param xx  the data/input matrix (test/full)
     *  @param yy  the response/output vector (first column only), (test/full)
     */
    def eval (xx: MatriD = x, yy: VectoD = y.col(0)): PredictorMat2 =
    {
        val yp0 = predict (xx).col(0)                                    // predict output/responses, first column
        val e   = yy - yp0                                               // error vector, first column
        fitA(0).diagnose (e, yy)                                         // compute diagonostics, first column
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of the fit for the parameter/weight matrices on the
     *  entire dataset or the test dataset.  Considers all the response/output
     *  variables/columns.
     *  @param xx  the data/input data matrix (test/full)
     *  @param yy  the response/output response matrix (test/full)
     */
    def eval (xx: MatriD, yy: MatriD): PredictorMat2 =
    {
        val yp = predict (xx)                                            // predict output/responses
        val e  = yy - yp                                                 // error matrix
        for (k <- e.range2) fitA(k).diagnose (e.col(k), yy.col(k))       // compute diagonostics, per column
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors for first response/output variable/column.
     */
    def residual: VectoD = ee.col(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the matrix of residuals/errors.
     */
    def residuals: MatriD = ee

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the quality of fit measures.
     */
    def fitLabel: Seq [String] = fitA(0).fitLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'fitMap' results for each y-column and print the overall 'rSq' average
     *  over all y-columns.
     */
    def fitMap: IndexedSeq [Map [String, String]] =
    {
        val fits = Array.ofDim [Map [String, String]] (fitA.length)
        var sst, sse = 0.0
        for (k <- fitA.indices) {
            fits(k) = fitA(k).fitMap
            sst += fits(k)("sst").toDouble
            sse += fits(k)("sse").toDouble
        } // for
        val rSq = (sst - sse) / sst
        println (s"fitMap: overall: rSq = $rSq, sst = $sst, sse = $sse")
        fits
    } // fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = hparam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter/weight vector (first layer, first output).
     */
    def parameter: VectoD = parameters (0).w(0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the all parameters (weights and biases).
     */
    def parameters: NetParams

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on the trained model.
     *  @see 'summary' method for more details
     */
    def report: String =
    {
        s"""
REPORT
    hparameter hp  = $hparameter
    parameters b   = $parameters
    fitMap     qof = ${fitMap.map ("\n" + _)}
        """
    } // report

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  Quality of Fit (QoF).  May be called repeatedly.
     *  Only selects based on the first response/output column.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, NetParams, VectoD)

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
     *  Each test dataset is defined by 'idx' and the rest of the data is the training dataset.
     *  @param algor  the prediction algorithm being applied (e.g., `NeuralNet_3L`)
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  whether to use randomized cross-validation
     */
    protected def crossValidate (algor: (MatriD, MatriD) => PredictorMat2, k: Int = 10,
                                 rando: Boolean = true): Array [Statistic] =
    {
        if (k < 3) flaw ("crossValidate", s"k = $k must be at least 3")
        val fLabel = fitA(0).fitLabel                                    // labels for qof measures
        val stats  = Array.ofDim [Statistic] (ny * fitLabel.length)
        for (i <- stats.indices) stats(i) = new Statistic (fLabel(i))
        val indices = if (rando) permGen.igen.split (k)                  // groups of indices
                      else       VectorI (0 until m).split (k)

        for (idx <- indices) {
            val x_te = x(idx)                                            // test data matrix
            val y_te = y(idx)                                            // test response vector
            val x_tr = x.selectRowsEx (idx)                              // training data matrix
            val y_tr = y.selectRowsEx (idx)                              // training response matrix

            if (DEBUG) {
//              println (s"test:  x_te = $x_te \n y_te = $y_te")         // test (data matrix, response vector)
//              println (s"train: x_tr = $x_tr \n y_tr = $y_tr")         // training (data matrix, response vector)
            } // if

            val model = algor (x_tr, y_tr)                               // construct next model using training dataset
            model.train ()                                               // train the model
            model.eval (x_te, y_te)                                      // evaluate model on test dataset
            for (j <- 0 until ny) {
                val fit_j = model.fitA(j)
                val qof   = fit_j.fit                                    // get quality of fit measures
                val qsz   = qof.size
                if (qof(fit_j.index_sst) > 0.0) {                           // requires variation in test set
                    for (q <- qof.range) stats(j*qsz + q).tally (qof(q))    // tally these measures
                } // if
            } // for
        } // for

        if (DEBUG){
            banner ("crossValidate: Statistical Table for QoF")
            println (Statistic.labels)
            for (i <- stats.indices) println (stats(i))
        } // if
        stats
    } // crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'crossVal' abstract method must be coded in implementing classes to
     *  call the above 'crossValidate' method.  The 'algor' parameter may be
     *  specified as a lambda function to create the prediction algorithm.
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true): Array [Statistic]

} // PredictorMat2 abstract class

