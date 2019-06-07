
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.{Map, Set}
import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
import scalation.math.sq
import scalation.plot.Plot
import scalation.stat.Statistic
import scalation.stat.StatVector.corr
import scalation.random.CDF.studentTCDF
import scalation.random.PermutedVecI
import scalation.util.{banner, Error, time}
import scalation.util.Unicode.sub

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorMat` abstract class supports multiple predictor analytics.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in for example the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  Note, "protected val" arguments required by `ResponseSurface`.
 *  @param x       the input/data m-by-n matrix
 *                     (augment with a first column of ones to include intercept in model)
 *  @param y       the response/output m-vector
 *  @param fname   the feature/variable names (if null, use x_j's)
 *  @param hparam  the hyper-parameters for the model
                       (as some implementing classes may not have any, it defaults to null)
 */
abstract class PredictorMat (protected val x: MatriD, protected val y: VectoD,
                             protected var fname: Strings, hparam: HyperParameter = null)
         extends Fit (y, x.dim2, (x.dim2 - 1, x.dim1 - x.dim2)) with Predictor with Error
         // if not using an intercept df = (x.dim2, x.dim1-x.dim2), correct by calling 'resetDF' method from `Fit`
{
    if (x.dim1 != y.dim) flaw ("constructor", "row dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) {
        flaw ("constructor", "PredictorMat requires more rows ${x.dim1} than columns ${x.dim2}")
    } // if

    private   val DEBUG   = true                                         // debug flag
    protected val m       = x.dim1                                       // number of data points (rows)
    protected val n       = x.dim2                                       // number of parameter (columns)
    protected val k       = x.dim2 - 1                                   // number of variables (k = n-1) - assumes intercept
    private   val stream  = 0                                            // random number stream to use
    private   val permGen = PermutedVecI (VectorI.range (0, m), stream)  // permutation generator

    protected var b: VectoD = null                                       // parameter/coefficient vector [b_0, b_1, ... b_k]
    protected var e: VectoD = null                                       // residual/error vector [e_0, e_1, ... e_m-1]

    if (fname == null) fname = x.range2.map ("x" + _).toArray            // default feature/variable names

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'yy's,
     *  train the prediction function 'yy = f(x)' by fitting its parameters.
     *  The 'x' values must be provided by the implementing class.
     *  @param yy  the response/output vector
     */
    def train (yy: VectoD = y): PredictorMat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data 'x' and 'yy', fit the parameter/weight vector 'b'.
     *  Minimize the error in the prediction by adjusting the parameter vector 'b'.
     *  Implementations of this method should optimize hyper-parameters.
     *  @param yy  the response/output vector
     */
    def train2 (yy: VectoD = y): PredictorMat =
    {
        throw new UnsupportedOperationException ("no hyper-parameters to optimize")
    } // train2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error (difference between actual and predicted) and useful
     *  diagnostics for the test dataset.
     *  @param xx  the test data/input matrix
     *  @param yy  the test response/output vector
     */
    def eval (xx: MatriD = x, yy: VectoD = y): PredictorMat =
    {
        val yp = predict (xx)                                            // y predicted for xx (test/full)
        e = yy - yp                                                      // compute residual/error vector e
        diagnose (e, yy)                                                 // compute diagnostics
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = hparam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values.
     */
    def parameter: VectoD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on the trained model.
     *  @see 'summary' method for more details
     */
    def report: String =
    {
        s"""
REPORT
    hparameter hp  = $hparameter
    parameter  b   = $parameter
    fitMap     qof = $fitMap
        """
    } // report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute and return summary diagostics for the regression model.
     */
    def summary: String =
    {
        (if (fname != null) "fname = " + fname.deep else "") + 
        super.summary (b, {
            val facCho = new Fac_Cholesky (x.t * x)                      // create a Cholesky factorization
            val l_inv  = facCho.factor1 ().inverse                       // take inverse of l from Cholesky factorization
            val varCov = l_inv.t * l_inv * mse_                          // variance-covariance matrix
            varCov.getDiag ().map (sqrt (_)) })                          // standard error of coefficients
    } // summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectoD = e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot z',
     *  e.g., '(b_0, b_1, b_2) dot (1, z_1, z_2)'.
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot z',
     *  for each row of matrix 'z'.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatriD = x): VectoD = VectorD (for (i <- z.range1) yield predict (z(i)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean): (Int, VectoD, VectoD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable from
     *  the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     *  @param first     first variable to consider for elimination
     */
    def backwardElim (cols: Set [Int], adjusted: Boolean, first: Int): (Int, VectoD, VectoD)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use 'k'-fold cross-validation to compute test quality of fit (qof) measures
     *  by iteratively dividing the dataset into a test dataset and a training dataset.
     *  Each test dataset is defined by 'idx' and the rest of the data is the training dataset.
     *  @param algor  the prediction algorithm being applied (e.g., `RidgeRegression`)
     *  @param xx     the data matrix to use (full data matrix 'x' or selected columns)
     *  @param k      the number of cross-validation iterations (defaults to 10x).
     *  @param rando  flag indicating whether to use randomized or simple cross-validation
     */
    protected def crossValidate (algor: (MatriD, VectoD) => PredictorMat,
                                 xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        if (k < 3) flaw ("crossValidate", s"k = $k must be at least 3")
        val fLabel  = fitLabel                                           // labels for qof measures
        val stats   = Array.ofDim [Statistic] (fLabel.length)
        for (i <- stats.indices) stats(i) = new Statistic (fLabel(i))
        val indices = if (rando) permGen.igen.split (k)                  // k groups of indices
                      else       VectorI (0 until m).split (k)

        for (idx <- indices) {
            val x_te = xx(idx)                                           // test data matrix
            val y_te = y(idx)                                            // test response vector
            val x_tr = xx.selectRowsEx (idx)                             // training data matrix
            val y_tr = y.selectEx (idx)                                  // training response vector

            if (DEBUG) {
//              println (s"test:  x_te = $x_te \n y_te = $y_te")         // test (data matrix, response vector)
//              println (s"train: x_tr = $x_tr \n y_tr = $y_tr")         // training (data matrix, response vector)
            } // if

            val model = algor (x_tr, y_tr)                               // construct next model using training dataset
            model.train ()                                               // train model on the training dataset
            model.eval (x_te, y_te)                                      // evaluate model on the test dataset
            val qof = model.fit                                          // get quality of fit (qof) measures
            if (DEBUG) println (s"crossValidate: qof = $qof") 
            if (qof(index_sst) > 0.0) {                                  // requires variation in test set
                for (q <- qof.range) stats(q).tally (qof(q))             // tally these qof measures
            } // if
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
     *  @param xx     the data matrix to use (defaults to full data matrix 'x')
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  flag for using randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic]

} // PredictorMat abstract class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorMat` companion object provides a meythod for splitting
 *  a combined data matrix in predictor matrix and a response vector.
 */
object PredictorMat
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pull out the designated response column from the combined matrix.
     *  When 'col' is negative or the last column, slice out the last column.
     *  @param xy   the combined data and response matrix
     *  @param col  the designated response column to be pulled out
     */
    def pullResponse (xy: MatriD, col: Int = -1): (MatriD, VectoD) =
    {
        if (col < 0 || col == xy.dim2-1) (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1))
        else                             (xy.sliceEx (xy.dim1, col), xy.col (col))
    } // pullResponse

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pull out the designated response column from the combined matrix.
     *  When 'col' is negative or the last column, slice out the last column.
     *  Customized for dense matrices and vectors.
     *  @param xy   the combined data and response matrix
     *  @param col  the designated response column to be pulled out
     */
    def pullResponse_ (xy: MatrixD, col: Int = -1): (MatrixD, VectorD) =
    {
        if (col < 0 || col == xy.dim2-1) (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1))
        else                             (xy.sliceEx (xy.dim1, col), xy.col (col))
    } // pullResponse_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Analyze a dataset using the given model using ordinary training with the
     *  'train' method.
     *  @param model  the model to be used
     */
    def analyze (model: PredictorMat)
    {
        println (model.train ().eval ().report)
    } // analyze

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Analyze a dataset using the given model where training includes
     *  hyper-parameter optimization with the 'train2' method.
     *  @param model  the model to be used
     */
    def analyze2 (model: PredictorMat)
    {
        println (model.train2 ().eval ().report)
    } // analyze2

} // PredictorMat object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorMatTest` is used to test the `PredictorMat` abstract class
 *  and its derived classes using the `ExampleBasketBall` dataset containing
 *  data matrix 'x' and response vector 'y'.
 *  > runMain scalation.analytics.PredictorMatTest
 */
object PredictorMatTest extends App
{
    import ExampleBasketBall._
    println ("xy = " + xy)                                       // combined data-response matrix

    val xs = ox.sliceCol (0, 2)                                  // only the first two columns of ox
    val xr = x.sliceCol (0, 1)                                   // only the first column of x

    banner ("Test NullModel")                                    // 1
    NullModel.analyze (new NullModel (y))

    banner ("Test SimplerRegression")                            // 2
    PredictorMat.analyze (new SimplerRegression (xr, y, fname))

    banner ("Test SimpleRegression")                             // 3
    PredictorMat.analyze (new SimpleRegression (xs, y, fname))

    banner ("Test Regression with no intercept")                 // 4
    PredictorMat.analyze (new Regression (x, y, fname))

    banner ("Test Regression with intercept")                    // 5
    PredictorMat.analyze (new Regression (ox, y, fname))

    banner ("Test Regression_WLS with intercept")                // 6
    PredictorMat.analyze (new Regression_WLS (ox, y, fname))

    banner ("Test RidgeRegression with no intercept")            // 7
    PredictorMat.analyze (new RidgeRegression (x, y, fname))

    banner ("Test RidgeRegression with intercept")               // 8
    PredictorMat.analyze (new RidgeRegression (ox, y, fname))

    banner ("Test LassoRegression with no intercept")            // 9
    PredictorMat.analyze (new LassoRegression (x, y, fname))

    banner ("Test LassoRegression with intercept")               // 10
    PredictorMat.analyze (new LassoRegression (ox, y, fname))

    banner ("Test TranRegression with intercept - log")          // 11
    PredictorMat.analyze (new TranRegression (ox, y, fname))

    banner ("Test TranRegression with intercept - sqrt")         // 12
    PredictorMat.analyze (new TranRegression (ox, y, fname, sqrt _, sq _))

    banner ("Test TranRegression with intercept - box-cox")      // 13
    PredictorMat.analyze (TranRegression (ox, y, fname))

    banner ("Test QuadRegression")                               // 14
    PredictorMat.analyze (new QuadRegression (x, y, fname))

    banner ("Test ResponseSurface Quadractic with Cross-Terms")  // 15
    PredictorMat.analyze (new ResponseSurface (x, y, fname))

    banner ("Test ResponseSurface with All Cubic Terms")         // 16
    PredictorMat.analyze (new ResponseSurface (x, y, fname, cubic = true))

    banner ("Test ExpRegression")                                // 17
    PredictorMat.analyze (new ExpRegression (ox, y, fname))

    banner ("Test PoissonRegression")                            // 18 - FIX
    PredictorMat.analyze (new PoissonRegression (ox, y, fname))

    banner ("Test KNN_Predictor")                                // 19
    PredictorMat.analyze (new KNN_Predictor (x, y, fname))

    banner ("Test RegressionTree")                               // 20
    PredictorMat.analyze (new RegressionTree (x, y, fname))

    banner ("Test RegressionTree_GB")                            // 21
    PredictorMat.analyze (new RegressionTree_GB (x, y, fname))

    import ActivationFun._
    banner ("Test Perceptron with sigmoid")                      // 22
    PredictorMat.analyze2 (Perceptron (oxy, fname))

    banner ("Test Perceptron with tanh")                         // 23
    PredictorMat.analyze2 (Perceptron (oxy, fname, f1 = f_tanh))

    banner ("Test Perceptron with id")                           // 24
    PredictorMat.analyze2 (Perceptron (oxy, fname, f1 = f_id))
//  PredictorMat.analyze2 (Perceptron (oxy, fname, Optimizer.hp.updateReturn ("eta", 0.01), f1 = f_id))

    banner ("Test Perceptron with lreLU")                        // 25
    PredictorMat.analyze2 (Perceptron (oxy, fname, f1 = f_lreLU))

    banner ("Test Perceptron with eLU")                          // 26
    PredictorMat.analyze2 (Perceptron (oxy, fname, f1 = f_eLU))

} // PredictorMatTest

