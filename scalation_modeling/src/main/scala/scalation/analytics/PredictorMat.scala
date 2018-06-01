
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.immutable.ListMap
import scala.collection.mutable.Set
import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
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
 *  Note, "protected val" arguments requires by `ResponseSurface`.
 *  @param x          the input/data m-by-n matrix
 *                        (augment with a first column of ones to include intercept in model)
 *  @param y          the response m-vector
 */
abstract class PredictorMat (protected val x: MatriD, protected val y: VectoD)
         extends Fit (y, x.dim2, (x.dim2-1, x.dim1-x.dim2)) with Predictor with Error
{
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 < x.dim2) flaw ("constructor", "NEGATIVE df - not enough data rows in matrix to use prediction")

    private   val DEBUG   = true                                         // debug flag
    protected val k       = x.dim2 - 1                                   // number of variables (k = n-1) FIX - assumes intercept
    protected val m       = x.dim1                                       // number of data points (rows)
    private   val stream  = 0                                            // random number stream to use
    private   val permGen = PermutedVecI (VectorI.range (0, m), stream)  // permutation generator

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'yy's,
     *  train the prediction function 'yy = f(x)' by fitting its parameters.
     *  The 'x' values must be provided by the implementing class.
     *  @param yy  the response vector
     */
    def train (yy: VectoD): PredictorMat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a set of data vectors 'x's and their corresponding responses 'y's,
     *  passed into the implementing class, train the prediction function 'y = f(x)'
     *  by fitting its parameters.
     */
    def train (): PredictorMat = train (y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     */
    def eval ()
    {
        e = y - x * b                                                    // compute residual/error vector e
        diagnose (e)                                                     // compute diagnostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the test dataset.
     *  @param xx  the test data matrix
     *  @param yy  the test response vector
     */
    override def eval (xx: MatriD, yy: VectoD)
    {
        e = yy - xx * b                                                  // compute residual/error vector e
        diagnose (e)                                                     // compute diagnostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute diagostics for the regression model.
     *  @param yy  the response vector
     */
    def summary ()
    {
        super.summary (b, {
            val facCho = new Fac_Cholesky (x.t * x)                      // create a Cholesky factorization
            val l_inv  = facCho.factor1 ().inverse                       // take inverse of l from Cholesky factorization
            val varCov = l_inv.t * l_inv * mse_                          // variance-covariance matrix
            varCov.getDiag ().map (sqrt (_)) })                          // standard error of coefficients
    } // summary

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
    def predict (z: MatriD): VectoD = z * b

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use 'k'-fold cross-validation to compute test quality of fit measures by
     *  dividing the dataset into a test dataset and a training dataset.
     *  The test dataset is defined by 'tRange' and the rest of the data is training dataset".
     *  @param x      the data matrix
     *  @param y      the response vector
     *  @param algor  the prediction algorithm being applied (e.g., `RidgeRegression`)
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  flag for using randomized cross-validation
     */
    def crossValidate (algor: (MatriD, VectoD) => PredictorMat, k: Int = 10,
                       rando: Boolean = true): Array [Statistic] =
    {
        val stats   = Array.fill (fitLabel.length) (new Statistic ())
        val indices = if (rando) permGen.igen.split (k)
                      else       VectorI (0 until m).split (k)

        for (idx <- indices) {
            val idxa = idx.toArray
            val x_te = x(idx)                                            // test data matrix
            val y_te = y(idx)                                            // test response vector
            val x_tr = x.selectRowsEx (idxa)                             // training data matrix
            val y_tr = y.selectEx (idxa)                                 // training response vector

            if (DEBUG) {
                println ("x_te = " + x_te)
                println ("y_te = " + y_te)
                println ("x_tr = " + x_tr)
                println ("y_tr = " + y_tr)
            } // if

            val model = algor (x_tr, y_tr)                               // construct next model using training dataset
            model.train ()                                               // train the model
            model.eval (x_te, y_te)                                      // evaluate model on test dataset
            val qm = model.fit                                           // get quality of fit measures
            for (q <- qm.indices) stats(q).tally (qm(q))                 // tally these measures
        } // for

        if (DEBUG) println ("stats = " + stats.deep)
        stats
    } // crossValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'crossVal' abstract method must be coded in implementing classes to
     *  call the above 'crossValidate' method.  The 'algor' parameter may be
     *  specified as a lambda function to create the prediction algorithm.
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  flag for using randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)

} // PredictorMat abstract class

