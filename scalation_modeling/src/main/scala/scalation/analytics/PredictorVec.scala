
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.{Map, Set}

import scalation.linalgebra._
import scalation.stat.Statistic
import scalation.random.PermutedVecI
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PredictorVec` class supports term expanded regression.
 *  Fit the parameter vector 'b' in the regression equation.
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y
 *      b  =  fac.solve (.)
 *  <p>
 *  @param t    the input vector: t_i expands to x_i = vector
 *  @param y    the response vector
 *  @param ord  the order of the expansion
 */
abstract class PredictorVec (t: VectoD, y: VectoD, ord: Int)
      extends Predictor with Error
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")
    if (t.dim <= ord)   flaw ("constructor", "not enough data points for the given order (ord)")

    private   val DEBUG   = true                                         // debug flag
    private   val stream  = 0                                            // random number stream to use
    private   val permGen = PermutedVecI (VectorI.range (0, t.dim), stream)  // permutation generator
    protected var rg: Regression = null                                  // delegated regression model


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the scalar 't' into a vector of terms/columns.
     *  @param t  the scalar to expand into the vector
     */
    def expand (t: Double): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the vector 't' into a matrix.
     *  @param t  the vector to expand into the matrix
     */
    def expand (t: VectoD): MatriD =
    {
        val x = new MatrixD (t.dim, 1 + ord)
        for (i <- t.range) x(i) = expand (t(i))
        x
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector 'b' in the
     *  multiple regression equation using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): Regression = rg.train (yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the entire dataset.
     */
    def eval () { rg.eval () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics for the test dataset.
     *  @param xx  the test data matrix
     *  @param yy  the test response vector
     */
    def eval (tt: VectoD, yy: VectoD) { rg.eval (expand (tt), yy) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficients.
     */
    override def coefficient: VectoD = rg.coefficient

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit measures including 'rSq'.
     */
    def fit: VectoD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    def fitLabel: Seq [String] = rg.fitLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of quality of fit measures.
     */
    def fitMap: Map [String, String] = rg.fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot expand (z)',
     *  e.g., '(b_0, b_1, b_2) dot (1, z, z^2)'.
     *  @param z  the new scalar to predict
     */
    def predict (z: Double): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new expanded/orhogonalized vector to predict
     */
    def predict (z: VectoD): Double = rg.predict (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    def forwardSel (cols: Set [Int]): (Int, VectoD, VectoD) = rg.forwardSel (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable from
     *  the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    def backwardElim (cols: Set [Int]): (Int, VectoD, VectoD) = rg.backwardElim (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectoD = rg.vif

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use 'k'-fold cross-validation to compute test quality of fit measures by
     *  dividing the dataset into a test dataset and a training dataset.
     *  The test dataset is defined by 'tRange' and the rest of the data is training dataset".
     *  @param x      the data matrix
     *  @param y      the response vector
     *  @param algor  the prediction algorithm being applied (e.g., `PolyRegression`)
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param rando  whether to use randomized cross-validation
     */
    def crossValidate (algor: (VectoD, VectoD, Int) => PredictorVec, k: Int = 10,
                       rando: Boolean = true): Array [Statistic] =
    {
        val stats   = Array.fill (fitLabel.length) (new Statistic ())
        val indices = if (rando) permGen.igen.split (k)
                      else       VectorI (0 until t.dim).split (k)

        for (idx <- indices) {
            val idxa = idx.toArray
            val t_te = t(idx)                                            // test data matrix
            val y_te = y(idx)                                            // test response vector
            val t_tr = t.selectEx (idxa)                                 // training data matrix
            val y_tr = y.selectEx (idxa)                                 // training response vector

            if (DEBUG) {
                println ("t_te = " + t_te)
                println ("y_te = " + y_te)
                println ("t_tr = " + t_tr)
                println ("y_tr = " + y_tr)
            } // if

            val model = algor (t_tr, y_tr, ord)                          // construct next model using training dataset
            model.train ()                                               // train the model
            model.eval (t_te, y_te)                                      // evaluate model on test dataset
            val qm = model.fit                                           // get quality of fit measures
            for (i <- qm.indices) stats(i).tally (qm(i))                 // tally these measures
        } // for 
        
        println ("stats = " + stats.deep)
        stats
    } // crossValidate


   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'crossVal' abstract method must be coded in implementing classes to
     *  call the above 'crossValidate' method.  The 'algor' parameter may be
     *  specified as a lambda function to create the prediction algorithm.
     *  @param k      the number of crosses and cross-validations (defaults to 10x).
     *  @param ord    the given order
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, ord: Int = 10, rando: Boolean = true)

} // PredictorVec abstract class

