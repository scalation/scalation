
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Jan  4 23:09:27 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.util.{Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVA` class supports ANalysis of COVAriance 'ANCOVA'.  It allows
 *  the addition of a categorical treatment variable 't' into a multiple linear
 *  regression.  This is done by introducing dummy variables 'dj' to distinguish
 *  the treatment level.  The problem is again to fit the parameter vector 'b'
 *  in the augmented regression equation
 *  <p>
 *      y  =  b dot x + e  =  b0  +  b_1   * x_1  +  b_2   * x_2  +  ... b_k * x_k
                                  +  b_k+1 * d_1  +  b_k+2 * d_2  +  ... b_k+l * d_l + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x_         the data/design matrix of continuous variables
 *  @param t          the treatment/categorical variable vector
 *  @param y          the response vector
 *  @param levels     the number of treatment levels (1, ... levels)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ANCOVA (x_ : MatriD, t: VectoI, y: VectoD, levels: Int, technique: RegTechnique = QR)
      extends Predictor  with Error
{
    if (x_.dim1 != y.dim) flaw ("constructor", "dimensions of x_ and y are incompatible")
    if (t.dim   != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")

    val x = new MatrixD (x_.dim1, x_.dim2 + levels - 1)    // augmented design matrix
    assignVars ()                                          // assign values for continuous variables
    assignDummyVars ()                                     // assign values for dummy variables
    val rg = new Regression (x, y, technique)              // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the continuous variables from the 'x' matrix.
     */
    def assignVars ()
    {
        for (i <- 0 until x_.dim1; j <- 0 until x_.dim2) x(i, j) = x_(i, j)
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the dummy variables based on the treatment vector 't'.
     */
    def assignDummyVars ()
    {
        for (i <- 0 until x_.dim1) {
            val lev = t(i)                                      // treatment level for ith item
            if (lev < 1 || lev > levels) flaw ("assignDummyVars", "treatment level is out of range")
            if (lev < levels) x(i, x_.dim2 + lev - 1) = 1.0
        } // for
    } // assignDummyVars

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k+l] dot [1, x_1, ..., d_1, ...] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD) { rg.train (yy) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  regression equation using the least squares method on 'y'.
     */
    def train () { rg.train () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficients.
     */
    override def coefficient: VectoD = rg.coefficient

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit 'rSquared'.
     */
    override def fit: VectorD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = rg.fitLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b0, b1, b2) dot (1, z1, z2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = rg.predict (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): (Int, VectoD, VectorD) = rg.backElim ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectorD = rg.vif

} // ANCOVA class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVATest` object tests the `ANCOVA` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2 + b_3*d_1 + b_4*d_2
 *  <p>
 *  > runMain scalation.analytics.ANCOVATest
 */
object ANCOVATest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((6, 3), 1.0, 36.0,  66.0,                 // 6-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0, 42.0,  83.0,
                                 1.0,  1.0, 101.0)
    val t = VectorI (1, 1, 2, 2, 3, 3)                             // treatments levels
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 643.0, 1598.0)    // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0, 0.0)

    println ("x = " + x)
    println ("t = " + t)
    println ("y = " + y)

    val levels = 3
    val anc    = new ANCOVA (x, t, y, levels)
    anc.train ()

    println (anc.fitLabels)
    println ("fit         = " + anc.fit)
    println ("coefficient = " + anc.coefficient)

    val yp = anc.predict (z)
    println ("predict (" + z + ") = " + yp)

} // ANCOVATest object

