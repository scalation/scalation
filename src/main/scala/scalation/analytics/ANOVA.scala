
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Jan  4 23:09:27 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectorI}
import scalation.util.Error

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANOVA` class supports one-way ANalysis Of VAraiance (ANOVA).  It is
 *  framed using GLM notation and supports the use of one binary/categorical
 *  treatment variable 't'.  This is done by introducing dummy variables 'd_j' to
 *  distinguish the treatment level.  The problem is again to fit the parameter
 *  vector 'b' in the following equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * d_1 +  b_1 * d_2 ... b_k * d_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.
 *  @see http://psych.colorado.edu/~carey/Courses/PSYC5741/handouts/GLM%20Theory.pdf
 *  @param t          the treatment/categorical variable vector
 *  @param y          the response vector
 *  @param levels     the number of treatment levels (1, ... levels)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ANOVA (t: VectorI, y: VectorD, levels: Int, technique: RegTechnique = Fac_QR)
      extends Predictor with Error
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")

    val x = new MatrixD (y.dim, levels)                    // design matrix
    assignDummyVars ()                                     // assign values for dummy variables
    val rg = new Regression (x, y, technique)              // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the dummy variables based on the treatment vector 't'.
     */
    def assignDummyVars ()
    {
        for (i <- 0 until x.dim1) {
            x(i, 0) = 1.0                                       // first column is always one
            val lev = t(i)                                      // treatment level for ith item
            if (lev < 1 || lev > levels) flaw ("assignDummyVars", "treatment level is out of range")
            if (lev < levels) x(i, lev) = 1.0
        } // for
    } // assignDummyVars

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  regression equation
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, d_1, ... d_k] + e
     *  using the least squares method.
     */
    def train () { rg.train () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, d_1, ... d_k] + e
     *  using the least squares method.
     *  @param yy  the new response vector
     */
    def train (yy: VectorD) { rg.train (yy) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including rSquared.
     */
    def fit: VectorD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = rg.predict (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): Tuple3 [Int, VectoD, VectorD] = rg.backElim ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-colinearity by regressing xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the varaince of xj can be predicted
     *  from the other variables, so xj is a candidate for removal from the model.
     */
    def vif: VectorD = rg.vif

} // ANOVA class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANOVATest` object tests the `ANOVA` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*d_1 + b_2*d_2
 *  <p>
 */
object ANOVATest extends App
{
    val t = VectorI (1, 1, 2, 2, 3)                              // treatements levels
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)         // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0)

    println ("t = " + t)
    println ("y = " + y)

    val levels = 3
    val anc    = new ANOVA (t, y, levels)
    anc.train ()
    println ("fit = " + anc.fit)

    val yp = anc.predict (z)
    println ("predict (" + z + ") = " + yp)

} // ANOVATest object

