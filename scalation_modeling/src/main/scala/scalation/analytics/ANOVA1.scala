
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Jan  4 23:09:27 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.plot.Plot
import scalation.util.{banner, Error}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANOVA1` class supports one-way ANalysis Of VAriance (ANOVA), i.e,
 *  it allows only one binary/categorial treatment variable.  It is framed using
 *  General Linear Model 'GLM' notation and supports the use of one
 *  binary/categorical treatment variable 't'.  This is done by introducing
 *  dummy variables 'd_j' to distinguish the treatment level.  The problem is
 *  again to fit the parameter vector 'b' in the following equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * d_1 +  b_1 * d_2 ... b_k * d_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y
 *      b  =  fac.solve (.)
 *  <p>
 *  @see psych.colorado.edu/~carey/Courses/PSYC5741/handouts/GLM%20Theory.pdf
 *  @param t          the binary/categorical treatment variable vector
 *  @param y          the response vector
 *  @param levels     the number of treatment levels (1, ... levels)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ANOVA1 (t: VectoI, y: VectoD, levels: Int, technique: RegTechnique = QR)
      extends Predictor with Error
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")

    val x = new MatrixD (y.dim, levels)                           // design matrix
    assignDummyVars ()                                            // assign values for dummy variables
    val rg = new Regression (x, y, technique)                     // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the dummy variables based on a treatment variable's level 'lev'.
     *  @param lev  treatment level of the variable
     */
    def assignDummyVar (lev: Int): VectorD =
    {
         val tvec = new VectorD (levels)
         tvec(0) = 1.0
         if (lev < 1 || lev > levels) flaw ("assignDummyVar", "treatment level is out of range")
         if (lev < levels) tvec(lev) = 1.0
         tvec
    } // assignDummyVar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the dummy variables based on the treatment vector 't'.
     */
    def assignDummyVars (tt: VectoI = t)
    {
        for (i <- tt.range) {
            x(i, 0) = 1.0                                         // first column is always one
            val lev = tt(i)                                       // treatment level for ith item
            if (lev < 1 || lev > levels) flaw ("assignDummyVars", "treatment level is out of range")
            if (lev < levels) x(i, lev) = 1.0
        } // for
    } // assignDummyVars

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, d_1, ... d_k] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): Regression [MatrixD, VectoD] = rg.train (yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  regression equation using the least squares method on 'y'.
     */
//    def train (): Regression [MatrixD, VectoD] = rg.train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param yy   the response vector
     */
    def eval (yy: VectoD = y) { rg.eval (yy) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.
     */
    override def fit: VectoD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = rg.fitLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: Int): Double = rg.predict (assignDummyVar (z))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
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
     *  vector and the new  quality of fit.  May be called repeatedly.
     *  @param cols  the columns of matrix x included in the existing model
     */
    def backwardElim (cols: Set [Int]): (Int, VectoD, VectoD) = rg.backwardElim (cols)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectoD = rg.vif

} // ANOVA1 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANOVA1Test` object tests the `ANOVA1` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*d_1 + b_2*d_2
 *  <p>
 *  > runMain scalation.analytics.ANOVA1Test
 */
object ANOVA1Test extends App
{
    val t  = VectorI (1, 1, 1, 2, 2, 2, 3, 3, 3)                 // treatment level data
    val y  = VectorD (755.0, 865.0, 815.0,
                      442.0, 420.0, 401.0,
                      282.0, 250.0, 227.0)                       // response vector
    val z  = VectorD (1.0, 20.0, 80.0, 1.0)

    println ("t = " + t)
    println ("y = " + y)

    val levels = 3
    val arg    = new ANOVA1 (t, y, levels)
    arg.train ().eval ()
    println ("coefficient = " + arg.coefficient)
    println ("              " + arg.fitLabels)
    println ("fit         = " + arg.fit)

    val yp1 = arg.predict (z)                                     // predict for one point
    println ("predict (" + z + ") = " + yp1)

    banner ("test predictions")
    val yp = new VectorD (y.dim)
    for (i <- yp.range) yp(i) = arg.predict (t(i))
    println (s" y = $y \n yp = $yp")
    new Plot (t.toDouble, y, yp, "ANOVA1")

} // ANOVA1Test object

