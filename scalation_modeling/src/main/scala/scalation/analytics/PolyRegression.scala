
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.plot.Plot
import scalation.stat.StatVector.corr
import scalation.util.{banner, Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyRegression` class supports polynomial regression.  In this case,
 *  't' is expanded to '[1, t, t^2 ... t^k]'.  Fit the parameter vector 'b' in the
 *  regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * t +  b_2 * t^2 ... b_k * t^k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y
 *      b  =  fac.solve (.)
 *  <p>
 *  @see www.ams.sunysb.edu/~zhu/ams57213/Team3.pptx
 *  @param t          the input vector: t_i expands to x_i = [1, t_i, t_i^2, ... t_i^k]
 *  @param y          the response vector
 *  @param k          the order of the polynomial (max degree)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 *  @param raw        whether the polynomials are raw or orthogonal
 */
class PolyRegression (t: VectoD, y: VectoD, k: Int, technique: RegTechnique = Cholesky,
                      raw: Boolean = true)
      extends Predictor with Error
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")
    if (t.dim <= k)     flaw ("constructor", "not enough data points for the given order (k)")

    private val DEBUG = true
    private var x     = new MatrixD (t.dim, 1 + k)           // data/design matrix built from t
    private var a: MatrixD = null                            // multipliers for orthogonal polynomials
    for (i <- t.range) x(i) = expand (t(i))                  // raw polynomials
    if (! raw) {
        val za = orthogonalize (x)                           // orthogonal polynomials
        x = za._1; a = za._2
    } // if
    private val rg    = new Regression (x, y, technique)     // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the scalar 't' into a vector of powers of 't:  [1, t, t^2 ... t^k]'.
     *  @param t  the scalar to expand into the vector
     */
    def expand (t: Double): VectoD = 
    {
        val v = new VectorD (1 + k)
        for (j <- 0 to k) v(j) = t~^j
        v
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Orthogonalize the data/design matrix 'x' using Gram-Schmidt Orthogonalization,
     *  returning the a new orthogonal matrix 'z' and the orthogonalization multipliers 'a'.
     *  This will eliminate the multi-collinearity problem.
     *  @param x  the matrix to orthogonalize
     */
    def orthogonalize (x: MatrixD): (MatrixD, MatrixD) =
    {
        val z = new MatrixD (x.dim1, x.dim2)
        val a = new MatrixD (x.dim2, x.dim2)
        z.setCol (0, x.col(0))
        for (j <- 1 until x.dim2) {                          // column to set
            z.setCol (j, x.col(j))
            for (k <- 0 until j) {                           // make orthogonal to prior columns
                a(j, k) = (z.col(j) dot z.col(k)) / z.col(k).normSq
                z.setCol (j, z.col(j) - z.col(k) * a(j, k))
            } // for
        } // for
        if (DEBUG) println (s"x = $x \nz = $z")
        (z, a)
    } // orthogonalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Follow the same transformations used to orthogonalize the data/design matrix 'x',
     *  on vector 'v', so its elements are correctly mapped.
     *  @param v  the vector to be transformed based the orthogonalize procedure
     */
    def orthoVector (v: VectoD): VectoD =
    {
        val u = new VectorD (v.dim)
        u(0)  = v(0)
        for (j <- 1 until v.dim) {                           // element to set
            u(j) = v(j)
            for (k <- 0 until j) u(j) -= u(k) * a(j, k)      // apply orthogonal multiplier
        } // for
        if (DEBUG) println (s"v = $v \nu = $u")
        u
    } // orthoVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector 'b' in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, t, t^2 ... t^k] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD): Regression [MatrixD, VectoD] = rg.train (yy)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector 'b' in the
     *  regression equation using the least squares method on 'y'
     */
    def train (): Regression [MatrixD, VectoD] = rg.train ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param yy   the response vector
     */
    def eval (yy: VectoD = y) { rg.eval (yy) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficients.
     */
    override def coefficient: VectoD = rg.coefficient

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    override def residual: VectoD = rg.residual

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.
     */
    override def fit: VectoD = rg.fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = rg.fitLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = b dot expand (z)',
     *  e.g., '(b_0, b_1, b_2) dot (1, z, z^2)'.
     *  @param z  the new scalar to predict
     */
    def predict (z: Double): Double =
    {
        if (raw) rg.predict (expand (z))                     // raw polynomials
        else rg.predict (orthoVector (expand (z)))           // orthogonal polynomials - FIX
    } // predict

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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the correlation matrix for the columns in data matrix 'x'.
     */
    def corrMatrix: MatriD = corr (x)

} // PolyRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyRegressionTest` object tests `PolyRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*t + b_2*t^2 + ... b_k*t_k
 *  <p>
 *  Note, the 'order' at which R-Squared drops is QR(7), Cholesky(14), SVD(6), Inverse(13).
 *  > runMain scalation.analytics.PolyRegressionTest
 */
object PolyRegressionTest extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 100.0)
    val t     = VectorD.range (0, 100)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 100) y(i) = 10.0 - 10.0 * i + i~^2 + i * noise.gen

    println ("t = " + t)
    println ("y = " + y)

    val order     = 6
    val technique = Cholesky                        // others: QR, SVD, LU or Inverse
    val prg       = new PolyRegression (t, y, order, technique)
    prg.train ().eval ()

    println ("coefficient = " + prg.coefficient)
    println ("            = " + prg.fitLabels)
    println ("fit         = " + prg.fit)

    banner ("test for collinearity")
    println ("corr = " + prg.corrMatrix)
    println ("vif  = " + prg.vif)

    banner ("test predictions")
    val yp = t.map (prg.predict (_))
    println (s" y = $y \n yp = $yp")
    new Plot (t, y, yp, "PolyRegression")

    val z = 10.5                                    // predict y for one point
    val yp2 = prg.predict (z)
    println ("predict (" + z + ") = " + yp2)

} // PolyRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PolyRegressionTest2` object tests `PolyRegression` class using the following
 *  regression equation.  This version uses orthogonal polynomials.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*t + b_2*t^2 + ... b_k*t_k
 *  <p>
 *  > runMain scalation.analytics.PolyRegressionTest2
 */
object PolyRegressionTest2 extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 100.0)
    val t     = VectorD.range (0, 100)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 100) y(i) = 10.0 - 10.0 * i + i~^2 + i * noise.gen

    println ("t = " + t)
    println ("y = " + y)

    val order     = 6
    val technique = Cholesky                        // others: QR, SVD, LU or Inverse
    val prg       = new PolyRegression (t, y, order, technique, false)
    prg.train ().eval ()

    println ("coefficient = " + prg.coefficient)
    println ("            = " + prg.fitLabels)
    println ("fit         = " + prg.fit)

    banner ("test for collinearity")
    println ("corr = " + prg.corrMatrix)
    println ("vif  = " + prg.vif)

    banner ("test predictions")
    val yp = t.map (prg.predict (_))
    println (s" y = $y \n yp = $yp")
    new Plot (t, y, yp, "PolyRegression")

    val z = 10.5                                    // predict y for one point
    val yp2 = prg.predict (z)
    println ("predict (" + z + ") = " + yp2)

} // PolyRegressionTest2 object

