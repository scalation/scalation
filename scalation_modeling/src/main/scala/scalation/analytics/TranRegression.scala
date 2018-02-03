
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 1.4
 *  @date    Sat Jan 20 15:41:27 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{abs, exp, log, sqrt}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.{double_exp, FunctionS2S, sq}
import scalation.random.Normal

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegression` class supports transformed multiple linear regression.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the transformed regression equation
 *  <p>
 *      transform (y)  =  b dot x + e  =  b_0 + b_1 * x_1 +  b_2 * x_2 ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model) and
 *  'transform' is the function (defaults to log) used to transform the response vector 'y'.
 *  Common transforms include 'log (y)', 'sqrt (y)' when 'y > 0', or even 'sq (y)', 'exp (y)'.
 *  More generally, a Box-Cox Transformation may be applied.
 *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.469.7176&rep=rep1&type=pdf
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector 'b'
 *  Note: this class does not provide transformations on columns of matrix 'x'.
 *  @see www.ams.sunysb.edu/~zhu/ams57213/Team3.pptx
 *  @param x          the design/data matrix
 *  @param y          the response vector
 *  @param transform  the transformation function (defaults to log)
 *  @param transInv   the inverse transformation function to rescale predictions to original y scale (defaults to exp)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class TranRegression [MatT <: MatriD, VecT <: VectoD] (x: MatT, y: VecT,  transform: FunctionS2S = log,
                     transInv: FunctionS2S = exp, technique: RegTechnique = QR)
      extends Regression (x, y.map (transform), technique)
{
    if (x.dim1 <= x.dim2) throw new IllegalArgumentException ("not enough data rows in matrix to use regression")
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (! y.isNonnegative)
        throw new IllegalArgumentException ("y must be positive for transformed regression (log, sqrt")
                                            // FIX - may work for other transformations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics
     *  @param yy   the response vector
     */
    override def eval (yy: VectoD = y) =
    {
        e = y - (x * b).map (transInv)                // residual/error vector
        diagnose (y)
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = transInv (b dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z    the new matrix to predict
     */
    override def predict (z: MatT): VectoD = (z * b).map (transInv)

} // TranRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegression` companion object provides transformation and inverse
 *  transformation function based on the parameter 'lambda'.
 *  It support the family of Box-Cox transformations.
 */
object TranRegression
{
    private var lambda = 0.5                  // the power parameter for Box-Cox tranformations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value for the 'lambda' parameter.
     *  @param lambda_  the new value for the 'lambda' parameter
     */
    def set_lambda (lambda_ : Double) { lambda = lambda_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform 'y' using the Box-Cox transformation.
     *  @param y  the value to be transformed
     */
    def box_cox (y: Double) = if (lambda == 0.0) log (y) else (y ~^ lambda - 1.0) / lambda

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Inverse transform 'z' using the Box-Cox transformation.
     *  @param z  the value to be inverse transformed
     */
    def cox_box (z: Double) = if (lambda == 0.0) exp (z) else (lambda * z + 1.0) ~^ (1.0 / lambda)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Create a `TranRegression` object that uses a Box-Cox transformation.
     *  @param x          the data/design matrix
     *  @param y          the response vector
     *  @param lamdba_    the Box-Cox power parameter
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
     *
    def apply (x: MatriD, y: VectoD, lambda_ : Double = 0.5, technique: RegTechnique = QR): TranRegression =
    {
        set_lambda (lambda_)
        new TranRegression (x, y, box_cox, cox_box, technique)
    } // apply
     */

} // TranRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      log (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest
 */
object TranRegressionTest extends App
{
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x)
    println ("y = " + y)

    val trg = new TranRegression (x, y)
    trg.train ().eval ()
    println ("fit = " + trg.fit)

    val yp = trg.predict (z)
    println ("predict (" + z + ") = " + yp)

} // TranRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest2` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      sqrt (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest2
 */
object TranRegressionTest2 extends App
{
    val cap     = 40
    val cap_rng = 0 until cap
    val (m, n)  = (cap * cap, 3)
    val std     = 10.0
    val err     = Normal (0.0, std)
    val x       = new MatrixD (m, n)
    val y       = new VectorD (m)
    for (i <- cap_rng; j <- cap_rng) x(cap * i + j) = VectorD (1.0, 2 * i, 3 * j)
    for (k <- x.range1) y(k) = abs (std + sq (2 * x(k, 0) + 3 * x(k, 1)) + err.gen)

    val rg = new Regression (x, y)
    rg.train ().eval ()
    val trg = new TranRegression (x, y, sqrt _, sq _)
    trg.train ().eval ()

    println (rg.fitLabels)
    println (s"rg.fit = ${rg.fit}")
    println (s"trg.fit = ${trg.fit}")

} // TranRegressionTest2 object

