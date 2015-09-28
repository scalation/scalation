
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Jan  6 13:17:19 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, MatrixD, VectorD}
import scalation.linalgebra.VectorD.one
import scalation.util.{Error, time}

import RegTechnique._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurface` class uses multiple regression to fit a quadratic/cubic
 *  surface to the data.  For example in 2D, the quadratic regression equation is
 *  <p>
 *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_1, x_0*x_1, x_1^2] + e
 *  <p>
 *  @see scalation.metamodel.QuadraticFit
 *  @param x_         the input vectors/points
 *  @param y          the response vector
 *  @param cubic      the order of the surface (defaults to quadratic, else cubic)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ResponseSurface (x_ : MatrixD, y: VectorD, cubic: Boolean = false, technique: RegTechnique = Fac_QR)
      extends Predictor with Error
{
    if (x_.dim1 != y.dim) flaw ("constructor", "the sizes of x_ and y are not compatible")

    /** The dimensionality (2D, 3D, etc.) of points in matrix x_
     */
    private val n = x_.dim2    

    /** The number of quadratic, linear and constant forms/terms (3, 6, 10, 15, ...)
     *  of cubic, quadratic, linear and constant forms/terms (4, 10, 20, 35, ...)
     */
    private val nt = if (cubic) (n + 1) * (n + 2) * (n + 3) / 6
                     else       (n + 1) * (n + 2) / 2

    /** The design matrix that includes all forms/terms for each point in x_
     */
    private val x = allForms ()

    /** The regression model
     */
    private val rsm = new Regression (x, y, technique)

    /** The parameter vector to be fit
     */
    private var b: VectorD = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create all forms/terms for each point placing them in a new matrix.
     */
    def allForms (): MatrixD =
    {
        val xa = new MatrixD (x_.dim1, nt)
        for (i <- 0 until x_.dim1)
            xa(i) = if (cubic) cForms (x_(i))    // vector values for all cubic forms
                    else       qForms (x_(i))    // vector values for all quadratic forms
        xa
    } // allForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a vector/point 'p', compute the values for all of its quadratic,
     *  linear and constant forms/terms, returning them as a vector.
     *  for 1D: p = (x_0)      => VectorD (1, x_0, x_0^2)
     *  for 2D: p = (x_0, x_1) => VectorD (1, x_0, x_0^2, x_0*x_1, x_1, x_1^2)
     *  @param p  the source vector/point for creating forms/terms
     */
    def qForms (p: VectorD): VectorD =
    {
        val q = one (1) ++ p          // augmented vector: [ 1., p(0), ..., p(n-1) ]
        val z = new VectorD (nt)      // vector of all forms/terms
        var l = 0;
        for (i <- 0 to n; j <- i to n) { z(l) = q(i) * q(j); l += 1 }
        z
    } // qForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a vector/point 'p', compute the values for all of its cubic, quadratic,
     *  linear and constant forms/terms, returning them as a vector.
     *  for 1D: p = (x_0)      => VectorD (1, x_0, x_0^2, x_0^3)
     *  for 2D: p = (x_0, x_1) => VectorD (1, x_0, x_0^2, x_0^3,
     *                                        x_0*x_1, x_0^2*x_1, x_0*x_1^2,
     *                                        x_1, x_1^2, x_1^3)
     *  @param p  the source vector/point for creating forms/terms
     */
    def cForms (p: VectorD): VectorD =
    {
        val q = one (1) ++ p          // augmented vector: [ 1., p(0), ..., p(n-1) ]
        val z = new VectorD (nt)      // vector of all forms/terms
        var l = 0;
        for (i <- 0 to n; j <- i to n; k <- j to n) { z(l) = q(i) * q(j) * q(k); l += 1 }
        z
    } // cForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  quadratic rsm regression equation, e.g., for 2D
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_1, x_1*x_0, x_1^2] + e
     *  using the least squares method.
     */
    def train () { rsm.train () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  quadratic rsm regression equation, e.g., for 2D
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_1, x_1*x_0, x_1^2] + e
     *  using the least squares method.
     *  @param yy  the new response vector
     */
    def train (yy: VectorD) { rsm.train (yy) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit including rSquared).
     */
    def fit: Tuple4 [VectorD, Double, Double, Double] =
    {
        val res = rsm.fit
        b = res._1
        res
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a point z, use the quadratic rsm regression equation to predict a
     *  value for the function at z.
     *  for 1D:  b_0 + b_1*z_0 + b_2*z_0^2
     *  for 2D:  b_0 + b_1*z_0 + b_2*z_0^2 + b_3*z_1 + b_4*z_1*z_0 + b_5*z_1^2
     *  @param z  the point/vector whose functional value is to be predicted
     */
    def predict (z: VectorD): Double =
    {
        val q   = one(1) ++ z    // augmented vector: [ 1., x(0), ..., x(n-1) ]
        var l   = 0
        var sum = 0.0
        if (cubic)
            for (i <- 0 to n; j <- i to n; k <- j to n) { sum += b(l) * q(i) * q(j) * q(k); l += 1 }
        else
            for (i <- 0 to n; j <- i to n) { sum += b(l) * q(i) * q(j); l += 1 }
        sum
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot zi for
     *  each row zi of matrix z.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatriD): VectorD = 
    {
        val sum = new VectorD (z.dim1)
        for (i <- 0 until z.dim1) sum(i) = predict (z(i))
        sum
    } // predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     */
    def backElim (): Tuple4 [Int, VectorD, Double, Double] = rsm.backElim ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-colinearity by regressing xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the varaince of xj can be predicted
     *  from the other variables, so xj is a candidate for removal from the model.
     */
    def vif: VectorD = rsm.vif

} // ResponseSurface class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurfaceTest` object is used to test the `ResponseSurface` class.
 */
object ResponseSurfaceTest extends App
{
    val x: MatrixD = null
    val y: VectorD = null

    val rsm = new ResponseSurface (x, y)

} // ResponseSurfaceTest object

