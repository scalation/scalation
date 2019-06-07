
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Jan  5 14:00:12 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.math.FunctionS2S
import scalation.linalgebra.{MatriD, VectoD, VectoI}
import scalation.linalgebra.MatrixD.++^
import scalation.linalgebra.VectorD.one

import PredictorMat.pullResponse
import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A General Linear Model 'GLM' can be developed using the `GLM` trait and object
 *  (see below).  The implementation currently supports univariate models with
 *  multivariate models (where each response is a vector) planned for the future.
 *  It provides factory methods for the following special types of GLMs:
 *  `SimpleRegression` - simple linear regression,
 *  `Regression`       - multiple linear regression using Ordinary Least Squares 'OLS'
 *  `Regression_WLS`   - multiple linear regression using Weighted Least Squares 'WLS'
 *  `RidgeRegression`  - robust multiple linear regression,
 *  `TranRegression`   - transformed (e.g., log) multiple linear regression,
 *  `PolyRegression`   - polynomial regression,
 *  `TrigRegression`   - trigonometric regression
 *  `ResponseSurface`  - response surface regression,
 *  `ANOVA1`           - GLM form of ANalysis Of VAriance, 
 *  `ANCOVA1`          - GLM form of ANalysis of COVAriance.
 */
trait GLM
{
    protected var add_1     = true       // by default, prepend a column of all ones to the data matrix
    protected var technique = QR         // by default, use QR Factorization for
                                         // the regression technique used to solve for b in x.t*x*b = x.t*y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Simple Linear Regression model, automatically prepending the
     *  column of ones (form matrix from two column vectors [ 1 x ]).
     *  @param x  the data/input m-by-1 vector
     *  @param y  the response/output m-vector
     */
    def apply (x: VectoD, y: VectoD): SimpleRegression =
    {
        new SimpleRegression (++^ (one (x.dim), x), y)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Regression model using Ordinary Least Squares 'OLS'.
     *  @param x  the data/input m-by-n matrix
     *  @param y  the response/output m-vector
     */
    def apply (x: MatriD, y: VectoD): Regression =
    {
        if (add_1)
            new Regression (one (x.dim1) +^: x, y, null, null, technique)
        else
            new Regression (x, y, null, null, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Regression model using Ordinary Least Squares 'OLS'.
     *  @param xy  the combined data/input m-by-n matrix and response m-vector
     */
    def apply (xy: MatriD): Regression =
    {
        val (x, y) = pullResponse (xy)
        if (add_1)
            new Regression (one (xy.dim1) +^: x, y, null, null, technique)
        else
            new Regression (x, y, null, null, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Regression model using Weighted Least Squares 'WLS'.
     *  @param x  the data/input m-by-n matrix
     *  @param y  the response m-vector
     */
    def apply (x: MatriD, y: VectoD, w: VectoD): Regression_WLS =
    {
        if (add_1)
            new Regression_WLS (one (x.dim1) +^: x, y, null, technique, w)
        else
            new Regression_WLS (x, y, null, technique, w)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Robust Regression model.
     *  @param x       the centered data/input m-by-n matrix NOT augmented with a first column of ones
     *  @param y       the centered response vector
     *  @param lambda  the shrinkage hyper-parameter (0 => OLS) in the penalty term 'lambda * b dot b'
     */
    def apply (x: MatriD, y: VectoD, lambda: Double): RidgeRegression =
    {
        val hp = RidgeRegression.hp.updateReturn ("lambda", lambda)
        new RidgeRegression (x, y, null, hp, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Robust Regression model.
     *  @param xY      the combined centered data/input m-by-n matrix and response vector
     *  @param lambda  the shrinkage parameter (0 => OLS) in the penalty term 'lambda * b dot b'
     */
    def apply (xy: MatriD, lambda: Double): RidgeRegression =
    {
        val (x, y) = pullResponse (xy)
        val hp = RidgeRegression.hp.updateReturn ("lambda", lambda)
        new RidgeRegression (x, y, null, hp, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Transformed Multiple Linear Regression model.
     *  @param x          the data/input m-by-n matrix
     *  @param y          the response m-vector
     *  @param transform  the transformation function (e.g., log)
     *  @param transInv   the inverse transformation function (e.g., exp)
     */
    def apply (x: MatriD, y: VectoD, transform: FunctionS2S, tranInv: FunctionS2S): TranRegression =
    {
        if (add_1)
            new TranRegression (one (x.dim1) +^: x, y, null, transform, tranInv, technique)
        else
            new TranRegression (x, y, null, transform, tranInv, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Transformed Multiple Linear Regression model.
     *  @param xy         the combined data/input m-by-n matrix and response m-vector
     *  @param transform  the transformation function (e.g., log)
     *  @param transInv   the inverse transformation function (e.g., exp)
     */
    def apply (xy: MatriD, transform: FunctionS2S, tranInv: FunctionS2S): TranRegression =
    {
        val (x, y) = pullResponse (xy)
        if (add_1)
            new TranRegression (one (xy.dim1) +^: x, y, null, transform, tranInv, technique)
        else
            new TranRegression (x, y, null, transform, tranInv, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Polynomial Regression model.
     *  @param t  the input vector: t_i expands to x_i = [1, t_i, t_i^2, ... t_i^k]
     *  @param y  the response vector
     *  @param k  the order of the polynomial
     */
    def apply (t: VectoD, y: VectoD, k: Int): PolyRegression =
    {
        new PolyRegression (t, y, k, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Polynomial Regression model.
     *  @param ty  the combined input vector and response vector
     *  @param k   the order of the polynomial
     */
    def apply (ty: MatriD, k: Int): PolyRegression =
    {
        new PolyRegression (ty.col (0), ty.col (1), k, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Trigonometric Regression model.
     *  @param t  the input vector: 't_i' expands to 'x_i'
     *  @param y  the response vector
     *  @param k  the maximum multiplier in the trig function 'kwt'
     *  @param p   extra parameter to make apply methods unique (pass in 0)
     */
    def apply (t: VectoD, y: VectoD, k: Int, p: Int): TrigRegression =
    {
        new TrigRegression (t, y, k, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Trigonometric Regression model.
     *  @param ty  the combined input vector and response vector
     *  @param k   the maximum multiplier in the trig function 'kwt'
     *  @param p   extra parameter to make apply methods unique (pass in 0)
     */
    def apply (ty: MatriD, k: Int, p: Int): TrigRegression =
    {
        new TrigRegression (ty.col (0), ty.col (1), k, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Response Surface model.
     *  @param x_     the input vectors/points
     *  @param y      the response vector
     *  @param cubic  the order of the surface (false for quadratic, true for cubic)
     */
    def apply (x_ : MatriD, y: VectoD, cubic: Boolean): ResponseSurface =
    {
        new ResponseSurface (x_, y, null, cubic)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an ANalysis Of VAriance (ANOVA) model.
     *  @param t       the treatment/categorical variable vector
     *  @param levels  the number of treatment levels (1, ... levels)
     *  @param y       the response vector
     */
    def apply (t: VectoD, levels: Int,  y: VectoD): ANOVA1 =
    {
        new ANOVA1 (t, y, levels, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an ANalysis of COVAriance (ANCOVA1) model.
     *  @param x_      the data/input matrix of continuous variables
     *  @param t       the treatment/categorical variable vector
     *  @param y       the response vector
     */
    def apply (x_ : MatriD, y: VectoD, t: VectoI): ANCOVA1 =
    {
        new ANCOVA1 (x_, t, y, null, technique)
    } // apply

} // GLM trait


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GLM` object makes the `GLM` trait's methods directly available.
 *  This approach (using traits and objects) allows the methods to also be inherited.
 */
object GLM extends GLM

import scalation.linalgebra.{MatrixD, VectorD, VectorI}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GLMTest` object tests the `GLM` object using the following regression
 *  equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2 + b_3*d_1 + b_4*d_2
 *  <p>
 */
object GLMTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val t = VectorI (1, 1, 2, 2, 3)                              // treatments levels
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)         // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0)

    println ("x = " + x)
    println ("t = " + t)
    println ("y = " + y)

    val levels = 3
/*
    val glm    = GLM ( /* TBD */ )
    glm.train ().eval ()
    println ("fit = " + glm.fit)

    val yp = glm.predict (z)
    println ("predict (" + z + ") = " + yp)
*/

} // GLMTest object

