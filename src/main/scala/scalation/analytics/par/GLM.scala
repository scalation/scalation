
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Jan  5 14:00:12 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.par

import scalation.calculus.Calculus.FunctionS2S
import scalation.linalgebra.{VectorD, VectorI}
import scalation.linalgebra.par.MatrixD
import scalation.linalgebra.par.MatrixD.+:
import scalation.linalgebra.VectorD.one

import scalation.analytics.RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A General Linear Model (GLM) can be developed using the `GLM` trait and object
 *  (see below).  The implementation currently supports univariate models with
 *  multivariate models (where each response is a vector) planned for the future.
 *  This version uses parallel processing to speed up execution.
 *  It provides factory methods for the following special types of GLMs:
 *  `Regression`       - multiple linear regression,
 *  `RidgeRegression`  - robust multiple linear regression,
 *  `TranRegression`   - transformed (e.g., log) multiple linear regression,
 *  `PolyRegression`   - polynomial regression,
 *  `TrigRegression`   - trigonometric regression
 *  `ResponseSurface`  - response surface regression,
 *  `ANCOVA`           - GLM form of ANalysis of COVAriance.
 *  The following special types are excluded since they do not utilize large matrices.
 *  `SimpleRegression` - simple linear regression,
 *  `ANOVA`            - GLM form of ANalysis Of VAriance, 
 */
trait GLM
{
    protected var add_1     = true       // by default, prepend a column of all ones to the design matrix
    protected var technique = Fac_QR     // by default, use QR Factorization for
                                         // the regression technique used to solve for b in x.t*x*b = x.t*y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Explicitly set the add_1 flag (column of all ones corresponding to b_0).
     *  @param _add_1  the value to set the add_1 flag to
     */
    def setAdd_1 (_add_1: Boolean = true) { add_1 = _add_1 }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Explicitly set the regression technique to use.
     *  @param _technique  the value to set technique to
     */
    def setTechnique (_technique: RegTechnique = Fac_QR) { technique = _technique }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Regression model.
     *  @param x  the input/design m-by-n matrix
     *  @param y  the response m-vector
     */
    def apply (x: MatrixD, y: VectorD): Regression =
    {
        if (add_1)
            new Regression (+: (one (x.dim1), x), y, technique)
        else
            new Regression (x, y, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Regression model.
     *  @param xy  the combined input/design m-by-n matrix and response m-vector
     */
    def apply (xy: MatrixD): Regression =
    {
        if (add_1)
            new Regression (+: (one (xy.dim1), xy.sliceCol (0, xy.dim2-1)), xy.col (xy.dim2-1),
                            technique)
        else
            new Regression (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1), technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Robust Regression model.
     *  @param x       the centered input/design m-by-n matrix NOT augmented with a first column of ones
     *  @param y       the centered response vector
     *  @param lambda  the shrinkage parameter (0 => OLS) in the penalty term 'lambda * b dot b'
     */
    def apply (x: MatrixD, y: VectorD, lambda: Double): RidgeRegression =
    {
        new RidgeRegression (x, y, lambda, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Multiple Linear Robust Regression model.
     *  @param xY      the combined centered input/design m-by-n matrix and response vector
     *  @param lambda  the shrinkage parameter (0 => OLS) in the penalty term 'lambda * b dot b'
     */
    def apply (xy: MatrixD, lambda: Double): RidgeRegression =
    {
        new RidgeRegression (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1), lambda, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Transformed Multiple Linear Regression model.
     *  @param x          the input/design m-by-n matrix
     *  @param y          the response m-vector
     *  @param transform  the transformation function (e.g., log)
     */
    def apply (x: MatrixD, y: VectorD, transform: FunctionS2S): TranRegression =
    {
        if (add_1)
            new TranRegression (+: (one (x.dim1), x), y, transform, technique)
        else
            new TranRegression (x, y, transform, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Transformed Multiple Linear Regression model.
     *  @param xy         the combined input/design m-by-n matrix and response m-vector
     *  @param transform  the transformation function
     */
    def apply (xy: MatrixD, transform: FunctionS2S): TranRegression =
    {
        if (add_1)
            new TranRegression (+: (one (xy.dim1), xy.sliceCol (0, xy.dim2-1)), xy.col (xy.dim2-1),
                            transform, technique)
        else
            new TranRegression (xy.sliceCol (0, xy.dim2-1), xy.col (xy.dim2-1), transform, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Polynomial Regression model.  It makes a matrix using 'expand'.
     *  @param t  the input vector: t_i expands to x_i = [1, t_i, t_i^2, ... t_i^k]
     *  @param y  the response vector
     *  @param k  the order of the polynomial
     */
    def apply (t: VectorD, y: VectorD, k: Int): PolyRegression =
    {
        new PolyRegression (t, y, k, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Polynomial Regression model.
     *  @param ty  the combined input vector and response vector
     *  @param k   the order of the polynomial
     */
    def apply (ty: MatrixD, k: Int): PolyRegression =
    {
        new PolyRegression (ty.col (0), ty.col (1), k, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Trigonometric Regression model.  It makes a matrix using 'expand'.
     *  @param t  the input vector: t_i expands to x_i
     *  @param y  the response vector
     *  @param k  the maximum multiplier in the trig function (kwt)
     *  @param w  the base displacement angle in radians
     */
    def apply (t: VectorD, y: VectorD, k: Int, w: Double): TrigRegression =
    {
        new TrigRegression (t, y, k, w, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Trigonomtetric Regression model.
     *  @param ty  the combined input vector and response vector
     *  @param k   the maximum multiplier in the trig function (kwt)
     *  @param w   the base displacement angle in radians
     */
    def apply (ty: MatrixD, k: Int, w: Double): TrigRegression =
    {
        new TrigRegression (ty.col (0), ty.col (1), k, w, technique)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Response Surface model.
     *  @param x_     the input vectors/points
     *  @param y      the response vector
     *  @param cubic  the order of the surface (false for quadratic, true for cubic)
     */
    def apply (x_ : MatrixD, y: VectorD, cubic: Boolean): ResponseSurface =
    {
        new ResponseSurface (x_, y, cubic)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an ANalysis of COVAriance (ANCOVA) model.
     *  @param x_      the data/design matrix of continuous variables
     *  @param t       the treatment/categorical variable vector
     *  @param y       the response vector
     *  @param levels  the number of treatment levels (1, ... levels)
     */
    def apply (x_ : MatrixD, t: VectorI, y: VectorD, levels: Int): ANCOVA =
    {
        new ANCOVA (x_, t, y, levels, technique)
    } // apply

} // GLM trait


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GLM` object makes the `GLM` trait's methods directly available.
 *  This approach (using traits and objects) allows the methods to also be inherited.
 */
object GLM extends GLM


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
    val t = VectorI (1, 1, 2, 2, 3)                              // treatements levels
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)         // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0)

    println ("x = " + x)
    println ("t = " + t)
    println ("y = " + y)

    val levels = 3
/*
    val glm    = GLM ( /* TBD */ )
    glm.train ()
    println ("fit = " + glm.fit)

    val yp = glm.predict (z)
    println ("predict (" + z + ") = " + yp)
*/

} // GLMTest object

