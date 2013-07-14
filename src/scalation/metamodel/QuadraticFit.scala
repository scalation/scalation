
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Nov 14 15:21:25 EST 201
 *  @see     LICENSE (MIT style license file).
 */

package scalation.metamodel

import collection.mutable.ArrayBuffer

import scalation.analytics.Regression
import scalation.calculus.Calculus.FunctionV2S
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.DoubleWithExp._
import scalation.minima.QuasiNewton

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class uses multiple regression to fit a quadratic surface to the
 *  function f.  This is useful when computing f is costly, for example in
 *  simulation optimization.  The fit is over a multi-dimensional grid and
 *  can be used for interpolation and limited extrapolation.
 *  @param f  the vector-to-scalar function to fit.
 *  @param n  the dimensionality of the domain of f
 */
class QuadraticFit (f: FunctionV2S, n: Int = 2)
{
    /** Regression coefficients to be determined using multiple regression
     */
    private var b: VectorD = null

    /** Grid of design points/vectors at which function f is to be evaluated
     */
    private var grid: ArrayBuffer [VectorD] = null

    /** The number of quadratic, linear and constant forms/terms (3, 6, 10, 15, ...)
     */
    private val nt = ((n + 1) * (n + 2)) / 2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a center point x, form a square grid around it.  This can be used
     *  to create a design matrix for use in multiple regression.  
     *  @param x  the center point/vector of the grid
     *  @param i  the current dimension (facilitates recursion)
     *  @param d  the distance to move on each step
     *  @param m  move m steps above and below x(i) for each dimension i
     */
    def formGrid (x: VectorD, i: Int, d: Double = 1., m: Int = 2)
    {
        if (i == 0) {              // the first dimension
            grid = new ArrayBuffer [VectorD] (((2*m + 1) ~^ n).toInt)
            grid += x              // add the center point
        } // if
        for (j <- -m to m if j != 0) grid += x + (j*d, i)
        if (i < n - 1) {
           for (x_k <- grid) formGrid (x_k, i + 1, d, m)
        } // if
    } // formGrid

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a grid of design points, create a design matrix 'xx' and response
     *  vector 'yy' returning them as a tuple.
     */
    def response (): Tuple2 [MatrixD, VectorD] =
    {
        val xx = new MatrixD (grid.size, nt)  // design matrix
        val yy = new VectorD (grid.size)      // response vector
        for (k <- 0 until grid.size) {
            val x = grid (k)
            xx(k) = qForms (x)                // vector values for all quadratic forms
            yy(k) = f(x)                      // functional value at x
        } // for
        (xx, yy)
    } // response

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a vector 'x', compute the values for all of its quadratic, linear
     *  and constant forms/terms, returning them as a vector.
     *  for 1D:  VectorD (1., x(0), x(0)~^2.)
     *  for 2D:  VectorD (1., x(0), x(0)~^2., x(1), x(1)*x(0), x(1)~^2.)
     *  @param x  the source vector for creating forms/terms
     */
    def qForms (x: VectorD): VectorD =
    {
        val y  = x.oneAt (0, 1) ++ x    // augmented vector: [ 1., x(0), ..., x(n-1) ]
        val z  = new VectorD (nt)       // vector of all forms/terms
        var k  = 0;
        for (i <- 0 to n; j <- 0 to i) { z(k) = y(i) * y(j); k += 1 }
        z
    } // qForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a point x, use the quadratic regression equation to estimate a value
     *  for the function at x.
     *  for 1D:  b(0) + b(1)*x(0) + b(2)*x(0)~^2.
     *  for 2D:  b(0) + b(1)*x(0) + b(2)*x(0)~^2. + b(3)*x(1) + b(4)*x(1)*x(0) + b(5)*x(1)~^2.
     *  @param x  the point whose functional value is to be predicted
     */
    def qFormsEval (x: VectorD): Double =
    {
        val y   = x.oneAt (0, 1) ++ x    // augmented vector: [ 1., x(0), ..., x(n-1) ]
        var k   = 0
        var sum = 0.
        for (i <- 0 to n; j <- 0 to i) { sum += b(k) * y(i) * y(j); k += 1 }
        sum
    } // qFormsEval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a design matrix and response vector, use multiple regression to fit
     *  the surface, i.e., determine the coefficients of the regression equation.
     *  @param result  the design matrix and response vector grouped in a tuple
     */
    def fit (result: Tuple2 [MatrixD, VectorD])
    {
        val reg = new Regression (result._1, result._2)
        reg.train ()
        val res = reg.fit
        b = res._1                               // coefficients in regression equation
        println ("fit: R-squared = " + res._2)   // coefficient of determination
    } // fit

} // QuadraticFit


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object id used to test the QuadraticFit class.
 */
object QuadraticFitTest extends App
{
    def f(x: VectorD): Double = (x(0) - 10.)~^2. + (x(1) - 20.)~^2. + 1.

    val x0 = new VectorD (5., 10.)                   // center of the grid
    val qf = new QuadraticFit (f)                    // construct a quadratic fit object
    qf.formGrid (x0, 0)                              // form a grid around point x0
    val result = qf.response ()                      // compute the response surface for the grid
    println (result)        
    qf.fit (result)                                  // use multiple regression to fit surface

    val x1 = new VectorD (6., 9.)                    // test the fit for point x1
    val yp = qf.qFormsEval (x1)                      // evaluate using regression equation
    println ("y  = " + f(x1))                        // actual y value at x1
    println ("yp = " + yp)                           // predicted y value at x1

    def fp (x: VectorD): Double = qf.qFormsEval (x)  // prediction function
    
    val bfgs = new QuasiNewton (fp)                  // optimize using nonlinear programming
    val x = bfgs.solve (x0)
    println ("optimal solution x = " + x + " with objective value f(x) = " + f(x) +
                                           " and predicted value fp(x) = " + fp(x))

} // QuadraticFitTest

