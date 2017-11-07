
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Nov 14 15:21:25 EST 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.pow

import scalation.calculus.Differential.FunctionV2S
import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.double_exp
import scalation.minima.QuasiNewton

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadraticFit` class uses multiple regression to fit a quadratic surface
 *  to the function 'f'.  This is useful when computing 'f' is costly, for example
 *  in simulation optimization.  The fit is over a multi-dimensional grid and
 *  can be used for interpolation and limited extrapolation.
 *  @param f  the vector-to-scalar function to fit.
 *  @param n  the dimensionality of the domain of f
 *  @param k  the number (odd number) of values for each dimension, e.g., 5, 7, 9
 */
class QuadraticFit (f: FunctionV2S, n: Int = 3, k: Int = 5)
{
    /** The middle position
     */
    private val m = k / 2

    /** The number of vectors in the grid
     */
    private val k_pow_n = pow (k, n).toInt

    /** Grid of design points/vectors at which function f is to be evaluated
     */
    private val grid = Array.ofDim [VectorD] (k_pow_n)

    /** The next point in the grid
     */
    private var nxt = 0

    /** The displacement vector, e.g., -2, -1, 0, 1, 2 for k = 5
     */
    private val y  = new VectorD (k)
    for (j <- 0 until k) y(j) = j - m

    /** The number of quadratic, linear and constant forms/terms (3, 6, 10, 15, ...)
     */
    private val nt = ((n + 1) * (n + 2)) / 2

    /** Regression coefficients to be determined using multiple regression
     */
    private var b: VectoD = null

    /** Regression class for performing multiple regression
     */
    private var reg: Regression [MatrixD, VectorD] = _

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a center point x, form a square grid around it.  This can be used
     *  to create a design matrix for use in multiple regression.  
     *  @param x  the center point/vector of the grid
     *  @param i  the current dimension (facilitates recursion)
     *  @param d  the distance to move on each step
     *  @param m  move m steps above and below x(i) for each dimension i
     */
    def formGrid (xc: VectorD, xs: VectorD)
    {
        nxt = 0                                     // reset the vector counter
        val x = new VectorD (n)
        for (i <- 0 until k_pow_n) {
           var ii = i
           for (j <- 0 until n) {
               x(j) = y(ii % k)
               ii /= k
           } // for
           val xx = x * xs + xc
           grid (nxt) = x * xs + xc                 // unscaled, uncentered point
           nxt += 1
        } // for
    } // formGrid

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print all the vectors/points in the grid.
     */
    def printGrid ()
    {
        println ("grid with " + nxt + " points")
        for (i <- 0 until grid.length) println (i + ":\t " + grid(i))
    } // printGrid

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
     *  for 1D:  'VectorD (1., x(0), x(0)~^2.)'
     *  for 2D:  'VectorD (1., x(0), x(0)~^2., x(1), x(1)*x(0), x(1)~^2.)'
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
        var sum = 0.0
        for (i <- 0 to n; j <- 0 to i) { sum += b(k) * y(i) * y(j); k += 1 }
        sum
    } // qFormsEval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a design matrix and response vector, use multiple regression to fit
     *  the surface, i.e., determine the coefficients of the regression equation.
     *  @param xx  the data/design matrix
     *  @param yy  the response vector
     */
    def fit (xx: MatrixD, yy: VectorD)
    {
        reg = new Regression (xx, yy)
        reg.train ()
        b = reg.coefficient                     // coefficients in regression equation
        println ("b = " + b)
        println ("fit = " + reg.fit)            // coefficient of determination, etc.
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reduce from the full model to one with fewer variable.
     *  FIX: adjust 'qFormEval' to skip left out variable
     */
    def reduce ()
    {
        val res = reg.backElim ()
        b = res._2                              // coefficients in regression equation
        println ("b = " + b)
        println ("fit = " + res._3)             // coefficient of determination
    } // reduce

} // QuadraticFit class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadraticFitTest` object is used to test the `QuadraticFit` class
 *  for a two dimensional case.
 */
object QuadraticFitTest extends App
{
    def f(x: VectorD): Double = (x(0) - 10.0)~^2.0 + (x(1) - 20.0)~^2.0 + 1.0

    // form the grid
    val xc = VectorD (5.0, 10.0)                     // center of the grid
    val xs = VectorD (1.0, 1.0)                      // scaling - none
    val qf = new QuadraticFit (f, 2)                 // construct a quadratic fit object
    qf.formGrid (xc, xs)                             // form a grid around point xc
    qf.printGrid ()

    // create the data/design matrix
    val (xx, yy) = qf.response ()                    // compute the response surface for the grid
    println ("design matrix + response vector")
    println (xx :+ yy)        

    // fit a multi-dimensional quadratic function
    println ("fit from regression")
    qf.fit (xx, yy)                                  // use multiple regression to fit surface

    // test closeness of fit
    val x1 = VectorD (6.0, 9.0)                      // test the fit for point x1
    val yp = qf.qFormsEval (x1)                      // evaluate using regression equation
    println ("y  = " + f(x1))                        // actual y value at x1
    println ("yp = " + yp)                           // predicted y value at x1

    // find minimal point in response surface
    def fp (x: VectorD): Double = qf.qFormsEval (x)  // prediction function
    
    val bfgs = new QuasiNewton (fp)                  // optimize using nonlinear programming
    val x = bfgs.solve (xc)
    println ("optimal solution x = " + x + " with objective value f(x) = " + f(x) +
                                           " and predicted value fp(x) = " + fp(x))

} // QuadraticFitTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadraticFitTest2` object is used to test the `QuadraticFit` class
 *  for a three dimensional case.
 */
object QuadraticFitTest2 extends App
{
    def f(x: VectorD): Double = (x(0) - 10.0)~^2.0 + (x(1) - 20.0)~^2.0 + (x(2) - 5.0)~^2.0 + 1.0

    // form the grid
    val xc = VectorD (10.0, 20.0, 10.0)              // center to orgin
    val xs = VectorD (1.0, 2.0, 1.0)                 // scaling to [-m, m]
    val qf = new QuadraticFit (f)                    // construct a quadratic fit object
    qf.formGrid (xc, xs)                             // form a grid around point xc
    qf.printGrid ()

    // create the data/design matrix
    val (xx, yy) = qf.response ()                    // compute the response surface for the grid
    println ("design matrix + response vector")
    println (xx :+ yy)        

    // fit a multi-dimensional quadratic function
    println ("fit from regression")
    qf.fit (xx, yy)                                  // use multiple regression to fit surface

    // find minimal point in response surface
    def fp (x: VectorD): Double = qf.qFormsEval (x)  // prediction function
    
    val bfgs = new QuasiNewton (fp)                  // optimize using nonlinear programming
    val x = bfgs.solve (xc)
    println ("optimal solution x = " + x + " with objective value f(x) = " + f(x) +
                                           " and predicted value fp(x) = " + fp(x))
} // QuadraticFitTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadraticFitTest3` object is used to test the `QuadraticFit` class
 *  for a three dimensional case with noise.
 */
object QuadraticFitTest3 extends App
{
    import scalation.random.Normal

    val e = Normal ()                                // standard normal distribution

    def f(x: VectorD): Double = (x(0) - 10.0)~^2.0 + (x(1) - 20.0)~^2.0 + (x(2) - 5.0)~^2.0 + 1.0 + e.gen

    // form the grid
    val xc = VectorD (10.0, 20.0, 10.0)              // center to orgin
    val xs = VectorD (1.0, 2.0, 1.0)                 // scaling to [-m, m]
    val qf = new QuadraticFit (f)                    // construct a quadratic fit object
    qf.formGrid (xc, xs)                             // form a grid around point xc
    qf.printGrid ()

    // create the data/design matrix
    val (xx, yy) = qf.response ()                    // compute the response surface for the grid
    println ("design matrix + response vector")
    println (xx :+ yy)

    // fit a multi-dimensional quadratic function
    println ("fit from regression")
    qf.fit (xx, yy)                                  // use multiple regression to fit surface
//  qf.reduce ()                                     // use a reduce model

    // find minimal point in response surface
    def fp (x: VectorD): Double = qf.qFormsEval (x)  // prediction function

    val bfgs = new QuasiNewton (fp)                  // optimize using nonlinear programming
    val x = bfgs.solve (xc)
    println ("optimal solution x = " + x + " with objective value f(x) = " + f(x) +
                                           " and predicted value fp(x) = " + fp(x))
} // QuadraticFitTest3

