
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.{abs, ceil, max, min, pow}

import scalation.calculus.Differential.{gradient, gradientD}
import scalation.linalgebra.FunctionV2S
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.math.double_exp
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StochasticGradient` class solves unconstrained Non-Linear Programming (NLP)
 *  problems using the Stochastic Gradient Descent algorithm.  Given a function 'f'
 *  and a starting point 'x0', the algorithm computes the gradient and takes steps
 *  in the opposite direction.  The algorithm iterates until it converges.  The
 *  algorithm is stochastic in sense that only a single batch is used in each step
 *  of the optimimation.  Examples (a number of rows) are are chosen for each batch.
 *  FIX - provide option to randomly select samples in batch
 *  @see leon.bottou.org/publications/pdf/compstat-2010.pdf
 *
 *  dir_k = -gradient (x)
 *
 *  minimize    f(x)
 *
 *  @param df       the data function on which to base the objective function
 *  @param dx       the data matrix
 *  @param dy       the response vector
 *  @param batch    the batch size
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 */
class StochasticGradient (fxy: (MatrixD, VectorD, VectorD) => Double, dx: MatrixD, dy: VectorD,
                          batch: Int = 10, exactLS: Boolean = true)
      extends Minimizer with Error
{
    private val DEBUG    = true                 // debug flag
    private val WEIGHT   = 1000.0               // weight on penalty for constraint violation
    private var given    = false                // default: functions for partials are not given

    private val nBatch   = ceil (dx.dim1 / batch.toDouble).toInt
    private var ib       = 0                    // index for ith batch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function for the ith batch.
     *  @param x  the vector to optimize (e.g., model parameters)
     */
    def f (x: VectorD): Double =
    {
        val dxib = dx.slice (ib * batch, min ((ib+1) * batch - 1, dx.dim1))
        val dyib = dy.slice (ib * batch, min ((ib+1) * batch - 1, dy.dim))
        fxy (dxib, dyib, x)
    } // f

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact 'GoldenSectionLS' or inexact 'WolfeLS' line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
    {
        def f_1D (z: Double): Double = f(x + dir * z)     // create a 1D function
        val ls = if (exactLS) new GoldenSectionLS (f_1D)  // Golden Section line search
                 else new WolfeLS (f_1D, .0001, .1)       // Wolfe line search (c1 = .0001, c2 = .1)
        ls.search (step)                                  // perform a line search
    } // lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the Stochastic Gradient
     *  Descent algorithm.
     *  @param x0     the starting point
     *  @param step   the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD =
    {
        var x    = x0                                  // current point
        var f_x  = f(x)                                // objective function at current point
        var y: VectorD = null                          // next point
        var f_y  = 0.0                                 // objective function at next point
        var dir  = -gradient (f, x)                    // initial direction is -gradient
        var dist = 1.0                                 // distance between current and next point
        var down = true                                // moving down flag

        for (k <- 1 to MAX_ITER if down && dist > toler && dir.normSq > toler) {

            for (ib <- 0 until nBatch) {                   // use each batch to make a step
                y   = x + dir * lineSearch (x, dir, step)  // determine the next point
                f_y = f(y)                                 // objective function value for next point
                dir = -gradient (f, y)                     // next search direction

                if (DEBUG) println ("solve: k = " + k + ", y = " + y + ", f_y = " + f_y + ", dir = " + dir)

                dist = (x - y).normSq                      // calc the distance between current and next point
                down = f_y < f_x                           // still moving down?
                if (down) { x = y; f_x = f_y }             // make the next point, the current point
            } // for
        } // for
        x                                              // return the current point
    } // solve

} // StochasticGradient class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StochasticGradientTest` object is used to test the `StochasticGradient` class.
 *  @see scalation.analytics.RegressionTest3
 *  > runMain scalation.minima.StochasticGradientTest
 */
object StochasticGradientTest extends App
{
    // 20 data points:      Constant      x_1     x_2    x_3      x_4
    //                                    Age  Weight    Dur   Stress
    val dx = new MatrixD ((20, 5), 1.0,   47.0,   85.4,   5.1,    33.0,
                                   1.0,   49.0,   94.2,   3.8,    14.0,
                                   1.0,   49.0,   95.3,   8.2,    10.0,
                                   1.0,   50.0,   94.7,   5.8,    99.0,
                                   1.0,   51.0,   89.4,   7.0,    95.0,
                                   1.0,   48.0,   99.5,   9.3,    10.0,
                                   1.0,   49.0,   99.8,   2.5,    42.0,
                                   1.0,   47.0,   90.9,   6.2,     8.0,
                                   1.0,   49.0,   89.2,   7.1,    62.0,
                                   1.0,   48.0,   92.7,   5.6,    35.0,
                                   1.0,   47.0,   94.4,   5.3,    90.0,
                                   1.0,   49.0,   94.1,   5.6,    21.0,
                                   1.0,   50.0,   91.6,  10.2,    47.0,
                                   1.0,   45.0,   87.1,   5.6,    80.0,
                                   1.0,   52.0,  101.3,  10.0,    98.0,
                                   1.0,   46.0,   94.5,   7.4,    95.0,
                                   1.0,   46.0,   87.0,   3.6,    18.0,
                                   1.0,   46.0,   94.5,   4.3,    12.0,
                                   1.0,   48.0,   90.5,   9.0,    99.0,
                                   1.0,   56.0,   95.7,   7.0,    99.0)
    //  response BP
    val dy = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
                      114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("dx = " + dx)
    println ("dy = " + dy)

    def fxy (x: MatrixD, y: VectorD, b: VectorD): Double = { val e = y - x * b; e dot e }   // least squares

    val sgd = new StochasticGradient (fxy, dx, dy)            // stochastic gradient descent
    val x0  = new VectorD (dx.dim2)                           // initial guess for parameters
    val x   = sgd.solve (x0)                                  // optimized value for parameters
    val e   = dy - dx * x                                     // error vector
    val sse = e dot e                                         // sum of squared errors
    val sst = (dy dot dy) - dy.sum~^2.0 / dx.dim1.toDouble    // total sum of squares
    val ssr = sst - sse                                       // regression sum of squares
    val rSquared = ssr / sst                                  // coefficient of determination

    println ("x        = " + x)
    println ("sse      = " + sse)
    println ("rSquared = " + rSquared)

} // StochasticGradientTest object

