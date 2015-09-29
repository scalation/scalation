
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Jan 28 17:18:16 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see gwu.geverstine.com/pdenum.pdf
 */

package scalation.calculus

import scalation.linalgebra.{MatrixD, VectorD}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Calculus` object contains function for computing derivatives, gradients
 *  and Jacobians.
 */
object Calculus
{
    type FunctionS2S = Double => Double      // function of a scalar
    type FunctionV2S = VectorD => Double     // function of a vector

    private var h  = 1E-6      // default step size used for estimating derivatives
    private var h2 = h + h     // twice the step size
    private var hh = h * h     // step size squared

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Zero function.
     */
    def _0f (x: Double): Double = 0.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** One function.
     */
    def _1f (x: Double): Double = 1.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the step size from its default step size to one more suitable for
     *  your function.  A heuristic for the central difference method is to let
     *  h = max (|x|,1) * (machine-epsilon)^(1/3)
     *  For double precision, the machine-epsilon is about 1E-16.
     *  @see http://www.karenkopecky.net/Teaching/eco613614/Notes_NumericalDifferentiation.pdf
     *  @param step  the new step size to reset h to
     */
    def resetH (step: Double) { h = step; h2 = h + h; hh = h * h }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // First Order
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the derivative of the scalar-to-scalar function f at x using
     *  a 1-sided method (forward difference).  Approximate the tangent line at
     *  (x, f(x)) with the secant line through points (x, f(x)) and (x+h, f(x+h)).
     *  @param f  the function whose derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative1 (f: FunctionS2S, x: Double): Double = (f(x + h) - f(x)) / h

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the derivative of the scalar-to-scalar function f at x using
     *  a 2-sided method (central difference).  Approximate the tangent line at
     *  (x, f(x)) with the secant line through points (x-h, f(x-h)) and (x+h, f(x+h)).
     *  Tends to be MORE ACCURATE than the 1-sided method.
     *  @see http://www.math.montana.edu/frankw/ccp/modeling/continuous/heatflow2/firstder.htm
     *  @param f  the function whose derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative (f: FunctionS2S, x: Double): Double = (f(x + h) - f(x - h)) / h2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the ith partial derivative of the vector-to-scalar function f at
     *  point x returning the value for the partial derivative for dimension i.
     *  @param f  the function whose partial derivative is sought
     *  @param x  the point (vector) at which to estimate the partial derivative
     *  @param i  the dimension to compute the partial derivative on
     */
    def partial (f: FunctionV2S, x: VectorD, i: Int): Double = (f(x + (h, i)) - f(x - (h, i))) / h2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the gradient of the vector-to-scalar function f at point x
     *  returning a value for the partial derivative for each dimension of x.
     *  @param f  the function whose gradient is sought
     *  @param x  the point (vector) at which to estimate the gradient
     */
    def gradient (f: FunctionV2S, x: VectorD): VectorD =
    {
        val c = new VectorD (x.dim)
        for (i <- 0 until x.dim) c(i) = (f(x + (h, i)) - f(x - (h, i))) / h2
        c
    } // gradient

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the gradient of the vector-to-scalar function f using partial
     *  derivative functions evaluated at point x.  Return a value for the
     *  partial derivative for each dimension of the vector x.
     *  @param d  the array of partial derivative functions
     *  @param x  the point (vector) at which to compute the gradient
     */
    def gradientD (d: Array [FunctionV2S], x: VectorD): VectorD =
    {
        val c = new VectorD (x.dim)
        for (i <- 0 until x.dim) c(i) = d(i)(x)
        c
    } // gradientD

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the slope of the vector-to-scalar function f defined on mixed
     *  real/integer vectors.
     *  @param f  the function whose slope is sought
     *  @param x  the point (vector) at which to estimate the slope
     *  @param n  the number of dimensions that are real-valued (rest are integers)
     */
    def slope (f: FunctionV2S, x: VectorD, n: Int = 0): VectorD =
    {
        val c = new VectorD (x.dim)
        for (i <- 0 until x.dim) {
            c(i) = if (i < n) (f(x + (h, i)) - f(x - (h, i))) / h2   // derivative
                   else       (f(x + (1, i)) - f(x - (1, i))) / 2.0  // difference
        } // for
        c
    } // slope

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Jacobian matrix for a vector-valued function represented as
     *  an array of scalar-valued functions.  The i-th row in the matrix is the
     *  gradient of the i-th function.
     *  @param f  the array of functions whose Jacobian is sought
     *  @param x  the point (vector) at which to estimate the Jacobian
     */
    def jacobian (f: Array [FunctionV2S], x: VectorD): MatrixD =
    {
        val j = new MatrixD (f.length, x.dim)
        for (i <- 0 until f.length) j(i) = gradient (f(i), x)
        j
    } // jacobian

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Second Order
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the second derivative of the scalar-to-scalar function f at x
     *  using the central difference formula for second derivatives.
     *  @param f  the function whose second derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative2 (f: FunctionS2S, x: Double): Double = (f(x + h) - 2.0*f(x) + f(x - h)) / hh

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the (i,j)th second partial derivative of the vector-to-scalar
     *  function f at point x returning the value for the second partial derivative
     *  for dimensions (i, j).  If i = j, the second partial derivative is
     *  called "pure", otherwise it is a "cross" second partial derivative.
     *  @param f  the function whose second partial derivative is sought
     *  @param x  the point (vector) at which to estimate the second partial derivative
     *  @param i  the first dimension to compute the second partial derivative on
     *  @param j  the second dimension to compute the second partial derivative on
     */
    def partial2 (f: FunctionV2S, x: VectorD, i: Int, j: Int): Double = 
    {
        if (i == j) (f(x + (h, i)) - 2.0*f(x) + f(x - (h, i))) / hh              // pure partial
        else 0.0 // FIX: (f(x + (h, i, j)) - 2.0*f(x) + f(x - (h, i, j))) / hh   // cross partial
    } // partial2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the Hessian of the vector-to-scalar function f at point x
     *  returning a matrix of second partial derivative.
     *  @param f  the function whose Hessian is sought
     *  @param x  the point (vector) at which to estimate the Hessian
     */
    def hessian (f: FunctionV2S, x: VectorD): MatrixD =
    {
        // FIX - to be implemented
        null
    } // hessian

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the Laplacian of the vector-to-scalar function f at point x
     *  returning the sum of the pure second partial derivatives.
     *  @param f  the function whose Hessian is sought
     *  @param x  the point (vector) at which to estimate the Hessian
     */
    def laplacian (f: FunctionV2S, x: VectorD): Double =
    {
        var sum = 0.0
        for (i <- 0 until x.dim) sum += (f(x + (h, i)) - 2.0*f(x) + f(x - (h, i))) / hh
        sum
    } // laplacian

} // Calculus object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CalculusTest` object is used to test the `Calculus` object.
 */
object CalculusTest extends App
{
    import Calculus._

    def g (y: Double): Double = 2.0 * (y - 3.0) * (y - 3.0)

    var y = 0.0
    println ("derivative  g(" + y + ") = " + derivative (g, y))
    y = 1.0
    println ("derivative  g(" + y + ") = " + derivative (g, y))

    def f (x: VectorD): Double = 2.0 * (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0)
    def df_dx0 (x: VectorD): Double = 4.0 * x(0) - 12.0
    def df_dx1 (x: VectorD): Double = 2.0 * x(1) - 8.0
    val df = Array [FunctionV2S] (df_dx0, df_dx1)

    var x = VectorD (0.0, 0.0)
    println ("gradient  f(" + x + ") = " + gradient (f, x))
    println ("gradientD f(" + x + ") = " + gradientD (df, x))
    x = VectorD (1.0, 1.0)
    println ("gradient  f(" + x + ") = " + gradient (f, x))
    println ("gradientD f(" + x + ") = " + gradientD (df, x))

    def f1 (x: VectorD): Double = 2 * x(0) + x(1)
    def f2 (x: VectorD): Double = 2 * x(0) - x(1)
    val fa = Array [FunctionV2S] (f1, f2)
    println ("jacobian  fa(" + x + ") = " + jacobian (fa, x))

} // CalculusTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CalculusTest2` object is used to test the `Calculus` object showing trade-offs
 *  of using  1-sided and 2-sided derivative approximations as well as different
 *  values for h.
 *  @see http://www.rose-hulman.edu/~bryan/lottamath/diffgrad.pdf
 */
object CalculusTest2 extends App
{
    import Calculus._
    import math.{abs, cos, sin}

    def f (x: Double): Double = sin (x)      // the function

    def d (x: Double): Double = cos (x)      // its derivative

    var x = Array (.0, .1, .2, .3, .4, .5, .6, .7, .8, .9)
    for (i <- 0 until x.length) {
        var hh = 1E-4
        println (" x \t\t h \t\t deriv \t\t 1-sided \t\t error \t\t 2-sided \t\t error")
        for (k <- 0 until 9) {
            resetH (hh)
            val (d0, d1, d2) = (d(x(i)), derivative1 (f, x(i)), derivative (f, x(i)))
            println (x(i) + "\t" + hh + "\t" + d0 + "\t" + d1 + "\t" + abs (d1-d0) + "\t" + d2 + "\t" + abs (d2-d0))
            hh /= 10.0
        } // for
        println ()
    } // for

} // CalculusTest2 object

