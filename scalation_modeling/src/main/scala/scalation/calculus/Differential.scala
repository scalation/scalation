
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Jan 28 17:18:16 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see gwu.geverstine.com/pdenum.pdf
 */

package scalation.calculus

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.math.FunctionS2S

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Differential` object contains functions for computing derivatives, partial
 *  derivatives, Laplacians, gradient vectors, Hessian matrices and Jacobian matrices.
 */
object Differential
{
    type FunctionV2S  = VectorD => Double      // function of a vector - VectorD
    type FunctionV_2S = VectoD  => Double      // function of a vector - VectoD  - base trait
    type FunctionsV   = Array [FunctionV2S]    // array of vector functions

    private var h   = 1E-6                     // default step size used for estimating derivatives
    private var h2  = h + h                    // twice the step size
    private var hh  = h * h                    // step size squared
    private var hh4 = 4.0 * hh                 // step size squared

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the step size from its default step size to one more suitable for
     *  your function.  A heuristic for the central difference method is to let
     *  'h = max (|x|,1) * (machine-epsilon)^(1/3)'
     *  For double precision, the machine-epsilon is about 1E-16.
     *  @see www.karenkopecky.net/Teaching/eco613614/Notes_NumericalDifferentiation.pdf
     *  @param step  the new step size to reset h to
     */
    def resetH (step: Double) { h = step; h2 = h + h; hh = h * h; hh4 = 4.0 * hh }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // First Order
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the derivative of the scalar-to-scalar function 'f' at 'x' using
     *  a 1-sided method (forward difference).  Approximate the tangent line at
     *  '(x, f(x))' with the secant line through points '(x, f(x))' and '(x+h, f(x+h))'.
     *  @param f  the function whose derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative1 (f: FunctionS2S, x: Double): Double = (f(x + h) - f(x)) / h

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the derivative of the scalar-to-scalar function 'f' at 'x' using
     *  a 2-sided method (central difference).  Approximate the tangent line at
     *  '(x, f(x))' with the secant line through points '(x-h, f(x-h))' and '(x+h, f(x+h))'.
     *  Tends to be MORE ACCURATE than the 1-sided method.
     *  @see www.math.montana.edu/frankw/ccp/modeling/continuous/heatflow2/firstder.htm
     *  @param f  the function whose derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative (f: FunctionS2S, x: Double): Double = (f(x + h) - f(x - h)) / h2

    def Ⅾ (f: FunctionS2S, x: Double): Double = (f(x + h) - f(x - h)) / h2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the 'i'th partial derivative of the vector-to-scalar function 'f' at
     *  point 'x' returning the value for the partial derivative for dimension 'i'.
     *  @param i  the dimension to compute the partial derivative on
     *  @param f  the function whose partial derivative is sought
     *  @param x  the point (vector) at which to estimate the partial derivative
     */
    def partial (i: Int)(f: FunctionV2S, x: VectorD): Double = (f(x + (i, h)) - f(x - (i, h))) / h2

    def ∂ (i: Int)(f: FunctionV2S, x: VectorD): Double = (f(x + (i, h)) - f(x - (i, h))) / h2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the gradient of the vector-to-scalar function 'f' at point 'x'
     *  returning a value for the partial derivative for each dimension of 'x'.
     *  @param f  the function whose gradient is sought
     *  @param x  the point (vector) at which to estimate the gradient
     */
    def gradient (f: FunctionV2S, x: VectorD): VectorD =
    {
        VectorD (for (i <- x.range) yield (f(x + (i, h)) - f(x - (i, h))) / h2)
    } // gradient

    def ∇ (f: FunctionV2S, x: VectorD): VectorD =
    {
        VectorD (for (i <- x.range) yield (f(x + (i, h)) - f(x - (i, h))) / h2)
    } // ∇

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the gradient of the vector-to-scalar function 'f' using partial
     *  derivative functions evaluated at point 'x.'  Return a value for the
     *  partial derivative for each dimension of the vector 'x.'
     *  @param d  the array of partial derivative functions
     *  @param x  the point (vector) at which to compute the gradient
     */
    def gradientD (d: FunctionsV, x: VectorD): VectorD =
    {
        VectorD (for (i <- x.range) yield d(i)(x))
    } // gradientD

    def ∇* (d: FunctionsV, x: VectorD): VectorD =
    {
        VectorD (for (i <- x.range) yield d(i)(x))
    } // ∇*

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the slope of the vector-to-scalar function 'f' defined on mixed
     *  real/integer vectors.
     *  @param f  the function whose slope is sought
     *  @param x  the point (vector) at which to estimate the slope
     *  @param n  the number of dimensions that are real-valued (rest are integers)
     */
    def slope (f: FunctionV2S, x: VectorD, n: Int = 0): VectorD =
    {
        val c = new VectorD (x.dim)
        for (i <- x.range) {
            c(i) = if (i < n) (f(x + (i, h)) - f(x - (i, h))) / h2     // derivative
                   else       (f(x + (i, 1)) - f(x - (i, 1))) / 2.0    // difference
        } // for
        c
    } // slope

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Jacobian matrix for a vector-valued function represented as
     *  an array of scalar-valued functions.  The 'i'th row in the matrix is the
     *  gradient of the 'i'th function.
     *  @param f  the array of functions whose Jacobian is sought
     *  @param x  the point (vector) at which to estimate the Jacobian
     */
    def jacobian (f: FunctionsV, x: VectorD): MatrixD =
    {
        MatrixD (for (i <- f.indices) yield gradient (f(i), x))
    } // jacobian

    def Ј (f: FunctionsV, x: VectorD): MatrixD =
    {
        MatrixD (for (i <- f.indices) yield gradient (f(i), x), false)   // false =>  rowwise
    } // Ј

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Second Order
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the second derivative of the scalar-to-scalar function 'f' at
     *  'x' using the central difference formula for second derivatives.
     *  @param f  the function whose second derivative is sought
     *  @param x  the point (scalar) at which to estimate the derivative
     */
    def derivative2 (f: FunctionS2S, x: Double): Double = (f(x + h) - 2.0*f(x) + f(x - h)) / hh

    def ⅮⅮ (f: FunctionS2S, x: Double): Double = (f(x + h) - 2.0*f(x) + f(x - h)) / hh

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the '(i,j)'th second partial derivative of the vector-to-scalar
     *  function 'f' at point 'x' returning the value for the second partial derivative
     *  for dimensions '(i, j)'.  If 'i = j', the second partial derivative is
     *  called "pure", otherwise it is a "cross" second partial derivative.
     *  @see www.uio.no/studier/emner/matnat/math/MAT-INF1100/h07/undervisningsmateriale/kap7.pdf
     *  @param i  the first dimension to compute the second partial derivative on
     *  @param j  the second dimension to compute the second partial derivative on
     *  @param f  the function whose second partial derivative is sought
     *  @param x  the point (vector) at which to estimate the second partial derivative
     */
    def partial2 (i: Int, j: Int)(f: FunctionV2S, x: VectorD): Double = 
    {
        val (hi, hj) = ((i, h), (j, h))
        if (i == j)                                                            // pure partial
            (f(x + hi) - 2.0*f(x) + f(x - hi)) / hh
        else                                                                   // cross partial
            (f(x + hi + hj) - f(x + hi - hj) - f(x - hi + hj) + f(x - hi - hj)) / hh4
    } // partial2

    def ∂∂ (i: Int, j: Int)(f: FunctionV2S, x: VectorD): Double = 
    {
        val (hi, hj) = ((i, h), (j, h))
        if (i == j)                                                            // pure partial
            (f(x + hi) - 2.0*f(x) + f(x - hi)) / hh
        else                                                                   // cross partial
            (f(x + hi + hj) - f(x + hi - hj) - f(x - hi + hj) + f(x - hi - hj)) / hh4 
    } // ∂∂

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the Hessian of the vector-to-scalar function 'f' at point 'x'
     *  returning a matrix of second partial derivative.
     *  @param f  the function whose Hessian is sought
     *  @param x  the point (vector) at which to estimate the Hessian
     */
    def hessian (f: FunctionV2S, x: VectorD): MatrixD =
    {
        MatrixD ((x.dim, x.dim), (for (i <- x.range; j <- 0 to i) yield ∂∂ (i, j)(f, x)) :_*)
    } // hessian

    def Η (f: FunctionV2S, x: VectorD): MatrixD =
    {
        MatrixD ((x.dim, x.dim), (for (i <- x.range; j <- 0 to i) yield ∂∂ (i, j)(f, x)) :_*)
    } // Η

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Estimate the Laplacian of the vector-to-scalar function 'f' at point 'x'
     *  returning the sum of the pure second partial derivatives.
     *  @param f  the function whose Hessian is sought
     *  @param x  the point (vector) at which to estimate the Hessian
     */
    def laplacian (f: FunctionV2S, x: VectorD): Double =
    {
        var sum = 0.0
        for (i <- x.range) sum += (f(x + (i, h)) - 2.0*f(x) + f(x - (i, h))) / hh
        sum
    } // laplacian

    def ∆ (f: FunctionV2S, x: VectorD): Double =
    {
        var sum = 0.0
        for (i <- x.range) sum += (f(x + (i, h)) - 2.0*f(x) + f(x - (i, h))) / hh
        sum
    } // ∆

} // Differential object

import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DifferentialTest` object is used to test the `Differential` object.
 *  > run-main scalation.calculus.DifferentialTest
 */
object DifferentialTest extends App
{
    import Differential._

    val g = (y: Double) => 2.0 * (y - 3.0) * (y - 3.0)
    val f = (x: VectorD) => 2.0 * (x(0) - 3.0) * (x(0) - 3.0) + (x(1) - 4.0) * (x(1) - 4.0) + x(0) * x(1)
    val f3 = (x: VectorD) => x(0) * x(1)
    val y = 2.0
    val x = VectorD (2.0, 2.0)

    banner ("Derivatives")
    println (s"Ⅾ g($y)  = ${Ⅾ (g, y)}")                       // derivative
    println (s"ⅮⅮ g($y) = ${ⅮⅮ (g, y)}")                      // second derivative

    banner ("Partial Derivatives")
    println (s"∂ (0)(f($x)    = ${∂ (0)(f, x)}")              // partial derivative wrt x0
    println (s"∂ (1)(f($x)    = ${∂ (1)(f, x)}")              // partial derivative wrt x1
    println (s"∂∂ (0,0)(f($x) = ${∂∂ (0,0)(f, x)}")           // second partial derivative wrt x0
    println (s"∂∂ (1,1)(f($x) = ${∂∂ (1,1)(f, x)}")           // second partial derivative wrt x1
    println (s"∂∂ (1,0)(f($x) = ${∂∂ (1,0)(f, x)}")           // cross partial derivative
    println (s"∂∂ (1,0)(f3($x) = ${∂∂ (1,0)(f3, x)}")           // cross partial derivative

    def df_dx0 (x: VectorD): Double = 4.0 * x(0) - 12.0
    def df_dx1 (x: VectorD): Double = 2.0 * x(1) - 8.0
    val df = Array [FunctionV2S] (df_dx0, df_dx1)

    banner ("Gradient Vectors")
    println (s"∇ f($x)  = ${∇ (f, x)}")                       // gradient
    println (s"∇* f($x) = ${∇* (df, x)}")                     // gradient with functions for partials

    def f1 (x: VectorD): Double = 2 * x(0) + x(1)
    def f2 (x: VectorD): Double = 2 * x(0) - x(1)
    val ff = Array [FunctionV2S] (f1, f2)

    banner ("Jacobian Matrices")
    println (s"Ј ff($x) = ${Ј (ff, x)}")                      // Jacobian

    banner ("Laplacians")
    println (s"∆ f($x) = ${∆ (f, x)}")                        // Laplacian

    banner ("Hessian Matrices")
    println (s"Η f($x) = ${Η (f, x)}")                        // Hessian

} // DifferentialTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DifferentialTest2` object is used to test the `Differential` object showing
 *  trade-offs of using  1-sided and 2-sided derivative approximations as well as
 *  different values for h.
 *  @see www.rose-hulman.edu/~bryan/lottamath/diffgrad.pdf
 *  > run-main scalation.calculus.DifferentialTest2
 */
object DifferentialTest2 extends App
{
    import Differential._
    import scala.math.{abs, cos, sin}

    def f (x: Double): Double = sin (x)      // the function

    def d (x: Double): Double = cos (x)      // its derivative

    var x = Array (.0, .1, .2, .3, .4, .5, .6, .7, .8, .9)
    for (i <- x.indices) {
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

} // DifferentialTest2 object

