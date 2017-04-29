
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Oct  8 12:37:32 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see en.wikipedia.org/wiki/Romberg%27s_method
 *  @see www.math.usm.edu/lambers/mat460/fall09/lecture29.pdf
 */

package scalation.calculus

import scala.math.{exp, pow}

import scalation.math.FunctionS2S
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Integral` object provides implementations for five basic integration methods:
 *  <p>
 *      ∫f(x)dx on interval [a, b]
 *  <p>
 *      trap      - trapezoidal method - linear
 *      simpson   - Simpson method     - quadratic
 *      simpson38 - 3/8 Simpson method - cubic
 *      boole     - Boole Method       - quartic
 *      romberg   - Romberg method     - recursive, uses trap
 *  <p>
 *  The first four are Composite Newton-Coates type integrators.
 *  @see en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas
 */
object Integral
{
    private val DEBUG  = false                             // debug flag
    private val ITER   = 8                                 // default iterations for Romberg
    private val ITMAX  = 20                                // maximum iterations for Romberg
    private val SUBDIV = 96                                // default subdivisions for Newton-Cotes

    private val _4up = (for (j <- 0 until ITMAX) yield pow (4, j)).toArray

    private val _1_2  = 1.0 / 2.0
    private val _1_3  = 1.0 / 3.0
    private val _3_8  = 3.0 / 8.0
    private val _2_45 = 2.0 / 45.0

    private val c_simp = Array (2.0, 4.0)                  // coefficients for simpson
    private val c_sp38 = Array (2.0, 3.0, 3.0)             // coefficients for 3/8 simpson 
    private val c_bool = Array (14.0, 32.0, 12.0, 32.0)    // coefficients for boole

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate ∫f(x)dx on interval [a, b] using the trapezoidal method.
     *  @param a   the start of the integration interval
     *  @param b   the end of the integration interval
     *  @param f   the function to be integrated
     *  @param sd  the number of subdivision (intervals) of [a, b]
     */
    def trap (a: Double, b: Double, f: FunctionS2S, sd: Int = SUBDIV): Double =
    {
        var x = a
        val dx = (b - a) / sd
        var sum = f(a) + f(b)
        for (i <- 1 until sd) { x += dx; sum += 2.0 * f(x) }
        _1_2 * dx * sum
    } // trap

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate ∫f(x)dx on interval [a, b] using the Simpson method.
     *  @param a   the start of the integration interval
     *  @param b   the end of the integration interval
     *  @param f   the function to be integrated
     *  @param sd  the number of subdivision (intervals) of [a, b]
     */
    def simpson (a: Double, b: Double, f: FunctionS2S, sd: Int = SUBDIV): Double =
    {
        var x = a
        val dx = (b - a) / sd
        var sum = f(a) + f(b)
        for (i <- 1 until sd) { x += dx; sum += c_simp (i % 2) * f(x) }
        _1_3 * dx * sum
    } // simpson

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate ∫f(x)dx on interval [a, b] using the 3/8 Simpson method.
     *  @param a   the start of the integration interval
     *  @param b   the end of the integration interval
     *  @param f   the function to be integrated
     *  @param sd  the number of subdivision (intervals) of [a, b]
     */
    def simpson38 (a: Double, b: Double, f: FunctionS2S, sd: Int = SUBDIV): Double =
    {
        var x = a
        val dx = (b - a) / sd
        var sum = f(a) + f(b)
        for (i <- 1 until sd) { x += dx; sum += c_sp38 (i % 3) * f(x) }
        _3_8 * dx * sum
    } // simpson38

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate ∫f(x)dx on interval [a, b] using the Boole method.
     *  @param a   the start of the integration interval
     *  @param b   the end of the integration interval
     *  @param f   the function to be integrated
     *  @param sd  the number of subdivision (intervals) of [a, b]
     */
    def boole (a: Double, b: Double, f: FunctionS2S, sd: Int = SUBDIV): Double =
    {
        var x = a
        val dx = (b - a) / sd
        var sum = 7.0 * (f(a) + f(b))
        for (i <- 1 until sd) { x += dx; sum += c_bool (i % 4) * f(x) }
        _2_45 * dx * sum
    } // boole

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate ∫f(x)dx on interval [a, b] using the Romberg method.
     *  Translation of Java code from the site below to Scala.
     *  @see cs.roanoke.edu/Spring2012/CPSC402A/Integrate.java
     *  FIX: shouldn't need a 2D array/matrix.
     *  @param a     the start of the integration interval
     *  @param b     the end of the integration interval
     *  @param f     the function to be integrated
     *  @param iter  the number of iterative steps
     */
    def romberg (a: Double, b: Double, f: FunctionS2S, iter: Int = ITER): Double =
    {
        val t = Array.ofDim [Double] (iter, iter)         // 2D array

        var sd = 1                                        // number of subdivisions
        for (k <- 0 until iter) {
            t(k)(0) = trap (a, b, f, sd)                  // use trapezoidal method
            for (j <- 1 to k) t(k)(j) = (_4up(j) * t(k)(j-1) - t(k-1)(j-1)) / (_4up(j) - 1)
            if (DEBUG) println ("t(k) = " + t(k).deep)
            sd += sd                                      // double the subdivisions
        } // for   
        t(iter-1)(iter-1)                                 // return lowest, rightmost element
    } // romberg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Integrate ∫f(x)dx on interval [a, b] using the default method.
     *  @param a     the start of the integration interval
     *  @param b     the end of the integration interval
     *  @param f     the function to be integrated
     */
    def ∫ (on: (Double, Double), f: FunctionS2S): Double = romberg (on._1, on._2, f)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test each of the numerical integrators:  ∫f(x)dx on interval [a, b].
     *  @param a    the start of the integration interval
     *  @param b    the end of the integration interval
     *  @param f    the function to be integrated
     *  @param ans  the answer to the integration problem, if known (for % error)
     *  @param sd   the number of subdivision (intervals) of [a, b]
     */
    def test (a: Double, b: Double, f: FunctionS2S, ans: Double, sd: Int = SUBDIV)
    {
        val itrap = trap (a, b, f, sd)
        val isimp = simpson (a, b, f, sd)
        val isp38 = simpson38 (a, b, f, sd)
        val ibool = boole (a, b, f, sd)
        val iromb = romberg (a, b, f)

        println (s"trap integral = $itrap,\t % error = ${100.0 * (ans - itrap)/ans}") 
        println (s"simp integral = $isimp,\t % error = ${100.0 * (ans - isimp)/ans}") 
        println (s"sp38 integral = $isp38,\t % error = ${100.0 * (ans - isp38)/ans}") 
        println (s"bool integral = $ibool,\t % error = ${100.0 * (ans - ibool)/ans}") 
        println (s"romb integral = $iromb,\t % error = ${100.0 * (ans - iromb)/ans}") 
    } // test

} // Integral object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GaussianFunc` class implements the Gaussian function,  a generalization
 *  of the Gaussian/Normal distribution density function.
 *  @see en.wikipedia.org/wiki/Gaussian_function
 *  @param a  the height parameter of the Gaussian function
 *  @param b  the position parameter of the Gaussian function
 *  @param c  the width parameter of the Gaussian function
 */
class GaussianFunc (a: Double, b: Double, c: Double)
{
    private val den = 2.0 * c * c 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the Gaussian function at x.
     *  @param x  the domain value for the functional evaluation
     */
    def gaussianf (x: Double): Double = a * exp (-(x-b)*(x-b) / den)

} // GaussianFunc class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntegralTest2` object tests the numerical integrators on simple problems.
 *  Easy problems.
 *  > run-main scalation.calculus.IntegralTest
 */
object IntegralTest extends App
{
    banner ("integral of 'x'")
    def f1 (x: Double): Double = x
    Integral.test (0.0, 2.0, f1, 2.0)

    banner ("integral of 'x^2'")
    def f2 (x: Double): Double = x * x
    Integral.test (0.0, 3.0, f2, 9.0)

    banner ("integral of 'x^3'")
    def f3 (x: Double): Double = x * x * x
    Integral.test (0.0, 4.0, f3, 64.0)

    // @see www.umiacs.umd.edu/~ramani/cmsc460/Lecture15_integration.pdf
    banner ("integral of 'x exp (2x)'")
    val ans = 0.25 * (1.0 + 7.0 * exp (8.0))
    def f4 (x: Double): Double = x * exp (2.0 * x)
    Integral.test (0.0, 4.0, f4, ans)

} // IntegralTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntegralTest2` object tests the numerical integrators using the Gauss
 *  function that has no analytic solution.  Hard problem.
 *  > run-main scalation.calculus.IntegralTest2
 */
object IntegralTest2 extends App
{
    banner ("integral of 'a exp (-(x-b)^2/(2c^2)'")
    val gf  = new GaussianFunc (1.0, 0.0, 1.0)
    val ans = 0.85562439189214880
    Integral.test (0.0, 1.0, gf.gaussianf, ans)

} // IntegralTest2 object

