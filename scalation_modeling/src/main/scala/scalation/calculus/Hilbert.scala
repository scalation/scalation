
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Oct  8 12:37:32 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.jstatsoft.org/article/view/v051i04/v51i04.pdf
 */

package scalation.calculus

import scala.math.sqrt

import scalation.math.{double_exp, FunctionS2S, int_exp}
import scalation.util.banner

import Integral.∫

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Hilbert` class provides operators to add, subtract, mutiply, divide and
 *  raise functions.  Given two functions, 'f' and 'g', a new function is created.
 *  It also provides methods for computing dot/inner products, norms and
 *  distances for functions defined in Hilbert Space.
 *  On interval [a, b]
 *  <p>
 *      Lp-norm (f) = [ ∫f(t)^p dt ]^1/p
 *  <p>
 *  @see implicit conversion 'functionS2S2Hilbert' in `package.scala`
 *  @param f  the function to convert into a Hilbert function
 */
class Hilbert (f: FunctionS2S)
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Negate the function 'f' (unary minus), returning a new function.
     */
    def unary_- = (x: Double) => -f(x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add function 'f' and function 'g', returning a new function.
     *  @param g  the other function
     */
    def + (g: FunctionS2S) = (x: Double) => f(x) + g(x)         // function of x
    def + (g: Double)      = (x: Double) => f(x) + g            // constant function 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From function 'f' subtract function 'g', returning a new function.
     *  @param g  the other function
     */
    def - (g: FunctionS2S) = (x: Double) => f(x) - g(x)
    def - (g: Double)      = (x: Double) => f(x) - g

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply function 'f' by function 'g', returning a new function.
     *  @param g  the other function
     */
    def * (g: FunctionS2S) = (x: Double) => f(x) * g(x)
    def * (g: Double)      = (x: Double) => f(x) * g

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide function 'f' by function 'g', returning a new function.
     *  @param g  the other function
     */
    def / (g: FunctionS2S) = (x: Double) => f(x) / g(x)
    def / (g: Double)      = (x: Double) => f(x) / g

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise function 'f' to the 'p'th power, returning a new function.
     *  @param p  the integer-valued power/exponent
     */
    def ~^ (p: Int) = (x: Double) => f(x) ~^ p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise function 'f' to the 'p'th power, returning a new function.
     *  @param p  the power/exponent
     */
    def ~^ (p: Double) = (x: Double) => f(x) ~^ p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot/inner product of functions 'f' and 'g'.
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dot (g: FunctionS2S, a: Double = 0.0, b: Double = 1.0): Double =
    {
        ∫ ((a, b), f * g)
    } // dot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the L2 norm squared of function 'f', returning a new function.
     *  @param a   the start of the interval
     *  @param b   the end of the interval
     */
    def normSq (a: Double = 0.0, b: Double = 1.0): Double =
    {
        ∫ ((a, b), f ~^ 2)
    } // normSq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the L2 norm of function 'f'.
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def norm (a: Double = 0.0, b: Double = 1.0): Double =
    {
        sqrt (normSq (a, b))
    } // norm

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Lp norm squared of function 'f'.
     *  @param p  the level, e.g., 1, 2, ...
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def normSq_p (p: Int, a: Double = 0.0, b: Double = 1.0): Double =
    {
        ∫ ((a, b), f ~^ p)
    } // normSq_p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Lp norm of function 'f'.
     *  @param p  the level, e.g., 1, 2, ...
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def norm_p (p: Int, a: Double = 0.0, b: Double = 1.0): Double =
    {
        normSq_p (p, a, b) ~^ (1.0 / p.toDouble)
    } // normP

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the distance in L2 space between function 'f' and function 'g'.
     *  @param g  the other function
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dist (g: FunctionS2S, a: Double = 0.0, b: Double = 1.0): Double =
    {
        (f - g).norm (a, b)
    } // dist

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the distance in Lp space between function 'f' and function 'g'.
     *  @param g  the other function
     *  @param p  the level, e.g., 1, 2, ...
     *  @param a  the start of the interval
     *  @param b  the end of the interval
     */
    def dist_p (g: FunctionS2S, p: Int, a: Double = 0.0, b: Double = 1.0): Double =
    {
        (f - g).norm_p (p, a, b)
    } // dist_p

} // Hilbert class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HilbertTest` object is used to test the `Hilbert` class.
 *  > run-main scalation.calculus.HilbertTest
 */
object HilbertTest extends App
{
    banner ("functions: f(x) = 2t, g(x) = t^2")
    val f = (t: Double) => 2.0 * t            // function definition
    def g (t: Double)   = t * t               // method defintion - `g _` converts unapplied method to function
                                              
    // Unapplied methods are only converted to functions when a function type is expected.
    // You can make this conversion explicit by writing `g _` or `g(_)` instead of `g`.

    banner ("functional operators")
    val h = f * 2 + g _                       // define a new function h
    println ("f(1)        = " + f(1))
    println ("g(1)        = " + g(1))
    println ("(-f)(1)     = " + (-f)(1))
    println ("(f + g)(1)  = " + (f + g _)(1))
    println ("(f - g)(1)  = " + (f - g _)(1))
    println ("(f * g)(1)  = " + (f * g _)(1))
    println ("(f / g)(1)  = " + (f / g _)(1))
    println ("(f ~^ 2)(1) = " + (f ~^ 2)(1))
    println ("h(1)        = " + h(1))

    banner ("dot product")
    println ("f dot f = " + (f dot f))
    println ("f dot g = " + (f dot g))

    banner ("norm (f)")
    println ("L2 norm = " + f.norm ())
    println ("L1 norm = " + f.norm_p (1))
    println ("L2 norm = " + f.norm_p (2))
    println ("L3 norm = " + f.norm_p (3))

    banner ("dist (f, g)")
    println ("L2 distance = " + f.dist (g))
    println ("L1 distance = " + f.dist_p (g, 1))
    println ("L2 distance = " + f.dist_p (g, 2))
    println ("L3 distance = " + f.dist_p (g, 3))

} // HilbertTest object

