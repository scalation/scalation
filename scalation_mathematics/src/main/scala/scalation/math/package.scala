
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed May 27 14:36:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see http://www.scala-lang.org/node/724
 *  @see http://www.scala-lang.org/old/node/12014.html
 */

package scalation

import java.lang.Double.isNaN
import java.lang.Math.{abs, cos, log, max, round, sin, tan, ulp}

import scala.language.implicitConversions

import math.ExtremeD.{MIN_NORMAL, EPSILON, TOL}    // smallest full precision, machine epsilon, tolerance

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `math` package contains classes, traits and objects for common mathematical
 *  operations.  Its package object defines exponentiation, logarithmic, trigonometric,
 *  etc. operators and functions.
 */
package object math
{
    /** The type definition for a function of a scalar (f: Double => Double)
     *  @see also `scalation.linalgebra`
     */
    type FunctionS2S  = Double  => Double      // function of a scalar - Double 

    /** The type definition for an array of scalar functions
     */
    type Functions = Array [FunctionS2S]

    /** The natural log of 2
     */
    val log_2  = log (2.0)

    /** The natural log of 10
     */
    val log_10 = log (10.0)

    /** Missing value representation (use null if available, otherwise most negative value)
     */
    val noComplex  = null.asInstanceOf [Complex]
    val noDouble   = Double.NegativeInfinity
    val noInt      = Int.MinValue
    val noLong     = Long.MinValue
    val noRational = null.asInstanceOf [Rational]
    val noReal     = null.asInstanceOf [Real]
    val noStrNum   = null.asInstanceOf [StrO.StrNum]
    val noTimeNum  = null.asInstanceOf [TimeO.TimeNum]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Int' to 'Int_Exp', which supports exponentiation
     *  and nearly equals.
     *  @param x  the base parameter
     */
    implicit def int_exp (x: Int) = new Int_Exp (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Long' to 'Long_Exp', which supports exponentiation
     *  and nearly equals.
     *  @param x  the base parameter
     */
    implicit def long_exp (x: Long) = new Long_Exp (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Double' to 'Double_Exp', which supports exponentiation
     *  and nearly equals.
     *  @param x  the base parameter
     */
    implicit def double_exp (x: Double) = new Double_Exp (x)

    /** The threshold used for near equality
     */
    val THRES = 4.0 * MIN_NORMAL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether two double precision floating point numbers 'x' and 'y'
     *  are nearly equal.  Two numbers are considered to be nearly equal, if within
     *  '2 EPSILON'.  A number is considered to be nearly zero, if within '2 MIN_NORMAL'.
     *  To accommodate round-off errors, may use 'TOL' instead of 'EPSILON'.
     *--------------------------------------------------------------------------
     *  @see `BasicTest`
     *  @see stackoverflow.com/questions/4915462/how-should-i-do-floating-point-comparison
     *--------------------------------------------------------------------------
     *  If both 'x' and 'y' are NaN (Not-a-Number), the IEEE standard indicates that should
     *  be considered always not equal.  For 'near_eq', they are considered nearly equal.
     *  Comment out the first line below to conform more closely to the IEEE standard.
     *  @see http://stackoverflow.com/questions/10034149/why-is-nan-not-equal-to-nan
     *--------------------------------------------------------------------------
     *  @param x  the first double precision floating point number
     *  @param y  the second double precision floating point number
     */
    def near_eq (x: Double, y: Double): Boolean =
    {
        if (isNaN (x) && isNaN (y)) return true             // comment out to follow IEEE standard
        val del = abs (x - y)

        if (x == y) {
            true                                            // they are equal
        } else if (x == 0.0 || y == 0.0 || del <= THRES) {
            del <= THRES                                    // small absolute error
        } else {                   
            del / (abs (x) + abs (y)) <= 1.5 * TOL          // small relative error (TOL or EPSILON)
        } // if
    } // near_eq

    /*  Check whether they are at most 3 Units in Least Precision (ULPs) apart.
     *  @see stackoverflow.com/questions/1088216/whats-wrong-with-using-to-compare-floats-in-java
     *
    {
        if (x == y) return true
        val maxUlp  = max (ulp (x), ulp (y))
        abs (x - y) < 3 * maxUlp
    } // near_eq 
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Round the given double to the desired number of decimal places.
     *  @param x       the double number which needs to be rounded off
     *  @param places  the desired number of decimal places
     */
    def roundTo (x: Double, places: Int = 4): Double =
    {
        val s = scala.math.pow (10, places)
        round (x * s) / s
    } // roundTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Power function for scala Longs 'x ~^ y'.  Compute: 'math.pow (x, y).toLong'
     *  without using floating point, so as to not lose precision.
     *  @param x  the Long base parameter
     *  @param y  the Long exponent parameter
     */
    def pow (x: Long, y: Long): Long =
    {
        var base   = x
        var exp    = y
        var result = 1l
        while (exp != 0l) {
            if ((exp & 1l) == 1l) result *= base
            exp >>= 1l
            base *= base
        } // while
        result
    } // pow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the square of 'x'.
     *  @param x  the number to sqaure
     */
    def sq (x: Double): Double = x * x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the 'y'-th root of 'x', i.e.,  'x ~^ 1/y' for scala Longs.
     *  'r = x ~^ 1/y' is largest long integer 'r' such that 'r ~^ y <= x'.
     *  @see http://en.wikipedia.org/wiki/Shifting_nth_root_algorithm
     *  @see http://stackoverflow.com/questions/8826822/calculate-nth-root-with-integer-arithmetic
     *  @param x  the Long base parameter
     *  @param y  the Long root level (reciprocal exponent) parameter
     */
    def root (x: Long, y: Long): Long =
    {
        var r = 1l                               // initial guess for root

        def step: Long = ((y-1) * r + x / r~^(y-1)) / y

        var q = step                             // find better root
        do { r = q; q = step } while (q < r)     // repeat looking for better root
        r
    } // root

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Negative exponential function (e to the minus 'x').
     *  @param x  the argument of the function
     */
    def nexp (x: Double) = scala.math.exp (-x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of 'x' base 2.
     *  @param x  the value whose log is sought
     */
    def log2 (x: Double): Double = log (x) / log_2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of 'x' base 10.
     *  @see scala.math.log10
     *  @param x  the value whose log is sought
     */
//  def log10 (x: Double): Double = log (x) / log_10

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of 'x' base 'b'.
     *  @param b  the base of the logarithm
     *  @param x  the value whose log is sought
     */
    def logb (b: Double, x: Double): Double = log (x) / log (b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of 'x' with the sign of 'y'.
     *  @param x  the value contributor
     *  @param y  the sign contributor
     */
    def sign (x: Double, y: Double): Double = if (y < 0.0) -abs (x) else abs (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 1 if condition 'cond' is true, else 0.
     *  @param cond  the condition to evaluate
     */
    def oneIf (cond: Boolean): Int = if (cond) 1 else 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cotangent of 'x'
     *  @param 'x'  the angle in radians
     */
    def cot (x: Double): Double = 1.0 / tan (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the cosecant of 'x'
     *  @param 'x'  the angle in radians
     */
    def csc (x: Double): Double = 1.0 / cos (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the secant of 'x'
     *  @param 'x'  the angle in radians
     */
    def sec (x: Double): Double = 1.0 / sin (x)

} // math package object 

