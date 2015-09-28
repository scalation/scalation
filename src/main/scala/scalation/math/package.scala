
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed May 27 14:36:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see http://www.scala-lang.org/node/724
 *  @see http://www.scala-lang.org/old/node/12014.html
 */

package scalation

import java.lang.Math.{abs, cos, log, max, sin, tan, ulp}

import language.implicitConversions

import math.ExtremeD.{MIN_NORMAL, EPSILON}             // smallest full precision, machine epsilon

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `math` package contains classes, traits and objects for common mathematical
 *  operations.  Its package object defines exponentiation/logarithmic operators
 *  and functions.
 */
package object math
{
    /** The natural log of 2
     */
    val log_2  = log (2.0)

    /** The natural log of 10
     */
    val log_10 = log (10.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Int_Exp` adds an exponentiation operator 'x ~^ y' and a near_eq
     *  operator 'x =~ y' to `Int`.
     *  The '~^' has higher precedence than '*' or '/'.
     *  @param x  the base parameter
     */
    case class Int_Exp (x: Int)
    { 
        def ~^ (y: Int)     = scala.math.pow (x, y).toInt 
        def =~ (y: Double)  = near_eq (x, y)
        def !=~ (y: Double) = ! near_eq (x, y)
    } // Int_Exp class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Long_Exp` adds an exponentiation operator 'x ~^ y' and a near_eq
     *  operator 'x =~ y' to `Long`.
     *  The '~^' has higher precedence than '*' or '/'.
     *  @param x  the base parameter
     */
    case class Long_Exp (x: Long)
    {
        def ~^ (y: Long)    = pow (x, y) 
        def =~ (y: Double)  = near_eq (x, y)
        def !=~ (y: Double) = ! near_eq (x, y)
    } // Long_Exp class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `Double_Exp` adds an exponentiation operator 'x ~^ y' and a near_eq
     *  operator 'x =~ y' to `Double`.
     *  The '~^' has higher precedence than '*' or '/'.
     *  @param x  the base parameter
     */
    case class Double_Exp (x: Double)
    {
        def ~^ (y: Double)  = scala.math.pow (x, y)
        def =~ (y: Double)  = near_eq (x, y)
        def !=~ (y: Double) = ! near_eq (x, y)
    } // Double_Exp class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Int' to 'Int_Exp', which supports exponentiation.
     *  @param x  the base parameter
     */
    implicit def int_exp (x: Int)       = Int_Exp (x)
    implicit def long_exp (x: Long)     = Long_Exp (x)
    implicit def double_exp (x: Double) = Double_Exp (x)

    /** The threshold used for near equality
     */
    val THRES = 4.0 * MIN_NORMAL

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether two double precision floating point numbers are nearly equal.
     *  Two numbers are considered to be nearly equal, if within '2 EPSILON'.
     *  A number is considered to be nearly zero, if within '2 MIN_NORMAL'.
     *--------------------------------------------------------------------------
     *  @see `BasicTest`
     *  @see stackoverflow.com/questions/4915462/how-should-i-do-floating-point-comparison
     *--------------------------------------------------------------------------
     *  @param x  the first double precision floating point number
     *  @param y  the second double precision floating point number
     *
     */
    def near_eq (x: Double, y: Double): Boolean =
    {
        val del = abs (x - y)

        if (x == y) {
            true                                           // they are equal
        } else if (x == 0.0 || y == 0.0 || del <= THRES) {
            del <= THRES                                   // small absolute error
        } else {                   
            del / (abs (x) + abs (y)) <= 1.5 * EPSILON     // small relative error
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
    /** Negative exponential funtion (e to the minus 'x').
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

} // match package object 

