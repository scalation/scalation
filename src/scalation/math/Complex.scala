
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Feb 22 12:11:17 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scala.math.{max, min}

import DoubleWithExp._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to represent complex numbers (a + bi) as (a, b), e.g.,
 *  (2.1, 3.2i).  Note: i * i = -1.
 *  @param re  the real part
 *  @param im  the imaginary part
 */
case class Complex (val re: Double, val im: Double = 0.0)
     extends Fractional [Complex] with Ordered [Complex]
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unary minus (-).
     */
    def unary_- () = Complex (-re, -im)
    def negate (c: Complex) = -c

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two complex numbers or complex plus double.
     *  @param c  add c to this
     */
    def + (c: Complex) = Complex (re + c.re, im + c.im)
    def plus (c: Complex, d: Complex) = c + d
    def + (r: Double) = Complex (re + r, im)
    def plus (c: Complex, r: Double) = c + r

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract two complex numbers or complex minus double.
     *  @param c  subtract c from this
     */
    def - (c: Complex) = Complex (re - c.re, im - c.im)
    def minus (c: Complex, d: Complex) = c - d
    def - (r: Double) = Complex (re - r, im)
    def minus (c: Complex, r: Double) = c - r

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply two complex numbers or complex times double.
     *  @param c  multiply this times c
     */
    def * (c: Complex) = Complex (re * c.re - im * c.im, re * c.im + im * c.re)
    def times (c: Complex, d: Complex) = c * d
    def * (r: Double) = Complex (re * r, im * r)
    def times (c: Complex, d: Double) = c * d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide two complex numbers or complex div double.
     *  @param c  divide this by c
     */
    def / (c: Complex) = Complex ((re * c.re + im * c.im) / (c.re * c.re + c.im * c.im),
                                  (im * c.re - re * c.im) / (c.re * c.re + c.im * c.im))
    def div (c: Complex, d: Complex) = c / d
    def / (r: Double) = Complex ((re * r) / (r * r), (im * r) / (r * r))
    def div (c: Complex, r: Double) = c / r

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise a complex to the d-th power.
     *  @param d  the power/exponent
     */
    def ~^ (r: Double) = Complex (re ~^ r, im ~^ r)
    def pow (c: Complex, r: Double) = c ~^ r

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the complex conjugate: if z = (a + bi) then z.bar = (a - bi).
     */
    def bar = Complex (re, -im)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of this complex number.
     */
    def abs = Complex (re.abs, im.abs)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of this and that complex numbers.
     *  @param c  that complex number to compare with this
     */
    def max (c: Complex) = if (c > this) c else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of this and that complex numbers.
     *  @param c  that complex number to compare with this
     */
    def min (c: Complex) = if (c < this) c else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this complex number is real (no imaginary part).
     */
    def isRe = im == 0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two complex numbers (negative for <, zero for ==, positive for >).
     *  @param c  the first complex number to compare
     *  @param d  the second complex number to compare
     */
    def compare (c: Complex, d: Complex) =
    {
        if (c.re == d.re) c.im compare d.im else c.re compare d.re
    } // compare

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this complex number with that complex number d.
     *  @param d  that complex number
     */	
    def compare (d: Complex) =
    {	
        if (re == d.re) im compare d.im else re compare d.re
    } // compare

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this complex number to a Double.
     *  @param d  that complex number to convert
     */
    def toDouble (c: Complex) = c.re
    def toDouble = re

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this complex number to a Float.
     *  @param d  that complex number to convert
     */
    def toFloat (c: Complex) = c.re.toFloat
    def toFloat = re.toFloat

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this complex number to an Int.
     *  @param d  that complex number to convert
     */
    def toInt (c: Complex) = c.re.toInt
    def toInt = re.toInt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this complex number to a Long.
     *  @param d  that complex number to convert
     */
    def toLong (c: Complex) = c.re.toLong
    def toLong = re.toLong

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from an Int.
     *  @param n  the integer used to create the complex number.
     */
    def fromDouble (x: Double) = Complex (x, 0.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from an Int.
     *  @param n  the integer used to create the complex number.
     */
    def fromFloat (x: Float) = Complex (x, 0.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from an Int.
     *  @param n  the integer used to create the complex number.
     */
    def fromInt (n: Int) = Complex (n, 0.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from an Int.
     *  @param n  the integer used to create the complex number.
     */
    def fromLong (n: Long) = Complex (n, 0.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether vector this equals vector b.
     *  @param b  the vector to compare with this
     */
    override def equals (c: Any): Boolean =
    {
        c.isInstanceOf [Complex] && (re equals c.asInstanceOf [Complex].re) &&
                                    (im equals c.asInstanceOf [Complex].im)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = re.hashCode + 41 * im.hashCode

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this complex number to a String.
     */
    override def toString = "Complex ( " + re + " , " + im + "i )"

} // Complex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This companion object defines the origin (zero) and the fourth roots of unity
 *  as well as some utility functions.
 */
object Complex
{
    val _0  = Complex (0.0, 0.0)     // zero (0)
    val _1  = Complex (1.0, 0.0)     // one (1)
    val _i  = Complex (0.0, 1.0)     // imaginary one (i)
    val _1n = Complex (-1.0, 0.0)    // negative one (-1)
    val _in = Complex (0.0, -1.0)    // negative imaginary one (-i)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute an arthmetic results (+, -, *, /) where the first argument is
     *  real and the second argument is complex.
     *  @param c  the first argument (a real number)
     *  @param c  the second argument (a complex number)
    def + (r: Double, c: Complex) = Complex (r + c.re, c.im)
    def - (r: Double, c: Complex) = Complex (r - c.re, c.im)
    def * (r: Double, c: Complex) = Complex (r * c.re, r * c.im)
    def / (r: Double, c: Complex) = Complex ((r * c.re) / (c.re * c.re + c.im * c.im),
                                            (-r * c.im) / (c.re * c.re + c.im * c.im))
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of that complex number.
     *  @param c  that complex number
     */
    def abs (c: Complex) = c.abs

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of that complex number.
     *  @param c  that complex number
     */
    def sqrt (c: Complex) = Complex (math.sqrt (c.re), math.sqrt (c.re))

} // Complex companion object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Complex class.
 */
object ComplexTest extends App
{
    val c = Complex (2.0, 3.0)
    val d = Complex (4.0, 5.0)
    val e = Complex (5.0)

    println ("c      = " + c)
    println ("d      = " + d)
    println ("e      = " + e)
    println ("-c     = " + -c)
    println ("c + d  = " + (c + d))
    println ("c - d  = " + (c - d))
    println ("c * d  = " + (c * d))
    println ("c / d  = " + (c / d))
    println ("c.bar  = " + c.bar)
    println ("c.abs  = " + c.abs)
    println ("d.isRe = " + d.isRe)
    println ("e.isRe = " + e.isRe)
    println ("c < d  = " + (c < d))

} // ComplexTest

