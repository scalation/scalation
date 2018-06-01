
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Mon Feb 22 12:11:17 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scala.language.implicitConversions
import scala.math.{acos, cos, max, min, sin}

import Complex._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Complex` class is used to represent and operate on complex numbers.
 *  Internally, a complex number is represented as two double precision
 *  floating point numbers (Double).  Externally, two forms are supported:
 *  <p>
 *      a+bi   = 2.1+3.2i       via: Complex ("2.1+3.2i"),  'toString'
 *      (a, b) = (2.1, 3.2)     via: create ("(2.1, 3.2)"),  'toString2'
 *  <p>
 *  Note: 'i * i = -1'.
 *-------------------------------------------------------------------------
 *  @param re  the real part (e.g., 2.1)
 *  @param im  the imaginary part (e.g., 3.2)
 */
case class Complex (val re: Double, val im: Double = 0.0)
     extends Fractional [Complex] with Ordered [Complex]
{
    /** Format `String` used for printing parts of complex numbers (change using 'setFormat')
     */
    private var fString = "%g"

    /** General alias for the parts of a complex number
     */
    val (val1, val2) = (re, im)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' number "is Infinite".
     */
    def isInfinity: Boolean = re.isInfinite () || im.isInfinite

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' number "is Not a Number".
     */
    def isNaN: Boolean = new scala.runtime.RichDouble (re).isNaN ||
                         new scala.runtime.RichDouble (im).isNaN

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unary minus (-).
     */
    def unary_- (): Complex = Complex (-re, -im)

    def negate (c: Complex) = -c

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two complex numbers.
     *  @param c  add complex c to this
     */
    def + (c: Complex): Complex = Complex (re + c.re, im + c.im)

    def plus (c: Complex, d: Complex): Complex = c + d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract two complex numbers.
     *  @param c  subtract c from this
     */
    def - (c: Complex): Complex = Complex (re - c.re, im - c.im)

    def minus (c: Complex, d: Complex): Complex = c - d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply two complex numbers.
     *  @param c  multiply 'this' times c
     */
    def * (c: Complex): Complex = Complex (re * c.re - im * c.im, re * c.im + im * c.re)

    def times (c: Complex, d: Complex): Complex = c * d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide two complex numbers.
     *  @param c  divide this by c
     */
    def / (c: Complex): Complex = Complex ((re * c.re + im * c.im) / (c.re * c.re + c.im * c.im),
                                           (im * c.re - re * c.im) / (c.re * c.re + c.im * c.im))
    def div (c: Complex, d: Complex): Complex = c / d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise a complex to the 'r'-th power (a double) using polar coordinates.
     *  @param r  the power/exponent
     */
    def ~^ (r: Double): Complex =
    {
        val (rad, ang) = polar
        Complex.create (double_exp (rad) ~^ r, ang * r)
    } // ~^

    def pow (c: Complex, r: Double): Complex = c ~^ r

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether two complex numbers are nearly equal.
     *  @param c  compare this with c
     */
    def =~ (c: Complex): Boolean = (re =~ c.re && im =~ c.im)
    def !=~ (c: Complex): Boolean = (re !=~ c.re && im !=~ c.im)

    def near_eq (c: Complex, d: Complex): Boolean = c =~ d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the radius of the complex number as a vector in the 're'-'im' plane.
     */
    def radius: Double = math.sqrt (double_exp (re) ~^ 2.0 + double_exp (im) ~^ 2.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the angle of the complex number as a vector in the 're'-'im' plane.
     */
    def angle: Double = acos (re / radius)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the complex number in polar coordinates (radius, angle).
     */
    def polar: Tuple2 [Double, Double] = { val rad = radius; (rad, acos (re / rad)) }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the complex conjugate: if z = (a + bi) then z.bar = (a - bi).
     */
    def bar: Complex = Complex (re, -im)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of 'this' complex number.
     */
    def abs: Complex = Complex (math.abs (re), math.abs (im))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of 'this' and 'c' complex numbers.
     *  @param c  that complex number to compare with this
     */
    def max (c: Complex): Complex = if (c > this) c else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of 'this' and 'c' complex numbers.
     *  @param c  that complex number to compare with this
     */
    def min (c: Complex): Complex = if (c < this) c else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' complex number is purely imaginary (no real part).
     */
    def isIm: Boolean = re =~ 0.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' complex number is real (no imaginary part).
     */
    def isRe: Boolean = im =~ 0.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two complex numbers (negative for <, zero for ==, positive for >).
     *  @param c  the first complex number to compare
     *  @param d  the second complex number to compare
     */
    def compare (c: Complex, d: Complex): Int =
    {
        if (c.re == d.re) c.im compare d.im else c.re compare d.re
    } // compare

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' complex number with that complex number 'd'.
     *  @param d  that complex number
     */	
    def compare (d: Complex): Int =
    {	
        if (re == d.re) im compare d.im else re compare d.re
    } // compare

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' complex number with that complex number 'd' for inequality.
     *  @param d  that complex number
     */
    def ≠ (d: Complex) = this != d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' complex number with that complex number 'd' for less than
     *  or equal to.
     *  @param d  that complex number
     */
    def ≤ (d: Complex) = this <= d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' complex number with that complex number 'd' for greater 
     *  than or equal to.
     *  @param d  that complex number
     */
    def ≥ (d: Complex) = this >= d

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to a `Complex`.
     *  @param c  that complex number to convert
     */
    def toComplex (c: Complex): Complex = c

    def toComplex: Complex = this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to a `Double`.
     *  @param c  that complex number to convert
     */
    def toDouble (c: Complex): Double = c.re

    def toDouble: Double = re

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to a `Float`.
     *  @param c  that complex number to convert
     */
    def toFloat (c: Complex): Float = c.re.toFloat

    def toFloat: Float = re.toFloat

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to an `Int`.
     *  @param c  that complex number to convert
     */
    def toInt (c: Complex): Int = c.re.toInt

    def toInt: Int = re.toInt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to a `Long`.
     *  @param c  that complex number to convert
     */
    def toLong (c: Complex): Long = c.re.toLong

    def toLong: Long = re.toLong

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from a `Double`.
     *  @see Complex.double2Complex
     *  @param x  the double used to create the complex number
     */
//  def fromDouble (x: Double): Complex = Complex (x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from a `Float`.  `Floa`t currently not fully supported.
     *  @param x  the float used to create the complex number
     */
//  def fromFloat (x: Float): Complex = Complex (x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from an `Int`.
     *  @param n  the integer used to create the complex number
     */
    def fromInt (n: Int): Complex = Complex (n)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from a `Long`.
     *  @param n  the long used to create the complex number
     */
    def fromLong (n: Long): Complex = Complex (n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether 'this' complex number equals complex 'c'.
     *  @param c  the complex number to compare with this
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

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat'.
     *  @param newFormat  the new format String
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to a String of the form "a+bi".
     */
    override def toString: String = fString.format (re) + "+" + fString.format (im) + "i"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' complex number to a String of the form "(a, b)".
     */
    def toString2: String = "(" + fString.format (re) + ", " + fString.format (im) + ")"

} // Complex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Complex` companion object defines the origin (zero) and the fourth roots
 *  of unity as well as some utility functions.
 */
object Complex
{
    /** Zero (0) as a Complex number
     */
    val _0  = Complex (0.0)

    /** One (1) as a Complex number
     */
    val _1  = Complex (1.0)

    /** Imaginary one (i) as a Complex number
     */
    val _i  = Complex (0.0, 1.0)

    /** Negative one (-1) as a Complex number
     */
    val _1n = Complex (-1.0)

    /** Negative imaginary one (-i) as a Complex number
     */
    val _in = Complex (0.0, -1.0)

    private val rr2 = 1.0 / math.sqrt (2.0)   // reciprocal root of 2.

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Double' to 'Complex'.
     *  @param d  the Double parameter to convert
     */
    implicit def double2Complex (d: Double): Complex = new Complex (d, 0.0)    

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from a pair (Tuple2) of 'Double's.
     *  @param ct  the tuple form of a complex number
     */
    def apply (ct: Tuple2 [Double, Double]): Complex = Complex (ct._1, ct._2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from its primary string representation "a+bi".
     *  Examples: "2.1+3.2i", "2.1", "3.2i".
     *  @param cs  the string form of a complex number
     */
    def apply (cs: String): Complex =
    {
        val pair = cs.split ('+')
        val p0 = pair(0)
        Complex (if (pair.length == 1) 
                     if (p0.charAt(p0.length-1) == 'i') (0.0, p0.dropRight(1).toDouble)
                     else                             (p0.toDouble, 0.0)
                 else (p0.toDouble, pair(1).dropRight(1).toDouble))
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from its secondary string representation "(a, b)".
     *  Examples: "(2.1, 3.2)", "(2.1, 0)", "(0, 3.2)".
     *  @param cs  the string form of a complex number
     */
    def create (cs: String): Complex =
    {
        val pair = cs.split (',')
        Complex (pair(0).drop(1).toDouble, pair(1).dropRight(1).toDouble)
    } // create

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a complex number from the given polar coordinates.
     *  @param rad  the radius (the length of the vector in the 're-im' plane)
     *  @param ang  the angle (the angle of the vector above the 're'-axis)
     */
    def create (rad: Double, ang: Double): Complex = Complex (rad * cos (ang), rad * sin (ang))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute an arithmetic results (+, -, *, /) where the first argument is
     *  real and the second argument is complex.
     *  @param c  the first argument (a real number)
     *  @param c  the second argument (a complex number)
     */
//  def + (r: Double, c: Complex) = Complex (r + c.re, c.im)
//  def - (r: Double, c: Complex) = Complex (r - c.re, c.im)
//  def * (r: Double, c: Complex) = Complex (r * c.re, r * c.im)
//  def / (r: Double, c: Complex) = Complex ((r * c.re) / (c.re * c.re + c.im * c.im),
//                                          (-r * c.im) / (c.re * c.re + c.im * c.im))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of that complex number.
     *  @param c  that complex number
     */
    def abs (c: Complex): Complex = c.abs

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of two complex number, 'c' and 'd'.
     *  @param c  the first complex number to compare
     *  @param d  the second complex number to compare
     */
    def max (c: Complex, d: Complex): Complex = if (d > c) d else c

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of two complex numbers, 'c' and 'd'.
     *  @param c  the first complex number to compare
     *  @param d  the second complex number to compare
     */
    def min (c: Complex, d: Complex): Complex = if (d < c) d else c

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the complex signum (csgn) of a complex number.  The values may
     *  be -1, 0, or 1.
     *  @see en.wikipedia.org/wiki/Sign_function
     *  @param c  the complex number to obtain the signum of
     */
    def signum (c: Complex): Complex =
    {
        if (c.re > 0.0)      _1
        else if (c.re < 0.0) _1n
        else                 math.signum (c.im).toComplex
    } // signum

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of that complex number.
     *  @see www.mathpropress.com/stan/bibliography/complexSquareRoot.pdf
     *  @param c  that complex number
     */
    def sqrt (c: Complex): Complex =
    { 
        val (a, b) = (c.re, c.im)
        val rad    = c.radius
        Complex (rr2 * math.sqrt (rad + a),
                 rr2 * math.sqrt (rad - a) * math.signum (b))
    } // sqrt

    /** Ordering for complex numbers
     */
    implicit val ord = new Ordering [Complex]
               { def compare (c: Complex, d: Complex) = c compare d }

    /** Implicit numeric value for establishing type
     */
    implicit val num = _0

} // Complex object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ComplexTest` object is used to test the `Complex` class.
 *  > runMain scalation.math.ComplexTest
 */
object ComplexTest extends App
{
    import scala.util.Sorting.quickSort

    val c = Complex (2.0, 3.0)
    val d = Complex (4.0, 5.0)
    val e = Complex (5.0)
    val x = 7.0                        // Double
    val r = sqrt (c)

    println ("c  = " + c)
    println ("d  = " + d)
    println ("e  = " + e)

    println ("-c    = " + -c)
    println ("c + d = " + (c + d))
    println ("c - d = " + (c - d))
    println ("c * d = " + (c * d))
    println ("c / d = " + (c / d))
    println ("c + x = " + (c + x))
    println ("c - x = " + (c - x))
    println ("c * x = " + (c * x))
    println ("c / x = " + (c / x))

    println ("c ~^ 2   = " + (c ~^ 2))
    println ("c ~^ 2.5 = " + (c ~^ 2.5))
    println ("c * c    = " + (c * c))
    println ("c.bar    = " + c.bar)
    println ("c.abs    = " + c.abs)
    println ("c max d  = " + (c max d))
    println ("c min d  = " + (c min d))
    println ("d.isIm   = " + d.isIm)
    println ("d.isRe   = " + d.isRe)
    println ("e.isRe   = " + e.isRe)
    println ("sqrt(c)  = " + r)
    println ("r * r    = " + (r * r))
    println ("c < d    = " + (c < d))
    println ("d < c    = " + (d < c))

    def sort (arr: Array [Complex]) { quickSort (arr)(Complex.ord) }

    val arr = Array (e, d, c)
    println ("arr = " + arr.deep)
    sort (arr)
    println ("arr = " + arr.deep)

    println (Complex ("2.1+3.2i"))
    println (Complex ("2.1"))
    println (Complex ("3.2i"))

} // ComplexTest object

