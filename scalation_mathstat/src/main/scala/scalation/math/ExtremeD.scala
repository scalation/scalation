
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat May 24 14:48:00 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import java.lang.Double

import scala.math.abs

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExtremeD` object contains constants representing extreme values for
 *  Double (IEEE 754 double precision floating point numbers).
 *  @see http://docs.oracle.com/javase/8/docs/api/java/lang/Double.html
 *  @see en.wikipedia.org/wiki/Double-precision_floating-point_format
 *------------------------------------------------------------------------------
 *  64 bits: 1 sign-bit, 11 exponent-bits, 52 mantissa-bits + 1 implicit
 */
object ExtremeD
{
    /** Smallest double such that 1.0 + EPSILON != 1.0, slightly above 2^-53.
     *  Also, known as the "machine epsilon".
     *  @see https://issues.scala-lang.org/browse/SI-3791
     */
    val EPSILON = 1.1102230246251568E-16               // 1 + EPSILON okay

    /** 2^-53   = 1.1102230246251565E-16 is too small for the "machine epsilon".
     */
    val _2_53 = 2.0 ~^ -53                             // 1 + 2^-53 underflows

    /** Largest exponent a finite double variable may have. 
     */
    val MAX_EXPONENT = Double.MAX_EXPONENT

    /** Largest positive finite value of type double, 2^1023
     */
    val MAX_VALUE = Double.MAX_VALUE

    /** Smallest exponent a normalized double variable may have.
     */
    val MIN_EXPONENT = Double.MIN_EXPONENT

    /** Smallest positive normal value of type double, 2^-1022 (retains full precision).
     *  Also, the smallest double such that 1.0 / 'SAFE_MIN' does not overflow.
     */
    val MIN_NORMAL = Double.MIN_NORMAL

    /** Smallest positive nonzero value of type double, 2^-1074 (minimal precision).
     */
    val MIN_VALUE = Double.MIN_VALUE

    /** Most negative finite value of type double.
     */
    val MOST_NEGATIVE = -Double.MAX_VALUE

    /** Default tolerance should be much larger than the "machine epsilon".
     *  Application dependent => redefine as needed per application.
     */
    val TOL = 1000.0 * EPSILON

    /** Special value representing negative infinity: 1111111111110...0
     *  Ex: -1.0 / 0.0
     *  @see http://stackoverflow.com/questions/13317566/what-are-the-infinity-constants-in-java-really
     */
    val NEGATIVE_INFINITY = Double.NEGATIVE_INFINITY

    /** Special value representing positive infinity: 0111111111110...0
     *  Ex: 1.0 / 0.0
     *  @see http://stackoverflow.com/questions/13317566/what-are-the-infinity-constants-in-java-really
     */
    val POSITIVE_INFINITY = Double.POSITIVE_INFINITY

    /** Special value representing an undefined value, i.e., "Not a Number": s11111111111m (m != 0)
     *  Ex: 0.0 / 0.0 = indeterminate, sqrt (-1.0) = Complex (0.0 + 1.0i) (not representable as a Double)
     *  @see https://en.wikipedia.org/wiki/NaN
     */
    val NaN = Double.NaN

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return true if x == y approximately, i.e., the difference is strictly
     *  less than 'TOL'.
     *  @param x  the first value to compare
     *  @param y  the second value to compare
     */
    def approx (x: Double, y: Double): Boolean =  abs (x - y) < TOL

} // ExtremeD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExtremeDTest` object is used to test the `ExtremeD` class.
 *  > run-main scalation.math.ExtremeDTest
 */
object ExtremeDTest extends App
{
    import java.lang.Double.{doubleToRawLongBits, toHexString}
    import java.lang.Long
    import scala.math.sqrt
    import ExtremeD._

    println ("----------------------------------------------------------------")
    println ("MAX_EXPONENT      = " + MAX_EXPONENT)
    println ("MAX_EXPONENT      = " + toHexString (MAX_EXPONENT))
    println ("MAX_VALUE         = " + MAX_VALUE)
    println ("MAX_VALUE         = " + toHexString (MAX_VALUE))
    println ("MIN_EXPONENT      = " + MIN_EXPONENT)
    println ("MIN_EXPONENT      = " + toHexString (MIN_EXPONENT))
    println ("MIN_NORMAL        = " + MIN_NORMAL)
    println ("MIN_NORMAL        = " + toHexString (MIN_NORMAL))
    println ("MIN_VALUE         = " + MIN_VALUE)
    println ("MIN_VALUE         = " + toHexString (MIN_VALUE))
    println ("MOST_NEGATIVE     = " + MOST_NEGATIVE)
    println ("MOST_NEGATIVE     = " + toHexString (MOST_NEGATIVE))
    println ("NEGATIVE_INFINITY = " + NEGATIVE_INFINITY)
    println ("NEGATIVE_INFINITY = " + Long.toHexString (doubleToRawLongBits (NEGATIVE_INFINITY)))
    println ("POSITIVE_INFINITY = " + POSITIVE_INFINITY)
    println ("POSITIVE_INFINITY = " + Long.toHexString (doubleToRawLongBits (POSITIVE_INFINITY)))
    println ("NaN               = " + NaN)
    println ("NaN               = " + Long.toHexString (doubleToRawLongBits (NaN)))
    println ("TOL               = " + TOL)
    println ("TOL               = " + toHexString (TOL))
    println ("EPSILON           = " + EPSILON)
    println ("EPSILON           = " + toHexString (EPSILON))

    println ("----------------------------------------------------------------")
    println ("1 + EPSILON       = " + (1.0 + EPSILON))          // still okay
    println ("_2_53             = " + _2_53)                    // 2^-53
    println ("1 + _2_53         = " + (1.0 + _2_53))            // underflow
    println ("MAX_VALUE + 1     = " + (MAX_VALUE + 1.0))        // does not overflow?
    println ("MAX_VALUE * 2     = " + (MAX_VALUE * 2.0))        // overflow
    println ("MOST_NEGATIVE - 1 = " + (MOST_NEGATIVE - 1.0))    // does not overflow?
    println ("MOST_NEGATIVE * 2 = " + (MOST_NEGATIVE * 2.0))    // overflow
    println ("-1.0 / 0.0        = " + -1.0 / 0.0)               // negative infinity
    println ("1.0 / 0.0         = " + 1.0 / 0.0)                // positive infinity
    println ("0.0 / 0.0         = " + 0.0 / 0.0)                // indeterminate NaN
    println ("sqrt (-1.0)       = " + sqrt (-1))                // not representable NaN

    println ("----------------------------------------------------------------")
    println (s"approx (5.0, 5.0)\t\t = ${approx (5.0, 5.0)}")
    println (s"approx (5.0, 5.1)\t\t = ${approx (5.0, 5.1)}")
    println (s"approx (5.0, ${5+TOL})\t = ${approx (5.0, 5+TOL)}")
    println (s"approx (5.0, ${5+TOL/2})\t = ${approx (5.0, 5+TOL/2.0)}")
    println (s"approx (5.0, ${5+TOL*2})\t = ${approx (5.0, 5+TOL*2.0)}")

} // ExtremeDTest object

