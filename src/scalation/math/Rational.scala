
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 1.0
 *  @date    Sat Jul 20 22:24:50 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import math.floor
import util.control.Breaks.{breakable, break}

import scalation.math.LongWithExp._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to represent rational numbers as 2 long integers.
 *  (a, b), e.g., (2, 3) meaning 2 / 3.  Arbitrary rational number can be
 *  created without loss of precision via the two constructors or the
 *  fromBigDecimal method.
 *  @param num  the numerator
 *  @param den  the denominator
 */
case class Rational (val num: Long, val den: Long = 1l)
     extends Fractional [Rational] with Ordered [Rational]
{
    require (den != 0l)         // the denominator must not be zero

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a rational number from a tuple of two Longs.
     *  @param y  the tuple of two Longs
     */
    def this (y: Tuple2 [Long, Long])
    {
        this (y._1, y._2)
    } // constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a rational number from a String of the form "12.3E+7".
     *  @see http://docs.oracle.com/javase/1.5.0/docs/api/java/math/BigDecimal.html
     *       #BigDecimal%28java.lang.String%29
     *  @param s  the given String representation of a number
     */
    def this (s: String)
    {
        this (Rational.from_BigDecimal (BigDecimal (s)))
    } // constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reduce the mangnitude of the numerator and denonimator by dividing
     *  both by their Greatest Common Divisor (GCD).
     */
    def reduce (): Rational =
    {
        val gc = gcd (num, den)
        Rational (num / gc, den / gc)
    } // reduce

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unary minus (-).
     */
    def unary_- (): Rational = Rational (-num, -den)
    def negate (q: Rational): Rational = -q

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two rational numbers (this + q)
     *  @param q  add rational q to this
     */
    def + (q: Rational): Rational = Rational (num * q.den + q.num * den, den * q.den)
    def plus (q: Rational, p: Rational): Rational = q + p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a rational number plus a long.
     *  @param l  add long l to this
     */
    def + (l: Long): Rational = Rational (num + l * den, den)
    def plus (q: Rational, l: Long): Rational = q + l

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract two rational numbers (this - q).
     *  @param q  subtract rational q from this
     */
    def - (q: Rational): Rational = Rational (num * q.den - q.num * den, den * q.den)
    def minus (q: Rational, p: Rational): Rational = q - p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract: a rational number minus a long.
     *  @param l  subtract long l from this
     */
    def - (l: Long): Rational = Rational (num - l * den, den)
    def minus (q: Rational, l: Long): Rational = q - l

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply two rational numbers (this * q).
     *  @param q  multiply this times rational q
     */
    def * (q: Rational): Rational = Rational (num * q.num, den * q.den)
    def times (q: Rational, p: Rational): Rational = q * p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply a rational number times a long.
     *  @param l  multiply this times long l
     */
    def * (l: Long): Rational = Rational (num * l, den)
    def times (q: Rational, l: Long): Rational = q * l

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide two rational numbers (this / q)
     *  @param q  divide this by rational q
     */
    def / (q: Rational): Rational = Rational (num * q.den, den * q.num) 
    def div (q: Rational, p: Rational): Rational = q / p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide a rational number div a long.
     *  @param l  divide this by long l
     */
    def / (l: Long): Rational = Rational (num, den * l)
    def div (q: Rational, l: Long): Rational = q / l

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise a rational number to the q-th power.
     *  @param q  the rational power/exponent
     */
    def ~^ (q: Rational): Rational = root (this ~^ q.num, q.den)
    def pow (q: Rational, p: Rational): Rational = q ~^ p

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise a rational number to the l-th power.
     *  @param l  the long power/exponent
     */
    def ~^ (l: Long): Rational = Rational (num ~^ l, den ~^ l)
    def pow (q: Rational, l: Long): Rational = q ~^ l
    def root (q: Rational, l: Long): Rational = Rational (q.num root l, q.den root l)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of this rational number.
     */
    def abs: Rational = Rational (num.abs, den.abs)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of that rational number.
     *  @param x  that rational number
     */
    def sqrt: Rational = this ~^ Rational (1l, 2l)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of this and that rational numbers.
     *  @param q  that rational number to compare with this
     */
    def max (q: Rational): Rational = if (q > this) q else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of this and that rational numbers.
     *  @param q  that rational number to compare with this
     */
    def min (q: Rational): Rational = if (q < this) q else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this rational number is integral.
     */
    def isIntegral: Boolean = den == 1l

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two rational numbers (negative for <, zero for ==, positive for >).
     *  @param q  the first rational number to compare
     *  @param p  the second rational number to compare
     */
    def compare (q: Rational, p: Rational): Int = q.num * p.den compare q.num * q.den

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this rational number with that rational number q.
     *  @param q  that rational number
     */
    def compare (q: Rational): Int = num * q.den compare q.num * den

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this rational number to a BigDecimal number.
     *  @param q  that rational number to convert
     */
    def toBigDecimal (q: Rational): BigDecimal = BigDecimal (q.num) / BigDecimal (q.den)
    def toBigDecimal: BigDecimal = BigDecimal (num) / BigDecimal (den)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this rational number to a Double.
     *  @param q  that rational number to convert
     */
    def toDouble (q: Rational): Double = q.num.toDouble / q.den.toDouble
    def toDouble: Double = num.toDouble / den.toDouble

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this rational number to a Float.
     *  @param q  that rational number to convert
     */
    def toFloat (q: Rational): Float = (q.num.toDouble / q.den.toDouble).toFloat
    def toFloat: Float = (num.toDouble / den.toDouble).toFloat

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/this rational number to an Int.
     *  @param q  that rational number to convert
     */
    def toInt (q: Rational): Int = (q.num / q.den).toInt
    def toInt: Int = (num / den).toInt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this rational number to a Long.
     *  @param q  that rational number to convert
     */
    def toLong (q: Rational): Long = q.num / q.den
    def toLong: Long = num / den

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a BigDecimal number.
     *  @param y  the BigDecimal used to create the rational number
     */
    def fromBigDecimal (y: BigDecimal): Rational = Rational.fromBigDecimal (y)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a Double.
     *  @param y  the Double used to create the rational number
     */
    def fromDouble (y: Double): Rational = Rational.fromDouble (y)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a Float.
     *  @param y  the Float used to create the rational number
     */
    def fromFloat (y: Float): Rational = Rational.fromDouble (y)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from an Int.
     *  @param n  the Int used to create the rational number
     */
    def fromInt (n: Int): Rational = Rational (n.toLong, 1l)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a Long.
     *  @param n  the Long used to create the rational number
     */
    def fromLong (n: Long): Rational = Rational (n, 1l)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether this rational number equals rational c.
     *  @param c  the rational number to compare with this
     */
    override def equals (c: Any): Boolean =
    {
        val q = c.asInstanceOf [Rational]
        (num * q.den) equals (q.num * den)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = num.hashCode + 41 * den.hashCode

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this rational number to a String.
     */
    override def toString: String = num + " / " + den
//  override def toString: String = "Rational ( " + num + " , " + den + " )"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the Great Common Demoninator (GCD) of two long integers.
     *  @param l1  the first long number
     *  @param l2  the second long number
     */
    private def gcd (l1: Long, l2: Long): Long =
    {
        BigInt (l1).gcd (l2).toLong
//      if (l2 == 0) l1 else gcd (l2, l1 % l2)
    } // gcd

} // Rational class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This companion object defines the origin (zero), one and minus one as well
 *  as some utility functions.
 */
object Rational
{
    /** Zero (0) as a Rational number
     */
    val _0  = Rational ( 0l)

    /** One (1) as a Rational number
     */
    val _1  = Rational ( 1l)

    /** Negative one (-1) as a Rational number
     */
    val _1n = Rational (-1l)

    /** Denominator (2 ~^ 54) big enough to capture largest Double significand (53 bits)
     */
    val maxDen = 0x40000000000000l
//  val maxDen = 18014398509481984l

    /** One in BigDecimal
     */
    val _1_big = BigDecimal (1)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of that rational number.
     *  @param x  that rational number
     */
    def abs (x: Rational) = x.abs

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of that rational number.
     *  @param x  that rational number
     */
    def sqrt (x: Rational): Rational = x ~^ Rational (1l, 2l)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Ordering for rational numbers.
     */
    val ord = new Ordering [Rational]
            { def compare (x: Rational, y: Rational) = x compare y }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a BigDecimal number.
     *  @param y  the BigDecimal used to create the rational number
     *  @param md  the maximum denominator
     */
    def fromBigDecimal (y: BigDecimal): Rational = new Rational (from_BigDecimal (y))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the numerator and denonimator of the closest rational number
     *  to the given BigDecimal number.
     *  @param y  the BigDecimal used to create the rational number
     *  @param md  the maximum denominator
     */
    def from_BigDecimal (y: BigDecimal, md: Long = Long.MaxValue): Tuple2 [Long, Long] =
    {
        val epsilon = _1_big / md
        var d = y
        val n = d.setScale (0, BigDecimal.RoundingMode.FLOOR)       // floor (d)
        d -= n
        if (d < epsilon) return (n.toLong, 1l)
        else if ((_1_big - epsilon) < d) return (n.toLong + 1l, 1l)
        var low_n = 0l
        var low_d = 1l
        var upp_n = 1l
        var upp_d = 1l
        var mid_n = 1l
        var mid_d = 1l

        breakable { while (true) {
            mid_n = low_n + upp_n
            mid_d = low_d + upp_d
            if (mid_d * (d + epsilon) < mid_n) {
                upp_n = mid_n
                upp_d = mid_d
            } else if (mid_n < (d - epsilon) * mid_d) {
                low_n = mid_n
                low_d = mid_d
            } else {
                break
            } // if
        }} // while
        (n.toLong * mid_d + mid_n, mid_d)
    } // from_BigDecimal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a double.
     *  @param y  the double used to create the rational number
     *  @param md  the maximum denominator
     */
    def fromDouble (y: Double): Rational = new Rational (from_Double (y))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the numerator and denonimator of the closest rational number
     *  to the given BigDecimal number.
     *  @see http://stackoverflow.com/questions/5124743/algorithm-for-simplifying-
     *       decimal-to-fractions/5128558#5128558
     *  @param y   the double used to create the rational number
     *  @param md  the maximum denominator
     */
    def from_Double (y: Double, md: Long = maxDen): Tuple2 [Long, Long] =
    {
        val epsilon = 1. / md
        var d = y
        val n = floor (d)
        d -= n
        if (d < epsilon) return (n.toLong, 1l)
        else if ((1. - epsilon) < d) return (n.toLong + 1l, 1l)
        var low_n = 0l
        var low_d = 1l
        var upp_n = 1l
        var upp_d = 1l
        var mid_n = 1l
        var mid_d = 1l

        breakable { while (true) {
            mid_n = low_n + upp_n
            mid_d = low_d + upp_d
            if (mid_d * (d + epsilon) < mid_n) {
                upp_n = mid_n
                upp_d = mid_d
            } else if (mid_n < (d - epsilon) * mid_d) {
                low_n = mid_n
                low_d = mid_d
            } else {
                break
            } // if
        }} // while
        (n.toLong * mid_d + mid_n, mid_d)
    } // from_Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a Double.
     *  FIX: if den not a power of 2, it goes to md.
     *  @see http://rosettacode.org/wiki/Convert_decimal_number_to_rational
     *  @param y   the double used to create the rational number.
     *  @param md  the maximum denominator
     */
    def fromDouble2 (y: Double, md: Long = maxDen): Rational = 
    {
        if (y == 0.0) return _0
        val neg = y < 0.0
        val h   = Array (0l, 1l, 0l)
        val k   = Array (1l, 0l, 0l)

        var f   = if (neg) -y else y
        var a   = 0l
        var n   = 1l
        var x   = 0l
        var end = false

        while ( f != floor (f)) { n <<= 1; f *= 2 }     // double f until no frac
        var d = f.toLong

        breakable { for (i <- 0 to 63) {
            a = if (n != 0l) d / n else 0l
            if (i > 0 && a == 0l) break
            x = d; d = n; n = x % n
            x = a
            if (k(1) * a + k(0) >= md) {
                x = (md - k(0)) / k(1)
                if (x * 2l >= a || k(1) >= md) end = true else break
            } // if

            h(2) = x * h(1) + h(0); h(0) = h(1); h(1) = h(2)
            k(2) = x * k(1) + k(0); k(0) = k(1); k(1) = k(2)

            if (end) break
        }} // for

        Rational (if (neg) -h(1) else h(1), k(1))
    } // fromDouble2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a Float.
     *  @param y  the float used to create the rational number.
     */
    def fromFloat (y: Float): Rational = fromDouble (y)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from an Int.
     *  @param n  the integer used to create the rational number.
     */
    def fromInt (n: Int) = Rational (n)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a rational number from a Long.
     *  @param n  the long used to create the rational number.
     */
    def fromLong (n: Long) = Rational (n)

} // Rational companion object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Rational class.
 */
object RationalTest extends App
{
    import Rational._

    val a = Rational (1l,  4l)
    val b = Rational (1l,  2l)
    val c = Rational (2l,  3l)
    val d = Rational (8l, 10l)
    val e = Rational (5l)

    println ("maxDen    = " + maxDen)

    println ("a         = " + a)
    println ("b         = " + b)
    println ("c         = " + c)
    println ("d         = " + d)
    println ("e         = " + e)
    println ("-c        = " + -c)
    println ("c + d     = " + (c + d))
    println ("c - d     = " + (c - d))
    println ("c * d     = " + (c * d))
    println ("c / d     = " + (c / d))
    println ("c ~^ 2l   = " + (c ~^ 2l))
    println ("a ~^ b    = " + (a ~^ b))
    println ("c.abs     = " + c.abs)
    println ("a.sqrt    = " + a.sqrt)
    println ("c < d     = " + (c < d))
    println ("d.reduce  = " + d.reduce)

    println ("fromDouble (.5))    = " + fromDouble (.5))
    println ("fromDouble (.25))   = " + fromDouble (.25))
    println ("fromDouble (.125))  = " + fromDouble (.125))
    println ("fromDouble (.0625)) = " + fromDouble (.0625))
    println ("fromDouble (-.125)) = " + fromDouble (-.125))
    println ("fromDouble (1./3.)) = " + fromDouble (1./3.))
    println ("fromDouble (.334))  = " + fromDouble (.334))
    println ("fromDouble (.2))    = " + fromDouble (.2))
    println ("fromDouble (0.0))   = " + fromDouble (0.0))

} // RationalTest object

