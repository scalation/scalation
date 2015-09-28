
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Jun 14 15:22:33 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import annotation.strictfp

import language.implicitConversions
import math.log10
import util.control.Breaks.{break, breakable}

import ExtremeD.MAX_VALUE

import Real._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Real` class provides higher precision floating point numbers using the
 *  Double Double technique, which supports 106 bits of precision.
 *-----------------------------------------------------------------------------
 *  Code adapted from DoubleDouble.java:
 *  @see http://tsusiatsoftware.net/dd/main.html
 *  @see oai.cwi.nl/oai/asset/9159/9159A.pdf
 *-----------------------------------------------------------------------------
 *  @param hi  the high portion of the real number
 *  @param lo  the low portion of the real number
 */
@strictfp
case class Real (hi: Double, lo: Double = 0.0) // extends strictfp
     extends Fractional [Real] with Ordered [Real]
{
    /** Debug flag
     */
    private val DEBUG = false

    /** 2^27+1, halfway for IEEE double 
     */
    private val SPLIT = 134217729.0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' number "is Infinite".
     */
    def isInfinity: Boolean = hi.isInfinite ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' number "is Not a Number".
     */
    def isNaN: Boolean = new scala.runtime.RichDouble (hi).isNaN

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unary minus (-).
     */
    def unary_- (): Real = if (isNaN) this else Real (-hi, -lo)

    def negate (x: Real): Real = -x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two real numbers.
     *  @param y  add real y to this
     */
    def + (y: Real): Real =
    {
        var t = 0.0                                                         // temp

        var (zh, zl) = (hi + y.hi, lo + y.lo)                               // z = x + y
        val (yh, yl) = (zh - hi, zl - lo)                                   // y' = z - x
        val (xh, xl) = (zh - yh, zl - yl)                                   // x' = z - y'
        val (dh, dl) = ((hi - xh) + (y.hi - yh), (lo - xl) + (y.lo - yl))   // d = (x - x') + (y - y')
        t = dh + zl
        val hh = zh + t; val hl = t + (zh - hh)

        t = dl + hl
        zh = hh + t; zl = t + (hh - zh)
        Real (zh, zl)
    } // +

    def plus (x: Real, y: Real): Real = x + y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract two real numbers.
     *  @param y  subtract y from this
     */
    def - (y: Real): Real = this + (-y)

    def minus (x: Real, y: Real): Real = x - y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply two real numbers.
     *  @param y  multiply this times y
     */
    def * (y: Real): Real =
    {
         var cx = SPLIT * hi
         val hx = cx - (cx - hi)
         val tx = hi - hx

         var cy = SPLIT * y.hi
         val hy = cy - (cy - y.hi)
         val ty = y.hi - hy

         cx = hi * y.hi
         cy = ((((hx*hy - cx) + hx*ty) + tx*hy) + tx*ty) + (hi*y.lo + lo*y.hi)

         val zh = cx + cy
         val zl = cy + (cx - zh)
         Real (zh, zl)
    } // *

    def times (x: Real, y: Real): Real = x * y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide two real numbers.
     *  @param y  divide this by y
     */
    def / (y: Real): Real =
    {
        var qh = hi / y.hi
        var c  = SPLIT * qh
        var hc = c - (c - qh)
        var tc = qh - hc

        var u  = SPLIT * y.hi
        var hy = u - (u - y.hi)
        var ty = y.hi - hy

        var hx = qh * y.hi

        u = (((hc*hy - hx) + hc*ty) + tc*hy) + tc*ty
        c = ((((hi - hx) - u) + lo) - qh*y.lo) / y.hi

        val zh = qh + c
        val zl = (qh - zh) + c
        Real (zh, zl)
    } // /

    def div (x: Real, y: Real): Real = x / y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the remainder of the 'this' divided by 'x', i.e., 'this mod x'.
     *  @param x  the modulus
     */
    def % (x: Real) =
    {
        this - (this / x).floor * x
    } // %

    def rem (x: Real, y: Real): Real = x % y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise a real number  to the 'k'-th power.
     *  @param k  the power/exponent (as a `Int`)
     */
    def ~^ (k: Int): Real =
    {
        if (k == 0) return _1

        var r = this             // the real number
        var s = _1               // hold product
        var n = k.abs

        if (n > 1) {             // use binary exponentiation 
            while (n > 0) {
                if (n % 2 == 1) s = s * r
                n /= 2
                if (n > 0) r = r * r
            } // while
        } else {
            s = r
        } // if

        if (k < 0) _1 / s else s
    } // ~^

    def pow (x: Real, k: Int): Real = x ~^ k

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise a real number  to the 'k'-th power.
     *  @param y  the power/exponent (as a `Real`)
     */
    def ~^ (y: Real): Real =
    {
        exp (y * Real.log (this))
    } // ~^

    def pow (x: Real, y: Real): Real = x ~^ y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether two real numbers are nearly equal.
     *  @param y the compare 'this' with y
     */
    def =~ (y: Real): Boolean = (hi == y.hi && lo =~ y.lo)             // FIX?
    def !=~ (y: Real): Boolean = ! (this =~ y)

    def near_eq (x: Real, y: Real): Boolean = x =~ y

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of 'this' real number.
     */
    def abs: Real = Real (math.abs (hi), math.abs (lo))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'ceil'ing (integer above) of 'this' real number.
     */
    def ceil: Real =
    {
        val chi = math.ceil (hi)
        val clo = if (chi == hi) math.ceil (lo) else 0.0 
        Real (chi, clo)
    } // ceil

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the floor (integer below) of 'this' real number.
     */
    def floor: Real =
    {
        val fhi = math.floor (hi)
        val flo = if (fhi == hi) math.floor (lo) else 0.0 
        Real (fhi, flo)
    } // floor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the signum of 'this' real number, 1, 0, -1 for >0, =0, <0.
     *  @param x  the real number whose signum is sought
     */
    def signum: Real = if (this > _0) _1 else if (this < _0) _1n else _0

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'round'ed (closest) integer to 'this' real number.
     */
    def round: Long = (this + _1by2).floor.toLong

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of 'this' and that real numbers.
     *  @param y  that real number to compare with this
     */
    def max (y: Real): Real = if (y > this) y else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of 'this' and that real numbers.
     *  @param y  that real number to compare with this
     */
    def min (y: Real): Real = if (y < this) y else this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two real numbers (negative for <, zero for ==, positive for >).
     *  @param x  the first real number to compare
     *  @param y  the second real number to compare
     */
    def compare (x: Real, y: Real): Int =
    {
        if (x.hi == y.hi) x.lo compare y.lo else x.hi compare y.hi
    } // compare

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' real number with that real number 'y'.
     *  @param y  that real number
     */	
    def compare (y: Real): Int =
    {	
        if (hi == y.hi) lo compare y.lo else hi compare y.hi
    } // compare

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/'this' real number to a `Real.
     *  @param x  that real number to convert
     */
    def toReal (x: Real): Real = x

    def toReal: Real = this

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/'this' real number to a `Double`.
     *  @param x  that real number to convert
     */
    def toDouble (x: Real): Double = x.hi

    def toDouble: Double = hi

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/'this' real number to a `Float`.
     *  @param x  that real number to convert
     */
    def toFloat (x: Real): Float = x.hi.toFloat

    def toFloat: Float = hi.toFloat

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert that/'this' real number to an `Int`.
     *  @param x  that real number to convert
     */
    def toInt (x: Real): Int = x.hi.toInt

    def toInt: Int = hi.toInt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' real number to a `Long`.
     *  @param x  that real number to convert
     */
    def toLong (x: Real): Long = x.hi.toLong

    def toLong: Long = hi.toLong

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a real number from an `Int`.
     *  @param n  the integer used to create the real number
     */
    def fromInt (x: Int): Real = Real (x)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a real number from an `Long`.
     *  @param n  the long integer used to create the real number
     */
    def fromLong (x: Long): Real = Real (x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether 'this' real number equals real 'y'.
     *  @param y  the real number to compare with this
     */
    override def equals (x: Any): Boolean =
    {
        x.isInstanceOf [Real] && (hi equals x.asInstanceOf [Real].hi) &&
                                 (lo equals x.asInstanceOf [Real].lo)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = hi.hashCode + 41 * lo.hashCode

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' real number to a String in standard form "dd.dd".
     *  FIX: add support for scientific notation
     */
    override def toString: String =
    {
        if (isNaN) return "NaN"
        if (this == _0) return "0.0"
        val (sigDigits, mag) = extractSigDigits
        val decimalPt = mag + 1
        if (DEBUG) println (s"(sigDigits, decimalPt) = ${(sigDigits, decimalPt)}")
        var num = sigDigits

        if (sigDigits.charAt (0) == '.') {
            num = "0" + sigDigits;
        } else if (decimalPt < 0) {
            val pad = "0" * -decimalPt
            num = "0." + pad + sigDigits
        } else if (sigDigits.indexOf ('.') == -1) {
            val pad = "0" * (decimalPt - sigDigits.length)
            num = sigDigits + pad + ".0"
        } // if
        if (this < _0) "-" + num else num
    } // toString

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract the significant digits from 'this' real number and determine
     *  its magnitude.
     */
    private def extractSigDigits: Tuple2 [String, Int] =
    {
        var y     = abs
        var mag   = magnitude (y.hi)
        val scale = _10 ~^ mag
        y = y / scale
        if (y > _10)     { y = y / _10; mag += 1 }
        else if (y < _1) { y = y * _10; mag -= 1 }
        val decimalPt = mag + 1

        val sb      = new StringBuilder ()
        val nDigits = 31
        breakable { for (i <- 0 until nDigits) {
            if (i == decimalPt) sb.append ('.')
            val digit      = y.hi.toLong
            if (DEBUG) println (s"digit = $digit")
            if (digit < 0) break

            var rebiasBy10 = false
            val digitChar  = if (digit > 9) { rebiasBy10 = true; '9' } else ('0' + digit).toChar
            if (DEBUG) println (s"digitChar = $digitChar")
            sb.append (digitChar)
            y = (y - Real (digit)) * _10
            if (rebiasBy10) y = y + _10
            
            var contExtracting = true
            val remMag = magnitude (y.hi)
            if (remMag < 0 && math.abs (remMag) >= (nDigits - i)) contExtracting = false
            if (! contExtracting) break
        }} // breakable for

        (sb.toString, mag)
    } // extractSigDigits
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' real number to a String of the form "(hi, lo)".
     */
    def toString2: String = s"($hi, $lo)"
    
} // Real class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Real` companion object defines the origin (zero), one half and one
 *  as well as some utility functions.
 */
object Real
{
    import java.lang.Double.NaN

    /** Zero (0) as a Real number
     */
    val _0 = Real (0.0)

    /** One Half (0.5) as a Real number
     */
    val _1by2 = Real (0.5)

    /** One (1) as a Real number
     */
    val _1 = Real (1.0)

    /** Negative One (-1) as a Real number
     */
    val _1n = Real (-1.0)

    /** Two (2) as a Real number
     */
    val _2 = Real (2.0)

    /** Ten (10) as a Real number
     */
    val _10 = Real (10.0)

    /** 700 as a real number
     */
    val _700 = Real (700.0)

    /** 2^9 as a real number
     */
    val _512 = Real (512.0)

    /** 'pi' with 106 bits of precision
     */
    val Pi = Real (3.141592653589793116,
                   1.224646799147353207E-16)

    /** '2 pi' with 106 bits of precision
     */
    val _2Pi = _2 * Pi
                   
    /** Euler's number 'e' with 106 bits of precision
     */
    val E = Real (2.718281828459045091,
                  1.445646891729250158E-16)

    /** Natural log of 2 as a real number
     */
    val log_2 = Real ("0.693147180559945309417232121458")

    /** Approximate machine epsilon for Double-Double
     */
    val EPSILON = 1.23259516440783E-32                 // 2^-106

    /** Represents Not a Number
     */
    val NaN = Real (Double.NaN, Double.NaN)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Double' to 'Real'.
     *  @param d  the Double parameter to convert
     */
    implicit def double2Real (d: Double) = new Real (d, 0.0)    

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a real number from a pair (Tuple2) of Doubles.
     *  @param xt  the tuple form of a real number
     */
    def apply (xt: Tuple2 [Double, Double]): Real = Real (xt._1, xt._2)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a real number from its primary string representation "dd.ddd".
     *  Example: "0.693147180559945309417232121458"
     *  @param xs  the string form of a real number
     */
    def apply (xs: String): Real =
    {
        var x     = _0                           // Real value to return
        var nd    = 0                            // number of digits processed
        var i     = 0                            // character position, ignoring whitespace
        var isNeg = false                        // whether the number is nagative
        var exp   = 0                            // optional exponent
        var decPt = 0                            // position of decimal point

        var str = xs.trim                        // trim away whitespace
        val ch0 = str(0)
        if (ch0 == '-' || ch0 == '+') {
            str = str.drop (1)                   // remove sign character
            if (ch0 == '-') isNeg = true         // record it's negative
            i += 1
        } // if

        for (ch <- str) {
            if (ch.isDigit) {
                val d = (ch - '0').toDouble
                x = x * _10 + Real (d)
                nd += 1
            } else if (ch == '.') {
                decPt = nd                       // number of digits before decimal point
            } else if (ch == 'e' || ch == 'E') {
                exp = str.substring (i).toInt    // convert the rest to an integer exponent
            } else { 
                println (s"apply: unexpected character '$ch' at position $i in $str")
                return _0
            } // if
            i += 1
        } // for

        val np = nd - decPt - exp
        x = if (np == 0)     x                   // rescale the real number
            else if (np > 0) x / _10 ~^ np
            else             x * _10 ~^ (-np)
        if (isNeg) -x else x
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of real number 'x'.
     *  @param x  the real number whose absolute value is sought
     */
    def abs (x: Real): Real = x.abs

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'e' raised to the 'x'-th power 'e^x'.  'x' is rescaled to improve
     *  convergence.  Rescaling causes slight reduction in precision.
     *  e = 2.718281828459045235360287471345   from scalation.math.Real.exp
     *  E = 2.718281828459045235360287471352   given contant
     *  @see http://mathworld.wolfram.com/e.html
     *  @see bt.pa.msu.edu/pub/papers/HICOSYMSU08/HICOSYMSU08.pdf
     *  FIX: use a faster and more precise algorithm
     *  @param x  the real number exponent
     */
    def exp (x: Real = _1): Real =
    {
        val xAbs = x.abs
        if (xAbs > _700) println (s"exp: magnitude of exponent x = $x is too large")
//      if (xAbs < _1by2) exp_ (x) else exp_ (x / _2048) ~^ 2048
        if (xAbs < _1by2) exp_ (x) else exp_ (x / _512) ~^ 512
    } // exp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'e' raised to the 'x' power 'e^x' when 'x' < 1/2.
     *  When 'x' is small the Taylor series will converge quickly.
     *  @param x  the real number exponenent
     */
    private def exp_ (x: Real): Real =
    {
        val MAX   = 200
        val TRHES = EPSILON
        var term  = _1
        var sum   = _1
        var i     = 1

        do {
            term = term * x / Real (i)
            sum  = sum + term
            i   += 1
        } while (i < MAX && term > THRES)
        println (s"exp_: i = $i, sum = $sum")
        sum   
    } // exp_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the natural log (log base 'e') of real number 'x'.
     *  @see bt.pa.msu.edu/pub/papers/HICOSYMSU08/HICOSYMSU08.pdf
     *  @param x  the real number whose log is sought
     */
    def log (x: Real): Real =
    {
        val m = math.round (log2 (x.toDouble))    // approx using Double
        m * log_2 + log_ (2.0 ~^ (-m) * x)
    } // log

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the natural log (log base 'e') of real number 'x', when 'x'
     *  is close to 1.
     *  @see bt.pa.msu.edu/pub/papers/HICOSYMSU08/HICOSYMSU08.pdf
     *  FIX: need better accuracy
     *  @param x  the real number whose log is sought
     */
    private def log_ (x: Real): Real =
    {
        val MAX   = 600
        val TRHES = EPSILON
        val r     = (x - _1) / (x + 1)
        val rr    = r * r
        var term  = r
        var sum   = _0
        var i     = 1

        do {
            sum  = sum + term / Real (i)
            term = term * rr
            i   += 2
        } while (i < MAX && term > THRES)
//      println (s"log_: i = $i, sum = $sum")
        _2 * sum  
    }  // log_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sin of real number 'x'.
     *  @param x  the real number whose sin is sought
     */
    def sin (x: Real): Real =
    {
        val y = x % _2Pi
        if (y.abs > Pi) -sin_ (if (y < _0) y + Pi else y - Pi)
        else            sin_ (y)
    } // sin

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sin of real number 'x', when 'x' is in [-Pi, Pi].
     *  @see bt.pa.msu.edu/pub/papers/HICOSYMSU08/HICOSYMSU08.pdf
     *  FIX: need better accuracy
     *  @param x  the real number whose sin is sought
     */
    private def sin_ (x: Real): Real =
    {
        val MAX   = 600
        val TRHES = EPSILON
        val xx    = x * x
        var term  = x
        var sum   = x
        var i     = 3
        var neg   = true

        do {
            term = term * xx / Real ((i-1) * i)
            if (neg) sum  = sum - term
            else     sum  = sum + term
            neg = ! neg
            i  += 2
        } while (i < MAX && term > THRES)
        println (s"sin_: i = $i, sum = $sum")
        sum  
    } // sin_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of two real numbers 'x' and 'y'.
     *  @param x  the first real number to compare
     *  @param y  the second real number to compare
     */
    def max (x: Real, y: Real): Real = if (y > x) y else x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of two real numbers 'x' and 'y'.
     *  @param x  the first real number to compare
     *  @param y  the second real number to compare
     */
    def min (x: Real, y: Real): Real = if (y < x) y else x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of real number 'x'.
     *  @param x  the real number whose square root is sought
     */
    def sqrt (x: Real): Real =
    { 
        if (x == _0) return _0
        if (x < _0)  return NaN

        val yd  = 1.0 / math.sqrt (x.hi)
        val y   = Real (x.hi * yd)
        val del = x - y * y
        y + Real (del.hi * (yd * 0.5))
    } // sqrt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the decimal magnitude of 'x', i.e., the largest integer 'k'
     *  such that '10^k <= x'.
     *  @param x  the number to find the magnitude of
     */
    def magnitude (x: Double): Int = 
    {
        val xAbs = math.abs (x)
        val xMag = math.floor (log10 (xAbs)).toInt
        if ((Double_Exp (10.0) ~^ xMag) * 10.0 <= xAbs) xMag + 1 else xMag   
    } // magnitude

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Ordering for real numbers.
     */
    val ord = new Ordering [Real]
            { def compare (x: Real, y: Real) = x compare y }

} // Real object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RealTest` object is used to test the `Real` class.
 *  > run-main scalation.math.RealTest
 */
object RealTest extends App
{
    import util.Sorting.quickSort

    val a = Real (10.0)
    val b = Real (6.0)
    val c = Real._1 / b
    val d = Real._0
    val x = 7.0                // Double
    val r = sqrt (a)

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("d     = " + d)
    println ("pi    = " + Pi)
    println ("pi/4  = " + Pi/4)

    println ("-a    = " + a)
    println ("a + c = " + (a + c))
    println ("a - c = " + (a - c))
    println ("a * c = " + (a * c))
    println ("a / c = " + (a / c))
    println ("a + x = " + (a + x))
    println ("a - x = " + (a - x))
    println ("a * x = " + (a * x))
    println ("a / x = " + (a / x))
    println ("a % b = " + (a % b))

    println ("a ~^ 2    = " + (a ~^ 2))
    println ("a ~^ 2.5  = " + (a ~^ 2.5))
    println ("a * a     = " + (a * a))
    println ("a.abs     = " + a.abs)
    println ("a.signum  = " + a.signum)
    println ("a max c   = " + (a max c))
    println ("a min c   = " + (a min c))
    println ("sqrt(a)   = " + r)
    println ("exp(a)    = " + exp(a))
    println ("exp(_1)   = " + exp())
    println ("E         = " + E)
    println ("log(E)    = " + log(E))
    println ("log(a)    = " + log(a))
    println ("sin(Pi/4) = " + sin(Pi/4))
    println ("r * r     = " + (r * r))
    println ("a < c     = " + (a < c))
    println ("a > c     = " + (a > c))

    def sort (arr: Array [Real]) { quickSort (arr)(Real.ord) }

    val arr = Array (a, b, c)
    println ("arr = " + arr.deep)
    sort (arr)
    println ("arr = " + arr.deep)

    println (s"123...    = 123456789012345678901234567890.0")
    println (s"R(123...) = ${Real ("123456789012345678901234567890.0")}")
    println (s"log (2)   = ${Real.log_2}")

} // RealTest object

