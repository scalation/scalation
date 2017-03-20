
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Sep  2 13:40:48 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scala.collection.immutable.StringOps
import scala.language.implicitConversions

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrO` object is used to represent and operate on string numbers.
 *  It contains an implicit class definition for `StrNum`, which extends
 *  strings with `Numeric` operations, it it can be used in `VectorS`. 
 *  The semantics of `StrNum` operators are similar those in Pike.
 *  @see http://docs.roxen.com/pike/7.0/tutorial/strings/operators.xml
 */
object StrO
{
    /** Zero (0) as a string number
     */
    val _0  = StrNum ("0")

    /** One (1) as a string number
     */
    val _1  = StrNum ("1")

    /** Ordering for string numbers
     */
    implicit val ord = new Ordering [StrNum]
               { def compare (s: StrNum, t: StrNum) = s compare t }

    /** Default element separator (e.g., in a CSV file)
     */
    private val SP = ","

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the absolute value of that string number.
     *  @param s  that string number
     */
    def abs (s: StrNum): StrNum = StrNum (s.ss.toUpperCase)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum of two string number, 's' and 't'.
     *  @param s  the first string number to compare
     *  @param t  the second string number to compare
     */
    def max (s: StrNum, t: StrNum): StrNum = if (t > s) t else s

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the minimum of two string numbers, 's' and 't'.
     *  @param s  the first string number to compare
     *  @param t  the second string number to compare
     */
    def min (s: StrNum, t: StrNum): StrNum = if (t < s) t else s

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the square root of that string number (currently not supported).
     *  @param s  that string number
     */
    def sqrt (s: StrNum): StrNum =
    {
//      throw new NoSuchMethodException ("StrNum does not support sqrt function")
        println ("StrNum.sqrt: sqrt function not supported")
        _0
    } // sqrt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a string number from a byte array.
     *  @param buffer  the byte array to use
     */
    def fromByteArray (buffer: Array [Byte]): StrNum =
    {
        new String (buffer.map (_.toChar))
    } // fromByteArray

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Implicit conversion from 'Double' to 'StrNum'.
     *  @param d  the Double parameter to convert
     */
    implicit def double2StrNum (d: Double): StrNum = new StrNum (d.toString)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `StrNum` implicit class is used to represent and operate on string
     *  numbers.  Internally, a string number is represented as String.
     *  The semantics of `StrNum` operators are similar those in Pike.
     *  @see http://docs.roxen.com/pike/7.0/tutorial/strings/operators.xml
     *---------------------------------------------------------------------
     *  @param ss  the underlying string
     */
    implicit class StrNum (val ss: String) extends Numeric [StrNum] with Ordered [StrNum]
    {
        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compute the unary minus (-).
         */
        def unary_- (): StrNum = "-" + ss
        def negate (s: StrNum): StrNum = "-" + s.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Add two string numbers.
         *  @param s  add string s to this
         */
        def + (s: StrNum): StrNum = ss + s.ss
        def plus (s: StrNum, t: StrNum): StrNum = s.ss + t.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Subtract two string numbers.
         *  @param s  subtract s from this
         */
        def - (s: StrNum): StrNum = ss diff s.ss
        def minus (s: StrNum, t: StrNum): StrNum = s.ss diff t.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Multiply two string numbers, by replacing 'SP' by 's'.
         *  @param s  multiply this times s
         */
        def * (s: StrNum): StrNum = ss.replace (SP, s.ss)
        def times (s: StrNum, t: StrNum): StrNum = s.ss.replace (SP, t.ss)

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Divide two string numbers, by replacing 's' by 'SP.
         *  @param s  divide this by s
         */
        def / (s: StrNum): StrNum = ss.replace (s.ss, SP)

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Raise a string to the 'r'-th power.
         *  @param r  the power/exponent
         */
        def ~^ (r: StrNum): StrNum =
        {
            val k = if (r.ss matches "\\d*") r.toInt (r) else r.ss.size
            var str = ss
            for (i <- 1 until k) str = str intersect ss
            StrNum (str)
        } // ~^

        def pow (s: StrNum, r: StrNum): StrNum = s ~^ r

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Return whether two string numbers are nearly equal.
         *  @param s  compare this with s
         */
        def =~ (s: StrNum): Boolean = ss == s.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Return whether two string numbers not are nearly equal.
         *  @param s  compare this with s
         */
        def !=~ (s: StrNum): Boolean = ss != s.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Return the maximum of 'this' and 's' string numbers.
         *  @param s  that string number to compare with this
         */
        def max (s: StrNum): StrNum = if (s > this) s else this

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Return the minimum of 'this' and 's' string numbers.
         *  @param s  that string number to compare with this
         */
        def min (s: StrNum): StrNum = if (s < this) s else this

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compare two string numbers (negative for <, zero for ==, positive for >).
         *  @param s  the first string number to compare
         *  @param t  the second string number to compare
         */
        def compare (s: StrNum): Int = ss compare s.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compare 'this' string number with that string number 't'.
         *  @param t  that string number
         */	
        def compare (s: StrNum, t: StrNum): Int = s.ss compare t.ss

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compare 'this' string number with that string number 't' for inequality.
         *  @param t  that string number
         */
        def ≠ (t: StrNum) = this != t

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compare 'this' string number with that string number 't' for less than
         *  or equal to.
         *  @param t  that string number
         */
        def ≤ (t: StrNum) = this <= t

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Compare 'this' string number with that string number 't' for greater
         *  than or equal to.
         *  @param t  that string number
         */
        def ≥ (t: StrNum) = this >= t

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Get the bytes for 'this' string number.
         */
        def getBytes (): Array [Byte] = ss.getBytes ()

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Create a string number from a `Double`.
         *  @param d  the source double
         */
        def fromDouble (d: Double): StrNum = d.toString

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Create a string number from an `Int`.
         *  @param n  the source integer
         */
        def fromInt (n: Int): StrNum = n.toString

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `StrNum`.
         *  @param s  that string number to convert
         */
        def toStrNum: StrNum = this

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `Complex`.
         *  @param s  that string number to convert
         */
        def toComplex (s: StrNum): Complex = Complex (s.ss)
        def toComplex: Complex = toComplex (this)

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `Rational`.
         *  @param s  that string number to convert
         */
        def toRational (s: StrNum): Rational = Rational (s.ss)
        def toRational: Rational = toRational (this)

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `Real`.
         *  @param s  that string number to convert
         */
        def toReal (s: StrNum): Real = Real (s.ss)
        def toReal: Real = toReal (this)

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `Double`.
         *  @param s  that string number to convert
         */
        def toDouble (s: StrNum): Double = new StringOps (s.ss).toDouble
        def toDouble: Double = new StringOps (ss).toDouble

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `Float`.
         *  @param s  that string number to convert
         */
        def toFloat (s: StrNum): Float = new StringOps (s.ss).toFloat
        def toFloat: Float = new StringOps (ss).toFloat

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to an `Int`.
         *  @param s  that string number to convert
         */
        def toInt (s: StrNum): Int = new StringOps (s.ss).toInt
        def toInt: Int = new StringOps (ss).toInt

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a `Long`.
         *  @param s  that string number to convert
         */
        def toLong (s: StrNum): Long = new StringOps (s.ss).toLong
        def toLong: Long = new StringOps (ss).toLong

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Override equals to determine whether 'this' string number equals
         *  string 's'.
         *  @param s  the string number to compare with this
         */
        override def equals (s: Any): Boolean =
        {
             s match {
             case _ : String => ss equals s
             case _ : StrNum => ss equals (s.asInstanceOf [StrNum]).ss
             case _          => false
             } // match
//           s.isInstanceOf [StrNum] && (ss equals (s.asInstanceOf [StrNum]).ss)
        } // equals

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Must also override hashCode to be be compatible with equals.
         */
        override def hashCode: Int = ss.hashCode

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert 'this' string number to a String.
         */
        override def toString: String = ss

    } // StrNum class

} // StrO object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrNumTest` object is used to test the `StrNum` class.
 *  > run-main scalation.math.StrNumTest
 */
object StrNumTest extends App
{
    import StrO._
    import scala.util.Sorting.quickSort

    val s1 = StrNum ("test1,test2")
//  val s2: StrNum = StrNum ("test3,test4")
    val s2: StrNum = "test3,test4"
    println ("s1 = " + s1)
    println ("s2 = " + s2)

    println ("-s1 = " + -s1)
    println ("s1 + s2 = " + (s1 + s2))
    println ("s1 - s2 = " + (s1 - s2))
    println ("s1 * s2 = " + (s1 * s2))
    println ("s1 / s2 = " + (s1 * s2))

    println ("abs (s1)  = " + abs (s1))
    println ("s1 max s2 = " + (s1 max s2))
    println ("s1 min s2 = " + (s1 min s2))
    println ("s1 < s2   = " + (s1 < s2))
    println ("s2 < s1   = " + (s2 < s1))

    val s3_2 = StrNum ("3.2")
    val s3   = StrNum ("3")

    println ("s3_2.fromDouble (3.2) = " + s3_2.fromDouble (3.2))
    println ("s3.fromInt (3)        = " + s3.fromInt (3))
    println ("s3_2.toDouble         = " + s3_2.toDouble (s3_2))
    println ("s3_2.toFloat          = " + s3_2.toFloat (s3_2))
    println ("s3.toInt              = " + s3.toInt (s3))
    println ("s3.toLong             = " + s3.toLong (s3))

    def sort (arr: Array [StrNum]) { quickSort (arr)(StrO.ord) }

    val arr = Array (s2, s3, s1)
    println ("arr = " + arr.deep)
    sort (arr)
    println ("arr = " + arr.deep)

} // StrNumTest object

