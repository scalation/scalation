
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Sep 24 20:46:45 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is used to represent probabilistic numbers (x, p) where x is a
 *  a real number and p is its probability of occurrence.
 *  Currently this class is half-baked!!!
 *  @see http://okmij.org/ftp/Computation/monads.html#random-var-monad
 *  @param x  the real number (double precision)
 *  @param p  the probability of its occurrence [0, 1]
 */
case class ProbNumber (x: Double, p: Double = 1.0)
     extends Numeric [ProbNumber] with Ordered [ProbNumber] with Error
{
    private val EPSILON = 1E-9        // number close to zero

    if (p < 0.0 || p > 1.0) flaw ("constructor", "p is not a probability " + p)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unary minus (-).
     */
    def unary_- () = ProbNumber (-x, p)

    def negate (xp: ProbNumber) = -xp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two probabilistic numbers.
     *  @param xp  add xp to this
     */
    def + (xp: ProbNumber) = ProbNumber (x * p + xp.x * xp.p, p + xp.p)

    def plus (xp: ProbNumber, yq: ProbNumber) = xp + yq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Substract two probabilistic numbers.
     *  @param c  subtract c from this
     */
    def - (xp: ProbNumber) = ProbNumber (x * p - xp.x * p, p + xp.p)

    def minus (xp: ProbNumber, yq: ProbNumber) = xp - yq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply two probabilistic numbers.
     *  @param c  multiply this times c
     */
    def * (xp: ProbNumber) = ProbNumber (x * p * xp.x * xp.p, p + xp.p)

    def times (xp: ProbNumber, yq: ProbNumber) = xp * yq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the probabilistic number is certain (probability = 1).
     */
    def isCertain = p >= 1.0 - EPSILON

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two probabilistic numbers (negative for <, zero for ==, positive for >).
     *  @param xp  the first probabilistic number to compare
     *  @param yq  the second probabilistic number to compare
     */
    def compare (xp: ProbNumber, yq: ProbNumber) =  xp.x compare yq.x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this probabilistic number with that probabilistic number yq.
     *  @param yq  that probabilistic number
     */	
    def compare (yq: ProbNumber) = x compare yq.x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to a Double.
     *  @param xp  the probabilistic number to convert
     */
    def toDouble (xp: ProbNumber) = xp.x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to a Float.
     *  @param xp  the probabilistic number to convert
     */
    def toFloat (xp: ProbNumber) = xp.x.asInstanceOf [Float]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to a Long.
     *  @param xpc  the probabilistic number to convert
     */
    def toLong (xp: ProbNumber) = xp.asInstanceOf [Long]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to an Int.
     *  @param xp  the probabilistic number to convert
     */
    def toInt (xp: ProbNumber) = xp.asInstanceOf [Int]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a probabilistic number from an Int.
     *  @param n  the integer used to create the probabilistic number.
     */
    def fromInt (n: Int) = ProbNumber (n, 0.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this probabilistic number to a String.
     */
    override def toString = "ProbNumber ( " + x + " , " + p + " )"

} // ProbNumber class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the ProbNumber class.
 */
object ProbNumberTest extends App
{
    import scalation.math.ProbNumber._

    val xp = ProbNumber (2.0, .5)
    val yq = ProbNumber (4.0, .5)
    println ("xp = " + xp)
    println ("yq = " + yq)
    println ("xp + yq = " + (xp + yq))
    println ("xp - yq = " + (xp - yq))
    println ("xp * yq = " + (xp * yq))

} // ProbNumberTest

