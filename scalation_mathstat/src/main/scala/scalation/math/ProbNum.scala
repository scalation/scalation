
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sat Sep 24 20:46:45 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

// U N D E R   D E V E L O P M E N T

import scalation.util.Error

import ExtremeD.approx

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ProbNum` class is used to represent probabilistic numbers '(x, p)'
 *  where 'x' is a real number and 'p' is its probability of occurrence.
 *  FIX:  Currently this class is half-baked!!!
 *  @see http://okmij.org/ftp/Computation/monads.html#random-var-monad
 *  @param x  the real number (double precision)
 *  @param p  the probability of its occurrence [0, 1]
 */
case class ProbNum (x: Double, p: Double = 1.0)
     extends Numeric [ProbNum] with Ordered [ProbNum] with Error
{
    if (p < 0.0 || p > 1.0) flaw ("constructor", "p is not a probability " + p)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the unary minus (-).
     */
    def unary_- () = ProbNum (-x, p)

    def negate (xp: ProbNum) = -xp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add two probabilistic numbers.
     *  @param xp  add 'xp' to this
     */
    def + (xp: ProbNum) = ProbNum (x * p + xp.x * xp.p, p + xp.p)

    def plus (xp: ProbNum, yq: ProbNum) = xp + yq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract two probabilistic numbers.
     *  @param xp  subtract 'xp' from this
     */
    def - (xp: ProbNum) = ProbNum (x * p - xp.x * p, p + xp.p)

    def minus (xp: ProbNum, yq: ProbNum) = xp - yq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply two probabilistic numbers.
     *  @param xp  multiply this times 'xp'
     */
    def * (xp: ProbNum) = ProbNum (x * p * xp.x * xp.p, p + xp.p)

    def times (xp: ProbNum, yq: ProbNum) = xp * yq

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the probabilistic number is certain (probability = 1).
     */
    def isCertain = approx (p, 1.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare two probabilistic numbers (negative for <, zero for ==, positive for >).
     *  @param xp  the first probabilistic number to compare
     *  @param yq  the second probabilistic number to compare
     */
    def compare (xp: ProbNum, yq: ProbNum) =  xp.x compare yq.x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this probabilistic number with that probabilistic number 'yq'.
     *  @param yq  that probabilistic number
     */	
    def compare (yq: ProbNum) = x compare yq.x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to a `Double`.
     *  @param xp  the probabilistic number to convert
     */
    def toDouble (xp: ProbNum) = xp.x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to a `Float`.
     *  @param xp  the probabilistic number to convert
     */
    def toFloat (xp: ProbNum) = xp.x.asInstanceOf [Float]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to a `Long`.
     *  @param xpc  the probabilistic number to convert
     */
    def toLong (xp: ProbNum) = xp.asInstanceOf [Long]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the probabilistic number to an `Int`.
     *  @param xp  the probabilistic number to convert
     */
    def toInt (xp: ProbNum) = xp.asInstanceOf [Int]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a probabilistic number from an `Int`.
     *  @param n  the integer used to create the probabilistic number.
     */
    def fromInt (n: Int) = ProbNum (n, 0.0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this probabilistic number to a `String`.
     */
    override def toString = "ProbNum ( " + x + " , " + p + " )"

} // ProbNum class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ProbNumTest` object is used to test the `ProbNum` class.
 *  > runMain scalation.math.ProbNumTest
 */
object ProbNumTest extends App
{
    import scalation.math.ProbNum._

    val xp = ProbNum (2.0, .5)
    val yq = ProbNum (4.0, .5)
    println ("xp = " + xp)
    println ("yq = " + yq)
    println ("xp + yq = " + (xp + yq))
    println ("xp - yq = " + (xp - yq))
    println ("xp * yq = " + (xp * yq))

} // ProbNumTest

