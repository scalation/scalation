
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Nov 14 2:34:38 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.scala-lang.org/node/724
 */

package scalation.math

import scala.language.implicitConversions

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LongWithExp` class defines an expontiation operator '~^' for Longs.
 *  To maintain 64 bit precision, no floating point operations are used.
 *  @param m  the long base
 */
case class LongWithExp (m: Long)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exponentiation operator for scala Longs (m ~^ n).  Compute:
     *  "math.pow (m, n).toLong" without using floating point, so as to not lose
     *  precision.
     *  @param n  the long exponent
     */
    def ~^ (n: Long) =
    {
        var base   = m
        var exp    = n
        var result = 1l
        while (exp != 0l) {
            if ((exp & 1l) == 1l) result *= base
            exp >>= 1l
            base *= base
        } // while
        result
    } // ~^ 

    def pow (n: Long): Long = LongWithExp (m) ~^ n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the n-th root of m, i.e.,  m ~^ (1/n) for scala Longs.
     *  @see http://en.wikipedia.org/wiki/Shifting_nth_root_algorithm
     *  @param n  the root level
     */
    def root (n: Long): Long =
    {
        1l   // FIX: use the shifting nth root algorithm
    } // root

} // LongWithExp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LongWithExp` obejct provides implicit conversion from Long to LongWithExp
 *  allowing '~^' to be applied to Longs.
 */
object LongWithExp
{
    implicit def longWithExp (n: Long) = LongWithExp (n)

} // LongWithExp object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LongWithExpTest` object is used to test the `LongWithExp` class.
 */
object LongWithExpTest extends App
{
    import LongWithExp._
    println (2l ~^ 3l)
    println (8l root 3l)

} // LongWithExpTest object

