
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Sep 16 16:12:08 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scala.collection.mutable.Set
import scala.math.round

import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateSet` abstract class serves as a base class for all the random
 *  variate set (RVS) generators. They use one of the Random Number Generators
 *  (RNG's) from Random.scala to generate numbers following their particular
 *  multivariate distribution.
 *-----------------------------------------------------------------------------
 *  @param stream  the random number stream
 */
abstract class VariateSet (stream: Int = 0)
         extends Error
{
    /** Random number stream selected by the stream number
     */
    protected val r = Random (stream)

    /** Indicates whether the distribution is discrete (default) or continuous
     */
    protected var _discrete = true

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the distribution is discrete or continuous.
     */
    def discrete: Boolean = _discrete

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean for the particular distribution.
     */
    def mean: Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the probability function (pf):
     *  The probability density function (pdf) for continuous RVV's or
     *  the probability mass function (pmf) for discrete RVV's.
     *  @param z  the mass point/set whose probability is sought
     */
    def pf (s: Set [Int]): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random double set for the particular distribution.
     */
    def gen: Set [Double]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the next random integer set for the particular distribution.
     *  It is only valid for discrete random variates.
     */
    def igen: Set [Int]

} // VariateSet class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomSet` class generates a random set/subset of integers.
 *  @param count   the size of the set (number of integer elements)
 *  @param max     generate integers in the range 0 (inclusive) to max (inclusive)
 *  @param skip    skip this number, i.e, do not use it
 *  @param stream  the random number stream
 */
case class RandomSet (count: Int = 10, max: Int = 20, skip: Int = -1, stream: Int = 0)
     extends VariateSet (stream)
{
    if (max < count) flaw ("constructor", "requires max >= count")

    private val rng = Randi0 (max, stream)              // random integer generator

    def mean: Double =  max / 2.0 

    def pf (s: Set [Int]): Double = throw new NoSuchMethodException ("'pf' not implemented")

    def gen: Set [Double] = throw new NoSuchMethodException ("'gen' not implemented, use 'igen' instead")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random set of unique integers 'r' in the range '0 to max'.
     *  The size of the set is given by 'count'.
     */
    def igen: Set [Int] =
    {
        val r = Set [Int] ()
        var e = 0
        for (i <- 0 until count) {
            do e = rng.igen while (e == skip || i > 0 && (r contains e))
            r += e
        } // for
        r
    } // igen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random set of unique integers 'r' in the range '0 to mx'.
     *  @param n    the size of the resultant random subset r
     *  @param mx   generate integers in the range 0 (inclusive) to 'mx' (inclusive)
     *  @param skp  skip this number, i.e, do not use it
     */
    def igen (n: Int, mx: Int, skp: Int = -1): Set [Int] =
    {
        val r = Set [Int] ()
        var e = 0
        for (i <- 0 until n) {
            do e = rng.iigen (mx) while (e == skp || i > 0 && (r contains e))
            r += e
        } // for
        r
    } // igen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a set of unique integers 'r' that is a random subset of set 's'.
     *  @param s  the given set
     *  @param n  the size of the resultant random subset r
     */
    def igen (s: Set [Int], n: Int): Set [Int] =
    {
        val sa = s.toArray
        val mx = sa.length - 1
        val r  = Set [Int] ()
        var e  = 0
        for (i <- 0 until n) {
            do e = sa(rng.iigen (mx)) while (i > 0 && (r contains e))
            r += e
        } // for
        r
    } // igen

} // RandomSet class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VariateSetTest` object is used to test the Random Variate Set (RVS)
 *  generators from the classes derived from `VariateSet`.
 *  > run-main scalation.random.VariateSetTest
 */
object VariateSetTest extends App
{
     val rsg = RandomSet (10)                          // variate set generator
     var rs:  Set [Int]  = null                        // variate set

     println ("Test: RandomSet random set generation ------------------------")
     println ("mean = " + rsg.mean)                     // random set generator
     for (k <- 0 until 30) { rs = rsg.igen;  println (rs) }

     println ("Test: RandomSet random subset generation ---------------------")
     println ("mean = " + rsg.mean)                     // random set generator
     for (k <- 0 until 30) println (rsg.igen (rs, 5))

} // VariateSetTest object

