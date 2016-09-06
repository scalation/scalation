
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Aug  7 20:40:29 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.random

import scalation.math.ExtremeD.NaN
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomStr` class generates a random string.
 *  @param lRange  the range of string lengths to generate
 *  @param cRange  the range of characters to generate
 *  @param stream  the random number stream
 */
case class RandomStr (lRange: Range = 4 to 6, cRange: Range = 97 to 122, stream: Int = 0)
     extends Variate (stream)
{
    private val lrng = Randi (lRange.start, lRange.end, stream)       // random integer generator
    private val crng = Randi (cRange.start, cRange.end, stream)       // random integer generator

    val mean = NaN

    def pf (s: Double): Double = throw new NoSuchMethodException ("'pf' not implemented")

    def gen: Double = throw new NoSuchMethodException ("'gen' not implemented, use 'sgen' instead")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random string.
     */
    override def sgen: String =
    {
        val sb = new StringBuilder ()
        for (i <- 0 until lrng.igen) sb.append (crng.igen.toChar)
        sb.toString
    } // igen

} // RandomStr class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomStr` class generates a random word from a predetermined set.
 *  @param nWords  the numbers of words to predetermine.
 *  @param lRange  the range of string lengths to generate
 *  @param cRange  the range of characters to generate
 *  @param stream  the random number stream
 */
case class RandomWord (nWords: Int = 10, lRange: Range = 4 to 6, cRange: Range = 97 to 122, stream: Int = 0)
     extends Variate (stream)
{
    private val rig   = Randi0 (nWords-1, stream)                       // random integer generator
    private val rsg   = RandomStr (lRange, cRange, stream)              // random string generator
    private val words = Array.ofDim [String] (nWords)

    for (i <- 0 until nWords) { var w: String = null; do { w = rsg.sgen } while (words contains w); words(i) = w }

    val mean = NaN

    def pf (s: Double): Double = throw new NoSuchMethodException ("'pf' not implemented")

    def gen: Double = throw new NoSuchMethodException ("'gen' not implemented, use 'sgen' instead")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random string.
     */
    override def sgen: String = words (rig.igen)

} // RandomWord class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomStrTest` object is used to test the Random Variate String (RVS)
 *  generator from the `RandomStr` class.
 *  > run-main scalation.random.RandomStrTest
 */
object RandomStrTest extends App
{
     val rsg = RandomStr ()                            // variate string generator
     var rs: String = null                             // variate string

     println ("Test: RandomStr random string generation ------------------------")
     for (k <- 0 until 30) { rs = rsg.sgen;  println (rs) }

} // RandomStrTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomWordTest` object is used to test the Random Variate Word (RVW)
 *  generator from the `RandomWord` class.
 *  > run-main scalation.random.RandomWordTest
 */
object RandomWordTest extends App
{
     val rsg = RandomWord ()                           // variate word generator
     var rs: String = null                             // variate word

     println ("Test: RandomWord random string generation ------------------------")
     for (k <- 0 until 30) { rs = rsg.sgen;  println (rs) }

} // RandomWordTest object

