
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Sep 30 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *  
 *  LCG (Linear Congruential Generator)
 */

package scalation.random

import scalation.util.time

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Random3` class generates random real numbers in the range (0, 1).
 *  It implements, using 64-bit integers (Int's), the 'MINSTD' generator, which
 *  is a multiplicative Linear Congruential Generator (LCG).
 *  These generators were commonly used in the last century.
 *  <br>
 *      x_i = a x_i-1 % m
 *  <br>
 *  @see http://random.mat.sbg.ac.at/results/karl/server/node4.html#SECTION00042000000000000000
 *  In case a better generator is needed, a Multiple Recursive Generator (MRG)
 *  or Composite Multiple Recursive Generator (CMRG) should be used.
 *  @see `Random`
 *  @see http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.1024
 *  @param stream  the random number stream index
 */
case class Random3 (stream: Int = 0)
     extends RNG (stream)
{
//  private val A     = 48271l                 // alternative multiplier for a popular 32-bit generator
    private val A     = 16807l                 // multiplier for a popular 32-bit generator (7^5)
    private val M     = 2147483647l            // modulus for a popular 32-bit generator (2^31 - 1)
    private val NORM  = 1.0 / M.toDouble       // normalization to (0, 1)

    private var x     = RandomSeeds3.seeds(stream).toLong   // set the stream value to its seed

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the modulus used by this random number generator.
     */
    def getM: Double = M

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next random number as a `Double` in the interval (0, 1).
     *  Compute x_i = a x_i-1 % m using x = a * x % m
     */
    def gen: Double =
    {
        x = A * x % M
        x * NORM
    } // gen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next stream value as a `Int` in the set {1, 2, ... , m-1}.
     *  Compute x_i = a x_i-1 % m using x = a * x % m
     */
    def igen: Int =
    {
        x = A * x % M
        x.toInt
    } // igen

} // Random3 class

