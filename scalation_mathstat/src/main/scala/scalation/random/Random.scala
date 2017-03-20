
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Mar 22 14:39:30 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  CMRG (Combined Multiple Recursive Generator) using 64-bit Long's
 */

package scalation.random

import scalation.util.time

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Random` class generates random real numbers in the range (0, 1).
 *  It implements, using 64-bit integers (Long's), the 'MRG31k3p' generator
 *  developed by L'Ecuyer and Touzin, described in "FAST COMBINED MULTIPLE
 *  RECURSIVE GENERATORS WITH MULTIPLIERS OF THE FORM a = 2^q +/- 2^r".
 *  MRG31k3p is a Combined Multiple Recursive Generator (CMRG) shown to have good
 *  performance and statistical properties for simulations.  It has a period of
 *  about 2^185 and is considered to be a faster alternative to the popular
 *  'MRG32k3' generator.  MRG31k3p combines MRG1 and MRG2.
 *  <br>
 *      MRG1: x_i = (0          + a_12 x_i-2 + a_13 x_i-3) % M1
 *      MRG2: x_i = (a_21 x_i-1 + 0          + a_23 x_i-3) % M2
 *  <br>
 *  where a_12 = 2^22, a_13 = 2^7+1, a_21 = 2^15 and a_23 = 2^15+1.
 *  @see http://www.informs-sim.org/wsc00papers/090.PDF
 *  @see http://www.iro.umontreal.ca/~simardr/ssj/doc/pdf/guiderng.pdf
 *  @param stream  the random number stream index
 */
case class Random (stream: Int = 0)
     extends RNG (stream)
{
    private val M1     = 2147483647l                   // modulus for MRG1 (2^31 - 1)
    private val M2     = 2147462579l                   // modulus for MRG2 (2^31 - 21069)
    private val MASK12 = 511l                          // mask to extract  9 lsb's (2^9 - 1)
    private val MASK13 = 16777215l                     // mask to extract 24 lsb's (s^24 - 1)
    private val MASK21 = 65535l                        // mask to extract 16 lsb's (s^16 - 1)
    private val MULT2  = 21069l                        // multiplier
    private val NORM   = 4.656612873077393e-10         // 1.0 / (M1 + 1.0) normalization to (0, 1)
    private val x      = RandomSeeds.seeds (stream)    // 6-dimensional vector of seed values

    private var x11    = x(0).toLong                   // x_i-1 for MRG1
    private var x12    = x(1).toLong                   // x_i-2 for MRG1
    private var x13    = x(2).toLong                   // x_i-3 for MRG1
    private var x21    = x(3).toLong                   // x_i-1 for MRG2
    private var x22    = x(4).toLong                   // x_i-2 for MRG2
    private var x23    = x(5).toLong                   // x_i-3 for MRG2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next random number as a real (Double) in the interval (0, 1).
     *  This calculation uses 64-bit integers (Long).
     */
    def gen: Double =
    {
        // Calculate MRG1 (first component)

        var y1 = (((x12 & MASK12) << 22) + (x12 >> 9)) + (((x13 & MASK13) << 7) + (x13 >> 24))
        if (y1 > M1) y1 -= M1
        y1 += x13
        if (y1 > M1) y1 -= M1
        x13 = x12; x12 = x11; x11 = y1

        // Calculate MRG2 (second component)

        y1 = ((x21 & MASK21) << 15) + MULT2 * (x21 >> 16)
        if (y1 > M2) y1 -= M2
        var y2 = ((x23 & MASK21) << 15) + MULT2 * (x23 >> 16)
        if (y2 > M2) y2 -= M2
        y2 += x23
        if (y2 > M2) y2 -= M2
        y2 += y1
        if (y2 > M2) y2 -= M2
        x23 = x22; x22 = x21; x21 = y2

        // Combine MRG1 and MRG2

        if (x11 <= x21) (x11 - x21 + M1) * NORM else (x11 - x21) * NORM
    } // gen

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next stream value as an integer 'Int'.
     *  This calculation uses 64-bit integers 'Long'.
     */
    def igen: Int =
    {
        // Calculate MRG1 (first component)

        var y1 = (((x12 & MASK12) << 22) + (x12 >> 9)) + (((x13 & MASK13) << 7) + (x13 >> 24))
        if (y1 > M1) y1 -= M1
        y1 += x13
        if (y1 > M1) y1 -= M1
        x13 = x12; x12 = x11; x11 = y1

        // Calculate MRG2 (second component)

        y1 = ((x21 & MASK21) << 15) + MULT2 * (x21 >> 16)
        if (y1 > M2) y1 -= M2
        var y2 = ((x23 & MASK21) << 15) + MULT2 * (x23 >> 16)
        if (y2 > M2) y2 -= M2
        y2 += x23
        if (y2 > M2) y2 -= M2
        y2 += y1
        if (y2 > M2) y2 -= M2
        x23 = x22; x22 = x21; x21 = y2

        // Combine MRG1 and MRG2

        (if (x11 <= x21) x11 - x21 + M1 else x11 - x21).toInt
    } // igen

} // Random class

