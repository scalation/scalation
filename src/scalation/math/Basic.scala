
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Feb  2 20:21:38 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import math.log                             // natural log

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object provides additional methods for computing logarithms and a
 *  method for transforming Booleans into Ints.
 */
object Basic
{
   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 1 if the condition is true else 0.
     *  @param cond  the condition to evaluate
     */
    def oneIf (cond: Boolean): Int = if (cond) 1 else 0

    /** The natural log of 2
     */
    val log_of_2  = log (2.)

    /** The natural log of 10
     */
    val log_of_10 = log (10.)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of x base 2
     *  @param x  the value whose log is sought
     */
    def log2 (x: Double): Double = log (x) / log_of_2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of x base 2
     *  @param x  the value whose log is sought
     */
    def log10 (x: Double): Double = log (x) / log_of_10

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of x base b
     *  @param b  the base of the logarithm
     *  @param x  the value whose log is sought
     */
    def logb (b: Double, x: Double): Double = log (x) / log (b)

} // Basic object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Basic object.
 */
object BasicTest extends App
{
    import Basic._

    println ("oneIf (2 > 1) = " + oneIf (2 > 1))
    println ("log2 (2)      = " + log2 (2.))
    println ("log2 (4)      = " + log2 (4.))
    println ("log10 (10)    = " + log10 (10.))
    println ("log10 (100)   = " + log10 (100.))
    println ("logb (4, 4)   = " + logb (4., 4.))
    println ("logb (4, 16)  = " + logb (4., 16.))

} // BasicTest object

