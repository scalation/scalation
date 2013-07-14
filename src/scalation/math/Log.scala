
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Sep 23 21:14:14 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import math.log                             // natural log

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object provides additional methods for computing logarithms.
 */
object Log
{
    val log_of_2  = log (2.0)                // natural log of 2
    val log_of_10 = log (10.0)               // natural log of 10

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

} // Log object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Log object.
 */
object LogTest extends App
{
    import Log._

    println ("log2 (2)    = " + log2 (2.0))
    println ("log2 (4)    = " + log2 (4.0))
    println ("log10 (10)  = " + log10 (10.0))
    println ("log10 (100) = " + log10 (100.0))
    println ("logb (4, 4)  = " + logb (4.0, 4.0))
    println ("logb (4, 16) = " + logb (4.0, 16.0))

} // LogTest object

