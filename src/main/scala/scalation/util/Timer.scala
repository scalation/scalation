
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Thu Sep 19 15:32:04 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import java.lang.System.nanoTime

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Timer` object allow any component that imports it in to easily time
 *  blocks of code.
 */
object Timer
{
    val ns_per_ms = 1E-6                // nanoseconds per millisecond

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the elapsed time in milliseconds (ms) for the execution of an
     *  arbitrary block of code:  'time { block }'.
     *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
     *  @param block  the block of code to be executed
     */
    def time [R] (block: => R): R =
    {
        val t0 = nanoTime ()
        val result = block         // call-by-name
        val t1 = nanoTime ()
        println ("Elapsed time: " + (t1 - t0) * ns_per_ms + " ms")
        result
    } // time
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the elapsed time in milliseconds (ms) for the execution of an
     *  arbitrary block of code:  'time { block }'.
     *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
     *  @param block  the block of code to be executed
     */
    def timer [R] (block: => R): (R, Double) = 
    {
        val t0 = nanoTime ()
        val result = block                       // call-by-name
        val t1 = nanoTime ()
        (result, (t1 - t0) * ns_per_ms)
    } // time

} // Timer object

