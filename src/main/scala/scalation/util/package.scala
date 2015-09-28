
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Jul  9 17:40:18 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import java.io.File
import java.lang.System.nanoTime

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `util` package contains classes, traits and objects for basic utility
 *  functions.
 */
package object util
{
    /** The file/path separation character: '/' for Linux/Mac, '\' for Windows
     */
    val SEP = File.separator

    /** The number of nanoseconds per millisecond
     */
    val NS_PER_MS = 1E-6

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the elapsed time in milliseconds (ms) for the execution of an
     *  arbitrary block of code:  'time { block }'.
     *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
     *  @param block  the block of code to be executed
     */
    def time [R] (block: => R): R =
    {
        val t0 = nanoTime ()
        val result = block                       // call-by-name
        val t1 = nanoTime ()
        println ("Elapsed time: " + (t1 - t0) * NS_PER_MS + " ms")
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
        (result, (t1 - t0) * NS_PER_MS)
    } // time

} // util package object 

