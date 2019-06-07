
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Oct 10 14:34:32 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Count class provides a convenient way to create counters (e.g., i++).
 *  @param i  the initial value of the counter
 */
case class Count (private var ii: Int = 0)
{
    def ++ = { ii += 1; ii }

} // Count class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Count class.
 */
object CountTest extends App
{
    val i = Count ()
    println (i++)
    println (i++)

} // CountTest object

