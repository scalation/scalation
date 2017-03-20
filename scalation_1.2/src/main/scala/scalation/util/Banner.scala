
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Aug 15 16:10:53 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `banner` object provides a convenient method for printing banners.
 */
object Banner
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print a banner, i.e., a string in a box.
     *  @param str  the string to put in the banner
     */
    def banner (str: String)
    {
        val len = str.size + 4
        println ("-" * len)
        println ("| " + str + " |")
        println ("-" * len)
    } // banner

} // Banner object

