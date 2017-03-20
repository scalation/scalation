
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Dec 23 17:00:46 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import scala.io.StdIn.readLine

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Replace` object is used to replace pattern pat1 with pat2.  It reads from
 *  standard input and writes to standard output.
 */
object Replace extends App
{
    private val pat1 = " ::"     // pattern to find (change as needed)
    private val pat2 = " *"      // replacement pattern (change as needed)
    private var line = ""

    do {
        line = readLine ()
        if (line != null) println (line.replace (pat1, pat2))
    } while (line != null)

} // Replace object

