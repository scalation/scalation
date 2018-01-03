
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Wed Dec 23 17:00:46 EST 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import scala.io.StdIn.readLine

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReplaceOne` object is used to replace pattern pat1 with pat2.  It reads from
 *  standard input and writes to standard output.
 */
object ReplaceOne extends App
{
//  private val pat1 = " ::"            // pattern to find (change as needed)
//  private val pat2 = " *"             // replacement pattern (change as needed)
    private val pat1 = "> runMain"     // pattern to find (change as needed)
    private val pat2 = "> runMain"      // replacement pattern (change as needed)
    private var line = ""

    do {
        line = readLine ()
        if (line != null) println (line.replace (pat1, pat2))
    } while (line != null)

} // ReplaceOne object

