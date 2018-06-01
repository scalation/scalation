
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   Michael Cotterell, John Miller
 *  @version  1.4
 *  @date     Mon May 13 12:53:29 EDT 2013
 *  @see      LICENSE (MIT style license file).
 */

package scalation.util

import scala.util.matching.Regex

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FloatLiteral` object is used to add '0' to floating point literals, which end in a
 *  dot ('.'), e.g., "12." -> "12.0".
 *  @see http://stackoverflow.com/questions/9655080/scala-operator-oddity
 *  "In scala 2.9 and before, 2. is interpreted as 2.0 so the ambiguous dot denotes a float literal.
 *  You should explicitly call the method by using the syntax (2).+(2).  The ambiguous floating
 *  point syntax has been be deprecated since scala 2.10."
 */
object FloatLiteral extends App
{
    val DEBUG = if (args.length > 1 && args(1) == "DEBUG") true else false

    val lines = getFromURL_File (args(0))

    for (line <- lines) {
        
        if (DEBUG) println ("before: " + line)

        var nline = line
        var cont  = true

        while (cont) {
            ("""[^a-zA-Z_]([0-9]+\.)[^0-9]""".r findFirstMatchIn nline) match {
                case Some (m) => nline = nline.substring (0, m.end(0) - 1) + "0" + nline.substring (m.end(0) - 1)
                case None     => cont  = false
            } // match
        } // while

        ("""[^a-zA-Z_]([0-9]+\.$)""".r findFirstMatchIn nline) match {
            case Some (m) => nline = nline.substring(0, m.end(0)) + "0"
            case None     => cont  = false
        } // match

        if (DEBUG) println (" after: " + nline) else println (nline)

    } // for

} // FloatLiteral object

