
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Aug 17 18:55:00 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import util.control.Breaks.{breakable, break}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object provides an example of how to use "breaks" in Scala.
 *  @see http://daily-scala.blogspot.com/2010/04/breaks.html
 *  @see http://www.scala-lang.org/archives/downloads/distrib/files/nightly/docs/library/scala/util/control/Breaks.html
 */
object TestBreak extends App
{
    breakable { for (i <- 1 to 10) {
        if (i == 5) break
        println ("i = " + i)
    }} // for

} // TestBreak

