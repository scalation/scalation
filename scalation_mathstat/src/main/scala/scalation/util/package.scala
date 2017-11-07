
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Thu Jul  9 17:40:18 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import java.io.File
import java.lang.System.nanoTime
import java.net.{URL, MalformedURLException}

import scala.io.Source.{fromFile, fromURL}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `util` package contains classes, traits and objects for basic utility
 *  functions.
 */
package object util
{
    /** The number of nanoseconds per millisecond
     */
    val NS_PER_MS = 1E-6

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the elapsed time in milliseconds (ms) for the execution of an
     *  arbitrary block of code:  'time { block }'.  Return any result produced
     *  by the block of code.
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
     *  arbitrary block of code:  'timed { block }'.  Return any result produced
     *  by the block of code and its elapsed time.
     *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
     *  @param block  the block of code to be executed
     */
    def timed [R] (block: => R): Tuple2 [R, Double] = 
    {
        val t0 = nanoTime ()
        val result = block                       // call-by-name
        val t1 = nanoTime ()
        (result, (t1 - t0) * NS_PER_MS)
    } // timed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the elapsed time in milliseconds (ms) for the execution of an
     *  arbitrary block of code:  'gauge { block }'.  Return the block of code's
     *  elapsed time.
     *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
     *  @param block  the block of code to be executed
     */
    def gauge [R] (block: => R): Double = 
    {
        val t0 = nanoTime ()
        val result = block                       // call-by-name
        val t1 = nanoTime ()
        (t1 - t0) * NS_PER_MS
    } // gauge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a line iterator for a line-oriented data source (e.g., CSV file).
     *  The data source is accessed via (1) URL, (2) file's absolute path, or
     *  (3) file's relative path (relative to 'DATA-DIR').
     *  @see stackoverflow.com/questions/5713558/detect-and-extract-url-from-a-string
     *  @param path  the path name of the data source (via URL or file's path name)
     */
    def getFromURL_File (path: String): Iterator [String] =
    {
       val urlPat = "(?i)((https?|ftp|file)://|file:/).*"     // (?i) => case insensitive
       if (path matches urlPat) {
           try {
               return fromURL (new URL (path)).getLines
           } catch {
               case mue: MalformedURLException => 
           } // try    
       } // if

      val file = new File (path)
//    if (file.isAbsolute ()) fromFile (file).getLines
      if (file.exists ()) fromFile (file).getLines
      else                fromFile (DATA_DIR + path).getLines
    } // getFromURL_File

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a string for use in printing a line of '-'s.
     *  @param n  the number of '-'s to use
     */
    def sline (n: Int = 60): String = "-" * 60 + "\n"

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

} // util package object 

