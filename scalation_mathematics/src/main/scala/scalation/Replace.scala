
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Jul 31 20:51:48 EDT 2013
 *  @see     LICENSE (MIT style license file).  
 *
 *  > runMain scalation.ReplaceAll
 */

package scalation

import java.io._

import collection.mutable.ArrayBuffer
import scala.io._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Replace` object is used to replace a repeated pattern in the source code.
 *  After running, must restore 'pat1' in this file, since it will get updated.
 */
object Replace
{
    private val DEBUG      = true                      // debug flag
    private val pat1       = "> run-main"              // pattern to find (change as needed)
    private val pat2       = "> runMain"               // replacement pattern (change as needed)
    private val SKIP       = "old"                     // do not process files in this directory
    private val sep        = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val SRC_DIR    = "."                       // source directory
    private val EXT        = ".scala"
    private val currentDir = SRC_DIR
//  private val currentDir = SRC_DIR + sep + "main" + sep + "scala"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the pattern in the given source (.scala) file.
     *  @param f   the file to update
     *  @param fn  the file's name
     */
    def update (f: File, fn: String)
    {
        if (fn.endsWith (EXT)) {
            if (DEBUG) println (s"update $fn")
            val source = Source.fromFile (f)
            val lines  = source.getLines ()
            val slines = ArrayBuffer [String] ()
            for (l <- lines) slines += l.toString
            source.close ()
            val pw = new PrintWriter (f)
            for (l <- slines) {
//              println (l.toString.replace (pat1, pat2))
                pw.println (l.toString.replace (pat1, pat2))
            } // for
            pw.close ()
        } // if
    } // update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the pattern for all source (.scala) files in current module(s).
     */
    def updateAll ()
    {
        println (s"Update pattern in source code files starting from currentDir = $currentDir")
        println (s"from $pat1 to $pat2")
        updater (new File (currentDir))
    } // updateAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively update the pattern for each source (.scala) file.
     *  @param f  the root file/directory to examine
     */
    private def updater (f: File)
    {
        val dirs = new ArrayBuffer [File] ()

        try {
            for (fi <- f.listFiles ()) {
                val fn = fi.getName ()
                if (! fi.isDirectory ()) update (fi, fn)          // file and file-name
                else if (fn != SKIP) dirs += fi
            } // for

            for (fi <- dirs if fi.isDirectory ()) updater (fi)    // recurse into each directory
        } catch { case _ : Throwable => }
    } // updater

} // Replace object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReplaceFile` object updates the pattern in a single source (.scala) file.
 *  > runMain scalation.ReplaceFile
 */
object ReplaceFile extends App
{
    val fname = "Test2.scala"
    Replace.update (new File (fname), fname)

} // ReplaceFile object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReplaceAll` object updates the pattern in all source (.scala) files
 *  at or below the 'currentDir'.
 *  > runMain scalation.ReplaceAll
 */
object ReplaceAll extends App
{
    Replace.updateAll ()

} // ReplaceAll object

