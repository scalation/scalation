
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Jul 31 20:51:48 EDT 2013
 *  @see     LICENSE (MIT style license file).  
 */

package apps

import java.io._

import collection.mutable.ArrayBuffer
import scala.io._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UpVersion` object is used to update the version number in the source code.
 */
object UpVersion
{
    private val DEBUG      = true                      // debug flag
    private val pat1       = "@version 1.3"            // pattern to find (change as needed)
    private val pat2       = "@version 1.3"            // replacement pattern (change as needed)
    private val SKIP       = "old"                     // do not process files in this directory
    private val sep        = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val SRC_DIR    = "."                       // source directory
    private val EXT        = ".scala"
    private val currentDir = SRC_DIR
//  private val currentDir = SRC_DIR + sep + "main" + sep + "scala"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the version number for the given source (.scala) file.
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
    /** Update the version number for all source (.scala) files.
     */
    def updateAll ()
    {
        println ("Update version number in source code files starting from currentDir = " + currentDir)
        updater (new File (currentDir))
    } // updateAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively update the version number for each source (.scala) file.
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

} // UpVersion object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UpVersionFile` object updates the version number of a single source
 *  (.scala) file.
 *  > run-main apps.UpVersionFile
 */
object UpVersionFile extends App
{
    val fn = "Test2.scala"
    UpVersion.update (new File (fn), fn)

} // UpVersionFile


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `UpVersionAll` object updates the version number of all source (.scala)
 *  files at or below the 'currentDir' (see `UpVersion`).
 *  > run-main apps.UpVersionAll
 */
object UpVersionAll extends App
{
    UpVersion.updateAll ()

} // UpVersionAll

