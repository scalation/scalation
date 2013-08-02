
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Jul 31 20:51:48 EDT 2013
 *  @see     LICENSE (MIT style license file).  
 */

import java.io._
import collection.mutable.ArrayBuffer
import scala.io._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'UpVersion' object is used to update the version number in the source code.
 */
object UpVersion extends App
{
    private val pat1       = "@version 1.0"            // pattern to find (change as needed)
    private val pat2       = "@version 1.1"            // replacement pattern (change as needed
    private val SKIP       = "old"                     // do not process files in this directory
    private val sep        = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val BASE_DIR   = ".."
    private val SRC_DIR    = BASE_DIR + sep + "src"
    private val currentDir = SRC_DIR + sep + "scalation"

    println ("Update version number in source code files starting from currentDir = " + currentDir)
    update (new File (currentDir))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively update the version number for each source (.scala) file.
     *  @param f  the file/directory to examine
     */
    def update (f: File)
    {
        val dirs = new ArrayBuffer [File] ()

        try {
            for (fi <- f.listFiles ()) {
                val fName = fi.getName ()
                if (! fi.isDirectory ()) {
                    if (fName.endsWith (".scala")) {
                        val source = Source.fromFile (fi)
                        val lines  = source.getLines
                        source.close ()
                        val pw = new PrintWriter (fi)
                        for (l <- lines) pw.println (l.replace (pat1, pat2))
                        pw.close ()
                    } // if
                } else if (fName != SKIP) {
                    dirs += fi
                } // if
            } // for

            for (fi <- dirs if fi.isDirectory ()) update (fi)       // recurse into each directory
        } catch { case _ => }
    } // update

} // UpVersion object

