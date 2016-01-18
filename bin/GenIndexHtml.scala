
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Srikalyan Swayampakula, Michael E. Cotterell
 *  @version 1.1
 *  @date    Wed Aug  27 14:16:12 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

import java.io.{BufferedWriter, File, FileWriter}

import collection.mutable.ArrayBuffer

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GenIndexHtml' object is used to create "index.html" files in source code
 *  directories to enable Web browsing of source code.
 */
object GenIndexHtml extends App
{
    private val SKIP       = "old"                     // do not process files in this directory
    private val sep        = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val BASE_DIR   = ".."
    private val SRC_DIR    = BASE_DIR + sep + "src"
    private val currentDir = SRC_DIR + sep + "main" + sep + "scala"

    println ("Generate index.html files starting from currentDir = " + currentDir)
    recCreate (new File (currentDir))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively create index.html files for each directory.
     *  @param f  the file/directory to examine
     */
    def recCreate (f: File)
    {
        recDeleteIndex (f)
        val dirs = new ArrayBuffer [File] ()

        try {
            val iFile = new File (f.getAbsolutePath () + "/index.html")      // the index.html file to write
            val fos   = new BufferedWriter (new FileWriter (iFile))
            fos.write ("<html>\n<body>\n<h1> Source files in " + f.getName () + " Package </h1><p>\n<ul>\n")

            for (fi <- f.listFiles () sortWith ( (f1, f2) => f1.getName.toLowerCase < f2.getName.toLowerCase ) ) {
                val fName = fi.getName ()
                if (! fi.isDirectory () && fName != "index.html") {
                    fos.write ("<li> <a href = './" + fName + "'> " + fName + " </a> </li>\n")
                } else if (fName != SKIP && fName != "index.html") {
                    dirs += fi
                } // if
            } // for

            for (fi <- dirs) {
                val fName = fi.getName ()
                if (fName != SKIP) {
                    fos.write ("<li> <a href = './" + fName + "'> " + fName + " </a> </li>\n")
                } // if
            } // for

            fos.write ("</ul>\n</body>\n<html>")
            fos.close ()

            for (fi <- dirs if fi.isDirectory ()) recCreate (fi)     // recurse into each directory
        } catch { case _ : Throwable => }
    } // recCreate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively delete index.html files for each directory (clean up step).
     *  @param f  the file/directory to examine
     */
    def recDeleteIndex (f: File)
    {
        if (! f.isDirectory ()) {
            if (f.getName () == "index.html") f.delete ()
        } else {
            val files = f.listFiles ()
            if (files != null) {
                for (fi <- files) try recDeleteIndex (fi) catch { case _ : Throwable => }
            } // if
        } // if
    } // recDeleteIndex

} // GenIndexHtml object

