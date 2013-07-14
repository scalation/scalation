
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Srikalyan Swayampakula, John Miller
 *  @version 1.0
 *  @date    Tue Feb 23 12:01:36 EST 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import java.io.{BufferedWriter, File, FileWriter}

import collection.mutable.ArrayBuffer

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to create "index.html" files in source code directories
 *  to enable Web browsing of source code.
 */
object GenIndexHtml extends App
{
    val home = System.getenv ("SCALATION_HOME")
    val currentDir = (if (home == null) "." else home) + "/src/scalation"
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
            val fos = new BufferedWriter (new FileWriter (new File (f.getAbsolutePath () + "/index.html")))
            fos.write ("<html>\n<body>\n<h1> Source files in " + f.getName () + " Package </h1><p>\n<ul>\n")

            for (fi <- f.listFiles ()) {
                if (! fi.isDirectory ()) {
                    fos.write ("<li> <a href = './" + fi.getName () + "'> " + fi.getName () + " </a> </li>\n")
                } else {
                    dirs += fi
                } // if
            } // for

            for (fi <- dirs) fos.write ("<li> <a href = './" + fi.getName () + "'> " + fi.getName () + " </a> </li>\n")
            fos.write ("</ul>\n</body>\n<html>")
            fos.close ()
            for (fi <- dirs if fi.isDirectory ()) recCreate (fi)
        } catch { case _ => }
    } // recCreate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively delete index.html files for each directory (clean up step).
     *  @param f  the file/directory to examine
     */
    def recDeleteIndex (f: File)
    {
        if ( ! f.isDirectory ()) {
            if (f.getName () == "index.html") f.delete ()
        } else {
            val files = f.listFiles ()
            if (files != null) {
                for (fi <- files) try recDeleteIndex (fi) catch { case _ => }
            } // if
        } // if
    } // recDeleteIndex

} // GenIndexHtml object

