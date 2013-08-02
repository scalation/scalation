
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael E. Cotterell
 *  @version 1.0
 *  @date    Sun May  5 13:49:22 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

import java.net.URL
import java.io._

import collection.mutable.{ArrayBuffer, ListBuffer}
import tools.nsc.{Global, ObjectRunner, Settings}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to run scala programs.
 *  FIX: finish implementation
 */
object Run extends App
{
    private val sep       = File.separator            // file separator ('/' for UNIX, '\' for Windows)
    private val BASE_DIR  = ".."
    private val CLASS_DIR = BASE_DIR + sep + "classes"
    private val APPS_DIR  = BASE_DIR + sep + "apps"

    try {
        val file = new File (CLASS_DIR)
        val url  = file.toURI.toURL
        ObjectRunner.run (List (url), "scalation.graphalytics.ColorDAGTest", Seq (args(0)))
    } catch {
      case e => println (e)
    } // try

} // Run object

