
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sat Apr 30 12:53:23 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

import java.io.File

import scala.util.Properties.envOrElse

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `scalation` package specifies system-wide constants for directory paths.
 *  Sub-packages may wish to define 'BASE-DIR = DATA_DIR + ⁄ + <package>' in their
 *  own 'package.scala' files.  For maintainability, directory paths should only
 *  be specified in 'package.scala' files.
 */
package object scalation
{
    /** The file path separation character: '/' for Linux/Mac, '\' for Windows
     *  Use either 'SEP' or '⁄' for portability, i.e., do not use '/' or '\'.
     */
    val SEP = File.separator
    val ⁄   = File.separator                                    // Unicode symbol

    /** Base directory for ScalaTion (pick one, comment out the rest)
     *  Under module differentiated, each module (scalation_mathstat, scalation_modeling and scalation_models)
     *  will have its own base directory, rather than sharing a common base directory.
     */
    val BASE = removeLast (System.getProperty ("user.dir"))     // absolute path
//  val BASE = ".."                                             // relative path
//  val BASE = System.getProperty ("user.dir")                  // module differentiated absolute path
//  val BASE = "."                                              // module differentiated relative path

    /** File system path for input/output data directory
     *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
     */
    val DATA_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "data" + ⁄

    /** File system path for log (log/file output) directory
     *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
     */
    val LOG_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "log" + ⁄

    /** File system path for memory mapped files directory
     *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
     */
    val MEM_MAPPED_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "mem_mapped" + ⁄

    /** File system path for src (source code) directory
     *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
     */
    val SRC_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "src" + ⁄
    val SRC_SCALA_DIR = SRC_DIR + ⁄ + "main" + ⁄ + "scala" + ⁄

    /** File system path for database storage directory
     *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
     */
    val STORE_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "store" + ⁄

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the last path element from the file/directory pathname.
     *  @param s  the string to be so truncated
     */
    def removeLast (s: String): String = s.substring (0, s.lastIndexOf (SEP))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the default parallelism level (number of threads to use) for an
     *  arbitrary block of code: 'parallel (n) { block }' when running parallel tasks.
     *  Then restore the old parallelism level.
     *  @see stackoverflow.com/questions/17865823/how-do-i-set-the-default-number-of-threads
        @see -for-scala-2-10-parallel-collections/18574345
     *  @param n  number of threads
     */
    def setParallelism [A] (n: Int)(block: => A): A =
    {
        import java.util.concurrent.ForkJoinPool
        import scala.collection._

        val n_old = (new ForkJoinPool).getParallelism
        println (s"Channge parallelism level from $n_old to $n")

        val parPkgObj = parallel.`package`
        val defaultTaskSupport = parPkgObj.getClass.getDeclaredFields.find{ _.getName == "defaultTaskSupport" }.get

        defaultTaskSupport.setAccessible (true)
        defaultTaskSupport.set (parPkgObj, new parallel.ForkJoinTaskSupport (new ForkJoinPool (n)))
        val ret = block
        defaultTaskSupport.set (parPkgObj, new parallel.ForkJoinTaskSupport (new ForkJoinPool (n_old)))
        ret
    } // setParallelism

} // scalation package object 


package scalation
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `scalationTest` object is used test the `scalation` package object.
     *  > runMain scalation.scalationTest
     */
    object scalationTest extends App
    {
        println ("DATA_DIR       = " + DATA_DIR)
        println ("LOG_DIR        = " + LOG_DIR)
        println ("MEM_MAPPED_DIR = " + MEM_MAPPED_DIR)
        println ("SRC_DIR        = " + SRC_DIR)
        println ("STORE_DIR      = " + STORE_DIR)

    } // scalationTest

} // scalation package

