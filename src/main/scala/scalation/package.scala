
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
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
    val ⁄   = File.separator                                  // Unicode symbol

    /** File system path for input/output data directory
     *  Use 'SCALATION_HOME' environment variable or else current directory "."
     */
    val DATA_DIR = envOrElse ("SCALATION_HOME", ".") + ⁄ + "data" + ⁄

    /** File system path for log (log/file output) directory
     *  Use 'SCALATION_HOME' environment variable or else current directory "."
     */
    val LOG_DIR = envOrElse ("SCALATION_HOME", ".") + ⁄ + "log" + ⁄

    /** File system path for memory mapped files directory
     *  Use 'SCALATION_HOME' environment variable or else current directory "."
     */
    val MEM_MAPPED_DIR = envOrElse ("SCALATION_HOME", ".") + ⁄ + "mem_mapped" + ⁄

    /** File system path for src (source code) directory
     *  Use 'SCALATION_HOME' environment variable or else current directory "."
     */
    val SRC_DIR = envOrElse ("SCALATION_HOME", ".") + ⁄ + "src" + ⁄

    /** File system path for database storage directory
     *  Use 'SCALATION_HOME' environment variable or else current directory "."
     */
    val STORE_DIR = envOrElse ("SCALATION_HOME", ".") + ⁄ + "store" + ⁄

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
     *  > run-main scalation.scalationTest
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

