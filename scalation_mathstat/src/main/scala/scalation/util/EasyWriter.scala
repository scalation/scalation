
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman, John Miller
 *  @version 1.5
 *  @date    Wed Jun  8 13:16:15 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package util

import java.io.{PrintWriter, Writer}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EasyWriter` class makes it easy to switch between writing to standard output
 *  and a (log) file.
 *  @param project   the project or directory involved
 *  @param filename  the name of the file to be written
 *  @param toFile    flag indicating whether to write to a file
 */
class EasyWriter (project: String, filename: String, private var toFile: Boolean = true)
      extends Writer ()
{
    /** The file path for the (log) file
     */
    private val LOG_PATH = LOG_DIR + ⁄ + project + ⁄ + filename

    /** The internal `PrintWriter` to write to a file
     */
    private val pw = new PrintWriter (LOG_PATH)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle between writing to a file and standard output.
     */
    def toggle () { toFile = ! toFile }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print/write the string 's'.
     *  @param s  the string to printed/written
     */
    def print (s: String) { if (toFile) pw.print (s) else Console.print (s) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print/write the string 's' and add a newline ('\n') at the end.
     *  @param s  the string to printed/written
     */
    def println (s: String) { if (toFile) pw.println (s) else Console.println (s) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print/write a newline ('\n').
     */
    def println () { if (toFile) pw.println ("") else Console.println () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write the character buffer.
     *  @param cbuf  the character buffer
     *  @param off   the offset in 'cbuf' to starting writing
     *  @param len   the length/number of characters to write 
     */
    def write (cbuf: Array [Char], off: Int, len: Int) 
    { 
        if (toFile) pw.write (cbuf, off, len) 
        else Console.print (cbuf.slice (off, off + len).mkString (""))
    } // write

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Flush (force write) the output to the file.
     */
    def flush () { if (toFile) pw.flush () else Console.flush () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the file (not applicable to standard output).
     */
    def close () { if (toFile) pw.close () }


} // EasyWriter class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EasyWriterTest` object is used to test the `EasyWriter` class.
 *  It will write into a file, unless there is a command-line argument.
 *  > runMain scalation.util.EasyWriterTest
 */
object EasyWriterTest extends App
{
    val ew = new EasyWriter ("util", "test.txt")

    if (args.length > 0 ) ew.toggle ()             // switch to standard output

    val s = "Hello World!"

    ew.print (s)
    ew.println (s)
    ew.write (s, 2, 4)
    ew.flush ()
    ew.close ()

} // EasyWriterTest object

