
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Thu Sep 24 14:03:17 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.programering.com/a/MDO2cjNwATI.html
 */

package scalation
package util

import java.io.RandomAccessFile
import java.nio.{ByteBuffer, MappedByteBuffer}
import java.nio.channels.FileChannel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MemoryMappedFileD` class provides support for memory-mapped files.
 *  This version works for `Double`
 *  @param fname  the file name
 *  @param sz     the size of the memory mapped file
 */
class MemoryMappedFileD (fname: String, sz: Int = 1024)
{
    /** The number of bytes required to store a `Double`
     */
    private val SIZE_DOUBLE = 8

    /** The random/direct access file
     */
    private val raf = new RandomAccessFile (MEM_MAPPED_DIR + fname, "rw");

    /** The random access file mapped into memory
     */
    private val mraf = raf.getChannel ().map (FileChannel.MapMode.READ_WRITE, 0, sz);

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the memory mapped file.
     */
    def size: Int = sz

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     */
    def get (index: Int): Double =
    {
        mraf.getDouble (SIZE_DOUBLE * index)
    } // get

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     *  @param x      the double value to put
     */
    def put (index: Int, x: Double): ByteBuffer =
    {
        mraf.putDouble (SIZE_DOUBLE * index, x)
    } // put

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the memory mapped file.
     */
    def close ()
    {
        raf.close ()
    } // close

} // MemoryMappedFileD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MemoryMappedFileDTest` is used to test the `MemoryMappedFileD` class.
 *  > runMain scalation.util.MemoryMappedFileDTest
 */
object MemoryMappedFileDTest extends App
{
    val fname = "ex_memory_mapped_filed.mmf";
    val mraf  = new MemoryMappedFileD (fname)
    val sz    = mraf.size

    // Write into the Memory Mapped File
    for (i <- 0 until sz/8) mraf.put (i, 1.0 + i % 2)
    println ("File '" + fname + "' is now " + sz + " bytes")

    // Read from the Memory Mapped File
    for (i <- 0 until sz/8) print (mraf.get (i) + " ")
    println ()
    println ("\nReading from memory-mapped file '" + fname + "' is complete.")

    mraf.close ()

} // MemoryMappedFileDTest object

