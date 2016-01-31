
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller 
 *  @version 1.2
 *  @date    Sat Sep 26 20:25:19 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.programering.com/a/MDO2cjNwATI.html
 */

// U N D E R   D E V E L O P M E N T

package scalation.util

import java.io.{RandomAccessFile, Serializable}
import java.lang.Cloneable
import java.nio.{ByteBuffer, MappedByteBuffer}
import java.nio.channels.FileChannel

import collection._
import collection.mutable.{AbstractSeq, IndexedSeq}

import scalation.math.StrO.{fromByteArray, StrNum}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayS` class provides support for large, persistent arrays via memory
 *  mapped files.  Currently, the size of a memory mapped array is limited to
 *  2GB (2^31), since indices are signed 32-bit integers.
 *  FIX: use Long for indices and multiple files to remove 2GB limitation
 *  @see https://github.com/xerial/larray/blob/develop/README.md
 *  @param _length  the number of elements in the mem_mapped array
 */
final class MM_ArrayS (_length: Int)
      extends AbstractSeq [StrNum] with IndexedSeq [StrNum] with Serializable with Cloneable
{
    import MM_ArrayS.{_count, E_SIZE}

    /** The number of bytes in this memory mapped file
     */
    val nBytes = _length * E_SIZE                     // FIX: StrNum's are variable length

    /** The file name for this memory mapped files
     */
    val fname = { _count += 1; "mem_mapped_" + _count }

    /** The random/direct access file
     */
    private val raf = new RandomAccessFile (MEM_MAPPED_DIR + fname, "rw");

    /** The raf file mapped into memory
     */
    private val mraf = raf.getChannel ().map (FileChannel.MapMode.READ_WRITE, 0, nBytes);

    /** The range of index positions for 'this' memory mapped array
     */
    private val range = 0 until _length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of elements in the memory mapped file.
     */
    def length: Int = _length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the bytes in the file starting at the position determined by 'index'.
     *  @param index  the logical index position in the file
     */
    def apply (index: Int): StrNum = 
    {
        val (offset, length) = (0, 10)           // FIX: lookup offset and length based on index
        val buffer = Array.ofDim [Byte] (length)
        mraf.position (offset)
        mraf.get (buffer, 0, length)
        fromByteArray (buffer)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the bytes in the file starting at the position determined by 'index'.
     *  @param index  the logical index position in the file
     *  @param x      the string number value to put
     */
    def update (index: Int, x: StrNum)
    {
        val buffer = x.getBytes ()
        val length = buffer.length
        val offset = 0                           // FIX: determine offset based on index and length
        mraf.position (offset)
        mraf.put (buffer, 0, length)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fold left through 'this' array.
     *  @param s0  the initial value
     *  @param f   the function to apply
     */
    def foldLeft (s0: StrNum)(f: (StrNum, StrNum) => StrNum): StrNum =
    {
        var s = s0
        for (i <- range) s = f (s, apply(i))
        s
    } // foldLeft

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map elements of 'this' array by applying the function 'f'.
     *  @param f  the function to be applied
     */
    def map (f: StrNum => StrNum): MM_ArrayS =
    {
        val c = new MM_ArrayS (_length)
        for (i <- range) c(i) = f(apply(i))
        c
    } // map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' starting at 'from' and continueing until 'till'
     *  @param from  the starting index for the slice (inclusive)
     *  @param till  the ending index for the slice (exclusive)
     */
    override def slice (from: Int, till: Int): MM_ArrayS =
    {
        MM_ArrayS (super.slice (from, till))
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether element 'x' is contained in this array.
     *  @param x  the element sought
     */
    def contains (x: StrNum): Boolean =
    {
        for (i <- range if x == apply(i)) return true
        false
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a sequence for 'this' array.
     */
    def deep: immutable.IndexedSeq [StrNum] = for (i <- range) yield apply(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the memory mapped file.
     */
    def close () { raf.close () }

} // MM_ArrayS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayS` companion object provides factory methods for the `MM_ArrayS`
 *  class.
 */
object MM_ArrayS
{
    /** The number of bytes required to store a `StrNum`
     */
    private val E_SIZE = -1           // variable length

    /** The counter for ensuring files names are unique
     */
    var _count = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array from one or more values (repeated values StrNum*).
     *  @param x   the first StrNum number
     *  @param xs  the rest of the StrNum numbers
     */
    def apply (x: StrNum, xs: StrNum*): MM_ArrayS =
    {
        val c = new MM_ArrayS (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.length) c(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def apply (xs: Seq [StrNum]): MM_ArrayS =
    {
        _count += 1
        val c = new MM_ArrayS (xs.length)
        for (i <- 0 until c.length) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def ofDim (n: Int): MM_ArrayS =
    {
        _count += 1
        new MM_ArrayS (n)
    } // ofDim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate memory mapped arrays 'a' and 'b'.
     */
    def concat (a: MM_ArrayS, b: MM_ArrayS): MM_ArrayS =
    {
        val (na, nb) = (a.length, b.length)
        val c  = new MM_ArrayS (na + nb)
        for (i <- 0 until na) c(i) = a(i)
        for (i <- 0 until nb) c(i + na) = b(i)
        c
    } // concat

} // MM_ArrayS object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArraySTest` is used to test the `MM_ArrayS` class.
 *  > run-main scalation.util.MM_ArraySTest
 */
object MM_ArraySTest extends App
{
    val n    = 100                         // number of elements
    val mraf = new MM_ArrayS (n)            // memory mapped array

    // Write into the Memory Mapped File
    for (i <- 0 until n) mraf(i) = 2 * i
    println ("\nWRITE: memory mapped file '" + mraf.fname + "' now has " + mraf.nBytes + " bytes")

    // Read from the Memory Mapped File
    println ()
//  for (i <- 0 until n) print (mraf(i) + " ")
    println (mraf.deep)
    println ("READ: memory mapped file '" + mraf.fname + "' completed.")

    mraf.close ()

} // MM_ArraySTest object

