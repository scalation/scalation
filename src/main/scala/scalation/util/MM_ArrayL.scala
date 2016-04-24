
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller 
 *  @builder scalation.util.bld.BldMM_Array
 *  @version 1.2
 *  @date    Thu Sep 24 14:03:17 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.programering.com/a/MDO2cjNwATI.html
 */

package scalation.util

import java.io.{RandomAccessFile, Serializable}
import java.lang.Cloneable
import java.nio.{ByteBuffer, MappedByteBuffer}
import java.nio.channels.FileChannel

import collection._
import collection.mutable.{AbstractSeq, IndexedSeq}


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayL` class provides support for large, persistent arrays via memory
 *  mapped files.  Currently, the size of a memory mapped array is limited to
 *  2GB (2^31), since indices are signed 32-bit integers.
 *  FIX: use Long for indices and multiple files to remove 2GB limitation
 *  @see https://github.com/xerial/larray/blob/develop/README.md
 *  @param _length  the number of elements in the `mem_mapped` array
 */
final class MM_ArrayL (_length: Int)
      extends AbstractSeq [Long] with IndexedSeq [Long] with Serializable with Cloneable
{
    import MM_ArrayL.{_count, E_SIZE}

    /** The number of bytes in this memory mapped file
     */
    val nBytes = _length * E_SIZE

    /** The file name for this memory mapped files
     */
    val fname = { _count += 1; "mem_mapped_" + _count }

    /** The random/direct access file
     */
    private val raf = new RandomAccessFile (MEM_MAPPED_DIR + fname, "rw");

    /** The random access file mapped into memory
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
    /** Get the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     */
    def apply (index: Int): Long = 
    {
        mraf.getLong (E_SIZE * index)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     *  @param x      the double value to put
     */
    def update (index: Int, x: Long)
    {
        mraf.putLong (E_SIZE * index, x)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fold left through 'this' array.
     *  @param s0  the initial value
     *  @param f   the function to apply
     */
    def foldLeft (s0: Long)(f: (Long, Long) => Long): Long =
    {
        var s = s0
        for (i <- range) s = f (s, apply(i))
        s
    } // foldLeft

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map elements of 'this' array by applying the function 'f'.
     *  @param f  the function to be applied
     */
    def map (f: Long => Long): MM_ArrayL =
    {
        val c = new MM_ArrayL (_length)
        for (i <- range) c(i) = f(apply(i))
        c
    } // map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' starting at 'from' and continuing until 'till'
     *  @param from  the starting index for the slice (inclusive)
     *  @param till  the ending index for the slice (exclusive)
     */
    override def slice (from: Int, till: Int): MM_ArrayL =
    {
        MM_ArrayL (super.slice (from, till))
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether element 'x' is contained in this array.
     *  @param x  the element sought
     */
    def contains (x: Long): Boolean =
    {
        for (i <- range if x == apply(i)) return true
        false
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a sequence for 'this' array.
     */
    def deep: immutable.IndexedSeq [Long] = for (i <- range) yield apply(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the memory mapped file.
     */
    def close () { raf.close () }

} // MM_ArrayL class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayL` companion object provides factory methods for the `MM_ArrayL`
 *  class.
 */
object MM_ArrayL
{
    /** The number of bytes required to store a `Long`
     */
    private val E_SIZE = 8

    /** The counter for ensuring files names are unique
     */
    var _count = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array from one or more values (repeated values `Long*`).
     *  @param x   the first `Long` number
     *  @param xs  the rest of the `Long` numbers
     */
    def apply (x: Long, xs: Long*): MM_ArrayL =
    {
        val c = new MM_ArrayL (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.length) c(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def apply (xs: Seq [Long]): MM_ArrayL =
    {
        _count += 1
        val c = new MM_ArrayL (xs.length)
        for (i <- 0 until c.length) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def ofDim (n: Int): MM_ArrayL =
    {
        _count += 1
        new MM_ArrayL (n)
    } // ofDim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate memory mapped arrays 'a' and 'b'.
     */
    def concat (a: MM_ArrayL, b: MM_ArrayL): MM_ArrayL =
    {
        val (na, nb) = (a.length, b.length)
        val c  = new MM_ArrayL (na + nb)
        for (i <- 0 until na) c(i) = a(i)
        for (i <- 0 until nb) c(i + na) = b(i)
        c
    } // concat

} // MM_ArrayL object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayLTest` is used to test the `MM_ArrayL` class.
 *  > run-main scalation.util.MM_ArrayLTest
 */
object MM_ArrayLTest extends App
{
    val n    = 100                         // number of elements
    val mraf = new MM_ArrayL (n)            // memory mapped array

    // Write into the Memory Mapped File
    for (i <- 0 until n) mraf(i) = 2 * i
    println ("\nWRITE: memory mapped file '" + mraf.fname + "' now has " + mraf.nBytes + " bytes")

    // Read from the Memory Mapped File
    println ()
//  for (i <- 0 until n) print (mraf(i) + " ")
    println (mraf.deep)
    println ("READ: memory mapped file '" + mraf.fname + "' completed.")

    mraf.close ()

} // MM_ArrayLTest object

