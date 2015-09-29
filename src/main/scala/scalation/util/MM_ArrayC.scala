
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller 
 *  @maker   scalation.util.bld.BldMM_Array
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
import scalation.math.Complex

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayC` class provides support for large, persistent arrays via memory
 *  mapped files.  Currently, the size of a memory mapped array is limited to
 *  2GB (2^31), since indices are signed 32-bit integers.
 *  FIX: use Long for indices and multiple files to remove 2GB limitation
 *  @see https://github.com/xerial/larray/blob/develop/README.md
 *  @param _length  the number of elements in the mem_mapped array
 */
final class MM_ArrayC (_length: Int)
      extends AbstractSeq [Complex] with IndexedSeq [Complex] with Serializable with Cloneable
{
    import MM_ArrayC.{_count, E_SIZE}

    /** The number of bytes in this memory mapped file
     */
    val nBytes = _length * E_SIZE

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
    /** Get the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     */
    def apply (index: Int): Complex = 
    {
        Complex (mraf.getDouble (E_SIZE * index), mraf.getDouble (E_SIZE * index + 8))
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     *  @param x      the double value to put
     */
    def update (index: Int, x: Complex)
    {
        mraf.putDouble (E_SIZE * index, x.val1); mraf.putDouble (E_SIZE * index + 8, x.val2)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fold left through 'this' array.
     *  @param s0  the initial value
     *  @param f   the function to apply
     */
    def foldLeft (s0: Complex)(f: (Complex, Complex) => Complex): Complex =
    {
        var s = s0
        for (i <- range) s = f (s, apply(i))
        s
    } // foldLeft

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map elements of 'this' array by applying the function 'f'.
     *  @param f  the function to be applied
     */
    def map (f: Complex => Complex): MM_ArrayC =
    {
        val c = new MM_ArrayC (_length)
        for (i <- range) c(i) = f(apply(i))
        c
    } // map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' starting at 'from' and continueing until 'till'
     *  @param from  the starting index for the slice (inclusive)
     *  @param till  the ending index for the slice (exclusive)
     */
    override def slice (from: Int, till: Int): MM_ArrayC =
    {
        MM_ArrayC (super.slice (from, till))
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether element 'x' is contained in this array.
     *  @param x  the element sought
     */
    def contains (x: Complex): Boolean =
    {
        for (i <- range if x == apply(i)) return true
        false
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a sequence for 'this' array.
     */
    def deep: immutable.IndexedSeq [Complex] = for (i <- range) yield apply(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the memory mapped file.
     */
    def close () { raf.close () }

} // MM_ArrayC class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayC` companion object provides factory methods for the `MM_ArrayC`
 *  class.
 */
object MM_ArrayC
{
    /** The number of bytes required to store a `Complex`
     */
    private val E_SIZE = 16

    /** The counter for ensuring files names are unique
     */
    var _count = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array from one or more values (repeated values Complex*).
     *  @param x   the first Complex number
     *  @param xs  the rest of the Complex numbers
     */
    def apply (x: Complex, xs: Complex*): MM_ArrayC =
    {
        val c = new MM_ArrayC (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.length) c(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def apply (xs: Seq [Complex]): MM_ArrayC =
    {
        _count += 1
        val c = new MM_ArrayC (xs.length)
        for (i <- 0 until c.length) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def ofDim (n: Int): MM_ArrayC =
    {
        _count += 1
        new MM_ArrayC (n)
    } // ofDim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate memory mapped arrays 'a' and 'b'.
     */
    def concat (a: MM_ArrayC, b: MM_ArrayC): MM_ArrayC =
    {
        val (na, nb) = (a.length, b.length)
        val c  = new MM_ArrayC (na + nb)
        for (i <- 0 until na) c(i) = a(i)
        for (i <- 0 until nb) c(i + na) = b(i)
        c
    } // concat

} // MM_ArrayC object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_ArrayCTest` is used to test the `MM_ArrayC` class.
 *  > run-main scalation.util.MM_ArrayCTest
 */
object MM_ArrayCTest extends App
{
    val n    = 100                         // number of elements
    val mraf = new MM_ArrayC (n)            // memory mapped array

    // Write into the Memory Mapped File
    for (i <- 0 until n) mraf(i) = 2 * i
    println ("\nWRITE: memory mapped file '" + mraf.fname + "' now has " + mraf.nBytes + " bytes")

    // Read from the Memory Mapped File
    println ()
//  for (i <- 0 until n) print (mraf(i) + " ")
    println (mraf.deep)
    println ("READ: memory mapped file '" + mraf.fname + "' completed.")

    mraf.close ()

} // MM_ArrayCTest object

