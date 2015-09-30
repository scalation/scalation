
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sat Sep 26 13:10:03 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldMM_Array` object is used to build memory mapped array classes for
 *  various base types.
 *  > run-main scalation.util.bld.BldMM_Array
 */
object BldMM_Array extends App with BldParams
{
    println ("BldMM_Array: generate code for MM_Array classes")

    for (k <- kind) {
        val MM_ARRAY  = k._1
        val BASE      = k._2
        val ESIZE     = k._3
        val BASE2     = k._4
        val SORTING   = k._5
        val ZERO      = k._6
        val ONE       = k._7
        val IMPORT    = if (BASE == "StrNum") "import scalation.math.StrO"
                        else if (CUSTOM contains BASE) s"import scalation.math.$BASE"
                        else ""
        val APPLY     = if (CUSTOM contains BASE) s"$BASE (mraf.get$BASE2 (E_SIZE * index), mraf.get$BASE2 (E_SIZE * index + 8))"
                        else s"mraf.get$BASE (E_SIZE * index)"
        val UPDATE    = if (CUSTOM contains BASE) s"mraf.put$BASE2 (E_SIZE * index, x.val1); mraf.put$BASE2 (E_SIZE * index + 8, x.val2)"
                        else s"mraf.put$BASE (E_SIZE * index, x)"

// Beginning of string holding code template -----------------------------------

        val code = raw"""
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
$IMPORT

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$MM_ARRAY` class provides support for large, persistent arrays via memory
 *  mapped files.  Currently, the size of a memory mapped array is limited to
 *  2GB (2^31), since indices are signed 32-bit integers.
 *  FIX: use Long for indices and multiple files to remove 2GB limitation
 *  @see https://github.com/xerial/larray/blob/develop/README.md
 *  @param _length  the number of elements in the mem_mapped array
 */
final class $MM_ARRAY (_length: Int)
      extends AbstractSeq [$BASE] with IndexedSeq [$BASE] with Serializable with Cloneable
{
    import $MM_ARRAY.{_count, E_SIZE}

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
    def apply (index: Int): $BASE = 
    {
        $APPLY
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the bytes in the file starting at 'index'.
     *  @param index  the index position in the file
     *  @param x      the double value to put
     */
    def update (index: Int, x: $BASE)
    {
        $UPDATE
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fold left through 'this' array.
     *  @param s0  the initial value
     *  @param f   the function to apply
     */
    def foldLeft (s0: $BASE)(f: ($BASE, $BASE) => $BASE): $BASE =
    {
        var s = s0
        for (i <- range) s = f (s, apply(i))
        s
    } // foldLeft

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map elements of 'this' array by applying the function 'f'.
     *  @param f  the function to be applied
     */
    def map (f: $BASE => $BASE): $MM_ARRAY =
    {
        val c = new $MM_ARRAY (_length)
        for (i <- range) c(i) = f(apply(i))
        c
    } // map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' starting at 'from' and continueing until 'till'
     *  @param from  the starting index for the slice (inclusive)
     *  @param till  the ending index for the slice (exclusive)
     */
    override def slice (from: Int, till: Int): $MM_ARRAY =
    {
        $MM_ARRAY (super.slice (from, till))
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether element 'x' is contained in this array.
     *  @param x  the element sought
     */
    def contains (x: $BASE): Boolean =
    {
        for (i <- range if x == apply(i)) return true
        false
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a sequence for 'this' array.
     */
    def deep: immutable.IndexedSeq [$BASE] = for (i <- range) yield apply(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the memory mapped file.
     */
    def close () { raf.close () }

} // $MM_ARRAY class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$MM_ARRAY` companion object provides factory methods for the `$MM_ARRAY`
 *  class.
 */
object $MM_ARRAY
{
    /** The number of bytes required to store a `$BASE`
     */
    private val E_SIZE = $ESIZE

    /** The counter for ensuring files names are unique
     */
    var _count = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array from one or more values (repeated values $BASE*).
     *  @param x   the first $BASE number
     *  @param xs  the rest of the $BASE numbers
     */
    def apply (x: $BASE, xs: $BASE*): $MM_ARRAY =
    {
        val c = new $MM_ARRAY (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.length) c(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def apply (xs: Seq [$BASE]): $MM_ARRAY =
    {
        _count += 1
        val c = new $MM_ARRAY (xs.length)
        for (i <- 0 until c.length) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a memory mapped array with 'n' elements.
     *  @param n  the number of elements
     */
    def ofDim (n: Int): $MM_ARRAY =
    {
        _count += 1
        new $MM_ARRAY (n)
    } // ofDim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate memory mapped arrays 'a' and 'b'.
     */
    def concat (a: $MM_ARRAY, b: $MM_ARRAY): $MM_ARRAY =
    {
        val (na, nb) = (a.length, b.length)
        val c  = new $MM_ARRAY (na + nb)
        for (i <- 0 until na) c(i) = a(i)
        for (i <- 0 until nb) c(i + na) = b(i)
        c
    } // concat

} // $MM_ARRAY object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `${MM_ARRAY}Test` is used to test the `$MM_ARRAY` class.
 *  > run-main scalation.util.${MM_ARRAY}Test
 */
object ${MM_ARRAY}Test extends App
{
    val n    = 100                         // number of elements
    val mraf = new $MM_ARRAY (n)            // memory mapped array

    // Write into the Memory Mapped File
    for (i <- 0 until n) mraf(i) = 2 * i
    println ("\nWRITE: memory mapped file '" + mraf.fname + "' now has " + mraf.nBytes + " bytes")

    // Read from the Memory Mapped File
    println ()
//  for (i <- 0 until n) print (mraf(i) + " ")
    println (mraf.deep)
    println ("READ: memory mapped file '" + mraf.fname + "' completed.")

    mraf.close ()

} // ${MM_ARRAY}Test object

"""

// Ending of string holding code template --------------------------------------

//      println (code)

        val writer = new PrintWriter (new File (DIR + _l + MM_ARRAY + ".scalaa"))
        writer.write (code)
        writer.close ()
    } // for

} // BldMM_Array object

