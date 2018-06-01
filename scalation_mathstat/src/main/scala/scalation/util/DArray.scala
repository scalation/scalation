
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   John Miller, Vishnu Gowda Harish
 *  @version  1.4
 *  @date     Thu Dec 22 15:34:52 EST 2016
 *  @see      LICENSE (MIT style license file).
 */

package scalation.util

import scala.Array.newBuilder
import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable.IndexedSeqOptimized
import scala.compat.Platform
import scala.math.ceil
import scala.reflect.ClassTag

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DArray` class provides an implementation of large mutable arrays.  It used a double
 *  array with hi 'i1' and low 'i2' indices.  Index 'i1' uses the hi-order bits to determine
 *  the chunk and 'i2' to determine the offset within a chunk.
 *-------------------------------------------------------------------------------------------
 *      'apply (i) => i1 = i >> bits; val i2 = i - i1'
 *-------------------------------------------------------------------------------------------
 *  Could be extended to use a 'Long' rather than 'Int' in apply to very large arrays.
 *-------------------------------------------------------------------------------------------
 *  @param _length  the initial size of the array, defaults to 32768
 */
class DArray [A] (_length: Int = 32768) (implicit arg0: ClassTag [A])
      extends IndexedSeq [A] with GenericTraversableTemplate [A, DArray] with IndexedSeqOptimized [A, DArray [A]]
{ 
    /** number of bits each, for hi (i1) and low (i2) indices
     */
    private val bits = 15

    /** 2 ^ bits
     */
    private val _2upbits = 1 << bits

    /** 2 ^ bits as a `Double`
     */
    private val _2upbitsd = _2upbits.toDouble

    /** maximum capacity
     */
    private val CAP = _2upbits * _2upbits

    /** the internal store (double array) for `DArray`.
     */
    private val store = new Array [Array [A]] (_2upbits)

    /** the current size of `DArray`.
     */
    protected var size0: Int = _length

    /** the current number of chunks allocated
     */
    private var chunks = ceil (_length / _2upbitsd).toInt

    for (i1 <- 0 until chunks) store(i1) = new Array [A] (_2upbits)    // allocate for current chunks
    
    ensureSize (initialSize)    // ensure that the internal array has at least `initialSize` cells
     
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the companion to null as not required for our implementation.
     */
    override def companion: GenericCompanion [DArray] = null
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the initial size of the `DArray`.
     */
    protected def initialSize: Int = chunks * _2upbits
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the length of 'this' `DArray`.
     */
    def length: Int = size0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the element at the 'i'-th index position.
     *  @param i  the index position
     */
    def apply (i: Int) =
    {
        if (i >= size0) throw new IndexOutOfBoundsException (i.toString)
        val i1 = i >> bits; val i2 = i - i1
        store(i1)(i2)
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the internal double array of the `DArray.` 
     */
    def apply (): Array [Array [A]] = store

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over the `DArray` element by element.
     *  @param f  the function to apply 
     */
    override def foreach [U] (f: A => U)
    {
        val top = size                             // made local for performance
        var i   = 0
        //while (i < top) { f (array(i).asInstanceOf [A]); i += 1 }
        while (i < top) { val i1 = i >> bits; val i2 = i - i1; f (store(i1)(i2)); i += 1 }
    } // foreach
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fill the given array 'xs' with at most 'len' elements of 'this' traversable
     *  starting at position 'start'.  Copying will stop once either the end of the
     *  current traversable is reached or 'len' elements have been copied or the end
     *  of the array is reached.
     *  @param xs     the array to fill
     *  @param start  starting index
     *  @param len    number of elements to copy
     *
    override def copyToArray [B >: A] (xs: Array [B], start: Int, len: Int)
    {
        val len1 = len min (xs.length - start) min length
        Array.copy (array, 0, xs, start, len1)
    } // copyToArray
     */
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Ensure that the internal array has at least 'n' cells. 
     *  @param n  the number of cells
     */
    protected def ensureSize (n: Int)
    {
        if (n > CAP) println (s"ensureSize: can't support arrays with with more than $CAP elements")
        if (n > chunks * _2upbits) {
            val chunks2 = ceil (n / _2upbitsd).toInt
            for (i1 <- chunks until chunks2) store(i1) = new Array [A] (_2upbits)
        } // if
    } // ensureSize
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove elements of this array at indices after 'sz'.
     *  @param sz  the index position
     */
    def reduceToSize (sz: Int)
    {
        require (sz <= size0)
        size0 = sz
    } // reduceToSize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move parts of the array.
     *  @param m    the source position
     *  @param n    the destination position
     *  @param len  the number of elements
     *
    protected def copy (m: Int, n: Int, len: Int) { Platform.arraycopy (array, m, array, n, len) }
     */
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the array to make sure it has the required size.
     *  @param sz  the new size requirement of the array
     */
    def expand (sz: Int) { ensureSize (sz); size0 = sz }
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' array's element at the 'i'- th index position. 
     *  @param i     the given index
     *  @param elem  the value to assign
     */
    override def update (i: Int, elem: A)
    {
        val i1 = i >> bits; val i2 = i - i1
        try {
            if (i >= size0) throw new IndexOutOfBoundsException (i.toString)
            store(i1)(i2) = elem
        } catch {
            case e: IndexOutOfBoundsException => expand (i + 1)
                                                 store(i1)(i2) = elem
        } // try-catch
    } // update
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `DArray` to a string.  FIX - clean up
     */
    override def toString: String =
    {
        val sb = new StringBuilder ("DArray(")
        for (i <- 0 until size0) { val i1 = i >> bits; val i2 = i - i1; sb.append (store(i1)(i2) + ", ") }
        sb.replace (sb.length-2, sb.length, ")").mkString
    } // toString

} // DArray


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DArray` object is the companion object for the `DArray` class.
 */
object DArray 
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `DArray` from an `Array`.
     *  @param a  the `Array` to be converted
     */
    def apply [A: ClassTag] (a: Array [A]): DArray [A] = 
    {
        val da = new DArray [A] (a.length)
        for (i <- a.indices) da(i) = a(i)
        da
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `DArray` of type `A` and dimension 'n' with its values determined
     *  by the 'elem' function.
     *  @param n     the size of the `DArray`
     *  @param elem  function giving the elements for the `DArray`
     */
    def fill [A] (n: Int)(elem: ⇒ A)(implicit arg0: ClassTag [A]): DArray [A] =  
    {   
        val b = newBuilder [A]
        b.sizeHint (n)
        var i = 0
        while (i < n) { b += elem; i += 1 }
        val a = b.result ()
        val da = new DArray (a.length)
        for (i <- a.indices) da(i) = a(i)
        da
    } // fill
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `DArray` with dimension 'n'.
     *  @param n  the size of the `DArray`
     */
    def ofDim [A: ClassTag] (n: Int): DArray [A] = new DArray [A] (n) 

} // DArray object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DArrayTest` object tests the operations provided by `DArrayTest`.
 *  > runMain scalation.util.DArrayTest
 */
object DArrayTest extends App
{  
    val x = Array (1, 2, 3, 4, 5)

    val a = new DArray [Int] (x.length); for (i <- x.indices) a(i) = x(i)
    val b = DArray (x)
    val c = DArray.fill (5)(1)
    val d = DArray.ofDim [Int] (5)
    val e = Array.ofDim [Int] (5)
    
    println ("a = " + a)
    println ("b = " + b)
    println ("c = " + c)
    println ("d = " + d)
    println ("e = " + e.deep)   

} // DArrayTest object

