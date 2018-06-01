
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   Vishnu Gowda Harish, John Miller, Yang Fan
 *  @version  1.4
 *  @date     Mon May 23 5:10:20 EDT 2016
 *  @see      LICENSE (MIT style license file).
 *
 *  @see      github.com/scala/scala/blob/v2.11.8/src/library/scala/collection/mutable/ResizableArray.scala
 */

package scalation.util

import scala.Array.newBuilder
import scala.collection.generic.{GenericCompanion, GenericTraversableTemplate}
import scala.collection.mutable
import scala.collection.mutable.{HashMap, IndexedSeqOptimized}
import scala.compat.Platform
import scala.reflect.ClassTag
import scalation.linalgebra.Vec

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReArray` class provides an implementation of mutable, resizable/dynamic arrays.  It is based
 *  on Scala's `ResizableArray` trait with additions and modifications.  Preliminary testing has
 *  shown it to be faster than its competition: `ArrayList` and `ArrayBuffer`.
 *-------------------------------------------------------------------------------------------
 *  Added methods:     'shiftLeft', 'shiftLeft2', 'shiftRight', 'shiftRight2' and 'expand'.                                                   
 *-------------------------------------------------------------------------------------------
 *  Modified methods:  'reduceToSize':  modified for performance reasons; instead of decreasing the size0 by 1 in
 *                                      a while loop and setting elements to null, it just decrements the size0 variable.
 *                                      FIX:  possible minor memory leak
 *                     'companion':     modified to return null as it is not required by our implementation;
 *                                      the `ReArray` object does not extend `SeqFactory` and thus 'newBuilder [A]' is absent. 
 *                     'update':        modified to catch the `IndexOutOfBoundsException' and automatically expand the array.
 *                     'constructor':   modified to take in the initial length and a normal `Array`; by doing this we can
 *                                      easily convert an object of type `Array` to a `ReArray` as the internal 'array' is
 *                                      changed from `AnyRef` to type `A'.
 *-------------------------------------------------------------------------------------------
 *  Modified variables:  'array':       the type is changed to `A` from `AnyRef`; this helps in assigning an `Array [A]` 
 *                                      to a `ReArray [A]`; the initial value is changed to check if '_array' is null.
 *                       'size0':       the initial value is changed to take care if '_array' is null.
 *-------------------------------------------------------------------------------------------
 *  @param _length  the initial size of the array, defaults to zero
 *  @param _array   the initial array, if any
 */
class ReArray [A] (_length: Int = 0, _array: Array [A] = null) (implicit arg0: ClassTag [A])
      extends IndexedSeq [A] with GenericTraversableTemplate [A, ReArray] with IndexedSeqOptimized [A, ReArray [A]]
{ 
    /** Set the internal array of `ReArray`.
     */
    protected var array: Array [A] = if (_array == null) new Array [A] (math.max (initialSize, 1)) else _array
    
    /** Set the size of `ReArray`.
     */
    protected var size0: Int = if (_array == null) 0 else _array.length

    ensureSize (initialSize)    // ensure that the internal array has at least `initialSize` cells

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the companion to null as not required for our implementation.
     */
    override def companion: GenericCompanion [ReArray] = null
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the initial size of the `ReArray`.
     */
    protected def initialSize: Int = math.max (50, _length);
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the length of 'this' `ReArray`.
     */
    def length: Int = size0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the element at the 'i'-th index position.
     *  @param i  the index position
     */
    def apply (i: Int) =
    {
        if (i >= array.length) throw new IndexOutOfBoundsException (i.toString)
        array(i).asInstanceOf [A]
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the internal array of the `ReArray.` 
     */
    def apply (): Array [A] = array

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over the `ReArray` element by element.
     *  @param f  the function to apply 
     */
    override def foreach [U] (f: A => U)
    {
        val top = size                             // made local for performance
        var i   = 0
//      while (i < top) { f (array(i).asInstanceOf [A]); i += 1 }
        while (i < top) { f (array(i)); i += 1 }
    } // foreach
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Fill the given array 'xs' with at most 'len' elements of 'this' traversable
     *  starting at position 'start'.  Copying will stop once either the end of the
     *  current traversable is reached or 'len' elements have been copied or the end
     *  of the array is reached.
     *  @param xs     the array to fill
     *  @param start  starting index
     *  @param len    number of elements to copy
     */
    override def copyToArray [B >: A] (xs: Array [B], start: Int, len: Int)
    {
        val len1 = len min (xs.length - start) min length
        Array.copy (array, 0, xs, start, len1)
    } // copyToArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Ensure that the internal array has at least 'n' cells. 
     *  @param n  the number of cells
     */
    protected def ensureSize (n: Int)
    {
        val arrayLength: Long = array.length         // use a Long to prevent overflows
        if (n > arrayLength) {
            var newSize = arrayLength * 5l
            while (n > newSize) newSize *= 5l
            if (newSize > Int.MaxValue) newSize = Int.MaxValue   // don't exceed Int.MaxValue

            val newArray = new Array [A] (newSize.toInt)
            Platform.arraycopy (array, 0, newArray, 0, size0)
            array = newArray
        } // if
    } // ensureSize
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap two elements of this array.
     *  @param i  the first element
     *  @param j  the second element
     */
    protected def swap (i: Int, j: Int)
    {
        val h    = array(i)
        array(i) = array(j)
        array(j) = h
    } // swap
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove elements of this array at indices after 'sz'.
     *  @param sz  the index position
     */
    def reduceToSize (sz: Int)
    {
        require (sz <= size0)
        size0 = sz
//      while (size0 > sz) { size0 -= 1; array(size0) = _ }
    } // reduceToSize
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Move parts of the array.
     *  @param m    the source position
     *  @param n    the destination position
     *  @param len  the number of elements
     */
    protected def copy (m: Int, n: Int, len: Int) { Platform.arraycopy (array, m, array, n, len) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the array to make sure it has the required size.
     *  @param sz  the new size requirement of the array
     */
    def expand (sz: Int) { ensureSize (sz); size0 = sz }
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' array's element at the 'i'- th index position. 
     *  @param idx   the given index
     *  @param elem  the value to assign
     */
    override def update (idx: Int, elem: A)
    {
        try {
            if (idx >= array.length) throw new IndexOutOfBoundsException (idx.toString)
            array(idx) = elem
            if(idx >= size0) size0 = size0+1
        } catch {
            case e: IndexOutOfBoundsException => expand (idx + 1)
                                                 array(idx) = elem
        } // try
    } // update
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the elements by one position to the right starting at the index 'i'. 
     *  This will also automatically expand the array.  The value will be repeated 
     *  at 'i' and 'i+1' positions.
     *  @param i  the index position to start the shift
     */
    def shiftRight (i: Int)
    {
        expand (size0 + 1)
        for (j <- size0 - 2 to i by -1) array(j+1) = array(j)
    } // shiftRight
      
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the elements by two positions to the right starting at the index 'i'. 
     *  This will also automatically expand the array. The value will be repeated 
     *  at 'i', 'i+1' and 'i+2' positions.
     *  @param i  the index position to start the shift
     */
    def shiftRight2 (i: Int)
    {
        expand (size0 + 2)
        for (j <- size0 - 3 to i by -1) { array(j+2) = array(j); array(j+1) = array(j) }
    } // shiftRight2
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove one element at the specified index position and shift the rest of
     *  the elements in the array one position to the left.
     *  @param i  the index of the element to be removed
     */
    def shiftLeft (i: Int)
    {
        if (i < size0) {
            for (j <- i + 1 until size0) array(j-1) = array(j)
            reduceToSize (size0 - 1)
        } // if
    } // shiftLeft
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove two elements starting at the specified index position and shift the
     *  rest of the elements in the array two positions to the left.
     *  @param i  the index of the element to be removed
     */
    def shiftLeft2 (i: Int)
    {
        if (i < size0 - 1) {
            for (j <- i + 2 until size0) array(j-2) = array(j)
            reduceToSize (size0 - 2)
        } // if
    } // shiftLeft2
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `ReArray` to a string.
     */
    override def toString: String = "Re" + array.slice (0, size0).deep

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect 'this' `ReArray` with `ReArray` 'b'.
     *  @param b  the ReArray to intersect with
     */
    def intersect (b: ReArray [A]): ReArray [A] =
    {
        var result = new ReArray [A] ()
        for (nb <- b) if (contains (nb)) result.update (result.length, nb)
        result
    } // intersect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear the `ReArray`.
     */
    def clear () { array = new Array [A] (math.max (initialSize, 1)) }

} // ReArray class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReArray` object is the companion object for the `ReArray` class.
 */
object ReArray 
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `ReArray` from an `Array`.
     *  @param a  the `Array` to be converted
     */
    def apply [A: ClassTag] (a: Array [A]): ReArray [A] = new ReArray [A] (a.length, a)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `ReArray` of type `A` and dimension 'n' with its values determined
     *  by the 'elem' function.
     *  @param n     the size of the `ReArray`
     *  @param elem  function giving the elements for the `ReArray`
     */
    def fill [A] (n: Int)(elem: ⇒ A)(implicit arg0: ClassTag [A]): ReArray [A] =  
    {   
        val b = newBuilder [A]
        b.sizeHint (n)
        var i = 0
        while (i < n) { b += elem; i += 1 }
        ReArray (b.result())
    } // fill
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `ReArray` with dimension 'n'.
     *  @param n  the size of the `ReArray`
     */
    def ofDim [A: ClassTag] (n: Int): ReArray [A] = new ReArray [A] (n) 

} // ReArray object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReArrayTest` object tests the operations provided by `ReArrayTest`.
 *  > run-main scalation.util.ReArrayTest
 */
object ReArrayTest extends App
{  
    val x = Array (1, 2, 3, 4, 5)

    val a = new ReArray (_array = x)
    val b = ReArray (x)
    val c = ReArray.fill (5)(1)
    val d = ReArray.ofDim [Int] (5)
    val e = Array.ofDim [Int] (5)
    
    println ("a = " + a)
    println ("b = " + b)
    println ("c = " + c)
    println ("d = " + d)
    println ("e = " + e.deep)

} // ReArrayTest object

