
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vishnu Gowda Harish
 *  @version 1.3
 *  @date    Fri Jan 29 15:43:08 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.{breakOut, Traversable}
import scala.collection.mutable.{ArrayBuffer, IndexedSeq}
import scala.util.Sorting.quickSort

import scala.math.{abs => ABS, max => MAX, sqrt}

import scalation.math.long_exp
import scalation.util.{Error, SortedLinkedHashMap}
import scalation.util.SortingL
import scalation.util.SortingL.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorL` class stores and operates on Numeric Vectors of base type `Long`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim_  the dimension/size of the vector
 *  @param v     the `SortedLinkedHashMap` used to store vector elements
 */
class SparseVectorL (val dim_ : Int,
           protected var v:     SparseVectorL.RowMap = null)
      extends VectoL
//    extends Traversable [Long] with PartiallyOrdered [SparseVectorL] with Vec with Error with Serializable
{
    /** Dimension
     */
    lazy val dim = dim_
    
    if (v == null) v = new SparseVectorL.RowMap ()
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: VectoL) { this (u.dim); for (i <- range) this(i) = u(i) } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param iv  the tuple containing (index, value)
     *  @param dm  the dimension for the new vector
     */
    def this (iv: (Int, Long), dm: Int) { this (dm); this(iv._1) = iv._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' vector.
     */
    def copy: SparseVectorL = new SparseVectorL (dim, v.clone.asInstanceOf [SparseVectorL.RowMap])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def zero (size: Int = dim): SparseVectorL = new SparseVectorL (size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def one (size: Int = dim): SparseVectorL = { val sv = new SparseVectorL (size); sv.set (1l); sv }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): SparseVectorL = new SparseVectorL ((j, 1l), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): SparseVectorL = new SparseVectorL ((j, -1l), size) 
      
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Long = if (i >= dim) throw new IndexOutOfBoundsException else v.getOrElse (i, 0l)
                                   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): SparseVectorL = slice (r.start, r.end)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire array.
     */
    def apply (): IndexedSeq [Long] =
    {
        (for (i <- range) yield v.getOrElse (i, 0l))(breakOut)
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Long) 
    { 
        if (i >= dim) throw new IndexOutOfBoundsException
        if (x =~ 0l) v.remove(i) else v(i) = x        
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Long)  { for (i <- r) this(i) = x } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectoL) { for (i <- r) this(i) = u(i - r.start) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Long => U) { var i = 0; while (i < dim) { f (this(i)); i += 1 } } 
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: Long) { for (i <- range) this(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in array 'u'.
     *  @param u  the array of values to be assigned
     */
    def set (u: Seq [Long]) { for (i <- range) this(i) = u(i) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorL` into a `VectorI`.
     */
    def toInt: VectorI = 
    {
        val c = new VectorI (dim)
        for (i <- range) c(i) = this(i).toInt
        c
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorL` into a `VectorL`.
      */
    def toLong: VectorL =
    { 
        val c = new VectorL (dim)
        for (i <- range) c(i) = this(i).toLong
        c
    } // toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorL` into a `VectorD`.
     */
    def toDouble: VectorD =
    {    
        val c = new VectorD (dim)
        for (i <- range) c(i) = this(i).toDouble
        c
    } // toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of 'this' vector by 'more' elements.
     *  @param more  the number of new elements to add
     */
    def expand (more: Int = dim): SparseVectorL =
    {
        if (more < 1) this       // no change
        else {
            val sv = new SparseVectorL (dim + more)
            sv.v ++= v
            sv
        } // if
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's entire array of stored (i.e., non-zero) elements.
     */
    private def nonZero: Array [Long] = v.values.toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's positive elements as an array.
     */
    private def positive: Array [Long] = v.values.filter (_ > 0l).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's negatives elements as an array.
     */
    private def negative: Array [Long] = v.values.filter (_ < 0l).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    override def filter (p: Long => Boolean): SparseVectorL =
    { 
        SparseVectorL (for (i <- range if p (this(i)) ) yield v(i))
    } // filter
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: Long => Boolean): IndexedSeq [Int] =
    {
        (for (i <- range if p (this(i))) yield i)(breakOut)
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: Long => Long): SparseVectorL = SparseVectorL (this ().map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): SparseVectorL =
    {
        null    // FIX: new SparseVectorL (till - from, v.slice (from, till))
    } // slice
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): SparseVectorL =
    {
        SparseVectorL (for (i <- 0 until basis.length) yield this(basis(i)))
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectoL): SparseVectorL =
    {
        val c = new SparseVectorL (dim + b.dim)
        for (i <- c.range) c(i) = if (i < dim) this(i) else b(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: Long): SparseVectorL =
    {
        val c = new SparseVectorL (dim + 1)
        for (i <- c.range) c(i) = if (i < dim) this(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: VectoL): SparseVectorL = { SparseVectorL (for ( i <- range) yield this(i) + b(i)) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: Long): SparseVectorL =
    {
        if (s =~ 0l) this 
        else SparseVectorL (for (i <- range) yield this(i) + s)       
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._2' only at position 's._1'.
     *  @param s  the (scalar, position) to add
     */
    def + (s: (Int, Long)): SparseVectorL =
    {
        if (s._1 =~ 0l) this
        else {
            val c = new SparseVectorL (this)
            c(s._1) += s._2
            c
        } // if    
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: VectoL): SparseVectorL = { for (i <- range) this(i) += b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: Long): SparseVectorL = { for (i <- range) this(i) += s; this } 
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        for (x <- v) c(x._1) = -x._2 
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: VectoL): SparseVectorL = { SparseVectorL (for ( i <- range) yield this(i) - b(i)) }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: Long): SparseVectorL =
    {
        if (s =~ 0l) this 
        else SparseVectorL (for (i <- range) yield this(i) - s)       
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._2' only at position 's._1'.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: (Int, Long)): SparseVectorL =
    {
        if (s._1 =~ 0l) this
        else {
            val c = new SparseVectorL (this)
            c(s._1) -= s._2
            c
        } // if    
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to add
     */
    def -= (b: VectoL): SparseVectorL = { for (i <- range) this(i) -= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to add
     */
    def -= (s: Long): SparseVectorL = { for (i <- range) this(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: VectoL): SparseVectorL = 
    {     
        val c = new SparseVectorL (dim)
        for (x <- v) c(x._1) = x._2 * b(x._1) 
        c    
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: Long): SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        if( s !=~ 0l ) for (x <- v) c(x._1) = x._2 * s  
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def *= (b: VectoL): SparseVectorL = { for (x <- v) this(x._1) = x._2 * b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def *= (s: Long): SparseVectorL = { for (x <- v) this(x._1) = x._2 * s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectoL): SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        for (x <- v) c(x._1) = x._2 / b(x._1) 
        c    
    } // /
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: Long): SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        for (x <- v) c(x._1) = x._2 / s  
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def /= (b: VectoL): SparseVectorL = { for (x <- v) this(x._1) = x._2 / b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def /= (s: Long): SparseVectorL = { for (x <- v) this(x._1) = x._2 / s; this }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Long): SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        if (s =~ 0l) c.set(1l)
        else for (x <- v) c(x._1) = x._2 ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Long): SparseVectorL = 
    {
        if (s =~ 0l) set (1l) else for (x <- v) this(x._1) = x._2 ~^ s
        this
    } // ~^= 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        for (x <- v) c(x._1) = 1l / x._2
        c
    } // recip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        for (x <- v) c(x._1) = ABS (x._2)
        c        
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */ 
    def sum: Long = 
    {
        var sum = 0l
        for (x <- v) sum += x._2
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the absolute value of the elements of 'this' vector.
     */
    def sumAbs: Long = 
    {
        var sum = 0l
        for (x <- v) sum += ABS (x._2)
        sum
    } // sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): Long = sum - this(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: Long = 
    {    
        var sum = 0l
        for (x <- v) sum += MAX (x._2, 0l)
        sum  
    } // sumPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish the rank order of the elements in 'self' vector, e.g.,
     *  (8.0, 2.0, 4.0, 6.0) is (3, 0, 1, 2).
     */
    def rank: VectorI = VectorI (iqsort (this().toArray))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of 'this' vector from left to right (e.g., create a
     *  CDF from a pmf).  Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        var sum = 0l
        for (x <- v) { sum += x._2; c(x._1) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: SparseVectorL = this * (1l / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: SparseVectorL = this * (1l / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: SparseVectorL = this * (1l / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: VectoL): Long = 
    {
        var sum = 0l
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: SparseVectorL): Long = 
    {
        var sum = 0l
        for (x <- v) sum += x._2 * b.v.getOrElse (x._1, 0l)
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: VectoL): Long = 
    {
        var sum = 0l
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // ⦁
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: SparseVectorL): Long = 
    {
        var sum = 0l
        for (x <- v) sum += x._2 * b.v.getOrElse(x._1, 0l)
        sum
    } // ⦁
     
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: Long =
    {  
        var sum = 0l
        for ( x <- v ) sum += ABS (x._2)
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Long =
    {
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 > c)) c = x._2
        c
    } // max
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectoL): SparseVectorL =
    { 
        val c = new SparseVectorL (dim)
        for (i <- range) { 
            val x = this(i)
            val y = b(i)
            c(i) = if (y > x) y else x 
        } // for
        c 
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): Long =
    {     
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 < c)) c = x._2
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectoL): SparseVectorL =
    {
        val c = new SparseVectorL (dim)
        for (i <- range) { 
            val x = this(i) 
            val y = b(i)
            c(i) = if (y < x) y else x 
        } // for
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of 'this' vector (index of maximum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (e: Int = dim): Int =
    {
        var i = 0
        for (x <- v if (x._1 < e && x._2 > this(i))) i = x._1
        i
    } // argmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of 'this' vector (index of maximum element).
     *  @param s  the starting index (inclusive) for the search
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (s: Int, e: Int): Int =
    {
        var i = s
        for (x <- v if (s < x._1 && x._1 < e && x._2 > this(i))) i = x._1
        i
    } // argmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of 'this' vector (index of minimum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (e: Int = dim): Int =
    {
        var i = 0
        for (x <- v if (x._1 < e && x._2 < this(i))) i = x._1
        i
    } // argmin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of 'this' vector (index of minimum element).
     *  @param s  the starting index (inclusive) for the search
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (s: Int, e: Int): Int =
    {
        var i = s
        for (x <- v if (s < x._1 && x._1 < e && x._2 < this(i))) i = x._1
        i
    } // argmin
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument minimum of 'this' vector (-1 if its not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int =
    {
        val j = argmin (e); if (v(j) < 0l) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > 0l) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int = { for (x <- v if x._2 < 0l) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int = { for (x <- v if x._2 > 0l) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: Long, e: Int = dim): Int =
    {        
        if (x =~ 0l) { for (i <- range if this(i) =~ 0l) return i; -1 }
        for (y <- v if y._1 < e && y._2 == x) return y._1; -1 
    } // indexOf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: (Long) => Boolean): Int =
    {
        for (i <- range if p(this(i))) return i; -1
    } // indexWhere

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (x <- v if x._2 < 0l) count += 1
        count
    } // countNeg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (x <- v if x._2 > 0l) count += 1
        count
    } // countPos

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of zero elements in 'this' vector.
     */
    def countZero: Int =
    {
        var count = 0
        for (x <- v if x._2 == 0l) count += 1
        count
    } // countZero

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new vector consisting of the distinct elements from 'this' vector.
     */
    def distinct: SparseVectorL = 
    {
        val us = new SparseVectorL (this); us.sort ()
        var c1 = ArrayBuffer [Long]()     
        c1 += us(0)  
        for (i <- 1 until dim) { val x = us(i); if (x != us(i-1)) c1 += x }
        SparseVectorL(c1)       
    } // distinct
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int =
    {
        var count = 1
        val us = new SparseVectorL (this); us.sort ()                // sorted vector
        for (i <- 1 until dim if us(i) != us(i-1)) count += 1
        count
    } // distinct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate 'pred' holds for some element in 'this' vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: (Long) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: Long): Boolean = v.values.exists (_ == x)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the order of the elements in 'this' vector.
     */
    def reverse (): SparseVectorL = 
    {        
         SparseVectorL (for (i <- (0 until dim).reverse) yield this(i))
    } // reverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' vector is in sorted (ascending) order.
     */
    def isSorted: Boolean = (new SortingL (this().toArray)).isSorted
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort () 
    {                  
        val neg = negative; quickSort(neg)
        val pos = positive; quickSort(pos)  
        v =  SparseVectorL (neg ++ Array.fill(dim - (neg.length + pos.length))(0l) ++ pos).v          
    } // sort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 ()
    {                  
        val neg = negative; qsort2 (neg)
        val pos = positive; qsort2 (pos)  
        v =  SparseVectorL (pos ++ Array.fill (dim - (neg.length + pos.length))(0l) ++ neg).v          
    } // sort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)
    {      
         val (x, y) = (this(i), this(j))
         if (x =~ 0l && y =~ 0l) return      
         this(i) = y; this(j) = x           
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector 'b' is at least as long as 'this' vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: SparseVectorL): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {   
        for ( x <- v if x._2 < 0l) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: SparseVectorL] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [SparseVectorL] (i)
            if      (ge && (v(i) compare b_i) < 0) ge = false
            else if (le && (v(i) compare b_i) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether 'this' vector equals vector 'b..
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
        b.isInstanceOf [SparseVectorL] && (v equals b.asInstanceOf [SparseVectorL].v)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode (): Int = v.hashCode
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' sparse vector to a dense vector.
     */
    def toDense: VectorL = VectorL (for (i <- range) yield this(i))
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a `String`.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("SparseVectorL(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            if (v contains i) sb.append (fString.format (v(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a `String`, showing the zero elements as well,
     */ 
    def toString2: String = 
    {
        var sb = new StringBuilder ("SparseVectorL(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (this(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString2
    
} // SparseVectorL class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorL` object is the companion object for the `SparseVectorL` class.
 */
object SparseVectorL
{
    /** Shorthand type definition for sparse vector
     */
    type RowMap = SortedLinkedHashMap [Int, Long]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorL` from one or more values (repeated values `Long`*).
     *  @param x   the first `Long` number
     *  @param xs  the rest of the `Long` numbers
     */
    def apply (x: Long, xs: Long*): SparseVectorL =
    {
        val c = new SparseVectorL (1 + xs.length)
        c(0) = x
        for (i <- 1 until c.dim) c(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorL` from a sequence of `Long`s.
     *  @param xs  the sequence of the Long numbers
     */
    def apply (xs: Seq [Long]): SparseVectorL =
    {
        val c = new SparseVectorL (xs.length)
        for (i <- 0 until c.dim) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorL` from one or more values (repeated values `String`*).
     *  @param x   the first `String`
     *  @param xs  the rest of the `String`s
     */
    def apply (x: String, xs: String*): SparseVectorL =
    {
        val c = new SparseVectorL (1 + xs.length)
        c(0)  = x.toLong
        for (i <- 0 until c.dim-1) c(i+1) = xs(i).toLong
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorL` from an array of `String`s.
     *  @param xs  the array of the `String`s
     */
    def apply (xs: Array [String]): SparseVectorL =
    {
        val c = new SparseVectorL (xs.length)
        for (i <- c.range) c(i) = xs(i).toLong
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorL` from an array of `String`s, skipping the first 'skip'
     *  elements.  If an element is non-numeric, use its hashcode.
     *  FIX:  Might be better to map non-numeric `String`s to ordinal values.
     *  @param xs    the array of the `String`s
     *  @param skip  the number of elements at the beginning to skip (e.g., id column)
     */
    def apply (xs: Array [String], skip: Int): SparseVectorL =
    {
        val c = new SparseVectorL (xs.length - skip)
        for (i <- skip until xs.length) {
            c.v(i - skip) = if (xs(i) matches "[\\-\\+]?\\d*(\\.\\d+)?") xs(i).toLong else xs(i).hashCode ()
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part
     */
    def ++ (b: Long, u: SparseVectorL): SparseVectorL =
    {
        val c = new SparseVectorL (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i - 1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `SparseVectorL` containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): SparseVectorL =
    {
        val c = new SparseVectorL (end - start)
        for (i <- c.range) c.v(i) = (start + i).toLong
        c
    } // range

} // SparseVectorL object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest` object tests the operations provided by `SparseVectorL`.
 *  > run-main scalation.linalgebra.SparseVectorDTest
 */
object SparseVectorLTest extends App
{   
    var x: SparseVectorL = null
    var y: SparseVectorL = null

    for (l <- 1 to 4) {
        println ("\n\tTest SparseVectorL on vectors of dim " + l)
        x = new SparseVectorL (l)
        y = new SparseVectorL (l)
        x.set (2)
        y.set (3)

        // test vector op scalar
        println ("x + 4    = " + (x + 4))
        println ("x - 4    = " + (x - 4))
        println ("x * 4    = " + (x * 4))
        println ("x / 4    = " + (x / 4))
        println ("x ~^ 4   = " + (x ~^ 4))

        // test vector op vector
        println ("x + y    = " + (x + y))
        println ("x - y    = " + (x - y))
        println ("x * y    = " + (x * y))
        println ("x / y    = " + (x / y))

        println ("x.min    = " + x.min ())
        println ("x.max    = " + x.max ())
        println ("x.sum    = " + x.sum)
        println ("x.sumNE  = " + x.sumNE (0))
        println ("x dot y  = " + (x dot y))
        println ("x ∙ y    = " + (x ∙ y))
        println ("x.normSq = " + x.normSq)
        println ("x.norm   = " + x.norm)
        println ("x < y    = " + (x < y))
    } // for

    println ("hashCode (" + x + ") = " + x.hashCode ())
    println ("hashCode (" + y + ") = " + y.hashCode ())

    val z = SparseVectorL ("1", "2", "3", "4")
    println ("z = " + z)
    println ("z.map (_ * 2)    = " + z.map ((e: Long) => e * 2))
    println ("z.filter (_ > 2) = " + z.filter (_ > 2))

} // SparseVectorDTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest2` object tests the dot product operation provided by `SparseVectorL`.
 *  > run-main scalation.linalgebra.SparseVectorDTest2
 */
object SparseVectorLTest2 extends App
{
    val r  = scala.util.Random  
    val v1 = new VectorL (1000000)  
    val v2 = new VectorL (1000000)

    for (x <- 0 until 10) {    
        var cnt = 0   
        while (cnt < 1000000) {
            val x  = r.nextInt (50).toLong + 1
            val x1 = r.nextInt (90)
            if(x1 == 1) v1(cnt) = x
            else v1(cnt) = 0l  
            val z  = r.nextInt (50).toLong + 1
            val z1 = r.nextInt (90)
            if(z1 == 1) v2(cnt) = z
            else v2(cnt) = 0l
            cnt += 1
        } // while   

    var z1 = SparseVectorL (v1())
    var z2 = SparseVectorL (v2())
  
    println (v1.dot (v2))
    println (z1.⦁(z2))
  } // for
    
} // SparseVectorDTest2
