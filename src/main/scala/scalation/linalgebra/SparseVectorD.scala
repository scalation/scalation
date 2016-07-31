
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vishnu Gowda Harish
 *  @version 1.2
 *  @date    Fri Jan 29 15:43:08 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.{breakOut, Traversable}
import scala.collection.mutable.{ArrayBuffer, IndexedSeq}
import scala.util.Sorting.quickSort

import scala.math.{abs => ABS, max => MAX, sqrt}

import scalation.math.double_exp
import scalation.util.{Error, SortedLinkedHashMap}
import scalation.util.SortingD
import scalation.util.SortingD.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorD` class stores and operates on Numeric Vectors of base type `Double`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim_  the dimension/size of the vector
 *  @param v     the `SortedLinkedHashMap` used to store vector elements
 */
class SparseVectorD (val dim_ : Int,
           protected var v:     SparseVectorD.RowMap = null)
      extends VectoD
//    extends Traversable [Double] with PartiallyOrdered [SparseVectorD] with Vec with Error with Serializable
{
    /** Dimension
     */
    lazy val dim = dim_
    
    if (v == null) v = new SparseVectorD.RowMap ()
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: VectoD) { this (u.dim); for (i <- range) this(i) = u(i) } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param iv  the tuple containing (index, value)
     *  @param dm  the dimension for the new vector
     */
    def this (iv: Tuple2 [Int, Double], dm: Int) { this (dm); this(iv._1) = iv._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' vector.
     */
    def copy: SparseVectorD = new SparseVectorD (dim, v.clone.asInstanceOf [SparseVectorD.RowMap])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def zero (size: Int = dim): SparseVectorD = new SparseVectorD (size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def one (size: Int = dim): SparseVectorD = { val sv = new SparseVectorD (size); sv.set (1.0); sv }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): SparseVectorD = new SparseVectorD ((j, 1.0), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): SparseVectorD = new SparseVectorD ((j, -1.0), size) 
      
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Double = if (i >= dim) throw new IndexOutOfBoundsException else v.getOrElse (i, 0.0)
                                   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): SparseVectorD = slice (r.start, r.end)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire array.
     */
    def apply (): IndexedSeq [Double] =
    {
        (for (i <- range) yield v.getOrElse (i, 0.0))(breakOut)
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Double) 
    { 
        if (i >= dim) throw new IndexOutOfBoundsException
        if (x =~ 0.0) v.remove(i) else v(i) = x        
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Double)  { for (i <- r) this(i) = x } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectoD) { for (i <- r) this(i) = u(i - r.start) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Double => U) { var i = 0; while (i < dim) { f (this(i)); i += 1 } } 
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: Double) { for (i <- range) this(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in array 'u'.
     *  @param u  the array of values to be assigned
     */
    def set (u: Seq [Double]) { for (i <- range) this(i) = u(i) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorD` into a `VectorI`.
     */
    def toInt: VectorI = 
    {
        val c = new VectorI (dim)
        for (i <- range) c(i) = this(i).toInt
        c
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorD` into a `VectorL`.
      */
    def toLong: VectorL =
    { 
        val c = new VectorL (dim)
        for (i <- range) c(i) = this(i).toLong
        c
    } // toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorD` into a `VectorD`.
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
    def expand (more: Int = dim): SparseVectorD =
    {
        if (more < 1) this       // no change
        else {
            val sv = new SparseVectorD (dim + more)
            sv.v ++= v
            sv
        } // if
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's entire array of stored (i.e., non-zero) elements.
     */
    private def nonZero: Array [Double] = v.values.toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's positive elements as an array.
     */
    private def positive: Array [Double] = v.values.filter (_ > 0.0).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's negatives elements as an array.
     */
    private def negative: Array [Double] = v.values.filter (_ < 0.0).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    override def filter (p: Double => Boolean): SparseVectorD =
    { 
        SparseVectorD (for (i <- range if p (this(i)) ) yield v(i))
    } // filter
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: Double => Boolean): IndexedSeq [Int] =
    {
        (for (i <- range if p (this(i))) yield i)(breakOut)
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: Double => Double): SparseVectorD = SparseVectorD (this ().map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): SparseVectorD =
    {
        null    // FIX: new SparseVectorD (till - from, v.slice (from, till))
    } // slice
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): SparseVectorD =
    {
        SparseVectorD (for (i <- 0 until basis.length) yield this(basis(i)))
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectoD): SparseVectorD =
    {
        val c = new SparseVectorD (dim + b.dim)
        for (i <- c.range) c(i) = if (i < dim) this(i) else b(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: Double): SparseVectorD =
    {
        val c = new SparseVectorD (dim + 1)
        for (i <- c.range) c(i) = if (i < dim) this(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: VectoD): SparseVectorD = { SparseVectorD (for ( i <- range) yield this(i) + b(i)) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: Double): SparseVectorD =
    {
        if (s =~ 0.0) this 
        else SparseVectorD (for (i <- range) yield this(i) + s)       
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._2' only at position 's._1'.
     *  @param s  the (scalar, position) to add
     */
    def + (s: Tuple2 [Int, Double]): SparseVectorD =
    {
        if (s._1 =~ 0.0) this
        else {
            val c = new SparseVectorD (this)
            c(s._1) += s._2
            c
        } // if    
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: VectoD): SparseVectorD = { for (i <- range) this(i) += b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: Double): SparseVectorD = { for (i <- range) this(i) += s; this } 
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        for (x <- v) c(x._1) = -x._2 
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: VectoD): SparseVectorD = { SparseVectorD (for ( i <- range) yield this(i) - b(i)) }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: Double): SparseVectorD =
    {
        if (s =~ 0.0) this 
        else SparseVectorD (for (i <- range) yield this(i) - s)       
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._2' only at position 's._1'.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: Tuple2 [Int, Double]): SparseVectorD =
    {
        if (s._1 =~ 0.0) this
        else {
            val c = new SparseVectorD (this)
            c(s._1) -= s._2
            c
        } // if    
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to add
     */
    def -= (b: VectoD): SparseVectorD = { for (i <- range) this(i) -= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to add
     */
    def -= (s: Double): SparseVectorD = { for (i <- range) this(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: VectoD): SparseVectorD = 
    {     
        val c = new SparseVectorD (dim)
        for (x <- v) c(x._1) = x._2 * b(x._1) 
        c    
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        if( s !=~ 0.0 ) for (x <- v) c(x._1) = x._2 * s  
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def *= (b: VectoD): SparseVectorD = { for (x <- v) this(x._1) = x._2 * b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def *= (s: Double): SparseVectorD = { for (x <- v) this(x._1) = x._2 * s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectoD): SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        for (x <- v) c(x._1) = x._2 / b(x._1) 
        c    
    } // /
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: Double): SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        for (x <- v) c(x._1) = x._2 / s  
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def /= (b: VectoD): SparseVectorD = { for (x <- v) this(x._1) = x._2 / b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def /= (s: Double): SparseVectorD = { for (x <- v) this(x._1) = x._2 / s; this }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Double): SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        if (s =~ 0.0) c.set(1.0)
        else for (x <- v) c(x._1) = x._2 ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Double): SparseVectorD = 
    {
        if (s =~ 0.0) set (1.0) else for (x <- v) this(x._1) = x._2 ~^ s
        this
    } // ~^= 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        for (x <- v) c(x._1) = 1.0 / x._2
        c
    } // recip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        for (x <- v) c(x._1) = ABS (x._2)
        c        
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */ 
    def sum: Double = 
    {
        var sum = 0.0
        for (x <- v) sum += x._2
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the absolute value of the elements of 'this' vector.
     */
    def sumAbs: Double = 
    {
        var sum = 0.0
        for (x <- v) sum += ABS (x._2)
        sum
    } // sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): Double = sum - this(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: Double = 
    {    
        var sum = 0.0
        for (x <- v) sum += MAX (x._2, 0.0)
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
    def cumulate: SparseVectorD =
    {
        val c = new SparseVectorD (dim)
        var sum = 0.0
        for (x <- v) { sum += x._2; c(x._1) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: SparseVectorD = this * (1.0 / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: SparseVectorD = this * (1.0 / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: SparseVectorD = this * (1.0 / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: VectoD): Double = 
    {
        var sum = 0.0
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: SparseVectorD): Double = 
    {
        var sum = 0.0
        for (x <- v) sum += x._2 * b.v.getOrElse (x._1, 0.0)
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: VectoD): Double = 
    {
        var sum = 0.0
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // ⦁
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: SparseVectorD): Double = 
    {
        var sum = 0.0
        for (x <- v) sum += x._2 * b.v.getOrElse(x._1, 0.0)
        sum
    } // ⦁
     
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: Double =
    {  
        var sum = 0.0
        for ( x <- v ) sum += ABS (x._2)
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Double =
    {
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 > c)) c = x._2
        c
    } // max
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectoD): SparseVectorD =
    { 
        val c = new SparseVectorD (dim)
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
    def min (e: Int = dim): Double =
    {     
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 < c)) c = x._2
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectoD): SparseVectorD =
    {
        val c = new SparseVectorD (dim)
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
    /** Return the argument minimum of 'this' vector (-1 if its not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int =
    {
        val j = argmin (e); if (v(j) < 0.0) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > 0.0) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int = { for (x <- v if x._2 < 0.0) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int = { for (x <- v if x._2 > 0.0) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: Double, e: Int = dim): Int =
    {        
        if (x =~ 0.0) { for (i <- range if this(i) =~ 0.0) return i; -1 }
        for (x <- v if (x._1 < e && x._2 == x)) return x._1; -1 
    } // indexOf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: (Double) => Boolean): Int =
    {
        for (i <- range if p(this(i))) return i; -1
    } // indexWhere

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (x <- v if x._2 < 0.0) count += 1
        count
    } // countNeg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (x <- v if x._2 > 0.0) count += 1
        count
    } // countPos
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new vector consisting of the distinct elements from 'this' vector.
     */
    def distinct: SparseVectorD = 
    {
        val us = new SparseVectorD (this); us.sort ()
        var c1 = ArrayBuffer [Double]()     
        c1 += us(0)  
        for (i <- 1 until dim) { val x = us(i); if (x != us(i-1)) c1 += x }
        SparseVectorD(c1)       
    } // distinct
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int =
    {
        var count = 1
        val us = new SparseVectorD (this); us.sort ()                // sorted vector
        for (i <- 1 until dim if us(i) != us(i-1)) count += 1
        count
    } // distinct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate 'pred' holds for some element in 'this' vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: (Double) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: Double): Boolean = v.values.exists (_ == x)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the order of the elements in 'this' vector.
     */
    def reverse (): SparseVectorD = 
    {        
         SparseVectorD (for (i <- (0 until dim).reverse) yield this(i))
    } // reverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' vector is in sorted (ascending) order.
     */
    def isSorted: Boolean = (new SortingD (this().toArray)).isSorted
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort () 
    {                  
        val neg = negative; quickSort(neg)
        val pos = positive; quickSort(pos)  
        v =  SparseVectorD (neg ++ Array.fill(dim - (neg.length + pos.length))(0.0) ++ pos).v          
    } // sort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 ()
    {                  
        val neg = negative; qsort2 (neg)
        val pos = positive; qsort2 (pos)  
        v =  SparseVectorD (pos ++ Array.fill (dim - (neg.length + pos.length))(0.0) ++ neg).v          
    } // sort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)
    {      
         val (x, y) = (this(i), this(j))
         if (x =~ 0.0 && y =~ 0.0) return      
         this(i) = y; this(j) = x           
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector 'b' is at least as long as 'this' vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: SparseVectorD): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {   
        for ( x <- v if x._2 < 0.0) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: SparseVectorD] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [SparseVectorD] (i)
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
        b.isInstanceOf [SparseVectorD] && (v equals b.asInstanceOf [SparseVectorD].v)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode: Int = v.hashCode
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' sparse vector to a dense vector.
     */
    def toDense: VectorD = VectorD (for (i <- range) yield this(i))
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a `String`.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("SparseVectorD(")
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
        var sb = new StringBuilder ("SparseVectorD(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (this(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString2
    
} // SparseVectorD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorD` object is the companion object for the `SparseVectorD` class.
 */
object SparseVectorD
{
    /** Shorthand type definition for sparse vector
     */
    type RowMap = SortedLinkedHashMap [Int, Double]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorD` from one or more values (repeated values `Double`*).
     *  @param x   the first `Double` number
     *  @param xs  the rest of the `Double` numbers
     */
    def apply (x: Double, xs: Double*): SparseVectorD =
    {
        val c = new SparseVectorD (1 + xs.length)
        c(0) = x
        for (i <- 1 until c.dim) c(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorD` from a sequence of `Double`s.
     *  @param xs  the sequence of the Double numbers
     */
    def apply (xs: Seq [Double]): SparseVectorD =
    {
        val c = new SparseVectorD (xs.length)
        for (i <- 0 until c.dim) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorD` from one or more values (repeated values `String`*).
     *  @param x   the first `String`
     *  @param xs  the rest of the `String`s
     */
    def apply (x: String, xs: String*): SparseVectorD =
    {
        val c = new SparseVectorD (1 + xs.length)
        c(0)  = x.toDouble
        for (i <- 0 until c.dim-1) c(i+1) = xs(i).toDouble
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorD` from an array of `String`s.
     *  @param xs  the array of the `String`s
     */
    def apply (xs: Array [String]): SparseVectorD =
    {
        val c = new SparseVectorD (xs.length)
        for (i <- c.range) c(i) = xs(i).toDouble
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorD` from an array of `String`s, skipping the first 'skip'
     *  elements.  If an element is non-numeric, use its hashcode.
     *  FIX:  Might be better to map non-numeric `String`s to ordinal values.
     *  @param xs    the array of the `String`s
     *  @param skip  the number of elements at the beginning to skip (e.g., id column)
     */
    def apply (xs: Array [String], skip: Int): SparseVectorD =
    {
        val c = new SparseVectorD (xs.length - skip)
        for (i <- skip until xs.length) {
            c.v(i - skip) = if (xs(i) matches "[\\-\\+]?\\d*(\\.\\d+)?") xs(i).toDouble else xs(i).hashCode ()
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part
     */
    def ++ (b: Double, u: SparseVectorD): SparseVectorD =
    {
        val c = new SparseVectorD (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i - 1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `SparseVectorD` containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): SparseVectorD =
    {
        val c = new SparseVectorD (end - start)
        for (i <- c.range) c.v(i) = (start + i).toDouble
        c
    } // range

} // SparseVectorD object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest` object tests the operations provided by `SparseVectorD`.
 *  > run-main scalation.linalgebra.SparseVectorDTest
 */
object SparseVectorDTest extends App
{   
    var x: SparseVectorD = null
    var y: SparseVectorD = null

    for (l <- 1 to 4) {
        println ("\n\tTest SparseVectorD on vectors of dim " + l)
        x = new SparseVectorD (l)
        y = new SparseVectorD (l)
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

    val z = SparseVectorD ("1", "2", "3", "4")
    println ("z = " + z)
    println ("z.map (_ * 2)    = " + z.map ((e: Double) => e * 2))
    println ("z.filter (_ > 2) = " + z.filter (_ > 2))

} // SparseVectorDTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest2` object tests the dot product operation provided by `SparseVectorD`.
 *  > run-main scalation.linalgebra.SparseVectorDTest2
 */
object SparseVectorDTest2 extends App
{
    val r  = scala.util.Random  
    val v1 = new VectorD (1000000)  
    val v2 = new VectorD (1000000)

    for (x <- 0 until 10) {    
        var cnt = 0   
        while(cnt < 1000000) {
            val x  = r.nextInt (50).toDouble + 1
            val x1 = r.nextInt (90)
            if(x1 == 1) v1(cnt) = x
            else v1(cnt) = 0.0  
            val z  = r.nextInt (50).toDouble + 1
            val z1 = r.nextInt (90)
            if(z1 == 1) v2(cnt) = z
            else v2(cnt) = 0.0
            cnt += 1
        } // while   

    var z1 = SparseVectorD (v1())
    var z2 = SparseVectorD (v2())
  
    println (v1.dot (v2))
    println (z1.⦁(z2))
  } // for
    
} // SparseVectorDTest2
