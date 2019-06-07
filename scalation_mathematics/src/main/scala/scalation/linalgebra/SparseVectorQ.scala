
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vishnu Gowda Harish
 *  @version 1.6
 *  @date    Fri Jan 29 15:43:08 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.{breakOut, Traversable}
import scala.collection.mutable.{ArrayBuffer, IndexedSeq}
import scala.util.Sorting.quickSort

import scalation.math.Rational.{abs => ABS, max => MAX, _}

import scalation.math.{Rational, noRational}
import scalation.util.{Error, SortedLinkedHashMap}
import scalation.util.SortingQ
import scalation.util.SortingQ.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorQ` class stores and operates on Numeric Vectors of base type `Rational`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim_  the dimension/size of the vector
 *  @param v     the `SortedLinkedHashMap` used to store vector elements
 */
class SparseVectorQ (val dim_ : Int,
           protected var v:     SparseVectorQ.RowMap = null)
      extends VectoQ
//    extends Traversable [Rational] with PartiallyOrdered [SparseVectorQ] with Vec with Error with Serializable
{
    /** Dimension
     */
    lazy val dim = dim_
    
    if (v == null) v = new SparseVectorQ.RowMap ()
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: VectoQ) { this (u.dim); for (i <- range) this(i) = u(i) } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param iv  the tuple containing (index, value)
     *  @param dm  the dimension for the new vector
     */
    def this (iv: (Int, Rational), dm: Int) { this (dm); this(iv._1) = iv._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' vector.
     */
    def copy: SparseVectorQ = new SparseVectorQ (dim, v.clone.asInstanceOf [SparseVectorQ.RowMap])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def zero (size: Int = dim): SparseVectorQ = new SparseVectorQ (size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def one (size: Int = dim): SparseVectorQ = { val sv = new SparseVectorQ (size); sv.set (_1); sv }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): SparseVectorQ = new SparseVectorQ ((j, _1), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): SparseVectorQ = new SparseVectorQ ((j, -_1), size) 
      
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Rational = if (i >= dim) throw new IndexOutOfBoundsException else v.getOrElse (i, _0)
                                   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): SparseVectorQ = slice (r.start, r.end)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire array.
     */
    def apply (): IndexedSeq [Rational] =
    {
        (for (i <- range) yield v.getOrElse (i, _0))(breakOut)
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Rational) 
    { 
        if (i >= dim) throw new IndexOutOfBoundsException
        if (x =~ _0) v.remove(i) else v(i) = x        
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Rational)  { for (i <- r) this(i) = x } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectoQ) { for (i <- r) this(i) = u(i - r.start) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Rational => U) { var i = 0; while (i < dim) { f (this(i)); i += 1 } } 
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: Rational) { for (i <- range) this(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in array 'u'.
     *  @param u  the array of values to be assigned
     */
    def set (u: Seq [Rational]) { for (i <- range) this(i) = u(i) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorQ` into a `VectorI`.
     */
    def toInt: VectorI = 
    {
        val c = new VectorI (dim)
        for (i <- range) c(i) = this(i).toInt
        c
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorQ` into a `VectorL`.
      */
    def toLong: VectorL =
    { 
        val c = new VectorL (dim)
        for (i <- range) c(i) = this(i).toLong
        c
    } // toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SparseVectorQ` into a `VectorD`.
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
    def expand (more: Int = dim): SparseVectorQ =
    {
        if (more < 1) this       // no change
        else {
            val sv = new SparseVectorQ (dim + more)
            sv.v ++= v
            sv
        } // if
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's entire array of stored (i.e., non-zero) elements.
     */
    private def nonZero: Array [Rational] = v.values.toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's positive elements as an array.
     */
    private def positive: Array [Rational] = v.values.filter (_ > _0).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's negatives elements as an array.
     */
    private def negative: Array [Rational] = v.values.filter (_ < _0).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    override def filter (p: Rational => Boolean): SparseVectorQ =
    { 
        SparseVectorQ (for (i <- range if p (this(i)) ) yield v(i))
    } // filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter out the missing elements of 'this' vector based on the predicate
     *  that 'e != noRational'.
     */
    def filterMissing: SparseVectorQ = filter (_ != noRational)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: Rational => Boolean): IndexedSeq [Int] =
    {
        (for (i <- range if p (this(i))) yield i)(breakOut)
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector and vector 'v2' based on the binary
     *  predicate 'p', returning the index positions for both vectors.
     *  @param v2  the other vector to compare with
     *  @param p   the binary predicate (`Boolean` function) to apply
     */
    def filterPos2 (v2: VectoQ, p: (Rational, Rational) => Boolean): IndexedSeq [(Int, Int)] =
    {
        var result = IndexedSeq [(Int, Int)] ()
        for (i <- range; j <- v2.range if p(this(i), v2(j))) result = result :+ (i, j)
        result
    } // filterPos2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: Rational => Rational): SparseVectorQ = SparseVectorQ (this ().map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): SparseVectorQ =
    {
        null    // FIX: new SparseVectorQ (till - from, v.slice (from, till))
    } // slice
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): SparseVectorQ =
    {
        SparseVectorQ (for (i <- 0 until basis.length) yield this(basis(i)))
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectoQ): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim + b.dim)
        for (i <- c.range) c(i) = if (i < dim) this(i) else b(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: Rational): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim + 1)
        for (i <- c.range) c(i) = if (i < dim) this(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: VectoQ): SparseVectorQ = { SparseVectorQ (for ( i <- range) yield this(i) + b(i)) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: Rational): SparseVectorQ =
    {
        if (s =~ _0) this 
        else SparseVectorQ (for (i <- range) yield this(i) + s)       
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._2' only at position 's._1' e.g., 'x + (3, 5.5).
     *  @param s  the (scalar, position) to add
     */
    def + (s: (Int, Rational)): SparseVectorQ =
    {
        if (s._1 =~ _0) this
        else { val c = new SparseVectorQ (this); c(s._1) += s._2; c }
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: VectoQ): SparseVectorQ = { for (i <- range) this(i) += b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: Rational): SparseVectorQ = { for (i <- range) this(i) += s; this } 
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        for (x <- v) c(x._1) = -x._2 
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: VectoQ): SparseVectorQ = { SparseVectorQ (for ( i <- range) yield this(i) - b(i)) }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: Rational): SparseVectorQ =
    {
        if (s =~ _0) this 
        else SparseVectorQ (for (i <- range) yield this(i) - s)       
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._2' only at position 's._1', e.g., 'x - (3, 5.5).
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: (Int, Rational)): SparseVectorQ =
    {
        if (s._1 =~ _0) this
        else { val c = new SparseVectorQ (this); c(s._1) -= s._2; c }
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to subtract
     */
    def -= (b: VectoQ): SparseVectorQ = { for (i <- range) this(i) -= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to subtract
     */
    def -= (s: Rational): SparseVectorQ = { for (i <- range) this(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: VectoQ): SparseVectorQ = 
    {     
        val c = new SparseVectorQ (dim)
        for (x <- v) c(x._1) = x._2 * b(x._1) 
        c    
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: Rational): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        if (s !=~ _0) for (x <- v) c(x._1) = x._2 * s  
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's._2' only at position 's._1', e.g., 'x * (3, 5.5).
     *  @param s  the (scalar, position) to multiply by
     */
    def * (s: (Int, Rational)): SparseVectorQ =
    {
        if (s._1 =~ _0) this
        else { val c = new SparseVectorQ (this); c(s._1) *= s._2; c }
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to multiply by
     */
    def *= (b: VectoQ): SparseVectorQ = { for (x <- v) this(x._1) = x._2 * b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def *= (s: Rational): SparseVectorQ = { for (x <- v) this(x._1) = x._2 * s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectoQ): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        for (x <- v) c(x._1) = x._2 / b(x._1) 
        c    
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: Rational): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        for (x <- v) c(x._1) = x._2 / s  
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's._2' only at position 's._1', e.g., 'x / (3, 5.5).
     *  @param s  the (scalar, position) to divide by
     */
    def / (s: (Int, Rational)): SparseVectorQ =
    {
        if (s._1 =~ _0) this
        else { val c = new SparseVectorQ (this); c(s._1) /= s._2; c }
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to divide by
     */
    def /= (b: VectoQ): SparseVectorQ = { for (x <- v) this(x._1) = x._2 / b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to divide by
     */
    def /= (s: Rational): SparseVectorQ = { for (x <- v) this(x._1) = x._2 / s; this }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Rational): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        if (s =~ _0) c.set(_1)
        else for (x <- v) c(x._1) = x._2 ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Rational): SparseVectorQ = 
    {
        if (s =~ _0) set (_1) else for (x <- v) this(x._1) = x._2 ~^ s
        this
    } // ~^= 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        for (x <- v) c(x._1) = _1 / x._2
        c
    } // recip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        for (x <- v) c(x._1) = ABS (x._2)
        c        
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */ 
    def sum: Rational = 
    {
        var sum = _0
        for (x <- v) sum += x._2
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): Rational = sum - this(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: Rational = 
    {    
        var sum = _0
        for (x <- v) sum += MAX (x._2, _0)
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
    def cumulate: SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
        var sum = _0
        for (x <- v) { sum += x._2; c(x._1) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: SparseVectorQ = this * (_1 / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: SparseVectorQ = this * (_1 / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: SparseVectorQ = this * (_1 / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: VectoQ): Rational = 
    {
        var sum = _0
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: SparseVectorQ): Rational = 
    {
        var sum = _0
        for (x <- v) sum += x._2 * b.v.getOrElse (x._1, _0)
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: VectoQ): Rational = 
    {
        var sum = _0
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // ⦁
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: SparseVectorQ): Rational = 
    {
        var sum = _0
        for (x <- v) sum += x._2 * b.v.getOrElse(x._1, _0)
        sum
    } // ⦁
     
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: Rational =
    {  
        var sum = _0
        for (x <- v) sum += ABS (x._2)
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Rational =
    {
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 > c)) c = x._2
        c
    } // max
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectoQ): SparseVectorQ =
    { 
        val c = new SparseVectorQ (dim)
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
    def min (e: Int = dim): Rational =
    {     
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 < c)) c = x._2
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectoQ): SparseVectorQ =
    {
        val c = new SparseVectorQ (dim)
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
        val j = argmin (e); if (v(j) < _0) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > _0) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int = { for (x <- v if x._2 < _0) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int = { for (x <- v if x._2 > _0) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: Rational, e: Int = dim): Int =
    {        
        if (x =~ _0) { for (i <- range if this(i) =~ _0) return i; -1 }
        for (y <- v if y._1 < e && y._2 == x) return y._1; -1 
    } // indexOf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: (Rational) => Boolean): Int =
    {
        for (i <- range if p(this(i))) return i; -1
    } // indexWhere

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (x <- v if x._2 < _0) count += 1
        count
    } // countNeg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (x <- v if x._2 > _0) count += 1
        count
    } // countPos

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of zero elements in 'this' vector.
     */
    def countZero: Int =
    {
        var count = 0
        for (x <- v if x._2 == _0) count += 1
        count
    } // countZero

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new vector consisting of the distinct elements from 'this' vector.
     */
    def distinct: SparseVectorQ = 
    {
        val us = new SparseVectorQ (this); us.sort ()
        var c1 = ArrayBuffer [Rational]()     
        c1 += us(0)  
        for (i <- 1 until dim) { val x = us(i); if (x != us(i-1)) c1 += x }
        SparseVectorQ(c1)       
    } // distinct
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int =
    {
        var count = 1
        val us = new SparseVectorQ (this); us.sort ()                // sorted vector
        for (i <- 1 until dim if us(i) != us(i-1)) count += 1
        count
    } // distinct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate 'pred' holds for some element in 'this' vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: (Rational) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: Rational): Boolean = v.values.exists (_ == x)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the order of the elements in 'this' vector.
     */
    def reverse (): SparseVectorQ = 
    {        
         SparseVectorQ (for (i <- (0 until dim).reverse) yield this(i))
    } // reverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' vector is in sorted (ascending) order.
     */
    def isSorted: Boolean = (new SortingQ (this().toArray)).isSorted
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort () 
    {                  
        val neg = negative; quickSort(neg)
        val pos = positive; quickSort(pos)  
        v =  SparseVectorQ (neg ++ Array.fill(dim - (neg.length + pos.length))(_0) ++ pos).v          
    } // sort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 ()
    {                  
        val neg = negative; qsort2 (neg)
        val pos = positive; qsort2 (pos)  
        v =  SparseVectorQ (pos ++ Array.fill (dim - (neg.length + pos.length))(_0) ++ neg).v          
    } // sort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)
    {      
         val (x, y) = (this(i), this(j))
         if (x =~ _0 && y =~ _0) return      
         this(i) = y; this(j) = x           
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector 'b' is at least as long as 'this' vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: SparseVectorQ): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {   
        for ( x <- v if x._2 < _0) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: SparseVectorQ] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [SparseVectorQ] (i)
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
        b.isInstanceOf [SparseVectorQ] && (v equals b.asInstanceOf [SparseVectorQ].v)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode (): Int = v.hashCode
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' sparse vector to a dense vector.
     */
    def toDense: VectorQ = VectorQ (for (i <- range) yield this(i))
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a `String`.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("SparseVectorQ(")
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
        var sb = new StringBuilder ("SparseVectorQ(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (this(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString2
    
} // SparseVectorQ class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorQ` object is the companion object for the `SparseVectorQ` class.
 */
object SparseVectorQ
{
    /** Shorthand type definition for sparse vector
     */
    type RowMap = SortedLinkedHashMap [Int, Rational]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorQ` from one or more values (repeated values `Rational`*).
     *  @param x   the first `Rational` number
     *  @param xs  the rest of the `Rational` numbers
     */
    def apply (x: Rational, xs: Rational*): SparseVectorQ =
    {
        val c = new SparseVectorQ (1 + xs.length)
        c(0) = x
        for (i <- 1 until c.dim) c(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorQ` from a sequence of `Rational`s.
     *  @param xs  the sequence of the Rational numbers
     */
    def apply (xs: Seq [Rational]): SparseVectorQ =
    {
        val c = new SparseVectorQ (xs.length)
        for (i <- 0 until c.dim) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorQ` from one or more values (repeated values `String`*).
     *  @param x   the first `String`
     *  @param xs  the rest of the `String`s
     */
    def apply (x: String, xs: String*): SparseVectorQ =
    {
        val c = new SparseVectorQ (1 + xs.length)
        c(0)  = Rational (x)
        for (i <- 0 until c.dim-1) c(i+1) = Rational (xs(i))
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorQ` from an array of `String`s.
     *  @param xs  the array of the `String`s
     */
    def apply (xs: Array [String]): SparseVectorQ =
    {
        val c = new SparseVectorQ (xs.length)
        for (i <- c.range) c(i) = Rational (xs(i))
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SparseVectorQ` from an array of `String`s, skipping the first 'skip'
     *  elements.  If an element is non-numeric, use its hashcode.
     *  FIX:  Might be better to map non-numeric `String`s to ordinal values.
     *  @param xs    the array of the `String`s
     *  @param skip  the number of elements at the beginning to skip (e.g., id column)
     */
    def apply (xs: Array [String], skip: Int): SparseVectorQ =
    {
        val c = new SparseVectorQ (xs.length - skip)
        for (i <- skip until xs.length) {
            c.v(i - skip) = if (xs(i) matches "[\\-\\+]?\\d*(\\.\\d+)?") Rational (xs(i)) else xs(i).hashCode ()
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part
     */
    def ++ (b: Rational, u: SparseVectorQ): SparseVectorQ =
    {
        val c = new SparseVectorQ (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i - 1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `SparseVectorQ` containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): SparseVectorQ =
    {
        val c = new SparseVectorQ (end - start)
        for (i <- c.range) c.v(i) = (start + i).toRational
        c
    } // range

} // SparseVectorQ object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest` object tests the operations provided by `SparseVectorQ`.
 *  > runMain scalation.linalgebra.SparseVectorDTest
 */
object SparseVectorQTest extends App
{   
    var x: SparseVectorQ = null
    var y: SparseVectorQ = null

    for (l <- 1 to 4) {
        println ("\n\tTest SparseVectorQ on vectors of dim " + l)
        x = new SparseVectorQ (l)
        y = new SparseVectorQ (l)
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

    val z = SparseVectorQ ("1", "2", "3", "4")
    println ("z = " + z)
    println ("z.map (_ * 2)    = " + z.map ((e: Rational) => e * 2))
    println ("z.filter (_ > 2) = " + z.filter (_ > 2))

} // SparseVectorDTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest2` object tests the dot product operation provided by `SparseVectorQ`.
 *  > runMain scalation.linalgebra.SparseVectorDTest2
 */
object SparseVectorQTest2 extends App
{
    val r  = scala.util.Random  
    val v1 = new VectorQ (1000000)  
    val v2 = new VectorQ (1000000)

    for (x <- 0 until 10) {    
        var cnt = 0   
        while (cnt < 1000000) {
            val x  = r.nextInt (50).toRational + 1
            val x1 = r.nextInt (90)
            if(x1 == 1) v1(cnt) = x
            else v1(cnt) = _0  
            val z  = r.nextInt (50).toRational + 1
            val z1 = r.nextInt (90)
            if(z1 == 1) v2(cnt) = z
            else v2(cnt) = _0
            cnt += 1
        } // while   

    var z1 = SparseVectorQ (v1())
    var z2 = SparseVectorQ (v2())
  
    println (v1.dot (v2))
    println (z1.⦁(z2))
  } // for
    
} // SparseVectorDTest2
