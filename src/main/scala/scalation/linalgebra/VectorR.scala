
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import collection.Traversable
import util.Sorting.quickSort

import scalation.math.LongWithExp._
import scalation.math.Rational
import scalation.math.Rational._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorR` class stores and operates on Numeric Vectors of base type Rational.
 *  It follows the framework of VectorN [T] and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
class VectorR (val dim: Int,
     protected var v:   Array [Rational] = null)
      extends Traversable [Rational] with PartiallyOrdered [VectorR] with Error with Serializable
{
    if (v == null) {
        v = Array.ofDim [Rational] (dim)
    } else if (dim != v.length) {
        flaw ("constructor", "dimension is wrong")
    } // if

    /** Range for the storage array
     */
    private val range = 0 until dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector from an array of values.
     *  @param u  the array of values
     */
    def this (u: Array [Rational]) { this (u.length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector u.
     *  @param u  the other vector
     */
    def this (u: VectorR)
    {
        this (u.dim)                               // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of this vector by 'more' elements.
     *  @param factor  the expansion factor
     */
    def expand (more: Int = dim): VectorR =
    {
        if (more < 1) this       // no change
        else          new VectorR (dim + more, Array.concat (v, new Array [Rational] (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): VectorR =
    {
        val c = new VectorR (size)
        for (i <- c.range) c.v(i) = if (i == j) _1 else _0
        c
    } // oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): VectorR =
    {
        val c = new VectorR (size)
        for (i <- c.range) c.v(i) = if (i == j) -_1 else _0
        c
    } // _oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a ramp-up vector of increasing values: 0, 1, 2, ..., size - 1.
     *  @param size  the size of the vector (upper bound = size - 1)
     *
    def ramp (size: Int = dim): VectorR =
    {
        val c = new VectorR (size)
        for (i <- c.range) c.v(i) = Rational (i)
        c
    } // ramp
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a VectorR into a VectorI.
     *  @param u  the vector to convert an integer vector
     *
    def toInt: VectorI =
    {
        val c = new VectorI (dim)
        for (i <- range) c(i) = v(i).toInt
        c
    } // toInt
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's element at the i-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Rational = v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): VectorR = slice (r.start, r.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's entire array.
     */
    def apply (): Array [Rational] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's element at the i-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Rational) { v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Rational) { for (i <- r) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectorR) { for (i <- r) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in this vector to x.
     *  @param x  the value to be assigned
     */
    def set (x: Rational) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in this vector to the values in array u.
     *  @param u  the array of values to be assigned
     */
    def setAll (u: Array [Rational]) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over the vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Rational => U)
    {
        var i = 0    
        while (i < dim) { f (v(i)); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of this vector by applying the mapping function f.
     *  @param f  the function to apply
     */
    def map (f: Rational => Rational): VectorR = new VectorR (this ().map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this vector from to end.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): VectorR = new VectorR (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of this vector corresponding to a index/basis.
     *  @param index  the set of index positions (e.g., 0, 2, 5)
     */
    def select (index: Array [Int]): VectorR =
    {
        val c = new VectorR (index.length)
        for (i <- c.range) c.v(i) = v(index(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and vector b.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectorR): VectorR =
    {
        val c = new VectorR (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and scalar b.
     *  @param b  the scalar to be concatenated
     */
    def ++ (b: Rational): VectorR =
    {
        val c = new VectorR (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and scalar b.
     *  @param b  the scalar to be concatenated
     */
    def ++ (b: Double): VectorR =
    {
        val c = new VectorR (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else Rational fromDouble (b)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and vector b.
     *  @param b  the vector to add
     */
    def + (b: VectorR): VectorR = 
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) + b.v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: Rational): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) + s
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: Double): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) + fromDouble (s)
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s._1 only at position s._2.
     *  @param s  the (scalar, position) to add
     */
    def + (s: Tuple2 [Rational, Int]): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) + s._1 else v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def += (b: VectorR): VectorR = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def += (s: Rational): VectorR = { for (i <- range) v(i) += s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def += (s: Double): VectorR = { for (i <- range) v(i) += fromDouble (s); this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of this vector (unary minus).
     */
    def unary_-(): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = -v(i)
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract vector b.
     *  @param b  the vector to subtract
     */
    def - (b: VectorR): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) - b.v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: Rational): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) - s
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: Double): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) - fromDouble (s)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s._1 only at position s._2.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: Tuple2 [Rational, Int]): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) - s._1 else v(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place vector b.
     *  @param b  the vector to add
     */
    def -= (b: VectorR): VectorR = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place scalar s.
     *  @param s  the scalar to add
     */
    def -= (s: Rational): VectorR = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place scalar s.
     *  @param s  the scalar to add
     */
    def -= (s: Double): VectorR = { for (i <- range) v(i) -= fromDouble (s); this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by vector b.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorR): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) * b.v(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: Rational): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) * fromDouble (s)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this 'row' vector by matrix m.  FIX: not yet implemented
     *  @param m  the matrix to multiply by
     */
    def * (m: Matrir): VectorR = m.t * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def *= (b: VectorR): VectorR = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def *= (s: Rational): VectorR = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def *= (s: Double): VectorR = { for (i <- range) v(i) *= fromDouble (s); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by vector b (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectorR): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) / b.v(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by scalar s.
     *  @param s  the scalar to divide by
     */
    def / (s: Rational): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) / s
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by scalar s.
     *  @param s  the scalar to divide by
     */
    def / (s: Double): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) / fromDouble (s)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def /= (b: VectorR): VectorR = { for (i <- range) v(i) /= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def /= (s: Rational): VectorR = { for (i <- range) v(i) /= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def /= (s: Double): VectorR = { for (i <- range) v(i) /= fromDouble (s); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of this vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Long): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of this vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Rational): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i) ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of this vector to the s-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Long) { for (i <- range) v(i) = v(i) ~^ s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of this vector to the s-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Rational) { for (i <- range) v(i) = v(i) ~^ s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Square each element of this vector.
     */
    def sq: VectorR = this * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector is absolute values.
     */
    def abs: VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = v(i).abs
        c
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector.
     */
    def sum: Rational = v.foldLeft (_0) (_ + _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector skipping the i-th element.
     *  @param i  the index of the element to skip
     */
    def sum_ne (i: Int): Rational = sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of this vector.
     */
    def sum_pos: Rational =
    {
        var sum = _0
        for (i <- range if v(i) > _0) sum += v(i)
        sum
    } // sum_pos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of this vector from left to right (e.g., create a
     *  cdf from a pmf). Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: VectorR =
    {
        val c   = new VectorR (dim)
        var sum = _0
        for (i <- range) { sum += v(i); c.v(i) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so that it sums to one (like a probability vector).
     */
    def normalize: VectorR = this * (_1 / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so its length is one (unit vector).
     */
    def normalizeU: VectorR = this * (_1 / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector to have a maximum of one.
     */
    def normalize1: VectorR = this * (_1 / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of this vector with vector b.
     *  @param b  the other vector
     */
    def dot (b: VectorR): Rational =
    {
        var s =_0 
        for (i <- range) s += v(i) * b.v(i)
        s
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) squared of this vector.
     */
    def normSq: Rational = this dot this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) of this vector.
     */
    def norm: Rational = sqrt (normSq)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of this vector.
     */
    def norm1: Rational =
    {
        var sum = _0
        for (i <- range) sum += v(i).abs
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Rational = 
    {
        var x = v(0)
        for (i <- 1 until e if v(i) > x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectorR): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): Rational = 
    {
        var x = v(0)
        for (i <- 1 until e if v(i) < x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectorR): VectorR =
    {
        val c = new VectorR (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the element with the greatest magnitude in this vector.
     */
    def mag: Rational = Rational.abs (max ()) max Rational.abs (min ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of this vector (index of maximum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (e: Int = dim): Int =
    {
        var j = 0
        for (i <- 1 until e if v(i) > v(j)) j = i
        j
    } // argmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of this vector (index of minimum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (e: Int = dim): Int =
    {
        var j = 0
        for (i <- 1 until e if v(i) < v(j)) j = i
        j
    } // argmin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument minimum of this vector (-1 if its not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int =
    {
        val j = argmin (e); if (v(j) < _0) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of this vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > _0) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in this vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) < _0) return i; -1
    } // firstNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in this vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) > _0) return i; -1
    } // firstPos

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative entries in this vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) < _0) count += 1
        count
    } // countNeg

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive entries in this vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) > _0) count += 1
        count
    } // countPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate pred holds for some element in this vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: (Rational) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether x is contained in this vector.
     *  @param x  the element to be checked
     */
    def contains (x: Rational): Boolean = v contains x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort this vector in-place in non-decreasing order.
     */
    def sort () { quickSort (v)(Rational.ord) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector is at least as long as this vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: VectorR): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range if v(i) < _0) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this vector with vector b.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: VectorR] (b: B)
        (implicit view$1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [VectorR] (i)
            if      (ge && (v(i) compare b_i) < 0) ge = false
            else if (le && (v(i) compare b_i) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether vector this equals vector b.
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
        b.isInstanceOf [VectorR] && (v.deep equals b.asInstanceOf [VectorR].v.deep)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Hash a vector into an integer.  Serves as the default hash function for
     *  vectors.  Warning, collisions may be unavoidable.
     *  @param x  the vector of type Rational to hash
     *
    override def hashCode: Int =
    {
        if (dim > prime.length) flaw ("hash", "not enough primes for computing hash function")
        var accum = 0
        for (i <- range) accum ^= (ceil (v(i).toDouble * prime(i))).toInt
        accum
    } // hashCode
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this rational vector to a String.
     */
    override def toString: String = 
    {
        val sb = new StringBuilder ("VectorR (")
        for (i <- range) { sb.append (v(i)); sb.append (",\t") } 
        sb.replace (sb.length-2, sb.length, ")").mkString
    } // toString
  
} // VectorR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorR` object is the companion object for `Vector` class.
 */
object VectorR
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorR from one or more values (repeated values Rational*).
     *  @param x   the first complex number
     *  @param xs  the rest of the complex numbers.
     */
    def apply (x: Rational, xs: Rational*): VectorR =
    {
        val c = new VectorR (1 + xs.length)
        c(0) = x
        for (i <- 1 until c.dim) c.v(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorR from one or more values (repeated values Double*).
     *  @param x   the first double
     *  @param xs  the rest of the doubles
     */
    def apply (x: Double, xs: Double*): VectorR =
    {
        val c = new VectorR (1 + xs.length)
        c(0)  = fromDouble (x)
        for (i <- 1 until c.dim) c.v(i) = fromDouble (xs(i-1))
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorR from one or more values (repeated values String*).
     *  @param x   the first String
     *  @param xs  the rest of the Strings
     */
    def apply (x: String, xs: String*): VectorR =
    {
        val c = new VectorR (1 + xs.length)
        c(0)  = Rational (x)
        for (i <- 1 until c.dim) c.v(i) = Rational (xs(i-1))
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorR from an array of Strings.
     *  @param xa  the array of the Strings
     */
    def apply (xa: Array [String]): VectorR =
    {
        val c = new VectorR (xa.length)
        for (i <- 0 until c.dim) c.v(i) = Rational (xa(i))
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a VectorR containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): VectorR =
    {
        val c = new VectorR (end - start)
        for (i <- c.range) c.v(i) = Rational (start + i)
        c
    } // range

} // VectorR object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorRTest` object tests the operations provided by `VectorR` class.
 */
object VectorRTest extends App
{
    var x:  VectorR = null
    var y:  VectorR = null
    val z = VectorR (_0, _1)
    println ("z = " + z)
    val c = Rational (4)

    for (l <- 1 to 4) {
        println ("\n\tTest VectorR on complex vectors of dim " + l)
        x = new VectorR (l)
        y = new VectorR (l)
        x.set (Rational (2, 1))
        y.set (Rational (3, 1))
        println ("x + y    = " + (x + y))
        println ("x - y    = " + (x - y))
        println ("x * y    = " + (x * y))
        println ("x * c    = " + (x * c))
        println ("x.min    = " + x.min ())
        println ("x.max    = " + x.max ())
        println ("x.sum    = " + x.sum)
        println ("x.sum_ne = " + x.sum_ne (0))
        println ("x dot y  = " + (x dot y))
        println ("x.normSq = " + x.normSq)
        println ("x.norm   = " + x.norm)
        println ("x < y    = " + (x < y))
    } // for

    println ("hashCode (" + x + ") = " + x.hashCode ())
    println ("hashCode (" + y + ") = " + y.hashCode ())

    val w = VectorR ("1/2", "2/3", "3/4", "4/5")
    println ("w = " + w)

} // VectorRTest

