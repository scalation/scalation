
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.{breakOut, Traversable}
import scala.collection.mutable.{IndexedSeq, WrappedArray}
import scala.util.Sorting.quickSort

import scala.math.{abs => ABS, max => MAX, sqrt}

import scalation.math.double_exp
import scalation.math.ExtremeD.TOL
import scalation.util.Error
import scalation.util.SortingD
import scalation.util.SortingD.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorD` class stores and operates on Numeric Vectors of base type `Double`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
class VectorD (val dim: Int,
     protected var v:   Array [Double] = null)
      extends VectoD
//    extends Traversable [Double] with PartiallyOrdered [VectorD] with Vec with Error with Serializable
{
    if (v == null) {
        v = Array.ofDim [Double] (dim)
    } else if (dim != v.length) {
        flaw ("constructor", "vector dimension is wrong: dim " + dim + " != v.length " + v.length)
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: VectoD) { this (u.dim); for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign 'value' at 'index' position.
     *  @param iv  the tuple containing (index, value)
     *  @param dm  the dimension for the new vector
     */
    def this (iv: Tuple2 [Int, Double], dm: Int) { this (dm); v(iv._1) = iv._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' vector.
     */
    def copy: VectorD = new VectorD (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def zero (size: Int = dim): VectorD = new VectorD (size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def one (size: Int = dim): VectorD = new VectorD (size, Array.fill (size)(1.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): VectorD = new VectorD ((j, 1.0), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): VectorD = new VectorD ((j, -1.0), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Double = v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): VectorD = slice (r.start, r.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire array.
     */
    def apply (): WrappedArray [Double] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Double) { v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Double) { for (i <- r) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectoD) { for (i <- r) v(i) = u(i - r.start) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: Double) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in sequence/array 'u'.
     *  @param u  the sequence/array of values to be assigned
     */
    def set (u: Seq [Double]) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Double => U) { var i = 0; while (i < dim) { f (v(i)); i += 1 } }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectorD` into a `VectorI`.
     */
    def toInt: VectorI = VectorI (v.map (_.toInt))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectorD` into a `VectorL`.
      */
    def toLong: VectorL = VectorL (v.map (_.toLong))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectorD` into a `VectorD`.
     */
    def toDouble: VectorD = VectorD (v.map (_.toDouble))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectorD` into a dense `VectorD`.
     */
    def toDense: VectorD = this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of 'this' vector by 'more' elements.
     *  @param more  the number of new elements to add
     */
    def expand (more: Int = dim): VectorD =
    {
        if (more < 1) this       // no change
        else          new VectorD (dim + more, Array.concat (v, new Array [Double] (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    override def filter (p: Double => Boolean): VectorD = VectorD (v.filter (p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: Double => Boolean): IndexedSeq [Int] =
    {
        (for (i <- range if p (v(i))) yield i)(breakOut)
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the binary predicate 'p',
     *  returning the index positions.
     *  @param v2  the other vector to compare with
     *  @param p   the binary predicate (Boolean function, between two elements) to apply
     */
    def filterPos2 (v2: VectoD, p: (Double, Double) => Boolean): IndexedSeq [(Int, Int)] =
    {
        var result = IndexedSeq [(Int, Int)] ()
        for (i <- range; j <- v2.range if p(v(i), v2(j))) result = result :+ (i, j)
        result
    } // filterPos2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: Double => Double): VectorD = VectorD (v.map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int = dim): VectorD = new VectorD (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split 'this' vector into 'k' arrays of equal sizes (perhaps except for the last one)
     *  @param k  the number of pieces the vector is to be splitted
     */
    def split (k: Int): Array [VectorD] =
    {
        if (k <= 0) flaw ("split", "k must be a positive integer")
        val pieces = Array.ofDim [VectorD] (k)
        val size = dim / k
        for (i <- 0 until k-1) pieces(i) = slice (i*size, (i+1)*size)
        pieces(k-1) = slice ((k-1)*size, dim)
        pieces
    } // split

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set/array of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): VectorD =
    {
        val c = new VectorD (basis.length)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set/vector of index positions (e.g., 0, 2, 5)
     */
    def select (basis: VectoI): VectorD =
    {
        val c = new VectorD (basis.dim)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated (any kind)
     */
    def ++ (b: VectoD): VectorD =
    {
        val c = new VectorD (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated (same kind, more efficient)
     */
    def ++ (b: VectorD): VectorD =
    {
        val c = new VectorD (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: Double): VectorD =
    {
        val c = new VectorD (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add (any kind)
     */
    def + (b: VectoD): VectorD = 
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) + b(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
     */
    def + (b: VectorD): VectorD = 
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) + b.v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) + s
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._2' only at position 's._1'.
     *  @param s  the (position, scalar) to add
     */
    def + (s: Tuple2 [Int, Double]): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (i == s._1) v(i) + s._2 else v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (any kind)
     */
    def += (b: VectoD): VectorD = { for (i <- range) v(i) += b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
     */
    def += (b: VectorD): VectorD = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: Double): VectorD = { for (i <- range) v(i) += s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = -v(i)
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract (any kind)
     */
    def - (b: VectoD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) - b(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract (same kind, more efficient)
     */
    def - (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) - b.v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) - s
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._2' only at position 's._1'.
     *  @param s  the (position, scalar) to subtract
     */
    def - (s: Tuple2 [Int, Double]): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (i == s._1) v(i) - s._2 else v(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to add (any kind)
     */
    def -= (b: VectoD): VectorD = { for (i <- range) v(i) -= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
     */
    def -= (b: VectorD): VectorD = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to add
     */
    def -= (s: Double): VectorD = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by (any kind)
     */
    def * (b: VectoD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) * b(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by (same kind, more efficient)
     */
    def * (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) * b.v(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (any kind)
     */
    def *= (b: VectoD): VectorD = { for (i <- range) v(i) *= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
     */
    def *= (b: VectorD): VectorD = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def *= (s: Double): VectorD = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by (any kind)
     */
    def / (b: VectoD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) / b(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by (same kind, more efficient)
     */
    def / (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) / b.v(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) / s
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (any kind)
     */
    def /= (b: VectoD): VectorD = { for (i <- range) v(i) /= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
     */
    def /= (b: VectorD): VectorD = { for (i <- range) v(i) /= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def /= (s: Double): VectorD = { for (i <- range) v(i) /= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise in-place each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Double): VectorD = { for (i <- range) v(i) = v(i) ~^ s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = 1.0 / v(i)
        c
    } // recip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = ABS (v(i))
        c
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */
    def sum: Double = v.foldLeft (0.0)((s, x) => s + x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the absolute value of the elements of 'this' vector.
     */
    def sumAbs: Double = v.foldLeft (0.0)((s, x) => s + ABS (x))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): Double = sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: Double = v.foldLeft (0.0)((s, x) => s + MAX (x, 0.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish the rank order of the elements in 'self' vector, e.g.,
     *  (8.0, 2.0, 4.0, 6.0) is (3, 0, 1, 2).
     */
    def rank: VectorI = VectorI (iqsort (v))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of 'this' vector from left to right (e.g., create a
     *  CDF from a pmf).  Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: VectorD =
    {
        val c = new VectorD (dim)
        var sum = 0.0
        for (i <- range) { sum += v(i); c.v(i) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: VectorD = this * (1.0 / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: VectorD = this * (1.0 / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: VectorD = this * (1.0 / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector (any kind)
     */
    def dot (b: VectoD): Double =
    {
        var sum = 0.0
        for (i <- range) sum += v(i) * b(i)
        sum
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector (same kind, more efficient)
     */
    def dot (b: VectorD): Double =
    {
        var sum = 0.0
        for (i <- range) sum += v(i) * b.v(i)
        sum
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: Double =
    {
        var sum = 0.0
        for (i <- range) sum += ABS (v(i))
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Double =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) > x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector (any kind)
     */
    def max (b: VectoD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (b(i) > v(i)) b(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector (same kind, more efficient)
     */
    def max (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): Double =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) < x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector (any kind)
     */
    def min (b: VectoD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (b(i) < v(i)) b(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector (same kind, more efficient)
     */
    def min (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of 'this' vector (index of maximum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (e: Int = dim): Int =
    {
        var j = 0
        for (i <- 1 until e if v(i) > v(j)) j = i
        j
    } // argmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of 'this' vector (index of maximum element).
     *  @param s  the starting index (inclusive) for the search
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (s: Int, e: Int): Int =
    {
        var j = s
        for (i <- s + 1 until e if v(i) > v(j)) j = i
        j
    } // argmax

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of 'this' vector (index of minimum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (e: Int = dim): Int =
    {
        var j = 0
        for (i <- 1 until e if v(i) < v(j)) j = i
        j
    } // argmin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of 'this' vector (index of minimum element).
     *  @param s  the starting index (inclusive) for the search
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (s: Int, e: Int): Int =
    {
        var j = s
        for (i <- s + 1 until e if v(i) < v(j)) j = i
        j
    } // argmin

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument minimum of 'this' vector (-1 if it's not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int =
    {
        val j = argmin (e); if (v(j) < 0.0) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if it's not positive).
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
    def firstNeg (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) < 0.0) return i; -1
    } // firstNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) > 0.0) return i; -1
    } // firstPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: Double, e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) == x) return i; -1
    } // indexOf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: (Double) => Boolean): Int = v.indexWhere (p)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) < 0.0) count += 1
        count
    } // countNeg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) > 0.0) count += 1
        count
    } // countPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' vector at or below the threshold 'thres' by setting
     *  them to zero.  Iterative algorithms give approximate values and if very close
     *  to zero, may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double = TOL, relative: Boolean = true): VectorD =
    {
        val s = if (relative) mag else 1.0              // use vector magnitude or 1
        for (i <- range) if (ABS (v(i)) <= thres * s) v(i) = 0.0
        this
    } // clean

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
//  def distinct: Int =
//  {
//      var count = 1
//      val us = new VectorD (this); us.sort ()                // sorted vector
//      for (i <- 1 until dim if us(i) != us(i-1)) count += 1
//      count
//  } // distinct

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new vector consisting of the distinct elements from 'this' vector.
     */
    def distinct: VectorD = VectorD (v.distinct)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int = v.distinct.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: Double): Boolean = v contains x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether any of the elements in 'x' are contained in 'this' vector.
     *  @param x  the vector of elements to be checked
     */
    def containsAny (x: VectorD): Boolean = (v intersect x.v).length != 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the intersection of vectors 'this' and 'x'.
     *  @param x  the other vector
     */
    def intersect (x: VectorD): VectorD = VectorD (v intersect x.v)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the order of the elements in 'this' vector.
     */
    def reverse (): VectorD = new VectorD (dim, v.reverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' vector is in sorted (ascending) order.
     */
    def isSorted: Boolean = (new SortingD (v)).isSorted

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort () { quickSort (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 () { qsort2 (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int) { val t = v(j); v(j) = v(i); v(i) = t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean = { for (i <- range if v(i) < 0.0) return false; true }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: VectorD] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [VectorD] (i)
            if      (ge && (v(i) compare b_i) < 0) ge = false
            else if (le && (v(i) compare b_i) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether 'this' vector equals vector 'b'.
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
//      b.isInstanceOf [VectorD] && (v.deep equals b.asInstanceOf [VectorD].v.deep)

        if (! b.isInstanceOf [VectoD]) return false
        val bb = b.asInstanceOf [VectoD]
        if (dim != bb.dim) return false
        val vm = if (dim > 0) mag else 1.0                             // maximum magnitude element in vector
        for (i <- range) {
//          if (v(i) !=~ bb(i)) return false                            // stricter
            if (v(i) !=~ bb(i) && v(i) + vm !=~ bb(i) + vm) return false
        } // for
        true
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode (): Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a String.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("VectorD(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (v(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString
  
} // VectorD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorD` object is the companion object for the `VectorD` class.
 */
object VectorD
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` from one or more values (repeated values `Double`*).
     *  @param x   the first `Double` number
     *  @param xs  the rest of the `Double` numbers
     */
    def apply (x: Double, xs: Double*): VectorD =
    {
        val c = new VectorD (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.dim-1) c.v(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` from a sequence/array of `Double`s.
     *  @param xs  the sequence/array of the `Double` numbers
     */
    def apply (xs: Seq [Double]): VectorD =
    {
        val c = new VectorD (xs.length)
        for (i <- 0 until c.dim) c.v(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` from one or more values (repeated values String*).
     *  @param x   the first String
     *  @param xs  the rest of the Strings
     */
    def apply (x: String, xs: String*): VectorD =
    {
        val c = new VectorD (1 + xs.length)
        c(0)  = x.toDouble
        for (i <- 0 until c.dim-1) c.v(i+1) = xs(i).toDouble
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` from an array of Strings.
     *  @param xs  the array of the Strings
     */
    def apply (xs: Array [String]): VectorD =
    {
        val c = new VectorD (xs.length)
        for (i <- c.range) c.v(i) = xs(i).toDouble
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` from an array of Strings, skipping the first 'skip'
     *  elements.  If an element is non-numeric, use its hashcode.
     *  FIX:  Might be better to map non-numeric Strings to ordinal values.
     *  @param xs    the array of the Strings
     *  @param skip  the number of elements at the beginning to skip (e.g., id column)
     */
    def apply (xs: Array [String], skip: Int): VectorD =
    {
        val c = new VectorD (xs.length - skip)
        for (i <- skip until xs.length) {
            c.v(i - skip) = if (xs(i) matches "[\\-\\+]?\\d*(\\.\\d+)?") xs(i).toDouble else xs(i).hashCode ()
        } // for
        c
    } // apply

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` with 'n' elements and fill it with the value 'x'.
     *  @param n  the number of elements
     *  @param x  the value to assign to all elements
     */
    def fill (n: Int)(x: Double): VectorD =
    {
        val c = new VectorD (n)
        c.set (x)
        c
    } // fill

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the size of the new vector
     */
    def one (size: Int): VectorD = new VectorD (size, Array.fill (size)(1.0))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part (any kind)
     */
    def ++ (b: Double, u: VectoD): VectorD =
    {
        val c = new VectorD (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u(i-1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part (same kind, more efficient)
     */
    def ++ (b: Double, u: VectorD): VectorD =
    {
        val c = new VectorD (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i-1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `VectorD` containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): VectorD =
    {
        val c = new VectorD (end - start)
        for (i <- c.range) c.v(i) = (start + i).toDouble
        c
    } // range

} // VectorD object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorDTest` object tests the operations provided by `VectorD`.
 *  > run-main scalation.linalgebra.VectorDTest
 */
object VectorDTest extends App
{
    var x: VectorD = null
    var y: VectorD = null

    for (l <- 1 to 4) {
        println ("\n\tTest VectorD on vectors of dim " + l)
        x = new VectorD (l)
        y = new VectorD (l)
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

    val z = VectorD ("1", "2", "3", "4")
    println ("z = " + z)
    println ("z.map (_ * 2)    = " + z.map ((e: Double) => e * 2))
    println ("z.filter (_ > 2) = " + z.filter (_ > 2))

} // VectorDTest

