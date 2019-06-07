
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug 26 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.gen

import scala.Numeric._
import scala.collection.Traversable
import scala.math.{BigDecimal, ceil, sqrt}
import scala.reflect.ClassTag
import scala.util.Sorting.quickSort

import scalation.linalgebra.VectorD
import scalation.math.Primes.prime
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Vectors` object contains convenience definitions for commonly used types of
 *  vectors.  For efficiency, non-generic versions of `VectorD`, `VectorC` and `VectorR`
 *  are provided in the `linalgebra` package.
 */
object Vectors
{
    type VectorI = VectorN [Int]              // Vector of Integers
    type VectorL = VectorN [Long]             // Vector of Long Integers
    type VectorF = VectorN [Float]            // Vector of Floating Point Numbers
    type VectorB = VectorN [BigDecimal]       // Vector of Arbitrary-precision Decimal Numbers
//  type VectorD = VectorN [Double]           // Vector of Double Precision Float
//  type VectorC = VectorN [Complex]          // Vector of Complex Numbers
//  type VectorR = VectorN [Rational]         // Vector of Rational Numbers

} // Vectors object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorN` class stores and operates on Numeric Vectors of various sizes
 *  and types.  The element type may be any subtype of Numeric.  Some methods
 *  only work for Fractional types.  When/if Scala adds 'sqrt' and 'pow' to
 *  `Fractional` types the following methods will be implemented: ~^, ~^=, 'normalizeU'.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
class VectorN [T <% Ordered [T]: ClassTag: Numeric] (val dim: Int,
                                             private var v:   Array [T] = null)
      extends Traversable [T] with PartiallyOrdered [VectorN [T]] with Error with Serializable
{
    import Vectors._

    {
        if (v == null) {
            v = new Array [T] (dim)
        } else if (dim != v.length) {
            flaw ("constructor", "dimension is wrong")
        } // if
    } // primary constructor

    /** Range for the storage array
     */
    private val range = 0 until dim

    /** Create and import Numeric evidence
     */
    private val nu = implicitly [Numeric [T]]
    import nu._

    /** Numeric zero (0)
     */
    val _0 = nu.zero

    /** Numeric one (1)
     */
    val _1 = nu.one

    /** Numeric minus one (-1)
     */
    val _1n = -_1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector from an array of values.
     *  @param u  the array of values
     */
    def this (u: Array [T]) { this (u.length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: VectorN [T])
    {
        this (u.dim)                               // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size 'dim' of this vector by 'more' elements.
     *  @param factor  the expansion factor
     */
    def expand (more: Int = dim): VectorN [T] =
    {
        if (more < 1) this       // no change
        else          new VectorN [T] (dim + more, Array.concat (v, new Array [T] (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position 'j'.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): VectorN [T] =
    {
        val c = new VectorN [T] (size)
        for (i <- 0 until size) c.v(i) = if (i == j) _1 else _0
        c
    } // oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position 'j'.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): VectorN [T] =
    {
        val c = new VectorN [T] (size)
        for (i <- 0 until size) c.v(i) = if (i == j) _1n else _0
        c
    } // _oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a ramp-up vector of increasing values: 0, 1, 2, ..., size - 1.
     *  @param size  the size of the vector (upper bound = size - 1)
     */
//  def ramp (size: Int = dim): VectorN [T] =
//  {
//      val c = new VectorN [T] (size)
//      for (i <- 0 until size) c.v(i) = nu.fromInt (i)
//      c
//  } // ramp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a `VectorN [T]` into a `VectorN [Int]`, i.e., `VectorI`.
     *  @param u  the vector to convert an integer vector
     */
    def toInt: VectorI =
    {
        val c = new VectorI (dim)
        for (i <- range) c.v(i) = nu.toInt (v(i))
        c
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a `VectorN [T]` into a `VectorN [Double]`, i.e., `VectorD`.
     *  @param u  the vector to convert a double vector
     */
    def toDouble: VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c(i) = nu.toDouble (v(i))
        c
    } // toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's element at the 'i'th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): T = v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): VectorN [T] = slice (r.start, r.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's entire array.
     */
    def apply (): Array [T] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's element at the 'i'th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: T) { v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: T) { for (i <- r) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectorN [T]) { for (i <- r) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in this vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: T) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in this vector to the values in array 'u'.
     *  @param u  the array of values to be assigned
     */
    def setAll (u: Array [T]) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over the vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: T => U)
    {
        var i = 0    
        while (i < dim) { f (v(i)); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): VectorN [T] = new VectorN [T] (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of this vector corresponding to a index/basis.
     *  @param index  the set of index positions (e.g., 0, 2, 5)
     */
    def select (index: Array [Int]): VectorN [T] =
    {
        val c = new VectorN [T] (index.length)
        for (i <- c.range) c.v(i) = v(index(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and scalar 'b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: T): VectorN [T] =
    {
        val c = new VectorN [T] (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and vector 'b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) + b.v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: T): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) + s
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar 's._1' only at position 's._2'.
     *  @param s  the (scalar, position) to add
     */
    def + (s: Tuple2 [T, Int]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) + s._1 else v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: VectorN [T]): VectorN [T] = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: T): VectorN [T] = { for (i <- range) v(i) += s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of this vector (unary minus).
     */
    def unary_-(): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = -v(i)
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) - b.v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: T): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) - s
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar 's._1' only at position 's._2'.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: Tuple2 [T, Int]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) - s._1 else v(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place vector 'b'.
     *  @param b  the vector to add
     */
    def -= (b: VectorN [T]): VectorN [T] = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place scalar 's'.
     *  @param s  the scalar to add
     */
    def -= (s: T): VectorN [T] = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: T): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) * b.v(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this 'row' vector by matrix 'm'.
     *  @param m  the matrix to multiply by
     */
    def * (m: Matrix [T]): VectorN [T] = m.t * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and vector 'b'.
     *  @param b  the vector to add
     */
    def *= (b: VectorN [T]): VectorN [T] = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def *= (s: T): VectorN [T] = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectorN [T]) (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) / b.v(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: T) (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i) / s
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and vector 'b'.
     *  @param b  the vector to add
     */
    def /= (b: VectorN [T]) (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        for (i <- range) v(i) /= b.v(i)
        this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def /= (s: T) (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        for (i <- range) v(i) /= s
        this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of this vector raised to the
     *  's'th power.
     *  @param s  the scalar exponent
     */
//  def ~^ (s: T) (implicit fr: Fractional [T]): VectorN [T] =
//  {
//      import fr._
//      val c = new VectorN [T] (dim)
//      for (i <- range) c.v(i) = math.pow (v(i), s)
//      c
//  } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of this vector to the 's'th power.
     *  @param s  the scalar exponent
     */
//  def ~^= (s: T) (implicit fr: Fractional [T])
//  {
//      import fr._
//      for (i <- range) v(i) = math.pow (v(i), s) 
//  } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Square each element of this vector.
     */
    def sq: VectorN [T] = this * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of this vector.
     */
    def abs: VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = v(i).abs
        c
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector.
     */
    def sum: T = v.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector skipping the 'i'th element.
     *  @param i  the index of the element to skip
     */
    def sum_ne (i: Int): T = sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of this vector.
     */
    def sum_pos: T =
    {
        var sum = _0
        for (i <- range if v(i) > _0) sum += v(i)
        sum
    } // sum_pos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of this vector from left to right (e.g., create a
     *  cdf from a pmf). Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: VectorN [T] =
    {
        var sum = _0
        val c = new VectorN [T] (dim)
        for (i <- range) { sum += v(i); c.v(i) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so that it sums to one (like a probability vector).
     */
    def normalize (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        this * (one / sum)
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so its length is one (unit vector).
     */
//  def normalizeU (implicit fr: Fractional [T]): VectorN [T] =
//  {
//      import fr._
//      this * (one / norm)
//  } // normalizeU

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector to have a maximum of one.
     */
    def normalize1 (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        this * (one / this.max ())
    } // normalize1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of this vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: VectorN [T]): T =
    {
        var s = _0
        for (i <- range) s += v(i) * b.v(i)
        s
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) squared of this vector.
     */
    def normSq: T = this dot this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) of this vector (requires `Fractional` type).
     */
    def norm (implicit fr: Fractional [T]): Double = sqrt (normSq.toDouble)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of this vector.
     */
    def norm1: T =
    {
        var sum = _0
        for (i <- range) sum += nu.abs (v(i))
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): T = v.slice (0, e).max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of this vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): T = v.slice (0, e).min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of this vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the element with the greatest magnitude in this vector.
     */
    def mag: T = nu.abs (max ()) max nu.abs (min ())

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
    /** Determine whether the predicate 'pred' holds for some element in this vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: (T) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether x is contained in this vector.
     *  @param x  the element to be checked
     */
    def contains (x: T): Boolean = v.contains (x)

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort this vector in-place in non-decreasing order.
     */
    def sort () { quickSort (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector is at least as long as this vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: VectorN [T]): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range if v(i) < _0) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: VectorN [T]] (b: B)
        (implicit view$1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [VectorN [T]] (i)
            if      (ge && (v(i) compare b_i) < 0) ge = false
            else if (le && (v(i) compare b_i) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareTo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether vector this equals vector 'b'.
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
        b match {
            case VectorN => (v.deep equals b.asInstanceOf [VectorN [T]].v.deep)
            case _       => false
        } // match
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Hash a vector into an integer.  Serves as the default hash function for
     *  vectors.  Warning, collisions may be unavoidable.
     *  @param x  the vector of type T to hash
     */
//  override def hashCode (): Int =
//  {
//      if (dim > prime.length) flaw ("hash", "not enough primes for computing hash function")
//      var accum = 0
//      for (i <- range) accum ^= (ceil (v(i).toDouble * prime(i))).toInt
//      accum
//  } // hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this vector to a string.
     */
    override def toString: String = 
    {
        val sb = new StringBuilder ("VectorN(")
        for (i <- range) { sb.append (v(i)); sb.append(",\t") } 
        sb.replace (sb.length-2, sb.length, ")").mkString
    } // toString
  
} // VectorN class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorN` object is the companion object for `VectorN` class.
 */
object VectorN extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorN [T]` from one or more values (repeated values T*).
     *  @param u0  the first value
     *  @param u   the rest of the values (zero or more additional values)
     */
    def apply [T <% Ordered [T]: ClassTag: Numeric] (x: T, xs: T*): VectorN [T] =
    {
        val c = new VectorN [T] (1 + xs.length)
        c(0)  = x
        for (i <- 1 until c.dim) c.v(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'VectorN [T]' from one or more values (repeated values String*).
     *  @param skip  dummy value to set data type
     *  @param x     the first String
     *  @param xs    the rest of the Strings
     */
    def apply [T <% Ordered [T]: ClassTag: Numeric] (skip: T, x: String, xs: String*): VectorN [T] =
    {
        val c = new VectorN [T] (1 + xs.length)
        for (i <- c.range) {
            val xx = if (i == 0) x else xs(i-1)
            c.v(i) = skip match {
                 case skip: Int        => xx.toInt.asInstanceOf [T]
                 case skip: Long       => xx.toLong.asInstanceOf [T]
                 case skip: Float      => xx.toFloat.asInstanceOf [T]
                 case skip: BigDecimal => BigDecimal (xx).asInstanceOf [T]
                 case _                => { flaw ("apply", "type " + skip.getClass + " not supported"); skip }
            } // match
        } // for
        c
    } // apply

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a 'VectorN [T]' from an array of strings.
     *  @param skip  dummy value to set data type
     *  @param xa    the array of the Strings
     */
    def apply [T <% Ordered [T]: ClassTag: Numeric] (skip: T, xa: Array [String]): VectorN [T] =
    {
        val c = new VectorN [T] (xa.length)
        for (i <- c.range) {
            val xx = xa(i)
            c.v(i) = skip match {
                 case skip: Int        => xx.toInt.asInstanceOf [T]
                 case skip: Long       => xx.toLong.asInstanceOf [T]
                 case skip: Float      => xx.toFloat.asInstanceOf [T]
                 case skip: BigDecimal => BigDecimal (xx).asInstanceOf [T]
                 case _                => { flaw ("apply", "type " + skip.getClass + " not supported"); skip }
            } // match
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `VectorN` containing a sequence of increasing integers in a range.
     *  @param skip   dummy value to set data type
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range [T <% Ordered [T]: ClassTag: Numeric] (skip: T, start: Int, end: Int): VectorN [T] =
    {
        val c = new VectorN [T] (end - start)
        for (i <- c.range) {
            val xx = start + i
            c.v(i) = skip match {
                 case skip: Int        => xx.toInt.asInstanceOf [T]
                 case skip: Long       => xx.toLong.asInstanceOf [T]
                 case skip: Float      => xx.toFloat.asInstanceOf [T]
                 case skip: BigDecimal => BigDecimal (xx).asInstanceOf [T]
                 case _                => { flaw ("apply", "type " + skip.getClass + " not supported"); skip }
            } // match
        } // for
        c
    } // range

} // VectorN object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorNTest` object tests the operations provided by `VectorN` class.
 */
object VectorNTest extends App
{
    import Vectors._

    var a: VectorI = null
    var b: VectorI = null
    var c: VectorI = null
    var x: VectorF = null
    var y: VectorF = null

    for (l <- 1 to 4) {
        println ("\n\tTest VectorN on integer vectors of dim " + l)
        a = new VectorI (l)
        b = new VectorI (l)
        a.set (2)
        b.set (3)
        println ("a + b    = " + (a + b))
        println ("a - b    = " + (a - b))
        println ("a * b    = " + (a * b))
        println ("a * 4    = " + (a * 4))
        println ("a.max    = " + a.max ())
        println ("a.min    = " + a.min ())
        println ("a.sum    = " + a.sum)
        println ("a.sum_ne = " + a.sum_ne (0))
        println ("a dot b  = " + (a dot b))
        println ("a.normSq = " + a.normSq)
        println ("a < b    = " + (a < b))
        for (x <- a) print (" " + x)
        println

        println ("\n\tTest VectorN on real vectors of dim " + l)
        x = new VectorF (l)
        y = new VectorF (l)
        x.set (2)
        y.set (3)
        println ("x + y    = " + (x + y))
        println ("x - y    = " + (x - y))
        println ("x * y    = " + (x * y))
        println ("x * 4.0  = " + (x * 4.0f))
        println ("x.min    = " + x.min ())
        println ("x.max    = " + x.max ())
        println ("x.sum    = " + x.sum)
        println ("x.sum_ne = " + x.sum_ne (0))
        println ("x dot y  = " + (x dot y))
        println ("x.normSq = " + x.normSq)
        println ("x.norm   = " + x.norm)
        println ("x < y    = " + (x < y))
    } // for

    c = VectorN (4, 2, 3, 1)
    println ("c            = " + c) 
    println ("c.cumulate   = " + c.cumulate)
    println ("range (1, 4) = " + VectorN.range (0, 1, 4))

    println ("hashCode (" + a + ") = " + a.hashCode ())
    println ("hashCode (" + b + ") = " + b.hashCode ())
    println ("hashCode (" + c + ") = " + c.hashCode ())
    println ("hashCode (" + x + ") = " + x.hashCode ())
    println ("hashCode (" + y + ") = " + y.hashCode ())

   val z = VectorN (0, "1", "2", "3", "4")
   println ("z = " + z)

} // VectorNTest object

