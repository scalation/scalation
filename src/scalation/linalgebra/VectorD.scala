
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIDouble style license file).
 */

package scalation.linalgebra

import math.sqrt
import util.Sorting.quickSort

import scalation.math.DoubleWithExp._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The VectorD class stores and operates on Numeric Vectors of base type Double.
 *  It follows the framework of VectorN [T] and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
class VectorD (val dim: Int,
     protected var v:   Array [Double] = null)
      extends PartiallyOrdered [VectorD] with Error with Serializable
{
    if (v == null) {
        v = Array.ofDim [Double] (dim)
    } else if (dim != v.length) {
        flaw ("constructor", "dimension is wrong")
    } // if

    /** Range for the storage array
     */
    private val range = 0 until dim

    /** Format string used for printing vector values (change using setFormat)
     */
    private var fString = "%12.6f"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector from an array of values.
     *  @param u  the array of values
     */
    def this (u: Array [Double]) { this (u.length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector u.
     *  @param u  the other vector
     */
    def this (u: VectorD)
    {
        this (u.dim)                               // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of this vector by 'more' elements.
     *  @param factor  the expansion factor
     */
    def expand (more: Int = dim): VectorD =
    {
        if (more < 1) this       // no change
        else          new VectorD (dim + more, Array.concat (v, new Array [Double] (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): VectorD =
    {
        val c = new VectorD (size)
        for (i <- c.range) c.v(i) = if (i == j) 1. else 0.
        c
    } // oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): VectorD =
    {
        val c = new VectorD (size)
        for (i <- c.range) c.v(i) = if (i == j) -1. else 0.
        c
    } // _oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a ramp-up vector of increasing values: 0, 1, 2, ..., size - 1.
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def ramp (size: Int = dim): VectorD =
    {
        val c = new VectorD (size)
        for (i <- c.range) c.v(i) = i
        c
    } // ramp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a VectorD into a VectorI.
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
    def apply (i: Int): Double = v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): VectorD = slice (r.start, r.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this vector's entire array.
     */
    def apply (): Array [Double] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's element at the i-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Double) { v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Double) { for (i <- r) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectorD) { for (i <- r) v(i) = u(i - r.start) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in this vector to x.
     *  @param x  the value to be assigned
     */
    def set (x: Double) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in this vector to the values in array u.
     *  @param u  the array of values to be assigned
     */
    def setAll (u: Array [Double]) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over the vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Double => U)
    {
        var i = 0    
        while (i < dim) { f (v(i)); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this vector from to end.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    def slice (from: Int, till: Int): VectorD = new VectorD (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of this vector corresponding to a basis.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): VectorD =
    {
        val c = new VectorD (basis.length)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and vector b.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectorD): VectorD =
    {
        val c = new VectorD (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this vector and scalar b.
     *  @param b  the scalar to be concatenated
     */
    def ++ (b: Double): VectorD =
    {
        val c = new VectorD (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and vector b.
     *  @param b  the vector to add
     */
    def + (b: VectorD): VectorD = 
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) + b.v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) + s
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this vector and scalar s._1 only at position s._2.
     *  @param s  the (scalar, position) to add
     */
    def + (s: Tuple2 [Double, Int]): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) + s._1 else v(i)
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def += (b: VectorD): VectorD = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def += (s: Double): VectorD = { for (i <- range) v(i) += s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of this vector (unary minus).
     */
    def unary_- (): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = -v(i)
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract vector b.
     *  @param b  the vector to subtract
     */
    def - (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) - b.v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) - s
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract scalar s._1 only at position s._2.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: Tuple2 [Double, Int]): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) - s._1 else v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place vector b.
     *  @param b  the vector to add
     */
    def -= (b: VectorD): VectorD = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this vector subtract in-place scalar s.
     *  @param s  the scalar to add
     */
    def -= (s: Double): VectorD = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by vector b.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) * b.v(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this vector by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this 'row' vector by matrix m.
     *  @param m  the matrix to multiply by
     */
    def * (m: Matrix): VectorD = m.t * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def *= (b: VectorD): VectorD = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def *= (s: Double): VectorD = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by vector b (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) / b.v(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this vector by scalar s.
     *  @param s  the scalar to divide by
     */
    def / (s: Double): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i) / s
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and vector b.
     *  @param b  the vector to add
     */
    def /= (b: VectorD): VectorD = { for (i <- range) v(i) /= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this vector and scalar s.
     *  @param s  the scalar to add
     */
    def /= (s: Double): VectorD = { for (i <- range) v(i) /= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of this vector raised to the
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
    /** Raise each element of this vector to the s-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Double) { for (i <- range) v(i) = v(i) ~^ s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the square each element of this vector.
     */
    def sq: VectorD = this * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of this matrix.
     */
    def abs: VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = v(i).abs
        c
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector.
     */
    def sum: Double = v.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of this vector skipping the i-th element.
     *  @param i  the index of the element to skip
     */
    def sum_ne (i: Int): Double = sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of this vector.
     */
    def sum_pos: Double =
    {
        var sum = 0.
        for (i <- range if v(i) > 0.) sum += v(i)
        sum
    } // sum_pos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of this vector from left to right (e.g., create a
     *  cdf from a pmf). Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: VectorD =
    {
        val c    = new VectorD (dim)
        var sum = 0.
        for (i <- range) { sum += v(i); c.v(i) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so that it sums to one (like a probability vector).
     */
    def normalize: VectorD = this * (1. / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector so its length is one (unit vector).
     */
    def normalizeU: VectorD = this * (1. / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize this vector to have a maximum of one.
     */
    def normalize1: VectorD = this * (1. / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of this vector with vector b.
     *  @param b  the other vector
     */
    def dot (b: VectorD): Double =
    {
        var s = 0.
        for (i <- range) s += v(i) * b.v(i)
        s
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) squared of this vector.
     */
    def normSq: Double = this dot this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) of this vector.
     */
    def norm: Double = sqrt (normSq)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of this vector.
     */
    def norm1: Double =
    {
        var sum = 0.
        for (i <- range) sum += math.abs (v(i))
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Double =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) > x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): Double =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) < x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of this vector with vector b (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectorD): VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the element with the greatest magnitude in this vector.
     */
    def mag: Double = math.abs (max ()) max math.abs (min ())

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
        val j = argmin (e); if (v(j) < 0.) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of this vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > 0.) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in this vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) < 0.) return i; -1
    } // firstNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in this vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) > 0.) return i; -1
    } // firstPos

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative entries in this vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) < 0.) count += 1
        count
    } // countNeg

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive entries in this vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) > 0.) count += 1
        count
    } // countPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate pred holds for some element in this vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
    def exists (pred: (Double) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether x is contained in this vector.
     *  @param x  the element to be checked
     */
    def contains (x: Double): Boolean = v contains x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort this vector in-place in non-decreasing order.
     */
    def sort () { quickSort (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector is at least as long as this vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: VectorD): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range if v(i) < 0.) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare this vector with vector b.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: VectorD] (b: B)
        (implicit view$1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [VectorD] (i)
            if      (ge && (v(i) compare b_i) < 0) ge = false
            else if (le && (v(i) compare b_i) > 0) le = false
        } // for
        if (ge && le) Some (0) else if (le) Some (-1) else if (ge) Some (1) else None
    } // tryCompareDoubleo

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether vector this equals vector b.
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
        b.isInstanceOf [VectorD] && (v.deep equals b.asInstanceOf [VectorD].v.deep)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode to be be compatible with equals.
     */
    override def hashCode: Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Hash a vector into an integer.  Serves as the default hash function for
     *  vectors.  Warning, collisions may be unavoidable.
     *  @param x  the vector of type Double to hash
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
    /** Set the format to the newFormat.
     *  @param  newFormat  the new format string
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this vector to a string.
     */
    override def toString: String = 
    {
        val sb = new StringBuilder ("VectorD (")
        for (i <- range) { sb.append (fString.format (v(i))); sb.append("\t") } 
        sb.replace (sb.length - 1, sb.length, ")").mkString
    } // toString
  
} // VectorD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for VectorD class.
 */
object VectorD
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a VectorD from one or more values (repeated values Double*).
     *  @param x   the first double
     *  @param xs  the rest of the doubles
     */
    def apply (x: Double, xs: Double*): VectorD =
    {
        val c = new VectorD (1 + xs.length)
        c(0)  = x
        for (i <- 1 until c.dim) c.v(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a VectorD containing a sequence of increasing integers in a range.
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
/** This object tests the operations provided by VectorD.
 */
object VectorDTest extends App
{
    var x: VectorD = null
    var y: VectorD = null

    for (l <- 1 to 4) {
        println ("\n\tTest VectorD on real vectors of dim " + l)
        x = new VectorD (l)
        y = new VectorD (l)
        x.set (2)
        y.set (3)
        println ("x + y    = " + (x + y))
        println ("x - y    = " + (x - y))
        println ("x * y    = " + (x * y))
        println ("x * 4.0  = " + (x * 4.0))
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

} // VectorDTest

