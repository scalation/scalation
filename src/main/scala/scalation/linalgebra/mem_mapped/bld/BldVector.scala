
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 27 18:29:28 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.mem_mapped.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldVector` object is used to build vector classes for various base types.
 *  > run-main scalation.linalgebra.mem_mapped.bld.BldVector
 */
object BldVector extends App with BldParams
{
    println ("BldVector: generate code for Vector classes")

    for (k <- kind) {
        val VECTOR     = k._1
        val BASE       = k._2
        val MM_ARRAY   = k._3
        val VECTOR2    = k._4
        val BASE2      = k._5
        val FORMAT     = k._6
        val MATRI      = k._7
        val MM_SORTING = k._8
        val ZERO       = k._9
        val ONE        = k._10
        val REGEX      = k._11
        val BASE_LC    = BASE.toLowerCase
        val EXPON      = if (BASE == "Complex" || BASE == "Real") "Double" else BASE
        val ORD        = if (BASE == "StrNum") "(StrO.ord)" else if (CUSTOM contains BASE) s"($BASE.ord)" else ""
        val FROM_STR1  = if (CUSTOM contains BASE) s"$BASE (x)" else s"x.to$BASE"
        val FROM_STR2  = if (CUSTOM contains BASE) s"$BASE (xs(i))" else s"xs(i).to$BASE"
        val IMPORT     = if (BASE == "StrNum") "scalation.math.StrO.{abs => ABS, max => MAX, _}"
                         else if (CUSTOM contains BASE) s"scalation.math.$BASE.{abs => ABS, max => MAX, _}"
                         else "math.{abs => ABS, max => MAX, sqrt}"
        val IMPORT2    = if (BASE == "StrNum") "scalation.math.StrO"
                         else if (CUSTOM contains BASE) s"scalation.math.$BASE"
                         else s"scalation.math.${BASE_LC}_exp"

// Beginning of string holding code template -----------------------------------

        val code = raw"""
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.linalgebra.mem_mapped.bld.BldVector
 *  @version 1.2
 *  @date    Sun Sep 27 18:29:28 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.mem_mapped

import collection.Traversable
import util.Sorting.quickSort

import $IMPORT

import $IMPORT2
import scalation.linalgebra.{VectorI => VectorII}
import scalation.util.{Error, $MM_ARRAY}
import scalation.util.$MM_SORTING.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$VECTOR` class stores and operates on Numeric Vectors of base type `$BASE`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D memory mapped array used to store vector elements
 */
class $VECTOR (val dim: Int,
     protected var v:   $MM_ARRAY = null)
      extends Traversable [$BASE] with PartiallyOrdered [$VECTOR] with Vec with Error with Serializable
{
    if (v == null) {
        v = $MM_ARRAY.ofDim (dim)
    } else if (dim != v.length) {
        flaw ("constructor", "dimension is wrong")
    } // if

    /** Number of elements in the vector as a $BASE
     */
    val nd = dim.to$BASE

    /** Range for the storage array
     */
    private val range = 0 until dim

    /** Format String used for printing vector values (change using setFormat)
     *  Ex: "%d,\t", "%.6g,\t" or "%12.6g,\t"
     */
    private var fString = "$FORMAT,\t"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector from an array of values.
     *  @param u  the array of values
     */
    def this (u: $MM_ARRAY) { this (u.length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: $VECTOR)
    {
        this (u.dim)                               // invoke primary constructor
        for (i <- range) v(i) = u(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size (number of elements) of 'this' vector.
     */
    override def size: Int = dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the range of all indices (0 to one less than dim).
     */
    def indices: Range = 0 until dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of 'this' vector by 'more' elements.
     *  @param more  the number of new elements to add
     */
    def expand (more: Int = dim): $VECTOR =
    {
        if (more < 1) this       // no change
        else          new $VECTOR (dim + more, $MM_ARRAY.concat (v, new $MM_ARRAY (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): $VECTOR =
    {
        val c = new $VECTOR (size)
        c.v(j) = $ONE
        c
    } // oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): $VECTOR =
    {
        val c = new $VECTOR (size)
        c.v(j) = -$ONE
        c
    } // _oneAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$VECTOR` into a `VectorI`.
     */
    def toInt: VectorI =
    {
        val c = new VectorI (dim)
        for (i <- range) c(i) = v(i).toInt
        c
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$VECTOR` into a `VectorD`.
     */
    def toDouble: VectorD =
    {
        val c = new VectorD (dim)
        for (i <- range) c(i) = v(i).toDouble
        c
    } // toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): $BASE = v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): $VECTOR = slice (r.start, r.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire array.
     */
    def apply (): $MM_ARRAY = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: $BASE) { v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: $BASE) { for (i <- r) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: $VECTOR) { for (i <- r) v(i) = u(i - r.start) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: $BASE) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in array 'u'.
     *  @param u  the array of values to be assigned
     */
    def setAll (u: $MM_ARRAY) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: $BASE => U)
    {
        var i = 0    
        while (i < dim) { f (v(i)); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (Boolean function) to apply
     */
    override def filter (p: $BASE => Boolean): $VECTOR = $VECTOR (v.filter (p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (Boolean function) to apply
     */
    def filterPos (p: $BASE => Boolean): Array [Int] =
    {
        (for (i <- range if p (v(i))) yield i).toArray
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: $BASE => $BASE): $VECTOR = new $VECTOR (v.map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): $VECTOR = new $VECTOR (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): $VECTOR =
    {
        val c = new $VECTOR (basis.length)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b.v(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: $BASE): $VECTOR =
    {
        val c = new $VECTOR (dim + 1)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: $VECTOR): $VECTOR = 
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) + b.v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: $BASE): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) + s
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._1' only at position 's._2'.
     *  @param s  the (scalar, position) to add
     */
    def + (s: Tuple2 [$BASE, Int]): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) + s._1 else v(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: $VECTOR): $VECTOR = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: $BASE): $VECTOR = { for (i <- range) v(i) += s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = -v(i)
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) - b.v(i)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: $BASE): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) - s
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._1' only at position 's._2'.
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: Tuple2 [$BASE, Int]): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = if (i == s._2) v(i) - s._1 else v(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to add
     */
    def -= (b: $VECTOR): $VECTOR = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to add
     */
    def -= (s: $BASE): $VECTOR = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) * b.v(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: $BASE): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' (row) vector by matrix 'm'.
     *  @param m  the matrix to multiply by
     */
    def * (m: $MATRI): $VECTOR = m.t * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def *= (b: $VECTOR): $VECTOR = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def *= (s: $BASE): $VECTOR = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) / b.v(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: $BASE): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) / s
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def /= (b: $VECTOR): $VECTOR = { for (i <- range) v(i) /= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def /= (s: $BASE): $VECTOR = { for (i <- range) v(i) /= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: $EXPON): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) ~^ s
        c
    } // ~^

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with that vector 'b' for inequality.
     *  @param b  that vector
     */
    def ≠ (b: $VECTOR) = this != b

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with that vector 'b' for less than or equal to.
     *  @param b  that vector
     */
    def ≤ (b: $VECTOR) = this <= b

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with that vector 'b' for greater than or equal to.
     *  @param b  that vector
     */
    def ≥ (b: $VECTOR) = this >= b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: $EXPON) { for (i <- range) v(i) = v(i) ~^ s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the square of each element of 'this' vector.
     */
    def sq: $VECTOR = this * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = $ONE / v(i)
        c
    } // inverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = ABS (v(i))
        c
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */
    def sum: $BASE = v.foldLeft ($ZERO)((s: $BASE, x: $BASE) => s + x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the absolute value of the elements of 'this' vector.
     */
    def sumAbs: $BASE = v.foldLeft ($ZERO)((s, x) => s + ABS (x))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): $BASE = sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: $BASE = v.foldLeft ($ZERO)((s, x) => s + MAX (x, $ZERO))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of the elements of 'this' vector.
     */
    def mean = sum / nd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (unbiased) sample variance of the elements of 'this' vector.
     */
    def variance = (normSq - sum * sum / nd) / (nd-$ONE)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population variance of the elements of 'this' vector.
     *  This is also the (biased) MLE estimator for sample variance.
     */
    def pvariance = (normSq - sum * sum / nd) / nd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish the rank order of the elements in 'self' vector, e.g.,
     *  (8.0, 2.0, 4.0, 6.0) is (3, 0, 1, 2).
     */
    def rank: VectorII = new VectorII (iqsort (v))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of 'this' vector from left to right (e.g., create a
     *  CDF from a pmf).  Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: $VECTOR =
    {
        val c = new $VECTOR (dim)
        var sum: $BASE = $ZERO
        for (i <- range) { sum += v(i); c.v(i) = sum }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: $VECTOR = this * ($ONE / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: $VECTOR = this * ($ONE / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: $VECTOR = this * ($ONE / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: $VECTOR): $BASE =
    {
        var sum: $BASE = $ZERO
        for (i <- range) sum += v(i) * b.v(i)
        sum
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ∙ (b: $VECTOR): $BASE =
    {
        var sum: $BASE = $ZERO
        for (i <- range) sum += v(i) * b.v(i)
        sum
    } // ∙

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) squared of 'this' vector.
     */
    def normSq: $BASE = this dot this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) of 'this' vector.
     */
    def norm: $BASE = sqrt (normSq).to$BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: $BASE =
    {
        var sum: $BASE = $ZERO
        for (i <- range) sum += ABS (v(i))
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): $BASE =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) > x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = if (b.v(i) > v(i)) b.v(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): $BASE =
    {
        var x = v(0)
        for (i <- 1 until e if v(i) < x) x = v(i)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = if (b.v(i) < v(i)) b.v(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the element with the greatest magnitude in 'this' vector.
     */
    def mag: $BASE = ABS (max ()) max ABS (min ())

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
    /** Return the argument minimum of 'this' vector (-1 if its not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int =
    {
        val j = argmin (e); if (v(j) < $ZERO) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if its not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int =
    {
        val j = argmax (e); if (v(j) > $ZERO) j else -1
    } // argmaxPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) < $ZERO) return i; -1
    } // firstNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) > $ZERO) return i; -1
    } // firstPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: Int, e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) == x) return i; -1
    } // indexOf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) < $ZERO) count += 1
        count
    } // countNeg

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) > $ZERO) count += 1
        count
    } // countPos

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def distinct: Int =
    {
        var count = 1
        val us = new $VECTOR (this); us.sort2 ()               // sorted vector
        for (i <- 1 until dim if us(i) != us(i-1)) count += 1
        count
    } // distinct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the predicate 'pred' holds for some element in 'this' vector.
     *  @param pred  the predicate to test (e.g., "_ == 5.")
     */
//  def exists (pred: ($BASE) => Boolean): Boolean = v.exists (pred)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: $BASE): Boolean = v contains x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
//  def sort () { quickSort (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 () { qsort2 (v) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)
    {
        val t = v(j); v(j) = v(i); v(i) = t
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector 'b' is at least as long as 'this' vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: $VECTOR): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range if v(i) < $ZERO) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: $VECTOR] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [$VECTOR] (i)
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
        b.isInstanceOf [$VECTOR] && (v.deep equals b.asInstanceOf [$VECTOR].v.deep)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode: Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat' (e.g., "%.6g,\t" or "%12.6g,\t").
     *  @param  newFormat  the new format String
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a String.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("$VECTOR(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (v(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString
  
} // $VECTOR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$VECTOR` object is the companion object for the `$VECTOR` class.
 */
object $VECTOR
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from one or more values (repeated values $BASE*).
     *  @param x   the first $BASE number
     *  @param xs  the rest of the $BASE numbers
     */
    def apply (x: $BASE, xs: $BASE*): $VECTOR =
    {
        val c = new $VECTOR (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.dim-1) c.v(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from a sequence of ${BASE}s.
     *  @param xs  the sequence of the $BASE numbers
     */
    def apply (xs: Seq [$BASE]): $VECTOR =
    {
        val c = new $VECTOR (xs.length)
        for (i <- 0 until c.dim) c.v(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from one or more values (repeated values String*).
     *  @param x   the first String
     *  @param xs  the rest of the Strings
     */
    def apply (x: String, xs: String*): $VECTOR =
    {
        val c = new $VECTOR (1 + xs.length)
        c(0)  = $FROM_STR1
        for (i <- 0 until c.dim-1) c.v(i+1) = $FROM_STR2
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from an array of Strings.
     *  @param xs  the array of the Strings
     */
    def apply (xs: Array [String]): $VECTOR =
    {
        val c = new $VECTOR (xs.length)
        for (i <- c.range) c.v(i) = $FROM_STR2
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from an array of Strings, skipping the first 'skip'
     *  elements.  If an element is non-numeric, use its hashcode.
     *  FIX:  Might be better to map non-numeric Strings to ordinal values.
     *  @param xs    the array of the Strings
     *  @param skip  the number of elements at the beginning to skip (e.g., id column)
     */
    def apply (xs: Array [String], skip: Int): $VECTOR =
    {
        val c = new $VECTOR (xs.length - skip)
        for (i <- skip until xs.length) {
            c.v(i - skip) = if (xs(i) matches $REGEX) $FROM_STR2 else xs(i).hashCode ()
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the size of the vector
     */
    def one (size: Int): $VECTOR =
    {
        val c = new $VECTOR (size)
        c.set ($ONE)
        c
    } // one

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part
     */
    def ++ (b: $BASE, u: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i - 1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `$VECTOR` containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): $VECTOR =
    {
        val c = new $VECTOR (end - start)
        for (i <- c.range) c.v(i) = (start + i).to$BASE
        c
    } // range

} // $VECTOR object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `${VECTOR}Test` object tests the operations provided by `$VECTOR`.
 *  > run-main scalation.linalgebra.mem_mapped.${VECTOR}Test
 */
object ${VECTOR}Test extends App
{
    var x: $VECTOR = null
    var y: $VECTOR = null

    for (l <- 1 to 4) {
        println ("\n\tTest $VECTOR on vectors of dim " + l)
        x = new $VECTOR (l)
        y = new $VECTOR (l)
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

    val z = $VECTOR ("1", "2", "3", "4")
    println ("z = " + z)
    println ("z.map (_ * 2)    = " + z.map ((e: $BASE) => e * 2))
    println ("z.filter (_ > 2) = " + z.filter (_ > 2))

} // ${VECTOR}Test

"""

// Ending of string holding code template --------------------------------------

//      println (code)

        val writer = new PrintWriter (new File (DIR + _l + VECTOR + ".scalaa"))
        writer.write (code)
        writer.close ()
    } // for

} // BldVector object

