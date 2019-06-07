
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat May 30 13:51:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldVector` object is used to build vector classes for various base types.
 *  > runMain scalation.linalgebra.bld.BldVector
 */
object BldVector extends App with BldParams
{
    println ("BldVector: generate code for Vector classes")

    for (k <- kind) {
        val VECTO     = k._1
        val VECTOR    = k._1.replace ("o", "or")
        val BASE      = k._2
        val VECTOR2   = k._3.replace ("o", "or")
        val SORTING   = k._7
        val ZERO      = k._8
        val ONE       = k._9
        val REGEX     = k._10
        val BASE_LC   = BASE.toLowerCase
        val EXPON     = if (BASE == "Complex" || BASE == "Real") "Double" else BASE
        val ORD       = if (BASE == "StrNum") "(StrO.ord)" else if (BASE == "TimeNum") "(TimeO.ord)" else ""
        val FROM_STR1 = if (CUSTOM contains BASE) s"$BASE (x)" else s"x.to$BASE"
        val FROM_STR2 = if (CUSTOM contains BASE) s"$BASE (xs(i))" else s"xs(i).to$BASE"
        val FROM_STR3 = if (CUSTOM contains BASE) s"$BASE (xs(i-1))" else s"xs(i-1).to$BASE"
        val IMPORT    = if (BASE == "StrNum") "scalation.math.StrO.{abs => ABS, max => MAX, _}"
                        else if (BASE == "TimeNum") "scalation.math.TimeO.{abs => ABS, max => MAX, _}"
                        else if (CUSTOM contains BASE) s"scalation.math.$BASE.{abs => ABS, max => MAX, _}"
                        else "scala.math.{abs => ABS, max => MAX, sqrt}"
        val IMPORT2   = if (BASE == "StrNum") "scalation.math.{StrO, noStrNum}"
                        else if (BASE == "TimeNum") "scalation.math.{TimeO, noTimeNum}"
                        else if (CUSTOM contains BASE) s"scalation.math.{$BASE, no$BASE}"
                        else s"scalation.math.{${BASE_LC}_exp, no$BASE}"


// Beginning of string holding code template -----------------------------------

        val code = raw"""
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.{breakOut, Traversable}
import scala.collection.mutable.{IndexedSeq, WrappedArray}
import scala.util.Sorting.quickSort

import $IMPORT

import $IMPORT2
import scalation.math.ExtremeD.TOL
import scalation.util.Error
import scalation.util.$SORTING
import scalation.util.$SORTING.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$VECTOR` class stores and operates on Numeric Vectors of base type `$BASE`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
class $VECTOR (val dim: Int,
     protected var v:   Array [$BASE] = null)
      extends $VECTO
//    extends Traversable [$BASE] with PartiallyOrdered [$VECTOR] with Vec with Error with Serializable
{
    if (v == null) {
        v = Array.ofDim [$BASE] (dim)
    } else if (dim > v.length) {
        flaw ("constructor", "vector dimension is larger than space: dim = " + dim + " > v.length = " + v.length)
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: $VECTO) { this (u.dim); for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign 'value' at 'index' position.
     *  @param iv  the tuple containing (index, value)
     *  @param dm  the dimension for the new vector
     */
    def this (iv: (Int, $BASE), dm: Int) { this (dm); v(iv._1) = iv._2 }
"""; val code2 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' vector.
     */
    def copy (): $VECTOR = new $VECTOR (this)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def zero (size: Int = dim): $VECTOR = new $VECTOR (size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def one (size: Int = dim): $VECTOR = new $VECTOR (size, Array.fill (size)($ONE))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): $VECTOR = new $VECTOR ((j, $ONE), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): $VECTOR = new $VECTOR ((j, -$ONE), size)

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
    def apply (): WrappedArray [$BASE] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements that are given in the index vector.
     *  @param iv  the index vector
     */
    override def apply (iv: VectoI): $VECTOR =
    {
        val c = new $VECTOR (iv.dim)
        for (i <- c.range) c.v(i) = v(iv(i))
        c
    } // apply

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
    def update (r: Range, u: $VECTO) { for (i <- r) v(i) = u(i - r.start) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: $BASE) { for (i <- range) v(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in sequence/array 'u'.
     *  @param u  the sequence/array of values to be assigned
     */
    def set (u: Seq [$BASE]) { for (i <- range) v(i) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: $BASE => U) { var i = 0; while (i < dim) { f (v(i)); i += 1 } }
"""; val code3 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$VECTOR` into a `VectorI`.
     */
    def toInt: VectorI = VectorI (v.map (_.toInt))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$VECTOR` into a `VectorL`.
      */
    def toLong: VectorL = VectorL (v.map (_.toLong))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$VECTOR` into a `VectorD`.
     */
    def toDouble: VectorD = VectorD (v.map (_.toDouble))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$VECTOR` into a dense `$VECTOR`.
     */
    def toDense: $VECTOR = this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the size (dim) of 'this' vector by 'more' elements.
     *  @param more  the number of new elements to add
     */
    def expand (more: Int = dim): $VECTOR =
    {
        if (more < 1) this       // no change
        else          new $VECTOR (dim + more, Array.concat (v, new Array [$BASE] (more)))
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    override def filter (p: $BASE => Boolean): $VECTOR = $VECTOR (v.filter (p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter out the missing elements of 'this' vector based on the predicate 
     *  that 'e != no$BASE'.
     */
    def filterMissing: $VECTOR = filter (_ != no$BASE) 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: $BASE => Boolean): IndexedSeq [Int] =
    {
        (for (i <- range if p (v(i))) yield i)(breakOut)
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the binary predicate 'p',
     *  returning the index positions.
     *  @param v2  the other vector to compare with
     *  @param p   the binary predicate (Boolean function, between two elements) to apply
     */
    def filterPos2 (v2: $VECTO, p: ($BASE, $BASE) => Boolean): IndexedSeq [(Int, Int)] =
    {
        var result = IndexedSeq [(Int, Int)] ()
        for (i <- range; j <- v2.range if p(v(i), v2(j))) result = result :+ (i, j)
        result
    } // filterPos2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: $BASE => $BASE): $VECTOR = $VECTOR (v.map (f))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int = dim): $VECTOR = new $VECTOR (till - from, v.slice (from, till))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split 'this' vector into 'k' arrays of equal sizes (perhaps except for the last one).
     *  @param k  the number of pieces the vector is to be split into
     */
    override def split (k: Int): Array [$VECTO] =
    {
        if (k <= 0) flaw ("split", "k must be a positive integer")
        val pieces = Array.ofDim [$VECTO] (k)
        val size = dim / k
        for (i <- 0 until k-1) pieces(i) = slice (i*size, (i+1)*size)
        pieces(k-1) = slice ((k-1)*size, dim)
        pieces
    } // split

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set/array of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): $VECTOR =
    {
        val c = new $VECTOR (basis.length)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select
"""; val code4 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set/vector of index positions (e.g., 0, 2, 5)
     */
    def select (basis: VectoI): $VECTOR =
    {
        val c = new $VECTOR (basis.dim)
        for (i <- c.range) c.v(i) = v(basis(i))
        c
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated (any kind)
     */
    def ++ (b: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (dim + b.dim)
        for (i <- c.range) c.v(i) = if (i < dim) v(i) else b(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated (same kind, more efficient)
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
     *  @param b  the vector to add (any kind)
     */
    def + (b: $VECTO): $VECTOR = 
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) + b(i)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
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
    /** Add 'this' vector and scalar 's._2' only at position 's._1', e.g., 'x + (3, 5.5)'.
     *  @param s  the (position, scalar) to add
     */
    def + (s: (Int, $BASE)): $VECTOR = { val c = copy (); c.v(s._1) += s._2; c }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (any kind)
     */
    def += (b: $VECTO): $VECTOR = { for (i <- range) v(i) += b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add (same kind, more efficient)
     */
    def += (b: $VECTOR): $VECTOR = { for (i <- range) v(i) += b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: $BASE): $VECTOR = { for (i <- range) v(i) += s; this }
"""; val code5 = raw"""
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
     *  @param b  the vector to subtract (any kind)
     */
    def - (b: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) - b(i)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract (same kind, more efficient)
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
    /** From 'this' vector subtract scalar 's._2' only at position 's._1', e.g., 'x - (3, 5.5)'.
     *  @param s  the (position, scalar) to subtract
     */
    def - (s: (Int, $BASE)): $VECTOR = { val c = copy (); c.v(s._1) -= s._2; c }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to subtract (any kind)
     */
    def -= (b: $VECTO): $VECTOR = { for (i <- range) v(i) -= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to subtract (same kind, more efficient)
     */
    def -= (b: $VECTOR): $VECTOR = { for (i <- range) v(i) -= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to subtract
     */
    def -= (s: $BASE): $VECTOR = { for (i <- range) v(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by (any kind)
     */
    def * (b: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) * b(i)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by (same kind, more efficient)
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
"""; val code6 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's._2' only at position 's._1', e.g., 'x * (3, 5.5)'.
     *  @param s  the (position, scalar) to multiply
     */
    def * (s: (Int, $BASE)): $VECTOR = { val c = copy (); c.v(s._1) *= s._2; c }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to multiply by (any kind)
     */
    def *= (b: $VECTO): $VECTOR = { for (i <- range) v(i) *= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to multiply by (same kind, more efficient)
     */
    def *= (b: $VECTOR): $VECTOR = { for (i <- range) v(i) *= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def *= (s: $BASE): $VECTOR = { for (i <- range) v(i) *= s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by (any kind)
     */
    def / (b: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = v(i) / b(i)
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by (same kind, more efficient)
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
    /** Divide 'this' vector by scalar 's._2' only at position 's._1', e.g., 'x / (3, 5.5)'.
     *  @param s  the (position, scalar) to divide
     */
    def / (s: (Int, $BASE)): $VECTOR = { val c = copy (); c.v(s._1) /= s._2; c }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to divide by (any kind)
     */
    def /= (b: $VECTO): $VECTOR = { for (i <- range) v(i) /= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to divide by (same kind, more efficient)
     */
    def /= (b: $VECTOR): $VECTOR = { for (i <- range) v(i) /= b.v(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to divide by
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise in-place each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: $EXPON): $VECTOR = { for (i <- range) v(i) = v(i) ~^ s; this }
"""; val code7 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = $ONE / v(i)
        c
    } // recip

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
    def sum: $BASE = v.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): $BASE = v.sum - v(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: $BASE = 
    {
        var s = $ZERO
        for (x <- v) if (x > $ZERO) s += x
        s
    } // sumPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish the rank order of the elements in 'self' vector, e.g.,
     *  (8.0, 2.0, 4.0, 6.0) is (3, 0, 1, 2).
     */
    def rank: VectorI = VectorI (iqsort (v))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of 'this' vector from left to right (e.g., create a
     *  CDF from a pmf).  Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: $VECTOR =
    {
        val c = new $VECTOR (dim)
        var s = $ZERO
        for (i <- range) { s += v(i); c.v(i) = s }
        c
    } // cumulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: $VECTOR = this * ($ONE / v.sum)

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
     *  @param b  the other vector (any kind)
     */
    def dot (b: $VECTO): $BASE =
    {
        var s = $ZERO
        for (i <- range) s += v(i) * b(i)
        s
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector (same kind, more efficient)
     */
    def dot (b: $VECTOR): $BASE =
    {
        var s = $ZERO
        for (i <- range) s += v(i) * b.v(i)
        s
    } // dot
"""; val code8 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector, i.e., the sum of the
     *  absolute values of the elements.
     */
    def norm1: $BASE =
    {
        var s = $ZERO
        for (x <- v) s += ABS (x)
        s
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
     *  @param b  the other vector (any kind)
     */
    def max (b: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = if (b(i) > v(i)) b(i) else v(i)
        c
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector (same kind, more efficient)
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
     *  @param b  the other vector (any kind)
     */
    def min (b: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (dim)
        for (i <- range) c.v(i) = if (b(i) < v(i)) b(i) else v(i)
        c
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector (same kind, more efficient)
     */
    def min (b: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (dim)
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
"""; val code9 = raw"""
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
        val j = argmin (e); if (v(j) < $ZERO) j else -1
    } // argmaxNeg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if it's not positive).
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
    def indexOf (x: $BASE, e: Int = dim): Int =
    {
        for (i <- 0 until e if v(i) == x) return i; -1
    } // indexOf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: ($BASE) => Boolean): Int = v.indexWhere (p)

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
    /** Count the number of zero elements in the 'this' vector.
     */
    def countZero: Int =
    {
        var count = 0
        for (i <- 0 until dim if v(i) == $ZERO) count += 1
        count
    } // countZero
"""; val code10 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' vector at or below the threshold 'thres' by setting
     *  them to zero.  Iterative algorithms give approximate values and if very close
     *  to zero, may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double = TOL, relative: Boolean = true): $VECTOR =
    {
        val s = if (relative) mag else $ONE              // use vector magnitude or 1
        for (i <- range) if (ABS (v(i)) <= thres * s) v(i) = $ZERO
        this
    } // clean

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
//  def distinct: Int =
//  {
//      var count = 1
//      val us = new $VECTOR (this); us.sort ()                // sorted vector
//      for (i <- 1 until dim if us(i) != us(i-1)) count += 1
//      count
//  } // distinct

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new vector consisting of the distinct elements from 'this' vector.
     */
    def distinct: $VECTOR = $VECTOR (v.distinct)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int = v.distinct.length

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: $BASE): Boolean = v contains x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether any of the elements in 'x' are contained in 'this' vector.
     *  @param x  the vector of elements to be checked
     */
    def containsAny (x: $VECTOR): Boolean = (v intersect x.v).length != 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the intersection of vectors 'this' and 'x'.
     *  @param x  the other vector
     */
    def intersect (x: $VECTOR): $VECTOR = $VECTOR (v intersect x.v)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the order of the elements in 'this' vector.
     */
    def reverse (): $VECTOR = new $VECTOR (dim, v.reverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' vector is in sorted (ascending) order.
     */
    def isSorted: Boolean = (new $SORTING (v)).isSorted

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort () { quickSort (v)$ORD }

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
    def isNonnegative: Boolean = { for (i <- range if v(i) < $ZERO) return false; true }

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
"""; val code11 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether 'this' vector equals vector 'b'.
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean =
    {
//      b.isInstanceOf [$VECTOR] && (v.deep equals b.asInstanceOf [$VECTOR].v.deep)

        if (! b.isInstanceOf [$VECTO]) return false
        val bb = b.asInstanceOf [$VECTO]
        if (dim != bb.dim) return false
        val vm = if (dim > 0) mag else $ONE                             // maximum magnitude element in vector
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
        var sb = new StringBuilder ("$VECTOR(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (v(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString
  
} // $VECTOR class
"""; val code12 = raw"""

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$VECTOR` object is the companion object for the `$VECTOR` class.
 */
object $VECTOR
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from one or more values (repeated values `${BASE}`*).
     *  @param x   the first `$BASE` number
     *  @param xs  the rest of the `$BASE` numbers
     */
    def apply (x: $BASE, xs: ${BASE}*): $VECTOR =
    {
        val c = new $VECTOR (1 + xs.length)
        c(0)  = x
        for (i <- 0 until c.dim-1) c.v(i+1) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from a sequence/array of `${BASE}`s.
     *  @param xs  the sequence/array of the `$BASE` numbers
     */
    def apply (xs: Seq [$BASE]): $VECTOR =
    {
        val c = new $VECTOR (xs.length)
        for (i <- 0 until c.dim) c.v(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from one or more values (repeated values String*).
     *  For numeric types, assign missing value indicator upon format failure.
     *  @param x   the first String
     *  @param xs  the rest of the Strings
     */
    def apply (x: String, xs: String*): $VECTOR =
    {
        val c = new $VECTOR (1 + xs.length)
        try c(0)  = $FROM_STR1
        catch { case _ : NumberFormatException | _ : ClassCastException => c.v(0) = no$BASE }
        for (i <- 1 until c.dim) {
            try c.v(i) = $FROM_STR3
            catch { case _ : NumberFormatException | _ : ClassCastException => c.v(i) = no$BASE }
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `$VECTOR` from an array of Strings.
     *  For numeric types, assign missing value indicator upon format failure.
     *  @param xs  the array of the Strings
     */
    def apply (xs: Array [String]): $VECTOR =
    {
        val c = new $VECTOR (xs.length)
        for (i <- c.range) {
            try c.v(i) = $FROM_STR2
            catch { case _ : NumberFormatException | _ : ClassCastException => c.v(i) = no$BASE }
        } // for
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
    /** Create a `$VECTOR` with 'n' elements and fill it with the value 'x'.
     *  @param n  the number of elements
     *  @param x  the value to assign to all elements
     */
    def fill (n: Int)(x: $BASE): $VECTOR =
    {
        val c = new $VECTOR (n)
        c.set (x)
        c
    } // fill

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the size of the new vector
     */
    def one (size: Int): $VECTOR = new $VECTOR (size, Array.fill (size)($ONE))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part (any kind)
     */
    def ++ (b: $BASE, u: $VECTO): $VECTOR =
    {
        val c = new $VECTOR (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u(i-1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part (same kind, more efficient)
     */
    def ++ (b: $BASE, u: $VECTOR): $VECTOR =
    {
        val c = new $VECTOR (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i-1)
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
"""; val code13 = raw"""

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `${VECTOR}Test` object tests the operations provided by `$VECTOR`.
 *  > runMain scalation.linalgebra.${VECTOR}Test
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

        println ("x.abs    = " + x.abs)
        println ("x.recip  = " + x.recip)
        println ("x.sum    = " + x.sum)
        println ("x.sumNE  = " + x.sumNE (0))
        println ("x.sumPos = " + x.sumPos)
        println ("x.rank   = " + x.rank)
        println ("x dot y  = " + (x dot y))
        println ("x ∙ y    = " + x ∙ y)
        println ("x.normSq = " + x.normSq)
        println ("x.norm   = " + x.norm)
        println ("x.norm1  = " + x.norm1)
        println ("x.min    = " + x.min ())
        println ("x.max    = " + x.max ())
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

//      println (code); println (code2); println (code3); println (code4)
//      println (code5); println (code6); println (code7); println (code8)
//      println (code9); println (code10); println (code11); println (code12)
//      println (code13)

        val writer = new PrintWriter (new File (DIR + _l + VECTOR + ".scalaa"))
        writer.write (code)
        writer.write (code2)
        writer.write (code3)
        writer.write (code4)
        writer.write (code5)
        writer.write (code6)
        writer.write (code7)
        writer.write (code8)
        writer.write (code9)
        writer.write (code10)
        writer.write (code11)
        writer.write (code12)
        writer.write (code13)
        writer.close ()
    } // for

} // BldVector object

