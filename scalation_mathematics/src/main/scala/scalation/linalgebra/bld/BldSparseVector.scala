
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Vishnu Gowda Harish
 *  @version 1.6
 *  @date    Sat May 30 13:51:12 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldSparseVector` object is used to build vector classes for various base types.
 *  > runMain scalation.linalgebra.bld.BldSparseVector
 */
object BldSparseVector extends App with BldParams
{
    println ("BldSparseVector: generate code for Sparse Vector classes")

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
        val ORD       = if (BASE == "StrNum") "(StrO.ord)" else ""
        val FROM_STR1 = if (CUSTOM contains BASE) s"$BASE (x)" else s"x.to$BASE"
        val FROM_STR2 = if (CUSTOM contains BASE) s"$BASE (xs(i))" else s"xs(i).to$BASE"
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
/** @author  John Miller, Vishnu Gowda Harish
 *  @version 1.6
 *  @date    Fri Jan 29 15:43:08 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.{breakOut, Traversable}
import scala.collection.mutable.{ArrayBuffer, IndexedSeq}
import scala.util.Sorting.quickSort

import $IMPORT

import $IMPORT2
import scalation.util.{Error, SortedLinkedHashMap}
import scalation.util.$SORTING
import scalation.util.$SORTING.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sparse$VECTOR` class stores and operates on Numeric Vectors of base type `$BASE`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim_  the dimension/size of the vector
 *  @param v     the `SortedLinkedHashMap` used to store vector elements
 */
class Sparse$VECTOR (val dim_ : Int,
           protected var v:     Sparse$VECTOR.RowMap = null)
      extends $VECTO
//    extends Traversable [$BASE] with PartiallyOrdered [Sparse$VECTOR] with Vec with Error with Serializable
{
    /** Dimension
     */
    lazy val dim = dim_
    
    if (v == null) v = new Sparse$VECTOR.RowMap ()
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param u  the other vector
     */
    def this (u: $VECTO) { this (u.dim); for (i <- range) this(i) = u(i) } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a vector and assign values from vector 'u'.
     *  @param iv  the tuple containing (index, value)
     *  @param dm  the dimension for the new vector
     */
    def this (iv: (Int, $BASE), dm: Int) { this (dm); this(iv._1) = iv._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' vector.
     */
    def copy: Sparse$VECTOR = new Sparse$VECTOR (dim, v.clone.asInstanceOf [Sparse$VECTOR.RowMap])
"""; val code2 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def zero (size: Int = dim): Sparse$VECTOR = new Sparse$VECTOR (size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the number of elements in the vector
     */
    def one (size: Int = dim): Sparse$VECTOR = { val sv = new Sparse$VECTOR (size); sv.set ($ONE); sv }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): Sparse$VECTOR = new Sparse$VECTOR ((j, $ONE), size)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): Sparse$VECTOR = new Sparse$VECTOR ((j, -$ONE), size) 
      
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): $BASE = if (i >= dim) throw new IndexOutOfBoundsException else v.getOrElse (i, $ZERO)
                                   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): Sparse$VECTOR = slice (r.start, r.end)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire array.
     */
    def apply (): IndexedSeq [$BASE] =
    {
        (for (i <- range) yield v.getOrElse (i, $ZERO))(breakOut)
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: $BASE) 
    { 
        if (i >= dim) throw new IndexOutOfBoundsException
        if (x =~ $ZERO) v.remove(i) else v(i) = x        
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: $BASE)  { for (i <- r) this(i) = x } 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: $VECTO) { for (i <- r) this(i) = u(i - r.start) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: $BASE => U) { var i = 0; while (i < dim) { f (this(i)); i += 1 } } 
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: $BASE) { for (i <- range) this(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in array 'u'.
     *  @param u  the array of values to be assigned
     */
    def set (u: Seq [$BASE]) { for (i <- range) this(i) = u(i) }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `Sparse$VECTOR` into a `VectorI`.
     */
    def toInt: VectorI = 
    {
        val c = new VectorI (dim)
        for (i <- range) c(i) = this(i).toInt
        c
    } // toInt
"""; val code3 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `Sparse$VECTOR` into a `VectorL`.
      */
    def toLong: VectorL =
    { 
        val c = new VectorL (dim)
        for (i <- range) c(i) = this(i).toLong
        c
    } // toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `Sparse$VECTOR` into a `VectorD`.
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
    def expand (more: Int = dim): Sparse$VECTOR =
    {
        if (more < 1) this       // no change
        else {
            val sv = new Sparse$VECTOR (dim + more)
            sv.v ++= v
            sv
        } // if
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's entire array of stored (i.e., non-zero) elements.
     */
    private def nonZero: Array [$BASE] = v.values.toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's positive elements as an array.
     */
    private def positive: Array [$BASE] = v.values.filter (_ > $ZERO).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return 'this' vector's negatives elements as an array.
     */
    private def negative: Array [$BASE] = v.values.filter (_ < $ZERO).toArray
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    override def filter (p: $BASE => Boolean): Sparse$VECTOR =
    { 
        Sparse$VECTOR (for (i <- range if p (this(i)) ) yield v(i))
    } // filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter out the missing elements of 'this' vector based on the predicate
     *  that 'e != no$BASE'.
     */
    def filterMissing: Sparse$VECTOR = filter (_ != no$BASE)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: $BASE => Boolean): IndexedSeq [Int] =
    {
        (for (i <- range if p (this(i))) yield i)(breakOut)
    } // filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector and vector 'v2' based on the binary
     *  predicate 'p', returning the index positions for both vectors.
     *  @param v2  the other vector to compare with
     *  @param p   the binary predicate (`Boolean` function) to apply
     */
    def filterPos2 (v2: $VECTO, p: ($BASE, $BASE) => Boolean): IndexedSeq [(Int, Int)] =
    {
        var result = IndexedSeq [(Int, Int)] ()
        for (i <- range; j <- v2.range if p(this(i), v2(j))) result = result :+ (i, j)
        result
    } // filterPos2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: $BASE => $BASE): Sparse$VECTOR = Sparse$VECTOR (this ().map (f))
"""; val code4 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int): Sparse$VECTOR =
    {
        null    // FIX: new Sparse$VECTOR (till - from, v.slice (from, till))
    } // slice
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): Sparse$VECTOR =
    {
        Sparse$VECTOR (for (i <- 0 until basis.length) yield this(basis(i)))
    } // select

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: $VECTO): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim + b.dim)
        for (i <- c.range) c(i) = if (i < dim) this(i) else b(i - dim)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: $BASE): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim + 1)
        for (i <- c.range) c(i) = if (i < dim) this(i) else s
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: $VECTO): Sparse$VECTOR = { Sparse$VECTOR (for ( i <- range) yield this(i) + b(i)) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: $BASE): Sparse$VECTOR =
    {
        if (s =~ $ZERO) this 
        else Sparse$VECTOR (for (i <- range) yield this(i) + s)       
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._2' only at position 's._1' e.g., 'x + (3, 5.5).
     *  @param s  the (scalar, position) to add
     */
    def + (s: (Int, $BASE)): Sparse$VECTOR =
    {
        if (s._1 =~ $ZERO) this
        else { val c = new Sparse$VECTOR (this); c(s._1) += s._2; c }
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: $VECTO): Sparse$VECTOR = { for (i <- range) this(i) += b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: $BASE): Sparse$VECTOR = { for (i <- range) this(i) += s; this } 
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        for (x <- v) c(x._1) = -x._2 
        c
    } // unary_-
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: $VECTO): Sparse$VECTOR = { Sparse$VECTOR (for ( i <- range) yield this(i) - b(i)) }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: $BASE): Sparse$VECTOR =
    {
        if (s =~ $ZERO) this 
        else Sparse$VECTOR (for (i <- range) yield this(i) - s)       
    } // -
"""; val code5 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._2' only at position 's._1', e.g., 'x - (3, 5.5).
     *  @param s  the (scalar, position) to subtract
     */
    def - (s: (Int, $BASE)): Sparse$VECTOR =
    {
        if (s._1 =~ $ZERO) this
        else { val c = new Sparse$VECTOR (this); c(s._1) -= s._2; c }
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to subtract
     */
    def -= (b: $VECTO): Sparse$VECTOR = { for (i <- range) this(i) -= b(i); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to subtract
     */
    def -= (s: $BASE): Sparse$VECTOR = { for (i <- range) this(i) -= s; this }
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: $VECTO): Sparse$VECTOR = 
    {     
        val c = new Sparse$VECTOR (dim)
        for (x <- v) c(x._1) = x._2 * b(x._1) 
        c    
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: $BASE): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        if (s !=~ $ZERO) for (x <- v) c(x._1) = x._2 * s  
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's._2' only at position 's._1', e.g., 'x * (3, 5.5).
     *  @param s  the (scalar, position) to multiply by
     */
    def * (s: (Int, $BASE)): Sparse$VECTOR =
    {
        if (s._1 =~ $ZERO) this
        else { val c = new Sparse$VECTOR (this); c(s._1) *= s._2; c }
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to multiply by
     */
    def *= (b: $VECTO): Sparse$VECTOR = { for (x <- v) this(x._1) = x._2 * b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def *= (s: $BASE): Sparse$VECTOR = { for (x <- v) this(x._1) = x._2 * s; this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: $VECTO): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        for (x <- v) c(x._1) = x._2 / b(x._1) 
        c    
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: $BASE): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        for (x <- v) c(x._1) = x._2 / s  
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's._2' only at position 's._1', e.g., 'x / (3, 5.5).
     *  @param s  the (scalar, position) to divide by
     */
    def / (s: (Int, $BASE)): Sparse$VECTOR =
    {
        if (s._1 =~ $ZERO) this
        else { val c = new Sparse$VECTOR (this); c(s._1) /= s._2; c }
    } // /
"""; val code6 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to divide by
     */
    def /= (b: $VECTO): Sparse$VECTOR = { for (x <- v) this(x._1) = x._2 / b(x._1); this }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to divide by
     */
    def /= (s: $BASE): Sparse$VECTOR = { for (x <- v) this(x._1) = x._2 / s; this }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: $EXPON): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        if (s =~ $ZERO) c.set($ONE)
        else for (x <- v) c(x._1) = x._2 ~^ s
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: $EXPON): Sparse$VECTOR = 
    {
        if (s =~ $ZERO) set ($ONE) else for (x <- v) this(x._1) = x._2 ~^ s
        this
    } // ~^= 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        for (x <- v) c(x._1) = $ONE / x._2
        c
    } // recip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        for (x <- v) c(x._1) = ABS (x._2)
        c        
    } // abs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */ 
    def sum: $BASE = 
    {
        var sum = $ZERO
        for (x <- v) sum += x._2
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): $BASE = sum - this(i)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: $BASE = 
    {    
        var sum = $ZERO
        for (x <- v) sum += MAX (x._2, $ZERO)
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
    def cumulate: Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
        var sum = $ZERO
        for (x <- v) { sum += x._2; c(x._1) = sum }
        c
    } // cumulate
"""; val code7 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: Sparse$VECTOR = this * ($ONE / sum)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: Sparse$VECTOR = this * ($ONE / norm)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: Sparse$VECTOR = this * ($ONE / max ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: $VECTO): $BASE = 
    {
        var sum = $ZERO
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: Sparse$VECTOR): $BASE = 
    {
        var sum = $ZERO
        for (x <- v) sum += x._2 * b.v.getOrElse (x._1, $ZERO)
        sum
    } // dot
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: $VECTO): $BASE = 
    {
        var sum = $ZERO
        for (x <- v) sum += x._2 * b(x._1) 
        sum
    } // ⦁
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ⦁ (b: Sparse$VECTOR): $BASE = 
    {
        var sum = $ZERO
        for (x <- v) sum += x._2 * b.v.getOrElse(x._1, $ZERO)
        sum
    } // ⦁
     
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: $BASE =
    {  
        var sum = $ZERO
        for (x <- v) sum += ABS (x._2)
        sum
    } // norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): $BASE =
    {
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 > c)) c = x._2
        c
    } // max
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: $VECTO): Sparse$VECTOR =
    { 
        val c = new Sparse$VECTOR (dim)
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
    def min (e: Int = dim): $BASE =
    {     
        var c = this(0)
        for (x <- v if (x._1 < e && x._2 < c)) c = x._2
        c
    } // max
"""; val code8 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: $VECTO): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (dim)
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
    def firstNeg (e: Int = dim): Int = { for (x <- v if x._2 < $ZERO) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int = { for (x <- v if x._2 > $ZERO) return x._1; -1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: $BASE, e: Int = dim): Int =
    {        
        if (x =~ $ZERO) { for (i <- range if this(i) =~ $ZERO) return i; -1 }
        for (y <- v if y._1 < e && y._2 == x) return y._1; -1 
    } // indexOf
"""; val code9 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: ($BASE) => Boolean): Int =
    {
        for (i <- range if p(this(i))) return i; -1
    } // indexWhere

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int =
    {
        var count = 0
        for (x <- v if x._2 < $ZERO) count += 1
        count
    } // countNeg

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int =
    {
        var count = 0
        for (x <- v if x._2 > $ZERO) count += 1
        count
    } // countPos

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of zero elements in 'this' vector.
     */
    def countZero: Int =
    {
        var count = 0
        for (x <- v if x._2 == $ZERO) count += 1
        count
    } // countZero

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a new vector consisting of the distinct elements from 'this' vector.
     */
    def distinct: Sparse$VECTOR = 
    {
        val us = new Sparse$VECTOR (this); us.sort ()
        var c1 = ArrayBuffer [$BASE]()     
        c1 += us(0)  
        for (i <- 1 until dim) { val x = us(i); if (x != us(i-1)) c1 += x }
        Sparse$VECTOR(c1)       
    } // distinct
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int =
    {
        var count = 1
        val us = new Sparse$VECTOR (this); us.sort ()                // sorted vector
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
    def contains (x: $BASE): Boolean = v.values.exists (_ == x)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the order of the elements in 'this' vector.
     */
    def reverse (): Sparse$VECTOR = 
    {        
         Sparse$VECTOR (for (i <- (0 until dim).reverse) yield this(i))
    } // reverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' vector is in sorted (ascending) order.
     */
    def isSorted: Boolean = (new $SORTING (this().toArray)).isSorted
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort () 
    {                  
        val neg = negative; quickSort(neg)
        val pos = positive; quickSort(pos)  
        v =  Sparse$VECTOR (neg ++ Array.fill(dim - (neg.length + pos.length))($ZERO) ++ pos).v          
    } // sort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 ()
    {                  
        val neg = negative; qsort2 (neg)
        val pos = positive; qsort2 (pos)  
        v =  Sparse$VECTOR (pos ++ Array.fill (dim - (neg.length + pos.length))($ZERO) ++ neg).v          
    } // sort2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)
    {      
         val (x, y) = (this(i), this(j))
         if (x =~ $ZERO && y =~ $ZERO) return      
         this(i) = y; this(j) = x           
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector 'b' is at least as long as 'this' vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: Sparse$VECTOR): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {   
        for ( x <- v if x._2 < $ZERO) return false
        true
    } // isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def tryCompareTo [B >: Sparse$VECTOR] (b: B)
        (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int] =
    {
        var le = true
        var ge = true

        for (i <- range) {
            val b_i = b.asInstanceOf [Sparse$VECTOR] (i)
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
        b.isInstanceOf [Sparse$VECTOR] && (v equals b.asInstanceOf [Sparse$VECTOR].v)
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode (): Int = v.hashCode
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' sparse vector to a dense vector.
     */
    def toDense: $VECTOR = $VECTOR (for (i <- range) yield this(i))
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a `String`.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("Sparse$VECTOR(")
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
        var sb = new StringBuilder ("Sparse$VECTOR(")
        if (dim == 0) return sb.append (")").mkString
        for (i <- range) {
            sb.append (fString.format (this(i)))
            if (i == dim-1) sb = sb.dropRight (1)
        } // for
        sb.replace (sb.length-1, sb.length, ")").mkString
    } // toString2
    
} // Sparse$VECTOR class
"""; val code10 = raw"""

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sparse$VECTOR` object is the companion object for the `Sparse$VECTOR` class.
 */
object Sparse$VECTOR
{
    /** Shorthand type definition for sparse vector
     */
    type RowMap = SortedLinkedHashMap [Int, $BASE]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Sparse$VECTOR` from one or more values (repeated values `$BASE`*).
     *  @param x   the first `$BASE` number
     *  @param xs  the rest of the `$BASE` numbers
     */
    def apply (x: $BASE, xs: $BASE*): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (1 + xs.length)
        c(0) = x
        for (i <- 1 until c.dim) c(i) = xs(i-1)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Sparse$VECTOR` from a sequence of `$BASE`s.
     *  @param xs  the sequence of the $BASE numbers
     */
    def apply (xs: Seq [$BASE]): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (xs.length)
        for (i <- 0 until c.dim) c(i) = xs(i)
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Sparse$VECTOR` from one or more values (repeated values `String`*).
     *  @param x   the first `String`
     *  @param xs  the rest of the `String`s
     */
    def apply (x: String, xs: String*): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (1 + xs.length)
        c(0)  = $FROM_STR1
        for (i <- 0 until c.dim-1) c(i+1) = $FROM_STR2
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Sparse$VECTOR` from an array of `String`s.
     *  @param xs  the array of the `String`s
     */
    def apply (xs: Array [String]): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (xs.length)
        for (i <- c.range) c(i) = $FROM_STR2
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Sparse$VECTOR` from an array of `String`s, skipping the first 'skip'
     *  elements.  If an element is non-numeric, use its hashcode.
     *  FIX:  Might be better to map non-numeric `String`s to ordinal values.
     *  @param xs    the array of the `String`s
     *  @param skip  the number of elements at the beginning to skip (e.g., id column)
     */
    def apply (xs: Array [String], skip: Int): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (xs.length - skip)
        for (i <- skip until xs.length) {
            c.v(i - skip) = if (xs(i) matches "[\\-\\+]?\\d*(\\.\\d+)?") $FROM_STR2 else xs(i).hashCode ()
        } // for
        c
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate scalar 'b' and vector 'u'.
     *  @param b  the scalar to be concatenated - first part
     *  @param u  the vector to be concatenated - second part
     */
    def ++ (b: $BASE, u: Sparse$VECTOR): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (u.dim + 1)
        for (i <- c.range) c(i) = if (i == 0) b else u.v(i - 1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a `Sparse$VECTOR` containing a sequence of increasing integers in a range.
     *  @param start  the start value of the vector, inclusive
     *  @param end    the end value of the vector, exclusive (i.e., the first value not returned)
     */
    def range (start: Int, end: Int): Sparse$VECTOR =
    {
        val c = new Sparse$VECTOR (end - start)
        for (i <- c.range) c.v(i) = (start + i).to$BASE
        c
    } // range

} // Sparse$VECTOR object
"""; val code11 = raw"""

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest` object tests the operations provided by `Sparse$VECTOR`.
 *  > runMain scalation.linalgebra.SparseVectorDTest
 */
object Sparse${VECTOR}Test extends App
{   
    var x: Sparse$VECTOR = null
    var y: Sparse$VECTOR = null

    for (l <- 1 to 4) {
        println ("\n\tTest Sparse$VECTOR on vectors of dim " + l)
        x = new Sparse$VECTOR (l)
        y = new Sparse$VECTOR (l)
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

    val z = Sparse$VECTOR ("1", "2", "3", "4")
    println ("z = " + z)
    println ("z.map (_ * 2)    = " + z.map ((e: $BASE) => e * 2))
    println ("z.filter (_ > 2) = " + z.filter (_ > 2))

} // SparseVectorDTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseVectorDTest2` object tests the dot product operation provided by `Sparse$VECTOR`.
 *  > runMain scalation.linalgebra.SparseVectorDTest2
 */
object Sparse${VECTOR}Test2 extends App
{
    val r  = scala.util.Random  
    val v1 = new $VECTOR (1000000)  
    val v2 = new $VECTOR (1000000)

    for (x <- 0 until 10) {    
        var cnt = 0   
        while (cnt < 1000000) {
            val x  = r.nextInt (50).to$BASE + 1
            val x1 = r.nextInt (90)
            if(x1 == 1) v1(cnt) = x
            else v1(cnt) = $ZERO  
            val z  = r.nextInt (50).to$BASE + 1
            val z1 = r.nextInt (90)
            if(z1 == 1) v2(cnt) = z
            else v2(cnt) = $ZERO
            cnt += 1
        } // while   

    var z1 = Sparse$VECTOR (v1())
    var z2 = Sparse$VECTOR (v2())
  
    println (v1.dot (v2))
    println (z1.⦁(z2))
  } // for
    
} // SparseVectorDTest2
"""

// Ending of string holding code template --------------------------------------

//      println (code); println (code2); println (code3); println (code4)
//      println (code5); println (code6); println (code7); println (code8)
//      println (code9); println (code10); println (code11)

        val writer = new PrintWriter (new File (DIR + _l + "Sparse"+ VECTOR + ".scalaa"))
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
        writer.close ()
    } // for

} // BldSparseVector object

