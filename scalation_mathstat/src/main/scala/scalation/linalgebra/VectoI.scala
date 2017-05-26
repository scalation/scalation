
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Fri Jan 29 15:43:08 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.Traversable
import scala.collection.mutable.IndexedSeq
import scala.util.Sorting.quickSort

import scala.math.{abs => ABS, max => MAX, sqrt}
import scalation.math.int_exp

import scalation.util.Error
import scalation.util.SortingD.{iqsort, qsort2}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectoI` class stores and operates on Numeric Vectors of base type `Int`.
 *  It follows the framework of `gen.VectorN [T]` and is provided for performance.
 *  @param dim  the dimension/size of the vector
 *  @param v    the 1D array used to store vector elements
 */
trait VectoI
      extends Traversable [Int] with PartiallyOrdered [VectoI] with Vec with Error with Serializable
{
    /** Vector dimension
     */
    val dim: Int

    /** Number of elements in the vector as a Int
     */
    val nd = dim.toDouble

    /** Range for the storage array
     */
    val range = 0 until dim

    /** Format String used for printing vector values (change using 'setFormat')
     *  Ex: "%d,\t", "%.6g,\t" or "%12.6g,\t"
     */
    protected var fString = "%d,\t"

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
    def expand (more: Int = dim): VectoI
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a zero vector (all elements are zero) of length 'size'.
     *  @param size  the size of the new vector
     */
    def zero (size: Int): VectoI 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a one vector (all elements are one) of length 'size'.
     *  @param size  the size of the new vector
     */
    def one (size: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... 1, ... 0) where the 1 is at position j.
     *  @param j     the position to place the 1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def oneAt (j: Int, size: Int = dim): VectoI 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector of the form (0, ... -1, ... 0) where the -1 is at position j.
     *  @param j     the position to place the -1
     *  @param size  the size of the vector (upper bound = size - 1)
     */
    def _oneAt (j: Int, size: Int = dim): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectoI` into a `VectoI`.
     */
    def toInt: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectoI` into a `VectoL`.
      */
    def toLong: VectoL
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectoI` into a `VectoD`.
     */
    def toDouble: VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `VectoI` into a dense version.
     */
    def toDense: VectorI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     */
    def apply (i: Int): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's elements within the given range (vector slicing).
     *  @param r  the given range
     */
    def apply (r: Range): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' vector's entire sequence/array.
     */
    def apply (): IndexedSeq [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's element at the 'i'-th index position. 
     *  @param i  the given index
     *  @param x  the value to assign
     */
    def update (i: Int, x: Int)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param x  the value to assign
     */
    def update (r: Range, x: Int)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' vector's elements over the given range (vector slicing).
     *  @param r  the given range
     *  @param u  the vector to assign
     */
    def update (r: Range, u: VectoI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' vector element by element.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Int => U)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set each value in 'this' vector to 'x'.
     *  @param x  the value to be assigned
     */
    def set (x: Int)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' vector to the values in sequence 'u'.
     *  @param u  the sequence of values to be assigned
     */
    def set (u: Seq [Int])
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a copy of this Vector.
     */
    def copy: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  a new vector.
     *  @param p  the predicate (`Boolean` function) to apply
     */
//  def filter (p: Int => Boolean): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the elements of 'this' vector based on the predicate 'p', returning
     *  the index positions.
     *  @param p  the predicate (`Boolean` function) to apply
     */
    def filterPos (p: Int => Boolean): IndexedSeq [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' vector by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    def map (f: Int => Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' vector 'from' to 'end'.
     *  @param from  the start of the slice (included)
     *  @param till  the end of the slice (excluded)
     */
    override def slice (from: Int, till: Int = dim): VectoI = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select a subset of elements of 'this' vector corresponding to a 'basis'.
     *  @param basis  the set of index positions (e.g., 0, 2, 5)
     */
    def select (basis: Array [Int]): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and vector' b'.
     *  @param b  the vector to be concatenated
     */
    def ++ (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' vector and scalar 's'.
     *  @param s  the scalar to be concatenated
     */
    def ++ (s: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def + (b: VectoI): VectoI 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def + (s: Int): VectoI
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' vector and scalar 's._2' only at position 's._1'.
     *  @param s  the (position, scalar) to add
     */
    def + (s: Tuple2 [Int, Int]): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def += (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def += (s: Int): VectoI
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the negative of 'this' vector (unary minus).
     */
    def unary_- (): VectoI
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract vector 'b'.
     *  @param b  the vector to subtract
     */
    def - (b: VectoI): VectoI
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's'.
     *  @param s  the scalar to subtract
     */
    def - (s: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract scalar 's._2' only at position 's._1'.
     *  @param s  the (position, scalar) to subtract
     */
    def - (s: Tuple2 [Int, Int]): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place vector 'b'.
     *  @param b  the vector to add
     */
    def -= (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' vector subtract in-place scalar 's'.
     *  @param s  the scalar to add
     */
    def -= (s: Int): VectoI
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by vector 'b'.
     *  @param b  the vector to multiply by
     */
    def * (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' vector by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def *= (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def *= (s: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by vector 'b' (element-by-element).
     *  @param b  the vector to divide by
     */
    def / (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' vector by scalar 's'.
     *  @param s  the scalar to divide by
     */
    def / (s: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and vector 'b'.
     *  @param b  the vector to add
     */
    def /= (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' vector and scalar 's'.
     *  @param s  the scalar to add
     */
    def /= (s: Int): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing each element of 'this' vector raised to the
     *  s-th power.
     *  @param s  the scalar exponent
     */
    def ~^ (s: Int): VectoI
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise in-place each element of 'this' vector to the 's'-th power.
     *  @param s  the scalar exponent
     */
    def ~^= (s: Int): VectoI

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with that vector 'b' for inequality.
     *  @param b  that vector
     */
    def ≠ (b: VectoI) = this != b

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with that vector 'b' for less than or equal to.
     *  @param b  that vector
     */
    def ≤ (b: VectoI) = this <= b

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with that vector 'b' for greater than or equal to.
     *  @param b  that vector
     */
    def ≥ (b: VectoI) = this >= b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the square of each element of 'this' vector.
     */
    def sq: VectoI = this * this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector containing the reciprocal of each element of 'this' vector.
     */
    def recip: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector that is the element-wise absolute value of 'this' vector.
     */
    def abs: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector.
     */
    def sum: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the elements of 'this' vector skipping the 'i'-th element (Not Equal 'i').
     *  @param i  the index of the element to skip
     */
    def sumNE (i: Int): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sum the positive (> 0) elements of 'this' vector.
     */
    def sumPos: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean of the elements of 'this' vector.
     */
    def mean = sum / nd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (unbiased) sample variance of the elements of 'this' vector.
     */
    def variance = (normSq - sum * sum / nd) / (nd-1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the population variance of the elements of 'this' vector.
     *  This is also the (biased) MLE estimator for sample variance.
     */
    def pvariance = (normSq - sum * sum / nd) / nd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish the rank order of the elements in 'self' vector, e.g.,
     *  (8.0, 2.0, 4.0, 6.0) is (3, 0, 1, 2).
     */
    def rank: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cumulate the values of 'this' vector from left to right (e.g., create a
     *  CDF from a pmf).  Example: (4, 2, 3, 1) --> (4, 6, 9, 10)
     */
    def cumulate: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so that it sums to one (like a probability vector).
     */
    def normalize: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector so its length is one (unit vector).
     */
    def normalizeU: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize 'this' vector to have a maximum of one.
     */
    def normalize1: VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def dot (b: VectoI): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product (or inner product) of 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
    def ∙ (b: VectoI): Int = this dot b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) squared of 'this' vector.
     */
    def normSq: Int = this dot this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Euclidean norm (2-norm) of 'this' vector.
     */
    def norm: Int = sqrt (normSq).toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Manhattan norm (1-norm) of 'this' vector.
     */
    def norm1: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def max (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the maximum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def max (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' vector.
     *  @param e  the ending index (exclusive) for the search
     */
    def min (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Take the minimum of 'this' vector with vector 'b' (element-by element).
     *  @param b  the other vector
     */
    def min (b: VectoI): VectoI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the element with the greatest magnitude in 'this' vector.
     */
    def mag: Int = ABS (max ()) max ABS (min ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of 'this' vector (index of maximum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument maximum of 'this' vector (index of maximum element).
     *  @param s  the starting index (inclusive) for the search
     *  @param e  the ending index (exclusive) for the search
     */
    def argmax (s: Int, e: Int): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of 'this' vector (index of minimum element).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the argument minimum of 'this' vector (index of minimum element).
     *  @param s  the starting index (inclusive) for the search
     *  @param e  the ending index (exclusive) for the search
     */
    def argmin (s: Int, e: Int): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument minimum of 'this' vector (-1 if it's not negative).
     *  @param e  the ending index (exclusive) for the search
     */
    def argminNeg (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the argument maximum of 'this' vector (-1 if it's not positive).
     *  @param e  the ending index (exclusive) for the search
     */
    def argmaxPos (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first negative element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstNeg (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first positive element in 'this' vector (-1 otherwise).
     *  @param e  the ending index (exclusive) for the search
     */
    def firstPos (e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the index of the first occurrence of element 'x' in 'this' vector,
     *  or -1 if not found.
     *  @param x  the given element
     *  @param e  the ending index (exclusive) for the search
     */
    def indexOf (x: Int, e: Int = dim): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return index of first element satisfying predicate 'p', or
     *  -1 if not found.
     *  @param p  the predicate to check
     */
    def indexWhere (p: (Int) => Boolean): Int

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly negative elements in 'this' vector.
     */
    def countNeg: Int

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strictly positive elements in 'this' vector.
     */
    def countPos: Int

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a vector consisting of the distinct elements in 'this' vector.
     */
    def distinct: VectoI

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of distinct elements in 'this' vector.
     */
    def countinct: Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'x' is contained in 'this' vector.
     *  @param x  the element to be checked
     */
    def contains (x: Int): Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in ascending (non-decreasing) order.
     */
    def sort ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort 'this' vector in-place in descending (non-increasing) order.
     */
    def sort2 ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap elements 'i' and 'j' in 'this' vector.
     *  @param i  the first element in the swap
     *  @param j  the second element in the swap
     */
    def swap (i: Int, j: Int)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the other vector 'b' is at least as long as 'this' vector.
     *  @param b  the other vector
     */
    def sameDimensions (b: VectoI): Boolean = dim <= b.dim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' vector is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' vector with vector 'b'.
     *  @param b  the other vector
     */
//  private def tryCompareTo [B >: VectoI] (b: B)
//      (implicit view_1: (B) => PartiallyOrdered [B]): Option [Int]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Override equals to determine whether 'this' vector equals vector 'b..
     *  @param b  the vector to compare with this
     */
    override def equals (b: Any): Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' vector to be compatible with equals.
     */
    override def hashCode (): Int

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat' (e.g., "%.6g,\t" or "%12.6g,\t").
     *  @param newFormat  the new format String
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' vector to a String.
     */
    override def toString: String
  
} // VectoI trait


