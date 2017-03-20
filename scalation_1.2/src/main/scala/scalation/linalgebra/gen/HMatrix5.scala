
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng, Zhe Jin
 *  @version 1.2
 *  @date    Mon Sep 01 16:00:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.gen

import scala.reflect.ClassTag

import scalation.linalgebra.{VectoI, VectorI}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix5` class is a simple implementation of a 5-dimensional hypermatrix.
 *  The first two dimensions must be fixed and known, while the third, fourth and fifth
 *  dimensions may be dynamically allocated by the user.
 *-----------------------------------------------------------------------------
 *  For Bayes classifier the convention is as follows:
 *  dimension 1: possible values for class c (e.g., k = 2 => 0, 1 )
 *  dimension 2: indicator for first feature x_j (e.g., n = 3 => x_0, x_1, x_3)
 *  dimension 3: possible values for x_j OR indicator for second feature y_j
 *  dimension 4: possible values for x_j's 1st parent OR possible values for x_j
 *  dimension 5: possible values for x_j's 2st parent OR possible values for y_j
 *-----------------------------------------------------------------------------
 *  @param dim1  size of the 1st dimension of the hypermatrix
 *  @param dim2  size of the 2nd dimension of the hypermatrix
 */
class HMatrix5 [T: ClassTag: Numeric] (val dim1: Int, val dim2: Int)
      extends Error
{
    /** Range for the first dimension
     */
    private val range1 = 0 until dim1

    /** Range for the second dimension
     */
    private val range2 = 0 until dim2

    /** Multi-dimensional array storage for hypermatrix
     */
    private val hmat = Array.ofDim [Array [Array [Array [T]]]] (dim1, dim2)

    /** Format string used for printing vector values (change using 'setFormat')
     */
    protected var fString = "%g,\t"

    /** Import Numeric evidence (gets nu val from superclass)
     */
    val nu = implicitly [Numeric [T]]
    import nu._

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat'.
     *  @param newFormat  the new format string
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a cuboidic 5-dimensional hypermatrix, where the last 3 dimensions
     *  are fixed as well.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     *  @param dim3  size of the 3rd dimension of the hypermatrix
     *  @param dim4  size of the 4th dimension of the hypermatrix
     *  @param dim5  size of the 5th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int, dim4: Int, dim5: Int) =
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dim3, dim4, dim5)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 5-dimensional hypermatrix, where the 3rd dimension
     *  is fixed as well, but the 4th and 5th dimensions may vary.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     *  @param dim3  size of the 3rd dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int) =
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [Array[Array [T]]] (dim3)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 5-dimensional hypermatrix, where the 3rd and 4th dimensions
     *  are fixed as well, but the 5th dimension may vary.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     *  @param dim3  size of the 3rd dimension of the hypermatrix
     *  @param dim4  size of the 4th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int, dim4: Int) =
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [Array [T]] (dim3, dim4)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 5-dimensional hypermatrix, where 3rd and 4th dimensions are
     *  fixed as well, and the 5th dimension varies only with the 4th dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dim2   size of the 2nd dimension of the hypermatrix
     *  @param dim3   size of the 3rd dimension of the hypermatrix
     *  @param dim4   size of the 4th dimension of the hypermatrix
     *  @param dims5  array of sizes of the 5th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int, dim4: Int, dims5: Array [Int]) =
    {
        this (dim1, dim2, dim3, dim4)
        if (dims5.length != dim4) flaw ("constructor", "wrong number of elements for 5th dimension")
        for (i <- range1; j <- range2; k <- 0 until dim3; l <- 0 until dim4) hmat(i)(j)(k)(l) = Array.ofDim [T] (dims5(l))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 5-dimensional hypermatrix, where the 3rd dimension is fixed as
     *  well, and the 4th varies with 2nd and 5th dimensions varies the 3rd dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dim2   size of the 2nd dimension of the hypermatrix
     *  @param dim3   size of the 3rd dimension of the hypermatrix
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     *  @param dims5  array of sizes of the 5th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int, dims4: Array [Int], dims5: Array [Int]) =
    {
        this (dim1, dim2, dim3)
        if (dims4.length != dim2) flaw ("constructor", "wrong number of elements for 4th dimension")
        if (dims5.length != dim3) flaw ("constructor", "wrong number of elements for 5th dimension")
        for (i <- range1; j <- range2; k <- 0 until dim3) hmat(i)(j)(k) = Array.ofDim [T] (dims4(j), dims5(k))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 5-dimensional hypermatrix, where the 3rd, 4th and 5th dimensions
     *  vary only with the 2nd dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dim2   size of the 2nd dimension of the hypermatrix
     *  @param dims3  array of sizes of the 3rd dimension of the hypermatrix
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     *  @param dims5  array of sizes of the 5th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dims3: Array [Int], dims4: Array [Int], dims5: Array [Int]) =
    {
        this (dim1, dim2)
        if (dims3.length != dim2) flaw ("constructor", "wrong number of elements for 3rd dimension")
        if (dims4.length != dim2) flaw ("constructor", "wrong number of elements for 4th dimension")
        if (dims5.length != dim2) flaw ("constructor", "wrong number of elements for 5th dimension")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dims3(j), dims4(j), dims5(j))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 3rd dimension for the given 'i' and 'j'.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     */
    def dim_3 (i: Int, j: Int): Int = hmat(i)(j).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 4th dimension for the given 'i', 'j', and 'k'.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     */
    def dim_4 (i: Int, j: Int, k: Int): Int = hmat(i)(j)(k).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 5th dimension for the given 'i', 'j', 'k', and 'l'.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     *  @param l  4rd dimension index of the hypermatrix
     */
    def dim_5 (i: Int, j: Int, k: Int, l: Int): Int = hmat(i)(j)(k)(l).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate a 3D array for the 3rd, 4th and 5th dimensions of the hypermatrix
     *  for the given '(i, j)' cell.
     *  @param i     1st dimension index of the hypermatrix
     *  @param j     2nd dimension index of the hypermatrix
     *  @param dim3  size of the 3rd dimension
     *  @param dim4  size of the 4th dimension
     *  @param dim5  size of the 5th dimension
     */
    def alloc (i: Int, j: Int, dim3: Int, dim4: Int, dim5: Int)
    {
        hmat(i)(j) = Array.ofDim [T] (dim3, dim4, dim5)
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate all elements of the 3rd and 4th dimensions of the hypermatrix,
     *  where the 3rd and 4th dimensions only vary with the 2nd dimension.
     *  @param dims3  array of sizes of the 3rd dimension of the hypermatrix
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     */
    def alloc (dims3: Array [Int], dims4: Array [Int])
    {
        if (dims3.length != dim2) flaw ("alloc", "wrong number of elements for 3rd dimension")
        if (dims4.length != dim2) flaw ("alloc", "wrong number of elements for 4th dimension")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [Array[T]] (dims3(j), dims4(j))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate all elements of the 4th and 5th dimensions of the hypermatrix,
     *  where the 4th and 5th(?) dimensions only vary with the 2nd dimension.
     *  FIX:  clean up and regualrize all the 'alloc' methods.
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     *  @param dims5  array of sizes of the 5th dimension of the hypermatrix
     */
    def alloc2 (dims4: Array [Int], dims5: Array [Int])
    {
        if (dims4.length != dim2) flaw ("alloc", "wrong number of elements for 4th dimension")
//      if (dims5.length != dim2) flaw ("alloc", "wrong number of elements for 5th dimension")
        for (i <- range1; j <- range2; k <- 0 until hmat(i)(j).length) hmat(i)(j)(k) = Array.ofDim [T] (dims4(j), dims5(j))
    } // alloc2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate the 3rd to 5th dimensions of the hypermatrix based on the given
     *  value counts for the dimensions.
     *  @param vc3   value count array giving sizes for 3rd dimension based on j
     *  @param vc4   value count array giving sizes for 4th dimension based on j
     *  @param vc5   value count array giving sizes for 5th dimension based on j
     */
    def alloc (vc3: VectoI, vc4: VectoI, vc5: VectoI)
    {
        if (vc3.size != dim2) flaw ("alloc", "Dimensions mismatch")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (vc3(j), vc4(j), vc5(j))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve a single element of the hypermatrix.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     *  @param l  4th dimension index of the hypermatrix
     *  @param m  5th dimension index of the hypermatrix
     */
    def apply (i: Int, j: Int, k: Int, l: Int, m: Int): T = hmat(i)(j)(k)(l)(m)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single element of the hypermatrix to the given value.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     *  @param l  4th dimension index of the hypermatrix
     *  @param m  5th dimension index of the hypermatrix
     *  @param v  the value to be updated at the above position in the hypermatrix
     */
    def update (i: Int, j: Int, k: Int, l: Int, m: Int, v: T) = hmat(i)(j)(k)(l)(m) = v

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' hypermatrix and hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def + (b: HMatrix5 [T]): HMatrix5 [T] =
    {
        val c = new HMatrix5 [T] (dim1, dim2)
        for (i <- range1; j <- range2) {
            val kk = dim_3(i, j)
            val ll = dim_4(i, j, kk)
            val mm = dim_5(i, j, kk, ll)
            c.alloc (i, j, kk, ll, mm)
            for (k <- 0 until kk; l <- 0 until ll; m <- 0 until mm) {
                c.hmat(i)(j)(k)(l)(m) = hmat(i)(j)(k)(l)(m) + b.hmat(i)(j)(k)(l)(m)
            } // for
        } // for
        c
    } // +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' hypermatrix subtract hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def - (b: HMatrix5 [T]): HMatrix5 [T] =
    {
        val c = new HMatrix5 [T] (dim1, dim2)
        for (i <- range1; j <- range2) {
            val kk = dim_3(i, j)
            val ll = dim_4(i, j, kk)
            val mm = dim_5(i, j, kk, ll)
            c.alloc (i, j, kk, ll, mm)
            for (k <- 0 until kk; l <- 0 until ll; m <- 0 until mm) {
                c.hmat(i)(j)(k)(l)(m) = hmat(i)(j)(k)(l)(m) - b.hmat(i)(j)(k)(l)(m)
            } // for
        } // for
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the dimensions of 'this' hypermatrix are less than or equal to
     *  'le' those of the other hypermatrix 'b'.
     *  @param b  the other hypermatrix
     */
    def leDimensions (b: HMatrix5 [T]): Boolean =
    {
        if (dim1 > b.dim1 || dim2 > b.dim2) return false
        for (i <- range1; j <- range2) {
            if (dim_3 (i,j) > b.dim_3 (i,j)) return false
            if (dim_4 (i, j, dim_3(i, j)) > b.dim_4(i, j, b.dim_3(i, j))) return false
            if (dim_5 (i, j, dim_3(i, j),dim_4 (i, j, dim_3(i, j))) > b.dim_5(i, j, b.dim_3(i, j),dim_4 (i, j, dim_3(i, j)))) return false
        } // for
        true
    } // leDimensions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the hypermatrix element values to 'x'.
     *  @param x  the value to set all elements to
     */
    def set (x: T) = { for (i <- range1; j <- range2; k <- 0 until dim_3 (i, j);
                            l <-0 until dim_4(i, j, dim_3(i, j));
                            m <-0 until dim_5 (i, j, dim_3(i, j),dim_4 (i, j, dim_3(i, j)) )) hmat(i)(j)(k)(l)(m) = x }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear (make null) all contents in the 3rd, 4th and 5th dimensions of the hypermatrix.
     */
    def clear () = { for (i <- range1; j <- range2) hmat(i)(j) = null }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' hypermatrix to a string.
     */
    override def toString: String =
    {
        val sb = new StringBuilder ("\nHMatrix5(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            for (j <- range2 if hmat(i)(j) != null) {
                for (k <- 0 until dim_3(i, j)){
                    sb.append (hmat(i)(j)(k).deep + ", ")
                    if (k == dim_3(i,j)-1) sb.replace (sb.length-1, sb.length, "\n\t\t")
                }
                if (j == dim2-1) sb.replace (sb.length-1, sb.length, "\n\t")
            } // for
        } // for
        sb.replace (sb.length-5, sb.length, ")").mkString
    } // toString

} // HMatrix5 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix5Test` object is used to test the `HMatrix5` class.
 *  > run-main scalation.linalgebra.gen.HMatrix5Test
 */
object HMatrix5Test extends App
{
    val tb = new HMatrix5 [Int] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) tb.alloc (i, j, 2, 2, 2)
    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2; l <- 0 until 2; m<- 0 until 2) tb(i, j, k, l, m) = i + j + k + l+ m
    println ("tb = " + tb)

} // HMatrix5Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix5Test2` object is used to test the `HMatrix5` class.
 *  > run-main scalation.linalgebra.gen.HMatrix5Test2
 */
object HMatrix5Test2 extends App
{
    val tb = new HMatrix5 [Double] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) tb.alloc (i, j, 2, 2, 2)
    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2; l <- 0 until 2; m <-0 until 2) tb(i, j, k, l, m) = (i + j + k + l+ m) * 0.5
    println ("tb = " + tb)

} // HMatrix5Test2 object

