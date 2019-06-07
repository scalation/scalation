
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.6
 *  @date    Mon Jul 27 01:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.gen

import scala.reflect.ClassTag

import scalation.linalgebra.{VectoI, VectorI}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix4` class is a simple implementation of a 4-dimensional hypermatrix.
 *  The first two dimensions must be fixed and known, while the third and fourth
 *  dimension may be dynamically allocated by the user.
 *  @param dim1  size of the 1st dimension of the hypermatrix
 *  @param dim2  size of the 2nd dimension of the hypermatrix
 */
class HMatrix4 [T: ClassTag: Numeric] (val dim1: Int, val dim2: Int)
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
    private val hmat = Array.ofDim [Array [Array [T]]] (dim1, dim2)

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
    /** Construct a cuboidic 4-dimensional hypermatrix, where the 3rd and 4th dimensions
     *  are fixed as well.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     *  @param dim3  size of the 3rd dimension of the hypermatrix
     *  @param dim4  size of the 4th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int, dim4: Int) =
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dim3, dim4)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 4-dimensional hypermatrix, where the 3rd dimension is fixed
     *  as well, but the 4th dimension my vary.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     *  @param dim3  size of the 3rd dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int) =
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [Array [T]] (dim3)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 4-dimensional hypermatrix, where the 3rd dimension is fixed,
     *  and the 4th dimension varies only with the 3rd dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dim2   size of the 2nd dimension of the hypermatrix
     *  @param dim3   size of the 3rd dimension of the hypermatrix
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int, dims4: Array [Int]) =
    {
        this (dim1, dim2, dim3)
        if (dims4.length != dim3) flaw ("constructor", "wrong number of elements for 4th dimension")
        for (i <- range1; j <- range2; k <- 0 until dim3) hmat(i)(j)(k) = Array.ofDim [T] (dims4(k))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 4-dimensional hypermatrix, where the 3rd and 4th dimensions
     *  vary only with the 2nd dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dim2   size of the 2nd dimension of the hypermatrix
     *  @param dims3  array of sizes of the 3rd dimension of the hypermatrix
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dims3: Array [Int], dims4: Array [Int]) =
    {
        this (dim1, dim2)
        if (dims3.length != dim2) flaw ("constructor", "wrong number of elements for 3rd dimension")
        if (dims4.length != dim2) flaw ("constructor", "wrong number of elements for 4th dimension")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dims3(j), dims4(j))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 3rd dimension for the given 'i' and 'j'.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     */
    def dim_3 (i: Int, j: Int): Int = hmat(i)(j).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 4th dimension for the given 'i' and 'j',
     *  and optionally 'k'.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     */
    def dim_4 (i: Int, j: Int, k: Int = 0): Int = hmat(i)(j)(k).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate a 2D array for the 3rd and 4th dimensions of the hypermatrix
     *  for the given '(i, j)' cell.
     *  @param i     1st dimension index of the hypermatrix
     *  @param j     2nd dimension index of the hypermatrix
     *  @param dim3  size of the 3rd dimension
     *  @param dim4  size of the 4th dimension
     */
    def alloc (i: Int, j: Int, dim3: Int, dim4: Int)
    {
        hmat(i)(j) = Array.ofDim [T] (dim3, dim4)
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate all elements of the 3rd and 4th dimensions of the hypermatrix,
     *  where the 3rd and 4th dimensions vary with the 2nd dimension.
     *  @param dims3  array of sizes of the 3rd dimension of the hypermatrix
     *  @param dims4  array of sizes of the 4th dimension of the hypermatrix
     */
    def alloc (dims3: Array [Int], dims4: Array [Int])
    {
        if (dims3.length != dim2) flaw ("alloc", "wrong number of elements for 3rd dimension")
        if (dims4.length != dim2) flaw ("alloc", "wrong number of elements for 4th dimension")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dims3(j), dims4(j))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate the 3rd and 4th dimensions of the hypermatrix based on the given
     *  value counts for the dimensions.
     *  @param vc3   value count array giving sizes for 3rd dimension based on j
     *  @param vc4   value count array giving sizes for 4th dimension based on j
     */
    def alloc (vc3: VectoI, vc4: VectoI)
    {
        if (vc3.size != dim2) flaw ("alloc", "Dimensions mismatch")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (vc3(j), vc4(j))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate the 3rd and 4th dimensions of the hypermatrix based on the given
     *  j-index set, value counts for the dimensions.
     *  @param jset  the set of index values for the second dimension (j) that are active
     *  @param vc3  value count array giving sizes for 3rd dimension based on j
     *  @param vc4  value count array giving sizes for 4th dimension based on j
     */
    def alloc (jset: Array [Boolean], vc3: VectoI, vc4: VectoI)
    {
        if (vc3.size != dim2) flaw ("alloc", "Dimensions mismatch")
        for (i <- range1; j <- range2 if jset(j)) hmat(i)(j) = Array.ofDim [T] (vc3(j), vc4(j))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve a single element of the hypermatrix.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     *  @param l  4th dimension index of the hypermatrix
     */
    def apply (i: Int, j: Int, k: Int, l: Int): T = hmat(i)(j)(k)(l)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the matrix formed by fixing the second dimension.
     *  @param j  the 2nd dimension index of the hypermatrix
     */
    def apply (j: Int): Matrix [T] =
    {
        val dim3 = dim_3 (0, j)
        val dim4 = dim_4 (0, j)
        val mat  = new MatrixN [T] (dim3, dim1 * dim4)
        for (k <- 0 until dim3; i <- 0 until dim1; l <- 0 until dim4) mat(k, i + l*dim1) = hmat(i)(j)(k)(l)
        mat
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single element of the hypermatrix to the given value.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     *  @param l  4th dimension index of the hypermatrix
     *  @param v  the value to be updated at the above position in the hypermatrix
     */
    def update (i: Int, j: Int, k: Int, l: Int, v: T) = hmat(i)(j)(k)(l) = v

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' hypermatrix and hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def + (b: HMatrix4 [T]): HMatrix4 [T] =
    {
        val c = new HMatrix4 [T] (dim1, dim2)
        for (i <- range1; j <- range2) {
            val (kk, ll) = (dim_3(i, j), dim_4(i, j))
            c.alloc (i, j, kk, ll)
            for (k <- 0 until kk; l <- 0 until ll) c.hmat(i)(j)(k)(l) = hmat(i)(j)(k)(l) + b.hmat(i)(j)(k)(l)
        } // for
        c
    } // +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add (in-place) 'this' hypermatrix and hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def += (b: HMatrix4 [T]): HMatrix4 [T] =
    {
        for (i <- range1; j <- range2) {
            val (kk, ll) = (dim_3(i, j), dim_4(i, j))
            for (k <- 0 until kk; l <- 0 until ll) hmat(i)(j)(k)(l) += b.hmat(i)(j)(k)(l)
        } // for
        this
    } // +=

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' hypermatrix subtract hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def - (b: HMatrix4 [T]): HMatrix4 [T] =
    {
        val c = new HMatrix4 [T] (dim1, dim2)
        for (i <- range1; j <- range2) {
            val (kk, ll) = (dim_3(i, j), dim_4(i, j))
            c.alloc (i, j, kk, ll)
            for (k <- 0 until kk; l <- 0 until ll) c.hmat(i)(j)(k)(l) = hmat(i)(j)(k)(l) - b.hmat(i)(j)(k)(l)
        } // for
        c
    } // -

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' hypermatrix subtract (in-place) hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def -= (b: HMatrix4 [T]): HMatrix4 [T] =
    {
        for (i <- range1; j <- range2) {
            val (kk, ll) = (dim_3(i, j), dim_4(i, j))
            for (k <- 0 until kk; l <- 0 until ll) hmat(i)(j)(k)(l) -= b.hmat(i)(j)(k)(l)
        } // for
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the dimensions of 'this' hypermatrix are less than or equal to
     *  'le' those of the other hypermatrix 'b'.
     *  @param b  the other hypermatrix
     */
    def leDimensions (b: HMatrix4 [T]): Boolean =
    {
        if (dim1 > b.dim1 || dim2 > b.dim2) return false
        for (i <- range1; j <- range2) {
            if (dim_3 (i,j) > b.dim_3 (i,j)) return false
            if (dim_4 (i, j, dim_3(i, j)) > b.dim_4(i, j, b.dim_3(i, j))) return false
        } // for
        true
    } // leDimensions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the hypermatrix element values to 'x'.
     *  @param x  the value to set all elements to
     */
    def set (x: T) = { for (i <- range1; j <- range2; k <- 0 until dim_3 (i, j);
                            l <-0 until dim_4(i, j)) hmat(i)(j)(k)(l) = x }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear (make null) all contents in the 3rd and 4th dimensions of the hypermatrix.
     */
    def clear () = { for (i <- range1; j <- range2) hmat(i)(j) = null }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' hypermatrix to a string.
     */
    override def toString: String =
    {
        val sb = new StringBuilder ("\nHMatrix4(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            for (j <- range2 if (hmat(i)(j) != null)) {
                sb.append (hmat(i)(j).deep + ", ")
                if (j == dim2-1) sb.replace (sb.length-1, sb.length, "\n\t")
            } // for
        } // for
        sb.replace (sb.length-3, sb.length, ")").mkString
    } // toString

} // HMatrix4 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix4Test` object is used to test the `HMatrix4` class.
 *  > runMain scalation.linalgebra.gen.HMatrix4Test
 */
object HMatrix4Test extends App
{
    val tb = new HMatrix4 [Int] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) tb.alloc (i, j, 2, 2)
    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2; l <- 0 until 2) tb(i, j, k, l) = i + j + k + l
    println ("tb = " + tb)

} // HMatrix4Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix4Test2` object is used to test the `HMatrix4` class.
 *  > runMain scalation.linalgebra.gen.HMatrix4Test2
 */
object HMatrix4Test2 extends App
{
    val tb = new HMatrix4 [Double] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) tb.alloc (i, j, 2, 2)
    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2; l <- 0 until 2) tb(i, j, k, l) = (i + j + k + l) * 0.5
    println ("tb = " + tb)

} // HMatrix4Test2 object

