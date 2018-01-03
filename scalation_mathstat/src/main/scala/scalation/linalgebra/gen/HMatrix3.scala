
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.4
 *  @date    Mon Jul 27 01:27:00 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.gen

import scala.reflect.ClassTag

import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix3` class is a simple implementation of a 3-dimensional hypermatrix.
 *  The first two dimensions must be fixed and known, while the third dimension
 *  may be dynamically allocated by the user.  The third dimension should only
 *  vary with the second dimension, not the first.
 *  Caveat:  currently this is a very limited implementation of hypermatrices.
 *-----------------------------------------------------------------------------
 *  @param dim1  size of the 1st dimension of the hypermatrix
 *  @param dim2  size of the 2nd dimension of the hypermatrix
 */
class HMatrix3 [T: ClassTag: Numeric] (val dim1: Int, val dim2: Int)
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
    private val hmat = Array.ofDim [Array [T]] (dim1, dim2)

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
    /** Construct a cuboidic 3-dimensional hypermatrix, where the 3rd dimension
     *  is fixed as well.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     *  @param dim3  size of the 3rd dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dim3: Int) =
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dim3)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 3-dimensional hypermatrix, where the 3rd dimension varies
     *  only with the 2nd dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dim2   size of the 2nd dimension of the hypermatrix
     *  @param dims3  array of sizes of the 3rd dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int, dims3: Array [Int]) =
    {
        this (dim1, dim2)
        if (dims3.length != dim2) flaw ("constructor", "wrong number of elements for 3rd dimension")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dims3(j))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 3rd dimension for the given 'j'.
     *  The size of the 3rd dimension must be the same for all 'i'.
     *  @param j  2nd dimension index of the hypermatrix
     */
    def dim_3 (j: Int): Int = hmat(0)(j).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate one element of the 3rd dimension of the hypermatrix for the
     *  specified '(i, j)' indices.  Although, this allows the 3rd dimension
     *  to vary with both the first and second dimensions, it should only vary
     *  with the second dimension. 
     *  @param i     1st dimension index of the hypermatrix
     *  @param j     2nd dimension index of the hypermatrix
     *  @param dim3  size of the array to be allocated in row i and column j
     */
    def alloc (i: Int, j: Int, dim3: Int)
    {
        hmat(i)(j) = Array.ofDim [T] (dim3)
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate all elements of the 3rd dimension of the hypermatrix, where
     *  the last dimension only with the second dimension.
     *  @param dims3  array of sizes of the 3rd dimension of the hypermatrix
     */
    def alloc (dims3: Array [Int])
    {
        if (dims3.length != dim2) flaw ("alloc", "wrong number of elements for 3rd dimension")
        for (i <- range1; j <- range2) hmat(i)(j) = Array.ofDim [T] (dims3(j))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve a single element of the hypermatrix.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     */
    def apply (i: Int, j: Int, k: Int): T = hmat(i)(j)(k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single element of the hypermatrix to the given value.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param k  3rd dimension index of the hypermatrix
     *  @param v  the value to be updated at the above position in the hypermatrix
     */
    def update (i: Int, j: Int, k: Int, v: T) = hmat(i)(j)(k) = v

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' hypermatrix and hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def + (b: HMatrix3 [T]): HMatrix3 [T] =
    {
        val c = new HMatrix3 [T] (dim1, dim2)
        for (i <- range1; j <- range2) {
            val d3 = hmat(i)(j).length
            c.alloc (i, j, d3)
            for (k <- 0 until d3) c.hmat(i)(j)(k) = hmat(i)(j)(k) + b.hmat(i)(j)(k)
        } // for
        c
    } // +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' hypermatrix subtract hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def - (b: HMatrix3 [T]): HMatrix3 [T] =
    {
        val c = new HMatrix3 [T] (dim1, dim2)
        for (i <- range1; j <- range2) {
            val d3 = hmat(i)(j).length
            c.alloc (i, j, d3)
            for (k <- 0 until d3) c.hmat(i)(j)(k) = hmat(i)(j)(k) - b.hmat(i)(j)(k)
        } // for
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the dimensions of 'this' hypermatrix are less than or equal to
     *  'le' those of the other hypermatrix 'b'.
     *  @param b  the other matrix
     */
    def leDimensions (b: HMatrix3 [T]): Boolean =
    {
        if (dim1 > b.dim1 || dim2 > b.dim2) return false 
        for (j <- range2 if dim_3 (j) > b.dim_3 (j)) return false
        true
    } // leDimensions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the hypermatrix element values to 'x'.
     *  @param x  the value to set all elements to
     */
    def set (x: T) = { for (i <- range1; j <- range2; k <- 0 until dim_3 (j)) hmat(i)(j)(k) = x }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear (make null) all contents in the 3rd dimension of the hypermatrix.
     */
    def clear () = { for (i <- range1; j <- range2) hmat(i)(j) = null }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' hypermatrix to a string.
     */
    override def toString: String = 
    {
        val sb = new StringBuilder ("\nHMatrix3(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            for (j <- range2) {
                sb.append (hmat(i)(j).deep + ", ")
                if (j == dim2-1) sb.replace (sb.length-1, sb.length, "\n\t")
            } // for
        } // for
        sb.replace (sb.length-3, sb.length, ")").mkString
    } // toString

} // HMatrix3 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix3Test` object is used to test the `HMatrix3` class, in cases
 *  where the 3rd dimension is fixed (i.e., for cuboidic 3-dimensional hypermatrices).
 *  > runMain scalation.linalgebra.gen.HMatrix3Test
 */
object HMatrix3Test extends App
{
    // three equivalent to allocate a hypermatrix
    //-------------------------------------------------------------------------
    val a = new HMatrix3 [Int] (2, 3, 2)
    //-------------------------------------------------------------------------
    val b = new HMatrix3 [Int] (2, 3)
    b.alloc (Array (2, 2, 2))
    //-------------------------------------------------------------------------
    val c = new HMatrix3 [Int] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) c.alloc (i, j, 2)
    //-------------------------------------------------------------------------

    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2) {
        val sum = i + j + k
        a(i, j, k) = sum
        b(i, j, k) = sum
        c(i, j, k) = sum
    } // for

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("a + b = " + (a + b))

} // HMatrix3Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix3Test2` object is used to test the `HMatrix3` class, in cases
 *  where the 3rd dimension is varied based on the index of the second dimension 'j'
 *  > runMain scalation.linalgebra.gen.HMatrix3Test2
 */
object HMatrix3Test2 extends App
{
    // three equivalent to allocate a hypermatrix
    //-------------------------------------------------------------------------
    val a = new HMatrix3 [Int] (2, 3, Array (1, 2, 3))
    //-------------------------------------------------------------------------
    val b = new HMatrix3 [Int] (2, 3)
    b.alloc (Array (1, 2, 3))
    //-------------------------------------------------------------------------
    val c = new HMatrix3 [Int] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) c.alloc (i, j, j+1)
    //-------------------------------------------------------------------------

    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until a.dim_3 (j)) {
        val sum = i + j + k
        a(i, j, k) = sum
        b(i, j, k) = sum
        c(i, j, k) = sum
    } // for

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("a + b = " + (a + b))

} // HMatrix3Test2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix3Test3` object is used to test the `HMatrix3` class, in cases
 *  where the 3rd dimension is fixed (i.e., for cuboidic 3-dimensional hypermatrices).
 *  Same as `HMatrix3Test`, but using `Double` rather than `Int`.
 *  > runMain scalation.linalgebra.gen.HMatrix3Test3
 */
object HMatrix3Test3 extends App
{
    // three equivalent to allocate a hypermatrix
    //-------------------------------------------------------------------------
    val a = new HMatrix3 [Double] (2, 3, 2)
    //-------------------------------------------------------------------------
    val b = new HMatrix3 [Double] (2, 3)
    b.alloc (Array (2, 2, 2))
    //-------------------------------------------------------------------------
    val c = new HMatrix3 [Double] (2, 3)
    for (i <- 0 until 2; j <- 0 until 3) c.alloc (i, j, 2)
    //-------------------------------------------------------------------------

    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2) {
        val sum = i + j + k + 0.5
        a(i, j, k) = sum
        b(i, j, k) = sum
        c(i, j, k) = sum
    } // for

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("a + b = " + (a + b))

} // HMatrix3Test3 object

