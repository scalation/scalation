
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
/** The `HMatrix2` class is a simple implementation of a 2-dimensional hypermatrix.
 *  The first dimension must be fixed and known, while the second dimension
 *  may be dynamically allocated by the user.  The second dimension may
 *  vary with the first dimension.
 *  Caveat:  currently this is a very limited implementation of hypermatrices.
 *-----------------------------------------------------------------------------
 *  @param dim1  size of the 1st dimension of the hypermatrix
 */
class HMatrix2 [T: ClassTag: Numeric] (val dim1: Int)
      extends Error
{
    /** Range for the first dimension
     */
    private val range1 = 0 until dim1

    /** Multi-dimensional array storage for hypermatrix
     */
    private val hmat = Array.ofDim [Array [T]] (dim1)

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
    /** Construct a rectangular 2-dimensional hypermatrix, where the 2nd dimension
     *  is fixed as well.
     *  @param dim1  size of the 1st dimension of the hypermatrix
     *  @param dim2  size of the 2nd dimension of the hypermatrix
     */
    def this (dim1: Int, dim2: Int) =
    {
        this (dim1)
        for (i <- range1) hmat(i) = Array.ofDim [T] (dim2)
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 2-dimensional hypermatrix, where the 2nd dimension varies
     *  only with the 1st dimension.
     *  @param dim1   size of the 1st dimension of the hypermatrix
     *  @param dims2  array of sizes of the 2nd dimension of the hypermatrix
     */
    def this (dim1: Int, dims2: Array [Int]) =
    {
        this (dim1)
        if (dims2.length != dim1) flaw ("constructor", "wrong number of elements for 2nd dimension")
        for (i <- range1) hmat(i) = Array.ofDim [T] (dims2(i))
    } // aux constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size of the 2nd dimension for the given 'i'.
     *  @param i  1st dimension index of the hypermatrix
     */
    def dim_2 (i: Int): Int = hmat(i).length

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate one element of the 2nd dimension of the hypermatrix for the
     *  specified 'i' index.
     *  @param i     1st dimension index of the hypermatrix
     *  @param dim2  size of the array to be allocated in row i
     */
    def alloc (i: Int, dim2: Int)
    {
        hmat(i) = Array.ofDim [T] (dim2)
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allocate all elements of the 2nd dimension of the hypermatrix, where
     *  the last dimension only with the first dimension.
     *  @param dims2  array of sizes of the 2nd dimension of the hypermatrix
     */
    def alloc (dims2: Array [Int])
    {
        if (dims2.length != dim1) flaw ("alloc", "wrong number of elements for 2nd dimension")
        for (i <- range1) hmat(i) = Array.ofDim [T] (dims2(i))
    } // alloc

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve a single element of the hypermatrix.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     */
    def apply (i: Int, j: Int): T = hmat(i)(j)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single element of the hypermatrix to the given value.
     *  @param i  1st dimension index of the hypermatrix
     *  @param j  2nd dimension index of the hypermatrix
     *  @param v  the value to be updated at the above position in the hypermatrix
     */
    def update (i: Int, j: Int, v: T) = hmat(i)(j) = v

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' hypermatrix and hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def + (b: HMatrix2 [T]): HMatrix2 [T] =
    {
        val c = new HMatrix2 [T] (dim1)
        for (i <- range1) {
            val d2 = hmat(i).length
            c.alloc (i, d2)
            for (j <- 0 until d2) c.hmat(i)(j) = hmat(i)(j) + b.hmat(i)(j)
        } // for
        c
    } // +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' hypermatrix subtract hypermatrix 'b'.
     *  @param b  the hypermatrix to add (requires 'leDimensions')
     */
    def - (b: HMatrix2 [T]): HMatrix2 [T] =
    {
        val c = new HMatrix2 [T] (dim1)
        for (i <- range1) {
            val d2 = hmat(i).length
            c.alloc (i, d2)
            for (j <- 0 until d2) c.hmat(i)(j) = hmat(i)(j) - b.hmat(i)(j)
        } // for
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the dimensions of 'this' hypermatrix are less than or equal to
     *  'le' those of the other hypermatrix 'b'.
     *  @param b  the other matrix
     */
    def leDimensions (b: HMatrix2 [T]): Boolean =
    {
        if (dim1 > b.dim1) return false 
        for (i <- range1 if dim_2 (i) > b.dim_2 (i)) return false
        true
    } // leDimensions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the hypermatrix element values to 'x'.
     *  @param x  the value to set all elements to
     */
    def set (x: T) = { for (i <- range1; j <- 0 until dim_2 (i)) hmat(i)(j) = x }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear (make null) all contents in the 2nd dimension of the hypermatrix.
     */
    def clear () = { for (i <- range1) hmat(i) = null }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' hypermatrix to a string.
     */
    override def toString: String = 
    {
        val sb = new StringBuilder ("\nHMatrix2(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            sb.append (hmat(i).deep + ", ")
            sb.replace (sb.length-1, sb.length, "\n\t")
        } // for
        sb.replace (sb.length-3, sb.length, ")").mkString
    } // toString

} // HMatrix2 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix2Test` object is used to test the `HMatrix2` class, in cases
 *  where the 2nd dimension is fixed (i.e., for rectangular 2-dimensional hypermatrices).
 *  > run-main scalation.linalgebra.gen.HMatrix2Test
 */
object HMatrix2Test extends App
{
    // three equivalent to allocate a hypermatrix
    //-------------------------------------------------------------------------
    val a = new HMatrix2 [Int] (2, 3)
    //-------------------------------------------------------------------------
    val b = new HMatrix2 [Int] (2)
    b.alloc (Array (3, 3))
    //-------------------------------------------------------------------------
    val c = new HMatrix2 [Int] (2)
    for (i <- 0 until 2) c.alloc (i, 3)
    //-------------------------------------------------------------------------

    for (i <- 0 until 2; j <- 0 until 3) {
        val sum = i + j
        a(i, j) = sum
        b(i, j) = sum
        c(i, j) = sum
    } // for

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("a + b = " + (a + b))

} // HMatrix2Test object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix2Test2` object is used to test the `HMatrix2` class, in cases
 *  where the 2nd dimension is varied based on the index of the first dimension 'i'
 *  > run-main scalation.linalgebra.gen.HMatrix2Test2
 */
object HMatrix2Test2 extends App
{
    // three equivalent to allocate a hypermatrix
    //-------------------------------------------------------------------------
    val a = new HMatrix2 [Int] (2, Array (2, 3))
    //-------------------------------------------------------------------------
    val b = new HMatrix2 [Int] (2)
    b.alloc (Array (2, 3))
    //-------------------------------------------------------------------------
    val c = new HMatrix2 [Int] (2)
    for (i <- 0 until 2) c.alloc (i, i+2)
    //-------------------------------------------------------------------------

    for (i <- 0 until 2; j <- 0 until a.dim_2 (i)) {
        val sum = i + j
        a(i, j) = sum
        b(i, j) = sum
        c(i, j) = sum
    } // for

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("a + b = " + (a + b))

} // HMatrix2Test2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HMatrix2Test3` object is used to test the `HMatrix2` class, in cases
 *  where the 2nd dimension is fixed (i.e., for rectangular 2-dimensional hypermatrices).
 *  Same as `HMatrix2Test`, but using `Double` rather than `Int`.
 *  > run-main scalation.linalgebra.gen.HMatrix2Test3
 */
object HMatrix2Test3 extends App
{
    // three equivalent to allocate a hypermatrix
    //-------------------------------------------------------------------------
    val a = new HMatrix2 [Double] (2, 3)
    //-------------------------------------------------------------------------
    val b = new HMatrix2 [Double] (2)
    b.alloc (Array (3, 3))
    //-------------------------------------------------------------------------
    val c = new HMatrix2 [Double] (2)
    for (i <- 0 until 2) c.alloc (i, 3)
    //-------------------------------------------------------------------------

    for (i <- 0 until 2; j <- 0 until 3) {
        val sum = i + j + 0.5
        a(i, j) = sum
        b(i, j) = sum
        c(i, j) = sum
    } // for

    println ("a     = " + a)
    println ("b     = " + b)
    println ("c     = " + c)
    println ("a + b = " + (a + b))

} // HMatrix2Test3 object

