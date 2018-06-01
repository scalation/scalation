
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 1.5
 *  @date    Thu May 10 15:50:15 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Tensor Algebra
 *  @see www.stat.uchicago.edu/~lekheng/work/icm1.pdf
 *  @see www.math.ias.edu/csdm/files/13-14/Gnang_Pa_Fi_2014.pdf
 *  @see tspace.library.utoronto.ca/bitstream/1807/65327/11/Vasilescu_M_Alex_O_200911_PhD_thesis.pdf
 */

package scalation.tenalgebra

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tensor3D` class is a simple implementation of a 3-dimensional tensor.
 *  The first two dimensions must be fixed and known, while the third dimension
 *  may be dynamically allocated by the user.  The third dimension should only
 *  vary with the second dimension, not the first.
 *-----------------------------------------------------------------------------
 *  @param dim1  size of the 1st level/dimension (sheet) of the tensor
 *  @param dim2  size of the 2nd level/dimension (row) of the tensor
 *  @param dim3  size of the 3rd level/dimension (column) of the tensor
 */
class Tensor3D (val dim1: Int, val dim2: Int, val dim3: Int,
                private [tenalgebra] var v: Array [Array [Array [Double]]] = null)
      extends Error with Serializable
{
    private val TAB    = "\t\t"                     // use "\t" for scala and "\t\t" for sbt
    private val range1 = 0 until dim1               // range for the first level/dimension
    private val range2 = 0 until dim2               // range for the second level/dimension
    private val range3 = 0 until dim3               // range for the third level/dimension

    /** Multi-dimensional array storage for tensor
     */
    
    if (v == null) {
        v = Array.ofDim [Double] (dim1, dim2, dim3)
    } else if (dim1 != v.length || dim2 != v(0).length || dim3 != v(0)(0).length) {
        flaw ("constructor", "dimensions are wrong")
    } // if

    /** Format string used for printing vector values (change using 'setFormat')
     */
    protected var fString = "%g,\t"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat'.
     *  @param newFormat  the new format string
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the 'i, j, k' element from the tensor.
     *  @param i  1st dimension (sheet) index of the tensor
     *  @param j  2nd dimension (row) index of the tensor
     *  @param k  3rd dimension (column) index of the tensor
     */
    def apply (i: Int, j: Int, k: Int): Double = v(i)(j)(k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the 'i, j' vector from the tensor.
     *  @param i  1st dimension index of the tensor
     *  @param j  2nd dimension index of the tensor
     */
    def apply (i: Int, j: Int): VectoD = VectorD (v(i)(j))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the 'i' matrix from the tensor.
     *  @param i  1st dimension index of the tensor
     */
    def apply (i: Int): MatriD = MatrixD (v(i))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single element of the tensor to the given value.
     *  @param i  1st dimension index of the tensor
     *  @param j  2nd dimension index of the tensor
     *  @param k  3rd dimension index of the tensor
     *  @param x  the value to be updated at the above position in the tensor
     */
    def update (i: Int, j: Int, k: Int, x: Double) = v(i)(j)(k) = x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the tensor element values to 'x'.
     *  @param x  the value to set all elements to
     */
    def set (x: Double) = { for (i <- range1; j <- range2; k <- range3) v(i)(j)(k) = x }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' tensor and tensor 'b'.
     *  @param b  the tensor to add (requires 'leDimensions')
     */
    def + (b: Tensor3D): Tensor3D =
    {
        val c = new Tensor3D (dim1, dim2, dim3)
        for (i <- range1; j <- range2; k <- range3) {
            c.v(i)(j)(k) = v(i)(j)(k) + b.v(i)(j)(k)
        } // for
        c
    } // +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tensor subtract tensor 'b'.
     *  @param b  the tensor to add (requires 'leDimensions')
     */
    def - (b: Tensor3D): Tensor3D =
    {
        val c = new Tensor3D (dim1, dim2, dim3)
        for (i <- range1; j <- range2; k <- range3) {
            c.v(i)(j)(k) = v(i)(j)(k) - b.v(i)(j)(k)
        } // for
        c
    } // -

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' tensor by scalar 's'.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): Tensor3D =
    {
        val c = new Tensor3D (dim1, dim2, dim3)
        for (i <- range1; j <- range2; k <- range3) {
            c.v(i)(j)(k) = v(i)(j)(k) * s
        } // for
        c
    } // *

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply (multilinear product) 'this' tensor by three matrices 'b', 'c' and 'd'.
     *  <p>
     *      this * (a, b, c)
     *  <p>
     *  @see www.stat.uchicago.edu/~lekheng/work/icm1.pdf - equation 15.1
     *  @param b  the first matrix to multiply by (requires 'leDimensions')
     *  @param c  the second matrix to multiply by (requires 'leDimensions')
     *  @param d  the thrid matrix to multiply by (requires 'leDimensions')
     */
    def * (b: MatrixD, c: MatrixD, d: MatrixD): Tensor3D =
    {
        val (m1, n1) = (b.dim1, b.dim2)
        val (m2, n2) = (c.dim1, c.dim2)
        val (m3, n3) = (d.dim1, d.dim2)
        if (n1 > dim2 || n2 > dim2 || n3 > dim3) flaw ("*", "dimensions don't match")

        val e = new Tensor3D (m1, m2, m3)
        for (i <-b.range1; j <- c.range1; k <- d.range1) {
            var sum = 0.0
            for (l1 <- b.range2; l2 <- c.range2; l3 <- d.range2) {
                sum += b(i, l1) * c(j, l2) * d(k, l3) * v(l1)(l2)(l3)
            } // for
            e.v(i)(j)(k) = sum
        } // for
        e
    } // *

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply elementwise (Hadamard product) 'this' tensor by tensor 'b'.
     *  @param b  the tensor to add (requires 'leDimensions')
     */
    def ** (b: Tensor3D): Tensor3D =
    {
        val c = new Tensor3D (dim1, dim2, dim3)
        for (i <- range1; j <- range2; k <- range3) {
            c.v(i)(j)(k) = v(i)(j)(k) * b.v(i)(j)(k)
        } // for
        c
    } // ** 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the dimensions of 'this' tensor are less than or equal to
     *  'le' those of the other tensor 'b'.
     *  @param b  the other matrix
     */
    def leDimensions (b: Tensor3D): Boolean =
    {
        dim1 <= b.dim1 && dim2 <= b.dim2 && dim3 <= b.dim3
    } // leDimensions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' tensor to a string with a double line break after each sheet
     *  and a single line break after each row.
     */
    override def toString: String = 
    {
        val sb = new StringBuilder ("\nTensor3D(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            for (j <- range2) {
                for (k <- range3) sb.append (v(i)(j)(k) + ", ")
                sb.append ("\n" + TAB)
            } // for
            sb.append ("\n" + TAB)
        } // for
        sb.replace (sb.length-5, sb.length, ")").mkString
    } // toString

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' tensor to a string with a line break after each sheet.
     */
    def toString2: String = 
    {
        val sb = new StringBuilder ("\nTensor3D(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            for (j <- range2) {
                sb.append (v(i)(j).deep + ", ")
                if (j == dim2-1) sb.replace (sb.length-1, sb.length, "\n\t")
            } // for
        } // for
        sb.replace (sb.length-3, sb.length, ")").mkString
    } // toString2

} // Tensor3D class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tensor3D` companion object provides factory methods for the `Tensor3D` class.
 */
object Tensor3D
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a tensor from the argument list 'x'.
     *  @param n1  the first dimension
     *  @param n2  the second dimension
     *  @param n3  the third dimension
     */
    def apply (n: (Int, Int, Int), x: Double*): Tensor3D =
    {
        val t = new Tensor3D (n._1, n._2, n._3)
        var l = 0
        for (i <- 0 until n._1; j <- 0 until n._2; k <- 0 until n._3) {
            t(i, j, k) = x(l)
            l += 1
        } // for
        t
    } // apply 

} // Tensor3D object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tensor3DTest` object is used to test the `Tensor3D` class.
 *  > runMain scalation.tenalgebra.Tensor3DTest
 */
object Tensor3DTest extends App
{
    val a = new Tensor3D (2, 3, 2)
    val b = new Tensor3D (2, 3, 2)
    //                                        sheet row column
    val c = Tensor3D ((2, 3, 2),  1,  2,      // 0   0   0 - 1
                                  3,  4,      // 0   1   0 - 1
                                  5,  6,      // 0   2   0 - 1

                                  7,  8,      // 1   0   0 - 1
                                  9, 10,      // 1   1   0 - 1
                                 11, 12)      // 1   2   0 - 1

    for (i <- 0 until 2; j <- 0 until 3; k <- 0 until 2) {
        val sum = i + j + k
        a(i, j, k) = sum
        b(i, j, k) = sum
    } // for

    println ("a       = " + a)
    println ("b       = " + b)
    println ("c       = " + c)
    println ("c(0)    = " + c(0))
    println ("c(0, 0) = " + c(0, 0))
    println ("a + b   = " + (a + b))
    println ("a - b   = " + (a - b))
    println ("c * 2   = " + c * 2)
    println ("a ** c  = " + a ** c)

    val x = MatrixD ((2, 2), 1, 2,
                             3, 4)
    val y = MatrixD ((2, 3), 1, 2, 3,
                             4, 5, 6)
    val z = MatrixD ((2, 2), 5, 6,
                             7, 8)

    println ("c * (x, y, z) = " + c * (x, y, z))

} // Tensor3DTest object

