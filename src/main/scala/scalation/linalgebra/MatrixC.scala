
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.linalgebra.bld.BldMatrix
 *  @version 1.2
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import java.io.PrintWriter

import io.Source.fromFile

import scalation.math.Complex.{abs => ABS, _}

import scalation.math.{Complex, oneIf}
import scalation.util.{Error, PackageInfo}

import MatrixC.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixC` class stores and operates on Numeric Matrices of type `Complex`.
 *  This class follows the `gen.MatrixN` framework and is provided for efficiency.
 *  @param d1  the first/row dimension
 *  @param d2  the second/column dimension
 *  @param v   the 2D array used to store matrix elements
 */
class MatrixC (val d1: Int,
               val d2: Int,
       private var v:  Array [Array [Complex]] = null)
      extends MatriC with Error with Serializable
{
    /** Dimension 1
     */
    lazy val dim1 = d1

    /** Dimension 2
     */
    lazy val dim2 = d2

    if (v == null) {
        v = Array.ofDim [Complex] (dim1, dim2)
    } else if (dim1 != v.length || dim2 != v(0).length) {
        flaw ("constructor", "dimensions are wrong")
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 'dim1' by 'dim1' square matrix.
     *  @param dim1  the row and column dimension
     */
    def this (dim1: Int) { this (dim1, dim1) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 'dim1' by 'dim2' matrix and assign each element the value 'x'.
     *  @param dim1  the row dimension
     *  @param dim2  the column dimesion
     *  @param x     the scalar value to assign
     */
    def this (dim1: Int, dim2: Int, x: Complex)
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) v(i)(j) = x
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix and assign values from array of arrays 'u'.
     *  @param u  the 2D array of values to assign
     */
    def this (u: Array [Array [Complex]]) { this (u.length, u(0).length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix from repeated values.
     *  @param dim  the (row, column) dimensions
     *  @param u    the repeated values
     */
    def this (dim: Tuple2 [Int, Int], u: Complex*)
    {
        this (dim._1, dim._2)
        for (i <- range1; j <- range2) v(i)(j) = u(i * dim2 + j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix and assign values from matrix 'b'.
     *  @param b  the matrix of values to assign
     */
    def this (b: MatrixC)
    {
        this (b.d1, b.d2)
        for (i <- range1; j <- range2) v(i)(j) = b.v(i)(j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's element at the 'i,j'-th index position. 
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): Complex = v(i)(j)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's vector at the 'i'-th index position ('i'-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorC = new VectorC (v(i))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise on range 'ir' and column-wise on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): MatrixC = slice (ir.start, ir.end, jr.start, jr.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's element at the 'i,j'-th index position to the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: Complex) { v(i)(j) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's row at the 'i'-th index position to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorC) { v(i) = u() }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice 'this' matrix row-wise on range ir and column-wise on range 'jr'.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: MatriC)
    {
        if (b.isInstanceOf [MatrixC]) {
            val bb = b.asInstanceOf [MatrixC]
            for (i <- ir; j <- jr) v(i)(j) = bb.v(i - ir.start)(j - jr.start)
        } else {
            flaw ("update", "must convert b to a MatrixC first")
        } // if
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in 'this' matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: Complex) { for (i <- range1; j <- range2) v(i)(j) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in 'this' matrix as copies of the values in 2D array 'u'.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [Complex]]) { for (i <- range1; j <- range2) v(i)(j) = u(i)(j) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's 'i'-th row starting at column 'j' to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectorC, j: Int = 0) { for (k <- 0 until u.dim) v(i)(k+j) = u(k) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'from' to 'end'.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): MatrixC =
    {
        new MatrixC (end - from, dim2, v.slice (from, end))
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix column-wise 'from' to 'end'.
     *  @param from  the start column of the slice (inclusive)
     *  @param end   the end column of the slice (exclusive)
     */
    def sliceCol (from: Int, end: Int): MatrixC =
    {
        val c = new MatrixC (dim1, end - from)
        for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(i)(j + from)
        c
    } // sliceCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'r_from' to 'r_end' and column-wise 'c_from' to 'c_end'.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): MatrixC = 
    {
        val c = new MatrixC (r_end - r_from, c_end - c_from)
        for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(i + r_from)(j + c_from)
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix excluding the given row and/or column.
     *  @param row  the row to exclude (0 until dim1, set to dim1 to keep all rows)
     *  @param col  the column to exclude (0 until dim2, set to dim2 to keep all columns)
     */
    def sliceExclude (row: Int, col: Int): MatrixC =
    {
        val c = new MatrixC (dim1 - oneIf (row < dim1), dim2 - oneIf (col < dim2))
        for (i <- range1 if i != row) for (j <- range2 if j != col) {
            c.v(i - oneIf (i > row))(j - oneIf (j > col)) = v(i)(j)
        } // for
        c
    } // sliceExclude

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from 'this' matrix according to the given index/basis.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): MatrixC =
    {
        val c = new MatrixC (rowIndex.length, dim2)
        for (i <- c.range1) c.v(i) = v(rowIndex(i))
        c
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from the matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): VectorC =
    {
        val u = new VectorC (dim1 - from)
        for (i <- from until dim1) u(i-from) = v(i)(col)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of the matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectorC) { for (i <- range1) v(i)(col) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from 'this' matrix according to the given index/basis.
     *  Ex: Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): MatrixC =
    {
        val c = new MatrixC (dim1, colIndex.length)
        for (j <- c.range2) c.setCol (j, col(colIndex(j)))
        c
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose 'this' matrix (rows => columns).
     */
    def t: MatrixC =
    {
        val b = new MatrixC (dim2, dim1)
        for (i <- b.range1; j <- b.range2) b.v(i)(j) = v(j)(i)
        b
    } // t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: VectorC): MatrixC =
    {
        if (u.dim != dim2) flaw ("+:", "vector does not match row dimension")
        val c = new MatrixC (dim1 + 1, dim2)
        for (i <- c.range1) c(i) = if (i == 0) u else this(i - 1)
        c
    } // +:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: VectorC): MatrixC =
    {
        if (u.dim != dim1) flaw ("+^:", "vector does not match column dimension")
        val c = new MatrixC (dim1, dim2 + 1)
        for (j <- c.range2) c.setCol (j, if (j == 0) u else col (j - 1))
        c
    } // +^:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (row) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: VectorC): MatrixC =
    {
        if (u.dim != dim2) flaw (":+", "vector does not match row dimension")
        val c = new MatrixC (dim1 + 1, dim2)
        for (i <- c.range1) c(i) = if (i < dim1) this(i) else u
        c
    } // :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (column) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: VectorC): MatrixC =
    {
        if (u.dim != dim1) flaw (":^+", "vector does not match column dimension")
        val c = new MatrixC (dim1, dim2 + 1)
        for (j <- c.range2) c.setCol (j, if (j < dim2) col (j) else u)
        c
    } // :^+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last rows in new matrix
     */
    def ++ (b: MatriC): MatrixC =
    {
        if (b.dim2 != dim2) flaw ("++", "matrix b does not match row dimension")
        val c = new MatrixC (dim1 + b.dim1, dim2)
        for (i <- c.range1) c(i) = if (i < dim1) this(i) else b(i - dim1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last columns in new matrix
     */
    def ++^ (b: MatriC): MatrixC =
    {
        if (b.dim1 != dim1) flaw ("++^", "matrix b does not match column dimension")
        val c = new MatrixC (dim1, dim2 + b.dim2)
        for (j <- c.range2) c.setCol (j, if (j < dim2) col (j) else b.col (j - dim2))
        c
    } // ++^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and matrix 'b'.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def + (b: MatrixC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + b.v(i)(j)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and matrix 'b' for any type extending MatriC.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def + (b: MatriC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + b(i, j)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def + (u: VectorC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + u(j)
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: Complex): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + x
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and matrix 'b'.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def += (b: MatrixC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) += b.v(i)(j)
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and matrix 'b' for any type extending MatriC.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def += (b: MatriC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) += b(i, j)
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def += (u: VectorC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) += u(j)
        this
    } // +=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: Complex): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) += x
        this
    } // +=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract matrix 'b'.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def - (b: MatrixC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) - b.v(i)(j)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract matrix 'b' for any type extending MatriC.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def - (b: MatriC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) - b(i, j)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract (row) vector 'u'.
     *  @param b  the vector to subtract
     */
    def - (u: VectorC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) - u(j)
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: Complex): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(i)(j) - x
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place matrix 'b'.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def -= (b: MatrixC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) -= b.v(i)(j)
        this
    } // -=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place matrix 'b'.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def -= (b: MatriC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) -= b(i, j)
        this
    } // -=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place (row) vector 'u'.
     *  @param b  the vector to subtract
     */
    def -= (u: VectorC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) -= u(j)
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: Complex): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) -= x
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b', transposing 'b' to improve efficiency.
     *  Use 'times' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: MatrixC): MatrixC =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c  = new MatrixC (dim1, b.dim2)
        val bt = b.t                         // transpose the b matrix
        for (i <- range1; j <- c.range2) {
            val va = v(i); val vb = bt.v(j)
            var sum = _0
            for (k <- range2) sum += va(k) * vb(k)
            c.v(i)(j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b', transposing 'b' to improve efficiency.
     *  Use 'times' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: MatriC): MatrixC =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c  = new MatrixC (dim1, b.dim2)
        val bt = b.t                         // transpose the b matrix
        for (i <- range1; j <- c.range2) {
            val va = v(i); val vb = bt(j)
            var sum = _0
            for (k <- range2) sum += va(k) * vb(k)
            c.v(i)(j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u' (vector elements beyond 'dim2' ignored).
     *  @param u  the vector to multiply by
     */
    def * (u: VectorC): VectorC =
    {
        if (dim2 > u.dim) flaw ("*", "matrix * vector - vector dimension too small")

        val c = new VectorC (dim1)
        for (i <- range1) {
            var sum = _0
            for (k <- range2) sum += v(i)(k) * u(k)
            c(i) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: Complex): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) * x
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by matrix 'b', transposing 'b' to improve
     *  efficiency.  Use 'times_ip' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires square and sameCrossDimensions)
     */
    def *= (b: MatrixC): MatrixC =
    {
        if (! b.isSquare)   flaw ("*=", "matrix 'b' must be square")
        if (dim2 != b.dim1) flaw ("*=", "matrix *= matrix - incompatible cross dimensions")

        val bt = b.t                                  // use the transpose of b
        for (i <- range1) {
            val row_i = new VectorC (dim2)            // save ith row so not overwritten
            for (j <- range2) row_i(j) = v(i)(j)      // copy values from ith row of 'this' matrix
            for (j <- range2) {
                val vb = bt.v(j)
                var sum = _0
                for (k <- range2) sum += row_i(k) * vb(k)
                v(i)(j) = sum
            } // for
        } // for
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by matrix 'b', transposing 'b' to improve
     *  efficiency.  Use 'times_ip' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires square and sameCrossDimensions)
     */
    def *= (b: MatriC): MatrixC =
    {
        if (! b.isSquare)   flaw ("*=", "matrix 'b' must be square")
        if (dim2 != b.dim1) flaw ("*=", "matrix *= matrix - incompatible cross dimensions")

        val bt = b.t                                  // use the transpose of b
        for (i <- range1) {
            val row_i = new VectorC (dim2)            // save ith row so not overwritten
            for (j <- range2) row_i(j) = v(i)(j)      // copy values from ith row of 'this' matrix
            for (j <- range2) {
                val vb = bt(j)
                var sum = _0
                for (k <- range2) sum += row_i(k) * vb(k)
                v(i)(j) = sum
            } // for
        } // for
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def *= (x: Complex): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) *= x
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and vector 'u', by first transposing
     *  'this' matrix and then multiplying by 'u' (ie., 'a dot u = a.t * u').
     *  @param u  the vector to multiply by (requires same first dimensions)
     */
    def dot (u: VectorC): VectorC =
    {
        if (dim1 != u.dim) flaw ("dot", "matrix dot vector - incompatible first dimensions")

        val c = new VectorC (dim2)
        val at = this.t                         // transpose the 'this' matrix
        for (i <- range2) {
            var sum = _0
            for (k <- range1) sum += at.v(i)(k) * u(k)
            c(i) = sum
        } // for
        c
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' without first transposing 'b'.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def times (b: MatrixC): MatrixC =
    {
        if (dim2 != b.dim1) flaw ("times", "matrix * matrix - incompatible cross dimensions")

        val c = new MatrixC (dim1, b.dim2)
        for (i <- range1; j <- c.range2) {
            var sum = _0
            for (k <- range2) sum += v(i)(k) * b.v(k)(j)
            c.v(i)(j) = sum
        } // for
        c
    } // times

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by matrix 'b' without first transposing 'b'.
     *  If b and this reference the same matrix (b == this), a copy of the this
     *  matrix is made.
     *  @param b  the matrix to multiply by (requires square and sameCrossDimensions)
     */
    def times_ip (b: MatrixC)
    {
        if (! b.isSquare)   flaw ("times_ip", "matrix 'b' must be square")
        if (dim2 != b.dim1) flaw ("times_ip", "matrix * matrix - incompatible cross dimensions")

        val bb = if (b == this) new MatrixC (this) else b
        for (i <- range1) {
            val row_i = new VectorC (dim2)            // save ith row so not overwritten
            for (j <- range2) row_i(j) = v(i)(j)      // copy values from ith row of 'this' matrix
            for (j <- range2) {
                var sum = _0
                for (k <- range2) sum += row_i(k) * bb.v(k)(j)
                v(i)(j) = sum
            } // for
        } // for
    } // times_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' using 'dot' product (concise solution).
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def times_d (b: MatriC): MatrixC =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c = new MatrixC (dim1, b.dim2)
        for (i <- range1; j <- c.range2) c.v(i)(j) = this(i) dot b.col(j)
        c
    } // times_d

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix b using the Strassen matrix multiplication
     *  algorithm.  Both matrices ('this' and 'b') must be square.  Although the
     *  algorithm is faster than the traditional cubic algorithm, its requires
     *  more memory and is often less stable (due to round-off errors).
     *  FIX:  could be make more efficient using a virtual slice (vslice) method.
     *  @see http://en.wikipedia.org/wiki/Strassen_algorithm
     *  @param b  the matrix to multiply by (it has to be a square matrix)
     */
    def times_s (b: MatrixC): MatrixC = 
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c = new MatrixC (dim1, dim1)  // allocate result matrix
        var d = dim1 / 2                  // half dim1
        if (d + d < dim1) d += 1          // if not even, increment by 1
        val evenDim = d + d               // equals dim1 if even, else dim1 + 1
        
        // decompose to blocks (use vslice method if available)
        val a11 = slice (0, d, 0, d)
        val a12 = slice (0, d, d, evenDim)
        val a21 = slice (d, evenDim, 0, d)
        val a22 = slice (d, evenDim, d, evenDim)
        val b11 = b.slice (0, d, 0, d)
        val b12 = b.slice (0, d, d, evenDim)
        val b21 = b.slice (d, evenDim, 0, d)
        val b22 = b.slice (d, evenDim, d, evenDim)
        
        // compute intermediate sub-matrices
        val p1 = (a11 + a22) * (b11 + b22)
        val p2 = (a21 + a22) * b11
        val p3 = a11 * (b12 - b22)
        val p4 = a22 * (b21 - b11)
        val p5 = (a11 + a12) * b22
        val p6 = (a21 - a11) * (b11 + b12)
        val p7 = (a12 - a22) * (b21 + b22)
        
        for (i <- c.range1; j <- c.range2) {
            c.v(i)(j) = if (i < d && j < d)  p1.v(i)(j) + p4.v(i)(j)- p5.v(i)(j) + p7.v(i)(j)
                   else if (i < d)           p3.v(i)(j-d) + p5.v(i)(j-d)
                   else if (i >= d && j < d) p2.v(i-d)(j) + p4.v(i-d)(j)
                   else                      p1.v(i-d)(j-d) - p2.v(i-d)(j-d) + p3.v(i-d)(j-d) + p6.v(i-d)(j-d)
        } // for
        c                                    // return result matrix
    } // times_s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u' to produce another matrix '(a_ij * u_j)'.
     *  E.g., multiply a matrix by a diagonal matrix represented as a vector.
     *  @param u  the vector to multiply by
     */
    def ** (u: VectorC): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) * u(j)
        c
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by vector 'u' to produce another matrix '(a_ij * u_j)'.
     *  @param u  the vector to multiply by
     */
    def **= (u: VectorC): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) = v(i)(j) * u(j)
        this
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: Complex): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) / x
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: Complex): MatrixC =
    {
        for (i <- range1; j <- range2) v(i)(j) = v(i)(j) / x
        this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' matrix to the 'p'th power (for some integer 'p' >= 2).
     *  Caveat: should be replace by a divide and conquer algorithm.
     *  @param p  the power to raise 'this' matrix to
     */
    def ~^ (p: Int): MatrixC =
    {
        if (p < 2)      flaw ("~^", "p must be an integer >= 2")
        if (! isSquare) flaw ("~^", "only defined on square matrices")

        val c = new MatrixC (dim1, dim1)
        for (i <- range1; j <- range1) {
            var sum = _0
            for (k <- range1) sum += v(i)(k) * v(k)(j)
            c.v(i)(j) = sum
        } // for
        if (p > 2) c ~^ (p-1) else c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): Complex =
    {
        var x = v(0)(0)
        for (i <- 1 until e; j <- range2 if v(i)(j) > x) x = v(i)(j)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): Complex =
    {
        var x = v(0)(0)
        for (i <- 1 until e; j <- range2 if v(i)(j) < x) x = v(i)(j)
        x
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor 'this' matrix into the product of upper and lower triangular
     *  matrices '(l, u)' using the LU Factorization algorithm.  This version uses
     *  no partial pivoting.
     */
    def lud_npp: Tuple2 [MatrixC, MatrixC] =
    {
        val l = new MatrixC (dim1, dim2)    // lower triangular matrix
        val u = new MatrixC (this)          // upper triangular matrix (a copy of this)

        for (i <- u.range1) {
            val pivot = u.v(i)(i)
            if (pivot =~ _0) flaw ("lud_npp", "use lud since you have a zero pivot")
            l.v(i)(i) = _1
            for (j <- i + 1 until u.dim2) l.v(i)(j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u.v(k)(i) / pivot
                l.v(k)(i) = mul
                for (j <- u.range2) u.v(k)(j) = u.v(k)(j) - mul * u.v(i)(j)
            } // for
        } // for
        Tuple2 (l, u)
    } // lud_npp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the LU Factorization algorithm.  This version uses
     *  partial pivoting.
     */
    def lud: Tuple2 [MatrixC, MatrixC] =
    {
        val l = new MatrixC (dim1, dim2)         // lower triangular matrix
        val u = new MatrixC (this)               // upper triangular matrix (a copy of this)

        for (i <- u.range1) {
            var pivot = u.v(i)(i)
            if (pivot =~ _0) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                u.swap (i, k, i)                 // swap rows i and k from column k
                pivot = u.v(i)(i)                // reset the pivot
            } // if
            l.v(i)(i) = _1
            for (j <- i + 1 until u.dim2) l.v(i)(j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u.v(k)(i) / pivot
                l.v(k)(i) = mul
                for (j <- u.range2) u.v(k)(j) = u.v(k)(j) - mul * u.v(i)(j)
            } // for
        } // for
        Tuple2 (l, u)
    } // lud

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor in-place 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the LU Factorization algorithm.  This version uses
     *  partial pivoting.
     */
    def lud_ip: Tuple2 [MatrixC, MatrixC] =
    {
        val l = new MatrixC (dim1, dim2)         // lower triangular matrix
        val u = this                             // upper triangular matrix (this)

        for (i <- u.range1) {
            var pivot = u.v(i)(i)
            if (pivot =~ _0) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                u.swap (i, k, i)                 // swap rows i and k from column k
                pivot = u.v(i)(i)                // reset the pivot
            } // if
            l.v(i)(i) = _1
            for (j <- i + 1 until u.dim2) l.v(i)(j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u.v(k)(i) / pivot
                l.v(k)(i) = mul
                for (j <- u.range2) u.v(k)(j) = u.v(k)(j) - mul * u.v(i)(j)
            } // for
        } // for
        Tuple2 (l, u)
    } // lud_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use partial pivoting to find a maximal non-zero pivot and return its row
     *  index, i.e., find the maximum element '(k, i)' below the pivot '(i, i)'.
     *  @param a  the matrix to perform partial pivoting on
     *  @param i  the row and column index for the current pivot
     */
    private def partialPivoting (a: MatrixC, i: Int): Int =
    {
        var max  = a.v(i)(i)   // initially set to the pivot
        var kMax = i           // initially the pivot row

        for (k <- i + 1 until a.dim1 if ABS (a.v(k)(i)) > max) {
            max  = ABS (a.v(k)(i))
            kMax = k
        } // for

        if (kMax == i) {
            flaw ("partialPivoting", "unable to find a non-zero pivot for row " + i)
        } // if
        kMax
    } // partialPivoting

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see lud above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: MatriC, u: MatriC, b: VectorC): VectorC =
    {
        val y  = new VectorC (l.dim2)
        for (k <- 0 until y.dim) {                   // solve for y in l*y = b
            y(k) = b(k) - (l(k) dot y)
        } // for

        val x = new VectorC (u.dim2)
        for (k <- x.dim - 1 to 0 by -1) {            // solve for x in u*x = y
            x(k) = (y(k) - (u(k) dot x)) / u(k, k)
        } // for
        x
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' where 'l = this'.  Requires
     *  'l' to be lower triangular.
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (u: MatriC, b: VectorC): VectorC = solve (this, u, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is 'this' matrix.
     *  @param b  the constant vector.
     */
    def solve (b: VectorC): VectorC = solve (lud, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine 'this' matrix with matrix 'b', placing them along the diagonal and
     *  filling in the bottom left and top right regions with zeros; '[this, b]'.
     *  @param b  the matrix to combine with 'this' matrix
     */
    def diag (b: MatriC): MatrixC =
    {
        val m = dim1 + b.dim1
        val n = dim2 + b.dim2
        val c = new MatrixC (m, n)

        for (i <- 0 until m; j <- 0 until n) {
            c.v(i)(j) = if (i <  dim1 && j <  dim2) v(i)(j)
                   else if (i >= dim1 && j >= dim2) b(i-dim1, j-dim2)
                      else                          _0
        } // for
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix '[Ip, this, Iq]' where Ir is a r-by-r identity matrix, by
     *  positioning the three matrices 'Ip', 'this' and 'Iq' along the diagonal.
     *  Fill the rest of matrix with zeros.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int = 0): MatrixC =
    {
        if (! isSquare) flaw ("diag", "'this' matrix must be square")

        val n = dim1 + p + q 
        val c = new MatrixC (n, n)

        for (i <- 0 until p)                       c.v(i)(i) = _1            // Ip
        for (i <- 0 until dim1; j <- 0 until dim1) c.v(i+p)(j+p) = v(i)(j)    // this
        for (i <- p + dim1 until n)                c.v(i)(i) = _1            // Iq
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the kth diagonal of 'this' matrix.  Assumes 'dim2 >= dim1'.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): VectorC =
    {
        val c  = new VectorC (dim1 - math.abs (k))
        val (j, l) = (math.max (-k, 0), math.min (dim1-k, dim1))
        for (i <- j until l) c(i-j) = v(i)(i+k)
        c
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the kth diagonal of 'this' matrix to the vector 'u'.  Assumes 'dim2 >= dim1'.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorC, k: Int = 0)
    { 
        val (j, l) = (math.max (-k, 0), math.min (dim1-k, dim1))
        for (i <- j until l) v(i)(i+k) = u(i-j)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of 'this' matrix to the scalar 'x'.  Assumes 'dim2 >= dim1'.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: Complex) { for (i <- range1) v(i)(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' matrix (requires a square matrix) and does not use partial pivoting.
     */
    def inverse_npp: MatrixC =
    {
        val b = new MatrixC (this)              // copy 'this' matrix into b
        val c = eye (dim1)                      // let c represent the augmentation

        for (i <- b.range1) {
            val pivot = b.v(i)(i)
            if (pivot =~ _0) flaw ("inverse_npp", "use inverse since you have a zero pivot")
            for (j <- b.range2) {
                b.v(i)(j) /= pivot
                c.v(i)(j) /= pivot
            } // for
            for (k <- 0 until b.dim1 if k != i) {
                val mul = b.v(k)(i)
                for (j <- b.range2) {
                    b.v(k)(j) -= mul * b.v(i)(j)
                    c.v(k)(j) -= mul * c.v(i)(j)
                } // for
            } // for
        } // for
        c
    } // inverse_npp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' matrix (requires a square matrix) and use partial pivoting.
     */
    def inverse: MatrixC =
    {
        val b = new MatrixC (this)              // copy 'this' matrix into b
        val c = eye (dim1)                      // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ _0) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                b.swap (i, k, i)                // in b, swap rows i and k from column i
                c.swap (i, k, 0)                // in c, swap rows i and k from column 0
                pivot = b.v(i)(i)               // reset the pivot
            } // if
            for (j <- b.range2) {
                b.v(i)(j) /= pivot
                c.v(i)(j) /= pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b.v(k)(i)
                for (j <- b.range2) {
                    b.v(k)(j) -= mul * b.v(i)(j)
                    c.v(k)(j) -= mul * c.v(i)(j)
                } // for
            } // for
        } // for
        c
    } // inverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place 'this' matrix (requires a square matrix) and uses partial pivoting.
     *  Note: this method turns the orginal matrix into the identity matrix.
     *  The inverse is returned and is captured by assignment.
     */
    def inverse_ip: MatrixC =
    {
        var b = this                            // use 'this' matrix for b
        val c = eye (dim1)                      // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ _0) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                b.swap (i, k, i)                // in b, swap rows i and k from column i
                c.swap (i, k, 0)                // in c, swap rows i and k from column 0
                pivot = b.v(i)(i)               // reset the pivot
            } // if
            for (j <- b.range2) {
                b.v(i)(j) /= pivot
                c.v(i)(j) /= pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b.v(k)(i)
                for (j <- b.range2) {
                    b.v(k)(j) -= mul * b.v(i)(j)
                    c.v(k)(j) -= mul * c.v(i)(j)
                } // for
            } // for
        } // for
        c                                       // return the solution
    } // inverse_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on 'this' matrix to make the left part embed an
     *  identity matrix.  A constraint on this 'm-by-n' matrix is that 'n >= m'.
     *  It can be used to solve 'a * x = b': augment 'a' with 'b' and call reduce.
     *  Takes '[a | b]' to '[I | x]'.
     */
    def reduce: MatrixC =
    {
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")

        val b = new MatrixC (this)    // copy 'this' matrix into b

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ _0) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                b.swap (i, k, i)                // in b, swap rows i and k from column i
                pivot = b.v(i)(i)               // reset the pivot
            } // if
            for (j <- b.range2) b.v(i)(j) /= pivot
            for (k <- 0 until dim1 if k != i) {
                val mul = b.v(k)(i)
                for (j <- b.range2) b.v(k)(j) -= mul * b.v(i)(j)
            } // for
        } // for
        b
    } // reduce

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction in-place on 'this' matrix to make the left part
     *  embed an identity matrix.  A constraint on this 'm-by-n' matrix is that 'n >= m'.
     *  It can be used to solve 'a * x = b': augment 'a' with 'b' and call reduce.
     *  Takes '[a | b]' to '[I | x]'.
     */
    def reduce_ip ()
    {
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")

        val b = this         // use 'this' matrix for b

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ _0) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                b.swap (i, k, i)                // in b, swap rows i and k from column i
                pivot = b.v(i)(i)               // reset the pivot
            } // if
            for (j <- b.range2) b.v(i)(j) /= pivot
            for (k <- 0 until dim1 if k != i) {
                val mul = b.v(k)(i)
                for (j <- b.range2) b.v(k)(j) -= mul * b.v(i)(j)
            } // for
        } // for
    } // reduce_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' matrix at or below the threshold 'thres' by setting
     *  them to zero.  Iterative algorithms give approximate values and if very close
     *  to zero, may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double, relative: Boolean = true): MatrixC =
    {
        val s = if (relative) mag else _1              // use matrix magnitude or 1
        for (i <- range1; j <- range2) if (ABS (v(i)(j)) <= thres * s) v(i)(j) = _0
        this
    } // clean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of 'this' 'm-by-n' matrix (requires 'n = m+1')
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.
     *  <p>
     *      nullspace (a) = set of orthogonal vectors v s.t. a * v = 0
     *  <p>
     *  The left nullspace of matrix 'a' is the same as the right nullspace of 'a.t'.
     *  FIX: need a more robust algorithm for computing nullspace (@see Fac_QR.scala).
     *  FIX: remove the 'n = m+1' restriction.
     *  @see http://ocw.mit.edu/courses/mathematics/18-06sc-linear-algebra-fall-2011/ax-b-and-the-four-subspaces
     *  /solving-ax-0-pivot-variables-special-solutions/MIT18_06SCF11_Ses1.7sum.pdf
     */
    def nullspace: VectorC =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")

        reduce.col(dim2 - 1) * -_1 ++ _1
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute in-place the (right) nullspace of 'this' 'm-by-n' matrix (requires 'n = m+1')
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.
     *  <p>
     *      nullspace (a) = set of orthogonal vectors v s.t. a * v = 0
     *  <p>
     *  The left nullspace of matrix 'a' is the same as the right nullspace of 'a.t'.
     *  FIX: need a more robust algorithm for computing nullspace (@see Fac_QR.scala).
     *  FIX: remove the 'n = m+1' restriction.
     *  @see http://ocw.mit.edu/courses/mathematics/18-06sc-linear-algebra-fall-2011/ax-b-and-the-four-subspaces
     *  /solving-ax-0-pivot-variables-special-solutions/MIT18_06SCF11_Ses1.7sum.pdf
     */
    def nullspace_ip: VectorC =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")

        reduce_ip
        col(dim2 - 1) * -_1 ++ _1
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of 'this' matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: Complex =
    {
        if ( ! isSquare) flaw ("trace", "trace only works on square matrices")

        var sum = _0
        for (i <- range1) sum += v(i)(i)
        sum
    } // trace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of 'this' matrix, i.e., the sum of its elements.
     */
    def sum: Complex =
    {
        var sum = _0
        for (i <- range1; j <- range2) sum += v(i)(j)
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the abs sum of 'this' matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: Complex =
    {
        var sum = _0
        for (i <- range1; j <- range2) sum += ABS (v(i)(j))
        sum
    } // sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of 'this' matrix.
     */
    def sumLower: Complex =
    {
        var sum = _0
        for (i <- range1; j <- 0 until i) sum += v(i)(j)
        sum
    } // sumLower

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of 'this' matrix.  The value of the determinant
     *  indicates, among other things, whether there is a unique solution to a
     *  system of linear equations (a nonzero determinant).
     */
    def det: Complex =
    {
        if ( ! isSquare) flaw ("det", "determinant only works on square matrices")

        var sum = _0
        var b: MatrixC = null
        for (j <- range2) {
            b = sliceExclude (0, j)   // the submatrix that excludes row 0 and column j
            sum += (if (j % 2 == 0) v(0)(j) * (if (b.dim1 == 1) b.v(0)(0) else b.det)
                    else           -v(0)(j) * (if (b.dim1 == 1) b.v(0)(0) else b.det))
        } // for 
        sum
    } // det

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is rectangular (all rows have the same number
     *  of columns).
     */
    def isRectangular: Boolean =
    {
        for (i <- range1 if v(i).length != dim2) return false
        true
    } // isRectangular

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' real (double precision) matrix to a string.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("\nMatrixC(")
        if (dim1 == 0) return sb.append (")").mkString
        for (i <- range1) {
            for (j <- range2) {
                sb.append (fString.format (v(i)(j)))
                if (j == dim2-1) sb.replace (sb.length-1, sb.length, "\n\t")
            } // for
        } // for
        sb.replace (sb.length-3, sb.length, ")").mkString
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' matrix to a CSV-formatted text file with name 'fileName'.
     *  @param fileName  the name of file to hold the data
     */
    def write (fileName: String)
    {
        val out = new PrintWriter (fileName)
        for (i <- range1) {
            for (j <- range2) { out.print (v(i)(j)); if (j < dim2-1) out.print (",") }
            out.println ()
        } // for
        out.close
    } // write
  
} // MatrixC class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixC` companion object provides operations for `MatrixC` that don't require
 *  'this' (like static methods in Java).  It provides factory methods for building
 *  matrices from files or vectors.
 */
object MatrixC extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the array of vectors 'u'.
     *  @param u           the array of vectors to assign
     *  @param columnwise  whether the vectors are treated as column or row vectors
     */
    def apply (u: Array [VectorC], columnwise: Boolean = true): MatrixC =
    {
        var x: MatrixC = null
        val u_dim = u(0).dim
        if (columnwise) {
            x = new MatrixC (u_dim, u.length)
            for (j <- 0 until u.length) x.setCol (j, u(j))    // assign column vectors
        } else {
            x = new MatrixC (u.length, u_dim)
            for (i <- 0 until u_dim) x(i) = u(i)              // assign row vectors
        } // if
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the Scala `Vector` of vectors 'u'.
     *  Assumes vectors are columwise.
     *  @param u  the Vector of vectors to assign
     */
    def apply (u: Vector [VectorC]): MatrixC =
    {
        val u_dim = u(0).dim
        val x = new MatrixC (u_dim, u.length)
        for (j <- 0 until u.length) x.setCol (j, u(j))        // assign column vectors
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix by reading from a text file, e.g., a CSV file.
     *  @param fileName  the name of file holding the data
     */
    def apply (fileName: String): MatrixC =
    {
        val sp     = ','                                          // character separating the values
        val lines  = fromFile (fileName).getLines.toArray         // get the lines from file
        val (m, n) = (lines.length, lines(0).split (sp).length)
        val x      = new MatrixC (m, n)
        for (i <- 0 until m) x(i) = VectorC (lines(i).split (sp))
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an 'm-by-n' identity matrix I (ones on main diagonal, zeros elsewhere).
     *  If 'n' is <= 0, set it to 'm' for a square identity matrix.
     *  @param m  the row dimension of the matrix
     *  @param n  the column dimension of the matrix (defaults to 0 => square matrix)
     */
    def eye (m: Int, n: Int = 0): MatrixC =
    {
        val nn = if (n <= 0) m else n             // square matrix, if n <= 0
        val mn = if (m <= nn) m else nn           // length of main diagonal
        val c = new MatrixC (m, nn)
        for (i <- 0 until mn) c.v(i)(i) = _1
        c
    } // eye

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vectors 'u' and 'w' to form a matrix with 2 rows.
     *  @param u  the vector to be concatenated as the new first row in matrix
     *  @param w  the vector to be concatenated as the new second row in matrix
     */
    def ++ (u: VectorC, w: VectorC): MatrixC =
    {
        if (u.dim != w.dim) flaw ("++", "vector dimensions do not match")

        val c = new MatrixC (2, u.dim)
        c(0) = u
        c(1) = w
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vectors 'u' and 'w' to form a matrix with 2 columns.
     *  @param u  the vector to be concatenated as the new first column in matrix
     *  @param w  the vector to be concatenated as the new second column in matrix
     */
    def ++^ (u: VectorC, w: VectorC): MatrixC =
    {
        if (u.dim != w.dim) flaw ("++^", "vector dimensions do not match")

        val c = new MatrixC (u.dim, 2)
        c.setCol (0, u)
        c.setCol (1, w)
        c
    } // ++^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector 'u' by matrix 'a'.  Treat 'u' as a row vector.
     *  @param u  the vector to multiply by
     *  @param a  the matrix to multiply by (requires sameCrossDimensions)
     */
    def times (u: VectorC, a: MatrixC): VectorC =
    {
        if (u.dim != a.dim1) flaw ("times", "vector * matrix - incompatible cross dimensions")

        val c = new VectorC (a.dim2)
        for (j <- a.range2) {
            var sum = _0
            for (k <- a.range1) sum += u(k) * a.v(k)(j)
            c(j) = sum
        } // for
        c
    } // times

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the outer product of vector 'x' and vector 'y'.  The result of the
     *  outer product is a matrix where 'c(i, j)' is the product of 'i'-th element
     *  of 'x' with the 'j'-th element of 'y'.
     *  @param x  the first vector
     *  @param y  the second vector
     */
    def outer (x: VectorC, y: VectorC): MatrixC =
    {
        val c = new MatrixC (x.dim, y.dim)
        for (i <- 0 until x.dim; j <- 0 until y.dim) c(i, j) = x(i) * y(j)
        c
    } // outer

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from two vectors 'x' and 'y', row-wise.
     *  @param x  the first vector -> row 0
     *  @param y  the second vector -> row 1
     */
    def form_rw (x: VectorC, y: VectorC): MatrixC =
    {
        if (x.dim != y.dim) flaw ("form_rw", "dimensions of x and y must be the same")

        val cols = x.dim
        val c = new MatrixC (2, cols)
        c(0) = x
        c(1) = y
        c
    } // form_rw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from scalar 'x' and a vector 'y', row-wise.
     *  @param x  the first scalar -> row 0 (repeat scalar)
     *  @param y  the second vector -> row 1
     */
    def form_rw (x: Complex, y: VectorC): MatrixC =
    {
        val cols = y.dim
        val c = new MatrixC (2, cols)
        for  (j <- 0 until cols) c(0, j) = x
        c(1) = y
        c
    } // form_rw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from a vector 'x' and a scalar 'y', row-wise.
     *  @param x  the first vector -> row 0
     *  @param y  the second scalar -> row 1 (repeat scalar)
     */
    def form_rw (x: VectorC, y: Complex): MatrixC =
    {
        val cols = x.dim
        val c = new MatrixC (2, cols)
        c(0) = x
        for  (j <- 0 until cols) c(1, j) = y
        c
    } // form_rw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from two vectors 'x' and 'y', column-wise.
     *  @param x  the first vector -> column 0
     *  @param y  the second vector -> column 1
     */
    def form_cw (x: VectorC, y: VectorC): MatrixC =
    {
        if (x.dim != y.dim) flaw ("form_cw", "dimensions of x and y must be the same")

        val rows = x.dim
        val c = new MatrixC (rows, 2)
        c.setCol(0, x)
        c.setCol(1, y)
        c
    } // form_cw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from a scalar 'x' and a vector 'y', column-wise.
     *  @param x  the first scalar -> column 0 (repeat scalar)
     *  @param y  the second vector -> column 1
     */
    def form_cw (x: Complex, y: VectorC): MatrixC =
    {
        val rows = y.dim
        val c = new MatrixC (rows, 2)
        for (i <- 0 until rows) c(i, 0) = x
        c.setCol(1, y)
        c
    } // form_cw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from a vector 'x' and a scalar 'y', column-wise.
     *  @param x  the first vector -> column 0
     *  @param y  the second scalar -> column 1 (repeat scalar)
     */
    def form_cw (x: VectorC, y: Complex): MatrixC =
    {
        val rows = x.dim
        val c = new MatrixC (rows, 2)
        c.setCol(0, x)
        for (i <- 0 until rows) c(i, 1) = y
        c
    } // form_cw

} // MatrixC companion object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixCTest` object tests the operations provided by `MatrixC` class.
 *  > run-main scalation.linalgebra.MatrixCTest
 */
object MatrixCTest extends App with PackageInfo
{
    for (l <- 1 to 4) {
        println ("\n\tTest MatrixC on real matrices of dim " + l)
        val x = new MatrixC (l, l)
        val y = new MatrixC (l, l)
        x.set (2)
        y.set (3)
        println ("x + y = " + (x + y))
        println ("x - y = " + (x - y))
        println ("x * y = " + (x * y))
        println ("x * 4 = " + (x * 4))
    } // for

    println ("\n\tTest MatrixC on additional operations")

    val z   = new MatrixC ((2, 2), 1, 2,
                                   3, 2)
    val t   = new MatrixC ((3, 3), 1, 2, 3,
                                   4, 3, 2,
                                   1, 3, 1)
    val zz  = new MatrixC ((3, 3), 3, 1, 0,
                                   1, 4, 2,
                                   0, 2, 5)
    val bz  = VectorC (5, 3, 6)
    val b   = VectorC (8, 7)
    val lu  = z.lud
    val lu2 = z.lud_npp

    println ("z            = " + z)
    println ("z.t          = " + z.t)
    println ("z.lud        = " + lu)
    println ("z.lud_npp    = " + lu2)
    println ("z.solve      = " + z.solve (lu._1, lu._2, b))
    println ("zz.solve     = " + zz.solve (zz.lud, bz))
    println ("z.inverse    = " + z.inverse)
    println ("z.inverse_ip = " + z.inverse_ip)
    println ("t.inverse    = " + t.inverse)
    println ("t.inverse_ip = " + t.inverse_ip)
    println ("z.inv * b    = " + z.inverse * b)
    println ("z.det        = " + z.det)
    println ("z            = " + z)
    z *= z                             // in-place matrix multiplication
    println ("z squared = " + z)

    val w = new MatrixC ((2, 3), 2,  3, 5, 
                                -4,  2, 3)
    val v = new MatrixC ((3, 2), 2, -4, 
                                 3,  2,
                                 5,  3)
    println ("w         = " + w)
    println ("v         = " + v)
    println ("w.reduce  = " + w.reduce)

    println ("right:    w.nullspace = " + w.nullspace)
    println ("check right nullspace = " + w * w.nullspace)

    println ("left:   v.t.nullspace = " + v.t.nullspace)
    println ("check left  nullspace = " + MatrixC.times (v.t.nullspace, v))

    for (row <- z) println ("row = " + row.deep)

    val aa = new MatrixC ((3, 2), 1, 2,
                                  3, 4,
                                  5, 6)
    val bb = new MatrixC ((2, 2), 1, 2,
                                  3, 4)

    println ("aa        = " + aa)
    println ("bb        = " + bb)
    println ("aa * bb   = " + aa * bb)
    aa *= bb
    println ("aa *= bb  = " + aa) 
    println ("aa dot bz = " + (aa dot bz))
    println ("aa.t * bz = " + aa.t * bz)

    val filename = getDataPath + "bb_matrix.csv"
    bb.write (filename)
    println ("bb_csv = " + MatrixC (filename))

} // MatrixCTest object

