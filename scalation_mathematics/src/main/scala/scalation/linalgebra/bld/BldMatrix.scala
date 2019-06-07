
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon May 25 17:57:02 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldMatrix` object is used to build matrix classes for various base types.
 *  > runMain scalation.linalgebra.bld.BldMatrix
 */
object BldMatrix extends App with BldParams
{
    println ("BldMatrix: generate code for Matrix classes")

    for (i <- 0 until kind.length-1) {        // do not generate `MatrixS`
        val VECTO     = kind(i)._1
        val VECTOR    = kind(i)._1.replace ("o", "or")
        val BASE      = kind(i)._2
        val VECTO2    = kind(i)._3
        val VECTOR2   = kind(i)._3.replace ("o", "or")
        val BASE2     = kind(i)._4
        val FORMAT    = kind(i)._5
        val MATRI     = kind(i)._6
        val ZERO      = kind(i)._8
        val ONE       = kind(i)._9
        val BASE_LC   = BASE.toLowerCase
        val MATRIX    = { val m = MATRI.splitAt (MATRI.size-1); m._1 + "x" + m._2 }
        val IMPORT    = if (CUSTOM contains BASE) s"scalation.math.$BASE.{abs => ABS, _}"
                                              else "scala.math.{abs => ABS}"
        val IMPORT2   = if (BASE == "StrNum") "scalation.math.{StrO, oneIf}"
                        else if (CUSTOM contains BASE) s"scalation.math.{$BASE, oneIf}"
                        else s"scalation.math.{${BASE_LC}_exp, oneIf}"

// Beginning of string holding code template -----------------------------------

        val code = raw"""
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.linalgebra.bld.BldMatrix
 *  @version 1.6
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import java.io.PrintWriter

import scala.io.Source.fromFile

import $IMPORT

import $IMPORT2
import scalation.math.ExtremeD.TOL
import scalation.util.Error

import $MATRIX.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$MATRIX` class stores and operates on Numeric Matrices of type `$BASE`.
 *  This class follows the `gen.MatrixN` framework and is provided for efficiency.
 *  Caveat:  Only works for rectangular matrices.  For matrix-like structures
 *  based on jagged arrays, where the second dimension varies,
 *  @see `scalation.linalgebra.gen.HMatrix2`
 *  @param d1  the first/row dimension
 *  @param d2  the second/column dimension
 *  @param v   the 2D array used to store matrix elements
 */
class $MATRIX (d1: Int,
               d2: Int,
               private [linalgebra] var v: Array [Array [$BASE]] = null)
      extends $MATRI with Error with Serializable
{
    /** Debug flag
     */
    private val DEBUG = true

    /** Dimension 1
     */
    lazy val dim1 = d1

    /** Dimension 2
     */
    lazy val dim2 = d2

    if (v == null) {
        v = Array.ofDim [$BASE] (dim1, dim2)
    } else if (dim1 != v.length || dim2 != v(0).length) {
        flaw ("constructor", "dimensions are wrong")
    } // if

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 'dim1' by 'dim1' square matrix.
     *  @param dim1  the row and column dimension
     */
    def this (dim1: Int) { this (dim1, dim1) }
"""; val code2 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a 'dim1' by 'dim2' matrix and assign each element the value 'x'.
     *  @param dim1  the row dimension
     *  @param dim2  the column dimension
     *  @param x     the scalar value to assign
     */
    def this (dim1: Int, dim2: Int, x: $BASE)
    {
        this (dim1, dim2)
        for (i <- range1; j <- range2) v(i)(j) = x
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix and assign values from array of arrays 'u'.
     *  @param u  the 2D array of values to assign
     */
    def this (u: Array [Array [$BASE]]) { this (u.length, u(0).length, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix from repeated values.
     *  @param dim  the (row, column) dimensions
     *  @param u    the repeated values
     */
    def this (dim: (Int, Int), u: $BASE*)
    {
        this (dim._1, dim._2)
        for (i <- range1; j <- range2) v(i)(j) = u(i * dim2 + j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix and assign values from matrix 'b'.
     *  @param b  the matrix of values to assign
     */
    def this (b: $MATRI)
    {
        this (b.dim1, b.dim2)
        for (i <- range1; j <- range2) v(i)(j) = b(i, j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' m-by-n matrix.
     */
    def copy (): $MATRIX = new $MATRIX (dim1, dim2, (for (i <- range1) yield v(i).clone ()).toArray)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an m-by-n matrix with all elements initialized to zero.
     *  @param m  the number of rows
     *  @param n  the number of columns
     */
    def zero (m: Int = dim1, n: Int = dim2): $MATRIX = new $MATRIX (m, n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's element at the 'i,j'-th index position. 
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): $BASE = v(i)(j)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's vector at the 'i'-th index position ('i'-th row).
     *  @param i  the row index
     */
    def apply (i: Int): $VECTOR = $VECTOR (v(i))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise on range 'ir' and column-wise on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): $MATRIX = slice (ir.start, ir.end, jr.start, jr.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the underlying 2D array for 'this' matrix.
     */
    def apply (): Array [Array [$BASE]] = v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the rows from 'this' matrix according to the given index/basis.
     *  The new matrix is formed by referencing rows in the current matrix,
     *  thereby saving space.
     *  @param iv  the row index vector
     */
    override def apply (iv: VectoI): $MATRIX =
    {
        val a = Array.ofDim [Array [$BASE]] (iv.dim)
        for (i <- a.indices) a(i) = v(iv(i))
        new $MATRIX (a.length, dim2, a)
    } // apply
"""; val code3 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's element at the 'i,j'-th index position to the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: $BASE) { v(i)(j) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's row at the 'i'-th index position to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: $VECTO) { v(i) = u().toArray }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice 'this' matrix row-wise on range 'ir' and column-wise on range 'jr'.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: $MATRI)
    {
        if (b.isInstanceOf [$MATRIX]) {
            val bb = b.asInstanceOf [$MATRIX]
            for (i <- ir; j <- jr) v(i)(j) = bb.v(i - ir.start)(j - jr.start)
        } else {
            flaw ("update", "must convert b to a $MATRIX first")
        } // if
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in 'this' matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: $BASE) { for (i <- range1; j <- range2) v(i)(j) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in 'this' matrix as copies of the values in 2D array 'u'.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [$BASE]]) { for (i <- range1; j <- range2) v(i)(j) = u(i)(j) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in 'this' matrix as copies of the values in matrix 'b'.
     *  @param b  the matrix of values to assign
     */
    def set (b: $MATRI)
    {
        if (b.isInstanceOf [$MATRIX]) {
            val bb = b.asInstanceOf [$MATRIX]
            for (i <- range1; j <- range2) v(i)(j) = bb.v(i)(j)
        } else {
            flaw ("set", "must convert b to a $MATRIX first")
        } // if
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's 'i'-th row starting at column 'j' to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: $VECTO, j: Int = 0) { for (k <- 0 until u.dim) v(i)(k+j) = u(k) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$MATRIX` into an integer matrix `MatrixI`.
     */
    def toInt: MatrixI =
    {
        val c = new MatrixI (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j).toInt
        c
    } // toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$MATRIX` into a double matrix `MatrixD`.
     */
    def toDouble: MatrixD =
    {
        val c = new MatrixD (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j).toDouble
        c
    } // toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' matrix to a dense matrix.
     */
    def toDense: $MATRIX = this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' matrix by applying the mapping function 'f'.
     *  @param f  the function to apply
     */
    override def map (f: $VECTO => $VECTO): $MATRIX =
    {
        $MATRIX (for (i <- range1) yield f(new $VECTOR (v(i).length, v(i))), false)
    } // map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'from' to 'end'.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): $MATRIX =
    {
        if (from >= end) return new $MATRIX (0, dim2)
        new $MATRIX (end - from, dim2, v.slice (from, end))
    } // slice
"""; val code4 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix column-wise 'from' to 'end'.
     *  @param from  the start column of the slice (inclusive)
     *  @param end   the end column of the slice (exclusive)
     */
    def sliceCol (from: Int, end: Int): $MATRIX =
    {
        if (from >= end) return new $MATRIX (dim1, 0)
        val c = new $MATRIX (dim1, end - from)
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
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): $MATRIX = 
    {
        if (r_from >= r_end || c_from >= c_end) return new $MATRIX (0, 0)
        val c = new $MATRIX (r_end - r_from, c_end - c_from)
        for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(i + r_from)(j + c_from)
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix excluding the given row and/or column.
     *  @param row  the row to exclude (0 until dim1, set to dim1 to keep all rows)
     *  @param col  the column to exclude (0 until dim2, set to dim2 to keep all columns)
     */
    def sliceEx (row: Int, col: Int): $MATRIX =
    {
        val c = new $MATRIX (dim1 - oneIf (row < dim1), dim2 - oneIf (col < dim2))
        for (i <- range1 if i != row) for (j <- range2 if j != col) {
            c.v(i - oneIf (i > row))(j - oneIf (j > col)) = v(i)(j)
        } // for
        c
    } // sliceEx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from 'this' matrix according to the given index/basis.
     *  The new matrix is formed by copying rows from the current matrix.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): $MATRIX =
    {
        val c = new $MATRIX (rowIndex.length, dim2)
        for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(rowIndex(i))(j)
        c
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from the matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): $VECTOR =
    {
        val u = new $VECTOR (dim1 - from)
        for (i <- from until dim1) u(i-from) = v(i)(col)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of the matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: $VECTO) { for (i <- range1) v(i)(col) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from 'this' matrix according to the given index/basis.
     *  Ex: Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): $MATRIX =
    {
        val c = new $MATRIX (dim1, colIndex.length)
        for (j <- c.range2) c.setCol (j, col(colIndex(j)))
        c
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose 'this' matrix (columns => rows).
     */
    def t: $MATRIX =
    {
        val c = new $MATRIX (dim2, dim1)
        for (j <- range1) {
            val v_j = v(j)
            for (i <- range2) c.v(i)(j) = v_j(i)
        } // for
        c
    } // t
"""; val code5 = raw"""
//  def t: $MATRIX =
//  {
//      val c = new $MATRIX (dim2, dim1)
//      for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(j)(i)
//      c
//  } // t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose, in-place, 'this' matrix (columns => rows).
     *  FIX: may wish to use algorithm with better data locality.
     */
    def tip (): $MATRIX =
    {
        for (i <- 1 until dim1; j <- 0 until i) {
            val t = v(i)(j); v(i)(j) = v(j)(i); v(j)(i) = t     // swap elements
        } // for
        this
    } // tip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: $VECTO): $MATRIX =
    {
        if (u.dim != dim2) flaw ("+:", "vector does not match row dimension")
        val c = new $MATRIX (dim1 + 1, dim2)
        for (i <- c.range1) c(i) = if (i == 0) u else this(i - 1)
        c
    } // +:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: $VECTO): $MATRIX =
    {
        if (u.dim != dim1) flaw ("+^:", "vector does not match column dimension")
        val c = new $MATRIX (dim1, dim2 + 1)
        for (j <- c.range2) c.setCol (j, if (j == 0) u else col (j - 1))
        c
    } // +^:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (row) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: $VECTO): $MATRIX =
    {
        if (u.dim != dim2) flaw (":+", "vector does not match row dimension")
        val c = new $MATRIX (dim1 + 1, dim2)
        for (i <- c.range1) c(i) = if (i < dim1) this(i) else u
        c
    } // :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (column) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: $VECTO): $MATRIX =
    {
        if (u.dim != dim1) flaw (":^+", "vector does not match column dimension")
        val c = new $MATRIX (dim1, dim2 + 1)
        for (j <- c.range2) c.setCol (j, if (j < dim2) col (j) else u)
        c
    } // :^+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last rows in new matrix
     */
    def ++ (b: $MATRI): $MATRIX =
    {
        if (b.dim2 != dim2) flaw ("++", "matrix b does not match row dimension")
        val c = new $MATRIX (dim1 + b.dim1, dim2)
        for (i <- c.range1) c(i) = if (i < dim1) this(i) else b(i - dim1)
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last columns in new matrix
     */
    def ++^ (b: $MATRI): $MATRIX =
    {
        if (b.dim1 != dim1) flaw ("++^", "matrix b does not match column dimension")
        val c = new $MATRIX (dim1, dim2 + b.dim2)
        for (j <- c.range2) c.setCol (j, if (j < dim2) col (j) else b.col (j - dim2))
        c
    } // ++^
"""; val code6 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and matrix 'b'.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def + (b: $MATRIX): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + b.v(i)(j)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and matrix 'b' for any type extending $MATRI.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def + (b: $MATRI): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + b(i, j)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def + (u: $VECTO): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + u(j)
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: $BASE): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) + x
        c
    } // +
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and matrix 'b'.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def += (b: $MATRIX): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) += b.v(i)(j)
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and matrix 'b' for any type extending $MATRI.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def += (b: $MATRI): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) += b(i, j)
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def += (u: $VECTO): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) += u(j)
        this
    } // +=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: $BASE): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) += x
        this
    } // +=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract matrix 'b'.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def - (b: $MATRIX): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) - b.v(i)(j)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract matrix 'b' for any type extending $MATRI.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def - (b: $MATRI): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) - b(i, j)
        c
    } // -
"""; val code7 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract (row) vector 'u'.
     *  @param b  the vector to subtract
     */
    def - (u: $VECTO): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) - u(j)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: $BASE): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- c.range1; j <- c.range2) c.v(i)(j) = v(i)(j) - x
        c
    } // -
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place matrix 'b'.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def -= (b: $MATRIX): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) -= b.v(i)(j)
        this
    } // -=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place matrix 'b'.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def -= (b: $MATRI): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) -= b(i, j)
        this
    } // -=
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place (row) vector 'u'.
     *  @param b  the vector to subtract
     */
    def -= (u: $VECTO): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) -= u(j)
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: $BASE): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) -= x
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b', transposing 'b' to improve efficiency.
     *  Use 'times' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires 'sameCrossDimensions')
     */
    def * (b: $MATRIX): $MATRIX =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c  = new $MATRIX (dim1, b.dim2)
        val bt = b.t                         // transpose the b matrix
        for (i <- range1; j <- c.range2) {
            val va = v(i); val vb = bt.v(j)
            var sum = $ZERO
            for (k <- range2) sum += va(k) * vb(k)
            c.v(i)(j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b', transposing 'b' to improve efficiency.
     *  Use 'times' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires 'sameCrossDimensions')
     */
    def * (b: $MATRI): $MATRIX =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c  = new $MATRIX (dim1, b.dim2)
        val bt = b.t                         // transpose the b matrix
        for (i <- range1; j <- c.range2) {
            val va = v(i); val vb = bt(j)
            var sum = $ZERO
            for (k <- range2) sum += va(k) * vb(k)
            c.v(i)(j) = sum
        } // for
        c
    } // *
"""; val code8 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by (column) vector 'u' (vector elements beyond 'dim2' ignored).
     *  @param u  the vector to multiply by
     */
    def * (u: $VECTO): $VECTOR =
    {
        if (dim2 > u.dim) flaw ("*", "matrix * vector - vector dimension too small")

        val c = new $VECTOR (dim1)
        for (i <- range1) {
            var sum = $ZERO
            for (k <- range2) sum += v(i)(k) * u(k)
            c(i) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: $BASE): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) * x
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by matrix 'b', transposing 'b' to improve
     *  efficiency.  Use 'times_ip' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires square and 'sameCrossDimensions')
     */
    def *= (b: $MATRIX): $MATRIX =
    {
        if (! b.isSquare)   flaw ("*=", "matrix 'b' must be square")
        if (dim2 != b.dim1) flaw ("*=", "matrix *= matrix - incompatible cross dimensions")

        val bt = b.t                                  // use the transpose of b
        for (i <- range1) {
            val row_i = new $VECTOR (dim2)            // save ith row so not overwritten
            for (j <- range2) row_i(j) = v(i)(j)      // copy values from ith row of 'this' matrix
            for (j <- range2) {
                val vb = bt.v(j)
                var sum = $ZERO
                for (k <- range2) sum += row_i(k) * vb(k)
                v(i)(j) = sum
            } // for
        } // for
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by matrix 'b', transposing 'b' to improve
     *  efficiency.  Use 'times_ip' method to skip the transpose step.
     *  @param b  the matrix to multiply by (requires square and 'sameCrossDimensions')
     */
    def *= (b: $MATRI): $MATRIX =
    {
        if (! b.isSquare)   flaw ("*=", "matrix 'b' must be square")
        if (dim2 != b.dim1) flaw ("*=", "matrix *= matrix - incompatible cross dimensions")

        val bt = b.t                                  // use the transpose of b
        for (i <- range1) {
            val row_i = new $VECTOR (dim2)            // save ith row so not overwritten
            for (j <- range2) row_i(j) = v(i)(j)      // copy values from ith row of 'this' matrix
            for (j <- range2) {
                val vb = bt(j)
                var sum = $ZERO
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
    def *= (x: $BASE): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) *= x
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and vector 'u', by first transposing
     *  'this' matrix and then multiplying by 'u' (i.e., 'a dot u = a.t * u').
     *  @param u  the vector to multiply by (requires same first dimensions)
     */
    def dot (u: $VECTO): $VECTOR =
    {
        if (dim1 != u.dim) flaw ("dot", "matrix dot vector - incompatible first dimensions")

        val c = new $VECTOR (dim2)
        val at = this.t                         // transpose the 'this' matrix
        for (i <- range2) {
            var sum = $ZERO
            for (k <- range1) sum += at.v(i)(k) * u(k)
            c(i) = sum
        } // for
        c
    } // dot
"""; val code9 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and matrix 'b' that results in a vector,
     *  by taking the dot product for each column 'j' of both matrices.
     *  @see www.mathworks.com/help/matlab/ref/dot.html
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def dot (b: $MATRI): $VECTOR =
    {
        if (dim1 != b.dim1) flaw ("dot", "matrix dot matrix - incompatible first dimensions")
        val c = new $VECTOR (dim2)
        for (i <- range1; j <- range2) c(j) += v(i)(j) * b(i, j)
        c
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and matrix 'b' that results in a vector,
     *  by taking the dot product for each column 'j' of both matrices.
     *  @see www.mathworks.com/help/matlab/ref/dot.html
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def dot (b: $MATRIX): $VECTOR =
    {
        if (dim1 != b.dim1) flaw ("dot", "matrix dot matrix - incompatible first dimensions")
        val c = new $VECTOR (dim2)
        for (i <- range1; j <- range2) c(j) += v(i)(j) * b.v(i)(j)
        c
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix dot product of 'this' matrix and matrix 'b', by first transposing
     *  'this' matrix and then multiplying by 'b' (i.e., 'a dot b = a.t * b').
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def mdot (b: $MATRI): $MATRIX =
    {
        if (dim1 != b.dim1) flaw ("mdot", "matrix mdot matrix - incompatible first dimensions")

        val c = new $MATRIX (dim2, b.dim2)
        val at = this.t                         // transpose the 'this' matrix
        for (i <- range2) {
            val at_i = at.v(i)                  // ith row of 'at' (column of 'a')
            for (j <- b.range2) {
                var sum = $ZERO
                for (k <- range1) sum += at_i(k) * b(k, j)
                c.v(i)(j) = sum
            } // for
        } // for
        c
    } // mdot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix dot product of 'this' matrix and matrix 'b', by first transposing
     *  'this' matrix and then multiplying by 'b' (i.e., 'a dot b = a.t * b').
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def mdot (b: $MATRIX): $MATRIX =
    {
        if (dim1 != b.dim1) flaw ("mdot", "matrix mdot matrix - incompatible first dimensions")

        val c = new $MATRIX (dim2, b.dim2)
        val at = this.t                         // transpose the 'this' matrix
        for (i <- range2) {
            val at_i = at.v(i)                  // ith row of 'at' (column of 'a')
            for (j <- b.range2) {
                var sum = $ZERO
                for (k <- range1) sum += at_i(k) * b.v(k)(j)
                c.v(i)(j) = sum
            } // for
        } // for
        c
    } // mdot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' without first transposing 'b'.
     *  @param b  the matrix to multiply by (requires 'sameCrossDimensions')
     */
    def times (b: $MATRIX): $MATRIX =
    {
        if (dim2 != b.dim1) flaw ("times", "matrix * matrix - incompatible cross dimensions")

        val c = new $MATRIX (dim1, b.dim2)
        for (i <- range1; j <- c.range2) {
            var sum = $ZERO
            for (k <- range2) sum += v(i)(k) * b.v(k)(j)
            c.v(i)(j) = sum
        } // for
        c
    } // times
"""; val code10 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by matrix 'b' without first transposing 'b'.
     *  If 'b' and 'this' reference the same matrix 'b == this', a copy of the 'this'
     *  matrix is made.
     *  @param b  the matrix to multiply by (requires square and 'sameCrossDimensions')
     */
    def times_ip (b: $MATRIX)
    {
        if (! b.isSquare)   flaw ("times_ip", "matrix 'b' must be square")
        if (dim2 != b.dim1) flaw ("times_ip", "matrix * matrix - incompatible cross dimensions")

        val bb = if (b == this) new $MATRIX (this) else b
        for (i <- range1) {
            val row_i = new $VECTOR (dim2)            // save ith row so not overwritten
            for (j <- range2) row_i(j) = v(i)(j)      // copy values from ith row of 'this' matrix
            for (j <- range2) {
                var sum = $ZERO
                for (k <- range2) sum += row_i(k) * bb.v(k)(j)
                v(i)(j) = sum
            } // for
        } // for
    } // times_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pre-multiply in-place 'this' ('a') matrix by matrix 'b', starting with column 'd'.
     *  <p>
     *      a(d:m, d:n) = b a(d:m, d:n)
     *  <p>
     *  @param b  the matrix to pre-multiply by 'this' (requires square and 'sameCrossDimensions')
     *  @param d  the column to start with
     */
    def times_ip_pre (b: $MATRIX, d: Int = 0)
    {
        val d1_d = dim1 - d
        if (! b.isSquare)   flaw ("times_ip_pre", "matrix 'b' must be square")
        if (d1_d != b.dim2) flaw ("times_ip_pre", "matrix * matrix - incompatible cross dimensions")

        for (j <- d until dim2) {
            val col_j = new $VECTOR (d1_d)                 // save jth column so not overwritten
            for (i <- d until dim1) col_j(i-d) = v(i)(j)
            for (i <- d until dim1) {
                var sum = $ZERO
                for (k <- 0 until d1_d) sum += col_j(k) * b.v(i-d)(k)
                v(i)(j) = sum
            } // for
        } // for
    } // times_ip_pre

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' using 'dot' product (concise solution).
     *  @param b  the matrix to multiply by (requires 'sameCrossDimensions')
     */
    def times_d (b: $MATRI): $MATRIX =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c = new $MATRIX (dim1, b.dim2)
        for (i <- range1; j <- c.range2) c.v(i)(j) = this(i) dot b.col(j)
        c
    } // times_d

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' using the Strassen matrix multiplication
     *  algorithm.  Both matrices ('this' and 'b') must be square.  Although the
     *  algorithm is faster than the traditional cubic algorithm, its requires
     *  more memory and is often less stable (due to round-off errors).
     *  FIX:  could be make more efficient using a virtual slice 'vslice' method.
     *  @see http://en.wikipedia.org/wiki/Strassen_algorithm
     *  @param b  the matrix to multiply by (it has to be a square matrix)
     */
    def times_s (b: $MATRIX): $MATRIX = 
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c = new $MATRIX (dim1, dim1)  // allocate result matrix
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
"""; val code11 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  E.g., multiply a matrix by a diagonal matrix represented as a vector.
     *  @param u  the vector to multiply by
     */
    def ** (u: $VECTO): $MATRIX =
    {
        val dm = math.min (dim2, u.dim)
        val c  = new $MATRIX (dim1, dm)
        for (i <- range1; j <- c.range2) c.v(i)(j) = v(i)(j) * u(j)
        c
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' elementwise (Hadamard product).
     *  @see en.wikipedia.org/wiki/Hadamard_product_(matrices)
     *  @param b  the matrix to multiply by
     */
    override def ** (b: $MATRI): $MATRIX = $MATRIX (for (i <- range1) yield this(i) * b(i), false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def **= (u: $VECTO): $MATRIX =
    {
        if (dim2 > u.dim) flaw ("**=", "vector u not large enough")
        for (i <- range1; j <- range2) v(i)(j) = v(i)(j) * u(j)
        this
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector 'u' by 'this' matrix to produce another matrix 'u_i * a_ij'.
     *  E.g., multiply a diagonal matrix represented as a vector by a matrix.
     *  This operator is right associative.
     *  @param u  the vector to multiply by
     */
    def **: (u: $VECTO): $MATRIX =
    {
        val dm = math.min (dim2, u.dim)
        val c  = new $MATRIX (dim1, dm)
        for (i <- range1; j <- c.range2) c.v(i)(j) = u(i) * v(i)(j)
        c
    } // **:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: $BASE): $MATRIX =
    {
        val c = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- range2) c.v(i)(j) = v(i)(j) / x
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: $BASE): $MATRIX =
    {
        for (i <- range1; j <- range2) v(i)(j) = v(i)(j) / x
        this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' matrix to the 'p'th power (for some integer 'p' >= 1) using
     *  a divide and conquer algorithm.
     *  @param p  the power to raise 'this' matrix to
     */
    def ~^ (p: Int): $MATRIX =
    {
        if (p < 1)      flaw ("~^", "p must be an integer >= 1")
        if (! isSquare) flaw ("~^", "only defined on square matrices")

        if (p == 2)          this * this
        else if (p == 1)     this
        else if (p % 2 == 1) this * this ~^ (p - 1)
        else { val c = this ~^ (p / 2); c * c }
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): $BASE =
    {
        if (e <= 0) flaw ("max", "ending row index e can't be negative")
        var x = v(0)(0)
        for (i <- 0 until e; j <- range2 if v(i)(j) > x) x = v(i)(j)
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): $BASE =
    {
        if (e <= 0) flaw ("max", "ending row index e can't be negative")
        var x = v(0)(0)
        for (i <- 0 until e; j <- range2 if v(i)(j) < x) x = v(i)(j)
        x
    } // min
"""; val code12 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the lower triangular of 'this' matrix (rest are zero).
     */ 
    def lowerT: $MATRIX =
    {
        val lo = new $MATRIX (dim1, dim2)
        for (i <- range1; j <- 0 to i) lo.v(i)(j) = v(i)(j)
        lo
    } // lowerT  
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the upper triangular of 'this' matrix (rest are zero).
     */ 
    def upperT: $MATRIX =
    {
        val up = new $MATRIX (dim2)
        for (i <- range2; j <- i until dim2) up.v(i)(j) = v(i)(j)
        up
    } // upperT

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor 'this' matrix into the product of upper and lower triangular
     *  matrices '(l, u)' using the 'LU' Factorization algorithm.
     *  Caveat:  This version requires square matrices and performs no partial pivoting.
     *  @see `Fac_LU` for a more complete implementation
     */
    def lud_npp: ($MATRIX, $MATRIX) =
    {
        if (! isSquare) throw new IllegalArgumentException ("lud_npp: requires a square matrix")

        val l = new $MATRIX (dim1)          // lower triangular matrix
        val u = new $MATRIX (this)          // upper triangular matrix (a copy of this)

        for (i <- u.range1) {
            val pivot = u.v(i)(i)
            if (pivot =~ $ZERO) flaw ("lud_npp", "use Fac_LU since there is a zero pivot at row " + i)
            l.v(i)(i) = $ONE
            for (j <- i + 1 until u.dim2) l.v(i)(j) = $ZERO
            for (k <- i + 1 until u.dim1) {
                val mul = u.v(k)(i) / pivot
                l.v(k)(i) = mul
                for (j <- u.range2) u.v(k)(j) = u.v(k)(j) - mul * u.v(i)(j)
            } // for
            if (DEBUG) println ("for iteration " + i + ": u = " + u)
        } // for
        (l, u)
    } // lud_npp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor in-place 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Factorization algorithm.
     */
    def lud_ip (): ($MATRIX, $MATRIX) =
    {
        if (! isSquare) throw new IllegalArgumentException ("lud_ip: requires a square matrix")

        val l = new $MATRIX (dim1)               // lower triangular matrix
        val u = this                             // upper triangular matrix (this)

        for (i <- u.range1) {
            var pivot = u.v(i)(i)
            if (pivot =~ $ZERO) flaw ("lud_ip", "use Fac_LU since there is a zero pivot at row" + i)
            l.v(i)(i) = $ONE
            for (j <- i + 1 until u.dim2) l.v(i)(j) = $ZERO
            for (k <- i + 1 until u.dim1) {
                val mul = u.v(k)(i) / pivot
                l.v(k)(i) = mul
                for (j <- u.range2) u.v(k)(j) = u.v(k)(j) - mul * u.v(i)(j)
            } // for
            if (DEBUG) println ("for iteration " + i + ": u = " + u)
        } // for
        (l, u)
    } // lud_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use partial pivoting to find a maximal non-zero pivot and return its row
     *  index, i.e., find the maximum element '(k, i)' below the pivot '(i, i)'.
     *  @param a  the matrix to perform partial pivoting on
     *  @param i  the row and column index for the current pivot
     */
    private def partialPivoting (a: $MATRIX, i: Int): Int =
    {
        var max  = a.v(i)(i)   // initially set to the pivot
        var kMax = i           // initially the pivot row

        for (k <- i + 1 until a.dim1 if ABS (a.v(k)(i)) > max) {
            max  = ABS (a.v(k)(i))
            kMax = k
        } // for

        if (kMax == i) flaw ("partialPivoting", "unable to find a non-zero pivot for row " + i)
        if (DEBUG) println ("partialPivoting: replace pivot (" + i + ", " + i + ") with pivot (" + kMax + ", " + i + ")")
        kMax
    } // partialPivoting
"""; val code13 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' using back substitution in the equation 'u*x = y' where
     *  'this' matrix ('u') is upper triangular (see 'lud_npp' above).
     *  @param y  the constant vector
     */
    def bsolve (y: $VECTO): $VECTOR =
    {
        val x = new $VECTOR (dim2)                   // vector to solve for
        for (k <- x.dim - 1 to 0 by -1) {            // solve for x in u*x = y
            val u_k = v(k)
            var sum = $ZERO
            for (j <- k + 1 until dim2) sum += u_k(j) * x(j)
            x(k) = (y(k) - sum) / v(k)(k)
        } // for
        x
    } // bsolve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud_npp' above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: $MATRI, u: $MATRI, b: $VECTO): $VECTO =
    {
        val y = new $VECTOR (l.dim2)                 // forward substitution
        for (k <- 0 until y.dim) {                   // solve for y in l*y = b
            val l_k = l(k)
            var sum = $ZERO
            for (j <- 0 until k) sum += l_k(j) * y(j)
            y(k) = b(k) - sum
        } // for
        u.bsolve (y)
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is 'this' matrix.
     *  @param b  the constant vector.
     */
    def solve (b: $VECTO): $VECTO = solve (lud_npp, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine 'this' matrix with matrix 'b', placing them along the diagonal and
     *  filling in the bottom left and top right regions with zeros; '[this, b]'.
     *  @param b  the matrix to combine with 'this' matrix
     */
    def diag (b: $MATRI): $MATRIX =
    {
        val m = dim1 + b.dim1
        val n = dim2 + b.dim2
        val c = new $MATRIX (m, n)

        for (i <- 0 until m; j <- 0 until n) {
            c.v(i)(j) = if (i <  dim1 && j <  dim2) v(i)(j)
                   else if (i >= dim1 && j >= dim2) b(i-dim1, j-dim2)
                      else                          $ZERO
        } // for
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix '[Ip, this, Iq]' where Ir is a 'r-by-r' identity matrix, by
     *  positioning the three matrices 'Ip', 'this' and 'Iq' along the diagonal.
     *  Fill the rest of matrix with zeros.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int = 0): $MATRIX =
    {
        if (! isSquare) flaw ("diag", "'this' matrix must be square")

        val n = dim1 + p + q 
        val c = new $MATRIX (n, n)

        for (i <- 0 until p)                       c.v(i)(i) = $ONE            // Ip
        for (i <- 0 until dim1; j <- 0 until dim1) c.v(i+p)(j+p) = v(i)(j)    // this
        for (i <- p + dim1 until n)                c.v(i)(i) = $ONE            // Iq
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the 'k'th diagonal of 'this' matrix.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): $VECTOR =
    {
        var i, j = 0
        val vs = if (k >= 0) { j =  k; math.min (dim1, dim2 - k) }
                 else        { i = -k; math.min (dim1 + k, dim2) }
        val c  = new $VECTOR (vs)
        for (l <- c.indices) { c(l) = v(i)(j); i += 1; j += 1 }
        c
    } // getDiag
"""; val code14 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the 'k'th diagonal of 'this' matrix to the vector 'u'.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: $VECTO, k: Int = 0)
    { 
        val dm = math.min (dim1, dim2)
        val (j, l) = (math.max (-k, 0), math.min (dm-k, dm))
        for (i <- j until l) v(i)(i+k) = u(i-j)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of 'this' matrix to the scalar 'x'.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: $BASE) { for (i <- 0 until math.min (dim1, dim2)) v(i)(i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' matrix (requires a square matrix) and does not use partial pivoting.
     */
    def inverse_npp: $MATRIX =
    {
        val b = new $MATRIX (this)              // copy 'this' matrix into b
        val c = eye (dim1)                      // let c represent the augmentation

        for (i <- b.range1) {
            val pivot = b.v(i)(i)
            if (pivot =~ $ZERO) flaw ("inverse_npp", "use inverse since you have a zero pivot")
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
    def inverse: $MATRIX =
    {
        val b = new $MATRIX (this)              // copy 'this' matrix into b
        val c = eye (dim1)                      // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ $ZERO) {
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
     *  Note: this method turns the original matrix into the identity matrix.
     *  The inverse is returned and is captured by assignment.
     */
    def inverse_ip (): $MATRIX =
    {
        var b = this                            // use 'this' matrix for b
        val c = eye (dim1)                      // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ $ZERO) {
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
"""; val code15 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on 'this' matrix to make the left part embed an
     *  identity matrix.  A constraint on this 'm-by-n' matrix is that 'n >= m'.
     *  It can be used to solve 'a * x = b': augment 'a' with 'b' and call reduce.
     *  Takes '[a | b]' to '[I | x]'.
     */
    def reduce: $MATRIX =
    {
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")

        val b = new $MATRIX (this)    // copy 'this' matrix into b

        for (i <- b.range1) {
            var pivot = b.v(i)(i)
            if (pivot =~ $ZERO) {
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
            if (pivot =~ $ZERO) {
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
    def clean (thres: Double = TOL, relative: Boolean = true): $MATRIX =
    {
        val s = if (relative) mag else $ONE              // use matrix magnitude or 1
        for (i <- range1; j <- range2) if (ABS (v(i)(j)) <= thres * s) v(i)(j) = $ZERO
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
     *  @see /solving-ax-0-pivot-variables-special-solutions/MIT18_06SCF11_Ses1.7sum.pdf
     */
    def nullspace: $VECTOR =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")

        reduce.col(dim2 - 1) * -$ONE ++ $ONE
    } // nullspace
"""; val code16 = raw"""
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
     *  @see /solving-ax-0-pivot-variables-special-solutions/MIT18_06SCF11_Ses1.7sum.pdf
     */
    def nullspace_ip (): $VECTOR =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace_ip", "requires n (columns) = m (rows) + 1")

        reduce_ip ()
        col(dim2 - 1) * -$ONE ++ $ONE
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of 'this' matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: $BASE =
    {
        if ( ! isSquare) flaw ("trace", "trace only works on square matrices")

        var sum = $ZERO
        for (i <- range1) sum += v(i)(i)
        sum
    } // trace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of 'this' matrix, i.e., the sum of its elements.
     */
    def sum: $BASE =
    {
        var sum = $ZERO
        for (i <- range1; j <- range2) sum += v(i)(j)
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'abs' sum of 'this' matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: $BASE =
    {
        var sum = $ZERO
        for (i <- range1; j <- range2) sum += ABS (v(i)(j))
        sum
    } // sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of 'this' matrix.
     */
    def sumLower: $BASE =
    {
        var sum = $ZERO
        for (i <- range1; j <- 0 until i) sum += v(i)(j)
        sum
    } // sumLower

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a normalized version of 'this' matrix.
      */
    def normalizeU: $MATRIX = $MATRIX (for (j <- range2) yield col(j).normalizeU)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of 'this' matrix.  The value of the determinant
     *  indicates, among other things, whether there is a unique solution to a
     *  system of linear equations (a nonzero determinant).
     */
    def det: $BASE =
    {
        if ( ! isSquare) flaw ("det", "determinant only works on square matrices")

        var sum = $ZERO
        var b: $MATRIX = null
        for (j <- range2) {
            b = sliceEx (0, j)             // the submatrix that excludes row 0 and column j
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
    /** Override equals to determine whether 'this' matrix equals matrix 'b'.
     *  @param b  the matrix to compare with this
     */
    override def equals (b: Any): Boolean =
    {
        if (! b.isInstanceOf [$MATRI]) return false
        val bb = b.asInstanceOf [$MATRI]
        for (i <- range1 if this(i) != bb(i)) return false         // within TOL
        true
    } // equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Must also override hashCode for 'this' matrix to be compatible with equals.
     */
    override def hashCode (): Int = v.deep.hashCode

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' real (double precision) matrix to a string.
     */
    override def toString: String = 
    {
        var sb = new StringBuilder ("\n$MATRIX(")
        if (dim1 == 0 || dim2 == 0) return sb.append (")").mkString
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
  
} // $MATRIX class
"""; val code17 = raw"""


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$MATRIX` companion object provides operations for `$MATRIX` that don't require
 *  'this' (like static methods in Java).  It provides factory methods for building
 *  matrices from files or vectors.
 */
object $MATRIX extends Error
{
    private val PROGRESS = 200000                            // give feedback at progress count
    private var sp       = ','                               // character separating the values

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the separating character from its CSV default.
     *  @param sp_  the new separating character
     */
    def setSp (sp_ : Char) { sp = sp_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix from a two-dimensional array 'u'.
     *  @param u  the 2D array for the matrix
     */
    def apply (u: Array [Array [$BASE]]): $MATRIX =
    {
        new $MATRIX (u.length, u(0).length, u)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from a sequence/array of vectors 'u'.
     *  @param u           the sequence/array of vectors to assign
     *  @param columnwise  whether the vectors are treated as column or row vectors
     */
    def apply (u: Seq [$VECTO], columnwise: Boolean = true): $MATRIX =
    {
        var x: $MATRIX = null
        val u_dim = u(0).dim
        if (columnwise) {
            x = new $MATRIX (u_dim, u.length)
            for (j <- x.range2) x.setCol (j, u(j))       // assign column vectors
        } else {
            x = new $MATRIX (u.length, u_dim)
            for (i <- x.range1) x(i) = u(i)              // assign row vectors
        } // if
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the Scala `Vector` of vectors 'u'.
     *  Assumes vectors are column-wise.
     *  @param u  the `Vector` of vectors to assign
     */
    def apply (u: Vector [$VECTO]): $MATRIX =
    {
        val u_dim = u(0).dim
        val x = new $MATRIX (u_dim, u.length)
        for (j <- 0 until u.length) x.setCol (j, u(j))        // assign column vectors
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a symmetric matrix from repeated values in a lower triangular form.
     *  @param dim  the (row, column) dimensions
     *  @param u    the repeated values
     */
    def apply (dim: (Int, Int), u: $BASE*): $MATRIX =
    {
        val x = new $MATRIX (dim._1, dim._2)
        var k = 0
        for (i <- 0 until dim._1; j <- 0 to i) {
            x.v(i)(j) = u(k)
            x.v(j)(i) = u(k)
            k += 1
        } // for
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix by reading from a text file, e.g., a CSV file.
     *  @param fileName  the name of file holding the data
     */
    def apply (fileName: String): $MATRIX =
    {
        val lines  = fromFile (fileName).getLines.toArray         // get the lines from file
        val (m, n) = (lines.length, lines(0).split (sp).length)
        val x      = new $MATRIX (m, n)
        for (i <- x.range1) {
            x(i) = $VECTOR (lines(i).split (sp))
            if ((i+1) % PROGRESS == 0) println (s"Read ${i+1} rows ...")
        } // for
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix by reading from a text file, e.g., a CSV file.
     *  @param fileName  the name of file holding the data
     *  @param skip      the initial number of lines to skip
     */
    def apply (fileName: String, skip: Int): $MATRIX =
    {
        val lines  = fromFile (fileName).getLines.toArray         // get the lines from file
        val (m, n) = (lines.length-skip, lines(0).split (sp).length)
        val x      = new $MATRIX (m, n)
        for (i <- x.range1) {
            x(i) = $VECTOR (lines(i+skip).split (sp))
            if ((i+1) % PROGRESS == 0) println (s"Read ${i+1} rows ...")
        } // for
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an 'm-by-n' identity matrix I (ones on main diagonal, zeros elsewhere).
     *  If 'n' is <= 0, set it to 'm' for a square identity matrix.
     *  FIX: store as a diagonal matrix.
     *  @param m  the row dimension of the matrix
     *  @param n  the column dimension of the matrix (defaults to 0 => square matrix)
     */
    def eye (m: Int, n: Int = 0): $MATRIX =
    {
        val nn = if (n <= 0) m else n             // square matrix, if n <= 0
        val mn = if (m <= nn) m else nn           // length of main diagonal
        val c = new $MATRIX (m, nn)
        for (i <- 0 until mn) c.v(i)(i) = $ONE
        c
    } // eye

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vectors 'u' and 'w' to form a matrix with 2 rows.
     *  @param u  the vector to be concatenated as the new first row in matrix
     *  @param w  the vector to be concatenated as the new second row in matrix
     */
    def ++ (u: $VECTO, w: $VECTO): $MATRIX =
    {
        if (u.dim != w.dim) flaw ("++", "vector dimensions do not match")

        val c = new $MATRIX (2, u.dim)
        c(0) = u
        c(1) = w
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vectors 'u' and 'w' to form a matrix with 2 columns.
     *  @param u  the vector to be concatenated as the new first column in matrix
     *  @param w  the vector to be concatenated as the new second column in matrix
     */
    def ++^ (u: $VECTO, w: $VECTO): $MATRIX =
    {
        if (u.dim != w.dim) flaw ("++^", "vector dimensions do not match")

        val c = new $MATRIX (u.dim, 2)
        c.setCol (0, u)
        c.setCol (1, w)
        c
    } // ++^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector 'u' by matrix 'a'.  Treat 'u' as a row vector.
     *  @param u  the vector to multiply by
     *  @param a  the matrix to multiply by (requires 'sameCrossDimensions')
     */
    def times (u: $VECTO, a: $MATRIX): $VECTOR =
    {
        if (u.dim != a.dim1) flaw ("times", "vector * matrix - incompatible cross dimensions")

        val c = new $VECTOR (a.dim2)
        for (j <- a.range2) {
            var sum = $ZERO
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
    def outer (x: $VECTO, y: $VECTO): $MATRIX =
    {
        val c = new $MATRIX (x.dim, y.dim)
        for (i <- 0 until x.dim; j <- 0 until y.dim) c(i, j) = x(i) * y(j)
        c
    } // outer

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from two vectors 'x' and 'y', row-wise.
     *  @param x  the first vector -> row 0
     *  @param y  the second vector -> row 1
     */
    def form_rw (x: $VECTO, y: $VECTO): $MATRIX =
    {
        if (x.dim != y.dim) flaw ("form_rw", "dimensions of x and y must be the same")

        val cols = x.dim
        val c = new $MATRIX (2, cols)
        c(0) = x
        c(1) = y
        c
    } // form_rw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from scalar 'x' and a vector 'y', row-wise.
     *  @param x  the first scalar -> row 0 (repeat scalar)
     *  @param y  the second vector -> row 1
     */
    def form_rw (x: $BASE, y: $VECTO): $MATRIX =
    {
        val cols = y.dim
        val c = new $MATRIX (2, cols)
        for  (j <- 0 until cols) c(0, j) = x
        c(1) = y
        c
    } // form_rw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from a vector 'x' and a scalar 'y', row-wise.
     *  @param x  the first vector -> row 0
     *  @param y  the second scalar -> row 1 (repeat scalar)
     */
    def form_rw (x: $VECTO, y: $BASE): $MATRIX =
    {
        val cols = x.dim
        val c = new $MATRIX (2, cols)
        c(0) = x
        for  (j <- 0 until cols) c(1, j) = y
        c
    } // form_rw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from two vectors 'x' and 'y', column-wise.
     *  @param x  the first vector -> column 0
     *  @param y  the second vector -> column 1
     */
    def form_cw (x: $VECTO, y: $VECTO): $MATRIX =
    {
        if (x.dim != y.dim) flaw ("form_cw", "dimensions of x and y must be the same")

        val rows = x.dim
        val c = new $MATRIX (rows, 2)
        c.setCol(0, x)
        c.setCol(1, y)
        c
    } // form_cw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from a scalar 'x' and a vector 'y', column-wise.
     *  @param x  the first scalar -> column 0 (repeat scalar)
     *  @param y  the second vector -> column 1
     */
    def form_cw (x: $BASE, y: $VECTO): $MATRIX =
    {
        val rows = y.dim
        val c = new $MATRIX (rows, 2)
        for (i <- 0 until rows) c(i, 0) = x
        c.setCol(1, y)
        c
    } // form_cw

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix from a vector 'x' and a scalar 'y', column-wise.
     *  @param x  the first vector -> column 0
     *  @param y  the second scalar -> column 1 (repeat scalar)
     */
    def form_cw (x: $VECTO, y: $BASE): $MATRIX =
    {
        val rows = x.dim
        val c = new $MATRIX (rows, 2)
        c.setCol(0, x)
        for (i <- 0 until rows) c(i, 1) = y
        c
    } // form_cw

} // $MATRIX companion object
"""; val code18 = raw"""


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `${MATRIX}Test` object tests the operations provided by `$MATRIX` class.
 *  > runMain scalation.linalgebra.${MATRIX}Test
 */
object ${MATRIX}Test extends App
{
    for (l <- 1 to 4) {
        println ("\n\tTest $MATRIX on real matrices of dim " + l)
        val x = new $MATRIX (l, l)
        val y = new $MATRIX (l, l)
        x.set (2)
        y.set (3)
        println ("x + y = " + (x + y))
        println ("x - y = " + (x - y))
        println ("x * y = " + (x * y))
        println ("x * 4 = " + (x * 4))
    } // for

    println ("\n\tTest $MATRIX on additional operations")

    val z   = new $MATRIX ((2, 2), 1, 2,
                                   3, 2)
    val t   = new $MATRIX ((3, 3), 1, 2, 3,
                                   4, 3, 2,
                                   1, 3, 1)
    val zz  = new $MATRIX ((3, 3), 3, 1, 0,
                                   1, 4, 2,
                                   0, 2, 5)
    val bz = $VECTOR (5, 3, 6)
    val b  = $VECTOR (8, 7)
    val lu = z.lud_npp

    println ("z            = " + z)
    println ("z.t          = " + z.t)
    println ("z.tip        = " + z.tip); z.tip   // restore back
    println ("z.lud_npp    = " + lu)
    println ("z.solve      = " + z.solve (lu._1, lu._2, b))
    println ("zz.solve     = " + zz.solve (zz.lud_npp, bz))
    println ("z.inverse    = " + z.inverse)
    println ("z.inverse_ip = " + z.inverse_ip ())
    println ("t.inverse    = " + t.inverse)
    println ("t.inverse_ip = " + t.inverse_ip ())
    println ("z.inv * b    = " + z.inverse * b)
    println ("z.det        = " + z.det)
    println ("z            = " + z)
    z *= z                                       // in-place matrix multiplication
    println ("z squared    = " + z)
    val zz1 = zz.selectRows (Array (0, 2))
    val zz2 = zz(VectorI (0, 2))
    zz(0, 0) = -3
    println ("zz.selectRows = " + zz1)
    println ("zz.apply      = " + zz2)

    val w = new $MATRIX ((2, 3), 2,  3, 5, 
                                -4,  2, 3)
    val v = new $MATRIX ((3, 2), 2, -4, 
                                 3,  2,
                                 5,  3)
    println ("w         = " + w)
    println ("v         = " + v)
    println ("w.reduce  = " + w.reduce)

    println ("right:    w.nullspace = " + w.nullspace)
    println ("check right nullspace = " + w * w.nullspace)

    println ("left:   v.t.nullspace = " + v.t.nullspace)
    println ("check left  nullspace = " + $MATRIX.times (v.t.nullspace, v))

    for (row <- z) println ("row = " + row.deep)

    val aa = new $MATRIX ((3, 2), 1, 2,
                                  3, 4,
                                  5, 6)
    val bb = new $MATRIX ((2, 2), 1, 2,
                                  3, 4)
    val cc = new $MATRIX ((3, 2), 3, 4,
                                  5, 6,
                                  7, 8)

    println ("aa         = " + aa)
    println ("bb         = " + bb)
    println ("aa * bb    = " + aa * bb)
    println ("aa dot bz  = " + (aa dot bz))
    println ("aa.t * bz  = " + aa.t * bz)

    println ("aa dot cc  = " + (aa dot cc))
    println ("aa.t * cc  = " + aa.t * cc)

    println ("aa.norm1   = " + aa.norm1)
    println ("aa.normINF = " + aa.normINF)
    println ("aa.normF   = " + aa.normF)

    aa *= bb
    println ("aa *= bb  = " + aa)

    val filename = scalation.DATA_DIR + "bb_matrix.csv"
    bb.write (filename)
    println ("bb_csv = " + $MATRIX (filename))

} // ${MATRIX}Test object

"""

// Ending of string holding code template --------------------------------------

//      println (code); println (code2); println (code3); println (code4)
//      println (code5); println (code6); println (code7); println (code8)
//      println (code9); println (code10); println (code11); println (code12)
//      println (code13); println (code14); println (code15); println (code16)
//      println (code17); println (code18)

        val writer = new PrintWriter (new File (DIR + _l + MATRIX + ".scalaa"))
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
        writer.write (code14)
        writer.write (code15)
        writer.write (code16)
        writer.write (code17)
        writer.write (code18)
        writer.close ()
    } // for

} // BldMatrix object

