
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon May 25 17:57:02 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldMatri` object is used to build Matrix traits for various base types.
 *  > runMain scalation.linalgebra.bld.BldMatri
 */
object BldMatri extends App with BldParams
{
    println ("BldMatri: generate code for Matri traits")

    for (k <- kind) {
        val VECTO     = k._1
        val BASE      = k._2
        val FORMAT    = k._5
        val MATRI     = k._6
        val ZERO      = k._8
        val ONE       = k._9
        val BASE_LC   = BASE.toLowerCase
        val MATRIX    = { val m = MATRI.splitAt (MATRI.size-1); m._1 + "x" + m._2 }
        val IMPORT    = if (BASE == "StrNum") "scalation.math.StrO.{abs => ABS, _}"
                        else if (BASE == "TimeNum") "scalation.math.TimeO.{abs => ABS, _}"
                        else if (CUSTOM contains BASE) s"scalation.math.$BASE.{abs => ABS, _}"
                        else "scala.math.{abs => ABS, signum, sqrt}"
        val IMPORT2   = if (BASE == "StrNum") "scalation.math.StrO"
                        else if (BASE == "TimeNum") "scalation.math.TimeO"
                        else if (CUSTOM contains BASE) s"scalation.math.$BASE"
                        else s"scalation.math.${BASE_LC}_exp"

// Beginning of string holding code template -----------------------------------

        val code = raw"""
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.linalgebra.bld.BldMatri
 *  @version 1.6
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import $IMPORT

import $IMPORT2
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `$MATRI` trait specifies the operations to be defined by the concrete
 *  classes implementing `$BASE` matrices, i.e.,
 *      `$MATRIX`           - dense matrix
 *      `Bid$MATRIX`        - bidiagonal matrix - useful for computing Singular Values
 *      `Rle$MATRIX`        - compressed matrix - Run Length Encoding (RLE)
 *      `Sparse$MATRIX`     - sparse matrix - majority of elements should be zero
 *      `SymTri$MATRIX`     - symmetric triangular matrix - useful for computing Eigenvalues
 *      `par.$MATRIX`       - parallel dense matrix
 *      `par.Sparse$MATRIX` - parallel sparse matrix
 * Some of the classes provide a few custom methods, e.g., methods beginning with "times"
 * or ending with 'npp'.
 *------------------------------------------------------------------------------
 *                  row-wise                 column-wise
 * Prepend:      vector +: matrix         vector +^: matrix  (right associative)
 * Append:       matrix :+ vector         matrix :^+ vector
 * Concatenate:  matrix ++ matrix         matrix ++^ matrix
 */
trait $MATRI
      extends Error
{
    /** Matrix dimension 1 (# rows)
     */
    val dim1: Int

    /** Matrix dimension 2 (# columns)
     */
    val dim2: Int

    /** Range for the storage array on dimension 1 (rows)
     */
    val range1 = 0 until dim1

    /** Range for the storage array on dimension 2 (columns)
     */
    val range2 = 0 until dim2

    /** Format string used for printing vector values (change using 'setFormat')
     */
    protected var fString = "$FORMAT,\t"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat'.
     *  @param newFormat  the new format string
     */
    def setFormat (newFormat: String) { fString = newFormat }
"""; val code2 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' m-by-n matrix.
     */
    def copy (): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an m-by-n matrix with all elements initialized to zero.
     *  @param m  the number of rows
     *  @param n  the number of columns
     */
    def zero (m: Int = dim1, n: Int = dim2): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's element at the 'i,j'-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's vector at the 'i'-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise on range 'ir' and column-wise on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise on range 'ir' and column-wise at index j.
     *  Ex: u = a(2..4, 3)
     *  @param ir  the row range
     *  @param j   the column index
     */
    def apply (ir: Range, j: Int): $VECTO = col(j)(ir)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise at index 'i' and column-wise on range 'jr'.
     *  Ex: u = a(2, 3..5)
     *  @param i   the row index
     *  @param jr  the column range
     */
    def apply (i: Int, jr: Range): $VECTO = this(i)(jr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the rows indicated by the index vector 'iv'
     *  FIX - implement in all implementing classes
     *  @param iv  the vector of row indices
     */
    def apply (iv: VectoI): $MATRI = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's element at the 'i,j'-th index position to the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: $BASE)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's row at the 'i'-th index position to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: $VECTO)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice of 'this' matrix row-wise on range 'ir' and column-wise on
     *  range 'jr' to matrix 'b'.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: $MATRI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice of 'this' matrix row-wise on range 'ir' and column-wise at
     *  index 'j' to vector 'u'.
     *  Ex: a(2..4, 3) = u
     *  @param ir  the row range
     *  @param j   the column index
     *  @param u   the vector to assign
     */
    def update (ir: Range, j: Int, u: $VECTO) { col(j)(ir) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice of 'this' matrix row-wise at index 'i' and column-wise on range
     *  'jr' to vector 'u'.
     *  Ex: a(2, 3..5) = u
     *  @param i   the row index
     *  @param jr  the column range
     *  @param u   the vector to assign
     */
    def update (i: Int, jr: Range, u: $VECTO) { this(i)(jr) = u }
"""; val code3 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in 'this' matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: $BASE)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' matrix as copies of the values in 2D array 'u'.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [$BASE]])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' matrix as copies of the values in matrix 'u'.
     *  @param u  the matrix of values to assign
     */
    def set (u: $MATRI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's 'i'th row starting a column 'j' to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: $VECTO, j: Int = 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `$MATRI` into a `MatriI`.
     */
    def toInt: MatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' matrix to a dense matrix.
     */
    def toDense: $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' matrix row by row applying method 'f'.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Array [$BASE] => U)
    {
        var i = 0
        while (i < dim1) { f (this(i)().toArray); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map the elements of 'this' matrix by applying the mapping function 'f'.
     *  FIX - remove ??? and implement in all implementing classes
     *  @param f  the function to apply
     */
    def map (f: $VECTO => $VECTO): $MATRI = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'r_from' to 'r_end' and column-wise 'c_from' to 'c_end'.
     *  @param r_from  the start of the row slice (inclusive)
     *  @param r_end   the end of the row slice (exclusive)
     *  @param c_from  the start of the column slice (inclusive)
     *  @param c_end   the end of the column slice (exclusive)
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'from' to 'end'.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise over the given range 'rg'.
     *  @param rg  the range specifying the slice
     */
    def slice (rg: Range): $MATRI = slice (rg.start, rg.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise excluding the given range 'rg'.
     *  @param rg  the excluded range of the slice
     */
    def sliceEx (rg: Range): $MATRI = slice (0, rg.start) ++ slice (rg.end, dim1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix excluding the given 'row' and 'column'.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceEx (row: Int, col: Int): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix column-wise 'from' to 'end'.
     *  @param from  the start column of the slice (inclusive)
     *  @param end   the end column of the slice (exclusive)
     */
    def sliceCol (from: Int, end: Int): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from 'this' matrix according to the given index/basis 'rowIndex'.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): $MATRI
"""; val code4 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select all rows from 'this' matrix excluding the rows from the given 'rowIndex'.
     *  @param rowIndex  the row indices to exclude
     */
    def selectRowsEx (rowIndex: Array [Int]): $MATRI = selectRows ((range1 diff rowIndex).toArray)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select all rows from 'this' matrix excluding the rows from the given 'rowIndex'.
     *  @param rowIndex  the row indices to exclude
     */
    def selectRowsEx (rowIndex: VectoI): $MATRI = selectRows ((range1 diff rowIndex()).toArray)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' starting 'from' in 'this' matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of 'this' matrix to vector 'u'.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: $VECTO)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from 'this' matrix according to the given index/basis 'colIndex'.
     *  Ex:  Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose 'this' matrix (rows => columns).
     */
    def t: $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (row) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (column) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last rows in new matrix
     */
    def ++ (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last columns in new matrix
     */
    def ++^ (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and matrix 'b' for any type extending `$MATRI`.
     *  Note, subtypes of `$MATRI` should also implement a more efficient version,
     *  e.g., `def + (b: $MATRIX): $MATRIX`.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def + (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def + (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and matrix 'b' for any type extending `$MATRI`.
     *  Note, subtypes of `$MATRI` should also implement a more efficient version,
     *  e.g., `def += (b: $MATRIX): $MATRIX`.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def += (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def += (u: $VECTO): $MATRI
"""; val code5 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract matrix 'b' for any type extending `$MATRI`.
     *  Note, subtypes of `$MATRI` should also implement a more efficient version,
     *  e.g., `def - (b: $MATRIX): $MATRIX`.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def - (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def - (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place matrix 'b' for any type extending `$MATRI`.
     *  Note, subtypes of `$MATRI` should also implement a more efficient version,
     *  e.g., `def -= (b: $MATRIX): $MATRIX`.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def -= (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def -= (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix and matrix 'b' for any type extending `$MATRI`.
     *  Note, subtypes of `$MATRI` should also implement a more efficient version,
     *  e.g., `def * (b: $MATRIX): $MATRIX`.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def * (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u'.
     *  @param u  the vector to multiply by
     */
    def * (u: $VECTO): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix and matrix 'b' for any type extending `$MATRI`.
     *  Note, subtypes of `$MATRI` should also implement a more efficient version,
     *  e.g., `def *= (b: $MATRIX): $MATRIX`.
     *  @param b  the matrix to multiply by (requires 'leDimensions')
     */
    def *= (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def *= (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply (row) vector 'u' by 'this' matrix.  Note '*:' is right associative.
     *  vector = vector *: matrix
     *  @param u  the vector to multiply by
     */
    def *: (u: $VECTO): $VECTO = this.t * u

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def ** (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by matrix 'b' elementwise (Hadamard product).
     *  @see en.wikipedia.org/wiki/Hadamard_product_(matrices)
     *  FIX - remove ??? and implement in all implementing classes
     *  @param b  the matrix to multiply by
     */
    def ** (b: $MATRI): $MATRI = ???

"""; val code6 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def **= (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector 'u' by 'this' matrix to produce another matrix 'u_i * a_ij'.
     *  E.g., multiply a diagonal matrix represented as a vector by a matrix.
     *  This operator is right associative.
     *  @param u  the vector to multiply by
     */
    def **: (u: $VECTO): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: $BASE): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' matrix to the 'p'th power (for some integer p >= 2).
     *  @param p  the power to raise 'this' matrix to
     */
    def ~^ (p: Int): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and vector 'u', by first transposing
     *  'this' matrix and then multiplying by 'u' (i.e., 'a dot u = a.t * u').
     *  @param u  the vector to multiply by (requires same first dimensions)
     */
    def dot (u: $VECTO): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and matrix 'b' that results in a vector,
     *  by taking the dot product for each column 'j' of both matrices.
     *  @see www.mathworks.com/help/matlab/ref/dot.html
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def dot (b: $MATRI): $VECTO
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix dot product of 'this' matrix and matrix 'b', by first transposing
     *  'this' matrix and then multiplying by 'b' (i.e., 'a dot b = a.t * b').
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def mdot (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the magnitude of 'this' matrix, the element value farthest from zero.
     */
    def mag: $BASE = ABS (max ()) max ABS (min ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap the elements in rows 'i' and 'k' starting from column 'col'.
     *  @param i    the first row in the swap
     *  @param k    the second row in the swap
     *  @param col  the starting column for the swap (default 0 => whole row)
     */
    def swap (i: Int, k: Int, col: Int = 0)
    {
        val a = this; var t = $ZERO
        for (j <- col until dim2) { t = a(k, j); a(k, j) = a(i, j); a(i, j) = t }
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap the elements in columns 'j' and 'l' starting from row 'row'.
     *  @param j    the first column in the swap
     *  @param l    the second column in the swap
     *  @param row  the starting row for the swap (default 0 => whole column)
     */
    def swapCol (j: Int, l: Int, row: Int = 0)
    {
        val a = this; var t = $ZERO
        for (i <- row until dim1) { t = a(i, l); a(i, l) = a(i, j); a(i, j) = t }
    } // swapCol
"""; val code7 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the lower triangular of 'this' matrix (rest are zero).
     */ 
    def lowerT: $MATRI 
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the upper triangular of 'this' matrix (rest are zero).
     */ 
    def upperT: $MATRI 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Decomposition algorithm.
     */
    def lud_npp: ($MATRI, $MATRI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor in-place 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Decomposition algorithm.
     */
    def lud_ip (): ($MATRI, $MATRI)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' using back substitution in the equation 'u*x = y' where
     *  'this' matrix ('u') is upper triangular (see 'lud' above).
     *  @param y  the constant vector
     */
    def bsolve (y: $VECTO): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud' above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: $MATRI, u: $MATRI, b: $VECTO): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud' above).
     *  @param lu  the lower and upper triangular matrices
     *  @param b   the constant vector
     */
    def solve (lu: ($MATRI, $MATRI), b: $VECTO): $VECTO = solve (lu._1, lu._2, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is 'this' matrix.
     *  @param b  the constant vector.
     */
    def solve (b: $VECTO): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine 'this' matrix with matrix 'b', placing them along the diagonal and
     *  filling in the bottom left and top right regions with zeros: '[this, b]'.
     *  @param b  the matrix to combine with this matrix
     */
    def diag (b: $MATRI): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix '[Ip, this, Iq]' where 'Ir' is a 'r-by-r' identity matrix, by
     *  positioning the three matrices 'Ip', this and 'Iq' along the diagonal.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int = 0): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the 'k'th diagonal of 'this' matrix.  Assumes dim2 >= dim1.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the 'k'th diagonal of 'this' matrix to the vector 'u'.  Assumes dim2 >= dim1.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: $VECTO, k: Int = 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of 'this' matrix to the scalar 'x'.  Assumes dim2 >= dim1.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: $BASE)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' matrix (requires a 'squareMatrix') and use partial pivoting.
     */
    def inverse: $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place 'this' matrix (requires a 'squareMatrix') and use partial pivoting.
     */
    def inverse_ip (): $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on 'this' matrix to make the left part embed an
     *  identity matrix.  A constraint on 'this' m by n matrix is that n >= m.
     */
    def reduce: $MATRI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction in-place on 'this' matrix to make the left part
     *  embed an identity matrix.  A constraint on 'this' m by n matrix is that n >= m.
     */ 
    def reduce_ip ()
"""; val code8 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' matrix at or below the threshold 'thres' by setting
     *  them to zero. Iterative algorithms give approximate values and if very close
     *  to zero, may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double, relative: Boolean = true): $MATRI

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
    def nullspace: $VECTO

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
    def nullspace_ip (): $VECTO

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of 'this' matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of 'this' matrix, i.e., the sum of its elements.
     */
    def sum: $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of 'this' matrix.
     */
    def sumLower: $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'abs' sum of 'this' matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column means of 'this' matrix.
     */
    def mean: $VECTO =
    {
        val cm = this(0).zero (dim2)
        for (j <- range2) cm(j) = col (j).sum / dim1.to$BASE
        cm
    } // mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column means of 'this' matrix ignoring zero elements (e.g.,
     *  a zero may indicate a missing value as in recommender systems).
     */
    def meanNZ: $VECTO =
    {
        val cm = this(0).zero (dim2)
        for (j <- range2) {
            val nzs = dim1 - col (j).countZero
            cm(j) = if (nzs > 0) col (j).sum / nzs.to$BASE else $ZERO
        } // for
        cm
    } // meanNZ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the row means of 'this' matrix.
     */
    def meanR: $VECTO =
    {
        val rm = this(0).zero (dim1)
        for (i <- range1) rm(i) = this(i).sum / dim2.to$BASE
        rm
    } // meanR
"""; val code9 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the row means of 'this' matrix ignoring zero elements (e.g.,
     *  a zero may indicate a missing value as in recommender systems).
     */
    def meanRNZ: $VECTO =
    {
        val rm = this(0).zero (dim1)
        for (i <- range1) {
            val nzs = dim2 - this(i).countZero
            rm(i) = if (nzs > 0) this(i).sum / nzs.to$BASE else $ZERO
        } // for
        rm
    } // meanRNZ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 1-norm of 'this' matrix, i.e., the maximum 1-norm of the
     *  column vectors.  This is useful for comparing matrices '(a - b).norm1'.
     *  @see en.wikipedia.org/wiki/Matrix_norm
     */
    def norm1: $BASE = (for (j <- range2) yield col(j).norm1).max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (infinity) INF-norm of 'this' matrix, i.e., the maximum 1-norm
     *  of the row vectors.
     *  @see en.wikipedia.org/wiki/Matrix_norm
     */
    def normINF: $BASE = (for (i <- range1) yield this(i).norm1).max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Compute the sqaure of the Frobenius-norm of 'this' matrix, i.e.,
      *  the sum of the squared values over all the elements (sse).
      *  FIX: for `MatriC` should take absolute values, first.
      *  @see en.wikipedia.org/wiki/Matrix_norm#Frobenius_norm
      */
     def normFSq: $BASE =
     {
         var sum = $ZERO
         for (i <- range1) sum += this(i).normSq
         sum
     } // normFSq

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Compute the Frobenius-norm of 'this' matrix, i.e., the square root of 
      *  the sum of the squared values over all the elements (sqrt (sse)).
      *  FIX: for `MatriC` should take absolute values, first.
      *  @see en.wikipedia.org/wiki/Matrix_norm#Frobenius_norm
      */
     def normF: $BASE =
     {
         var sum = $ZERO
         for (i <- range1) sum += this(i).normSq
         sqrt (sum).to$BASE
     } // normF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of 'this' matrix.
     */
    def det: $BASE

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix and the other matrix 'b' have the same dimensions.
     *  @param b  the other matrix
     */
    def sameDimensions (b: $MATRI): Boolean = dim1 == b.dim1 && dim2 == b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix dimensions are less than or equal to 'le' those
     *  of the other matrix 'b'.
     *  @param b  the other matrix
     */
    def leDimensions (b: $MATRI): Boolean = dim1 <= b.dim1 && dim2 <= b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix and the other matrix 'b' have the same cross
     *  dimensions.
     *  @param b  the other matrix
     */
    def sameCrossDimensions (b: $MATRI): Boolean = dim2 == b.dim1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is bidiagonal (has non-zero elements only in
     *  main diagonal and super-diagonal).  The method may be overriding for
     *  efficiency.
     */
    def isBidiagonal: Boolean =
    {
        for (i <- range1; j <- range2) {
            if (i != j && i != j+1 && this(i, j) !=~ $ZERO) return false
        } // for
        true
    } // isBidiagonal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range1; j <- range2 if this(i, j) < $ZERO) return false
        true
    } // isNonegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is rectangular (all rows have the same number
     *  of columns).
     */
    def isRectangular: Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is square (same row and column dimensions).
     */
    def isSquare: Boolean = dim1 == dim2 && isRectangular
"""; val code10 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is symmetric.
     */
    def isSymmetric: Boolean =
    {
        for (i <- 0 to dim1 - 2; j <- i + 1 until dim2) {
            if (this(i, j) !=~ this(j, i)) return false
        } // for
        true
    } // isSymmetric

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is bidiagonal (has non-zero elements only in
     *  main diagonal and super-diagonal).  The method may be overriding for
     *  efficiency.
     */
    def isTridiagonal: Boolean =
    {
        for (i <- range1; j <- range2) {
            if (ABS (i-j) > 1 && this(i, j) !=~ $ZERO) return false
        } // for
        true
    } // isTridiagonal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' matrix to a CSV-formatted text file with name 'fileName'.
     *  @param fileName  the name of file to hold the data
     */
    def write (fileName: String)

} // $MATRI trait

"""

// Ending of string holding code template --------------------------------------

//      println (code); println (code2); println (code3); println (code4)
//      println (code5); println (code6); println (code7); println (code8)
//      println (code9); println (code10)

        val writer = new PrintWriter (new File (DIR + _l + MATRI + ".scalaa"))
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
        writer.close ()
    } // for

} // BldMatri object

