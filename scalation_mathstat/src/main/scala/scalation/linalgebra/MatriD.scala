
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.linalgebra.bld.BldMatri
 *  @version 1.3
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.math.{abs => ABS}

import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatriD` trait specifies the operations to be defined by the concrete
 *  classes implementing `Double` matrices, i.e.,
 *      `MatrixD`           - dense matrix
 *      `BidMatrixD`        - bidiagonal matrix - useful for computing Singular Values
 *      `SparseMatrixD`     - sparse matrix - majority of elements should be zero
 *      `SymTriMatrixD`     - symmetric triangular matrix - useful for computing Eigenvalues
 *      `par.MatrixD`       - parallel dense matrix
 *      `par.SparseMatrixD` - parallel sparse matrix
 * Some of the classes provide a few custom methods, e.g., methods beginning with "times"
 * or ending with 'npp'.
 *------------------------------------------------------------------------------
 *                  row-wise                 column-wise
 * Prepend:      vector +: matrix         vector +^: matrix  (right associative)
 * Append:       matrix :+ vector         matrix :^+ vector
 * Concatenate:  matrix ++ matrix         matrix ++^ matrix
 */
trait MatriD
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
    protected var fString = "%g,\t"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the 'newFormat'.
     *  @param newFormat  the new format string
     */
    def setFormat (newFormat: String) { fString = newFormat }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' m-by-n matrix.
     */
    def copy (): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an m-by-n matrix with all elements initialized to zero.
     *  @param m  the number of rows
     *  @param n  the number of columns
     */
    def zero (m: Int = dim1, n: Int = dim2): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's element at the 'i,j'-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' matrix's vector at the 'i'-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise on range 'ir' and column-wise on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise on range 'ir' and column-wise at index j.
     *  Ex: u = a(2..4, 3)
     *  @param ir  the row range
     *  @param j   the column index
     */
    def apply (ir: Range, j: Int): VectoD = col(j)(ir)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' matrix row-wise at index 'i' and column-wise on range 'jr'.
     *  Ex: u = a(2, 3..5)
     *  @param i   the row index
     *  @param jr  the column range
     */
    def apply (i: Int, jr: Range): VectoD = this(i)(jr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's element at the 'i,j'-th index position to the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's row at the 'i'-th index position to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectoD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice of 'this' matrix row-wise on range 'ir' and column-wise on
     *  range 'jr' to matrix 'b'.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: MatriD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice of 'this' matrix row-wise on range 'ir' and column-wise at
     *  index 'j' to vector 'u'.
     *  Ex: a(2..4, 3) = u
     *  @param ir  the row range
     *  @param j   the column index
     *  @param u   the vector to assign
     */
    def update (ir: Range, j: Int, u: VectoD) { col(j)(ir) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice of 'this' matrix row-wise at index 'i' and column-wise on range
     *  'jr' to vector 'u'.
     *  Ex: a(2, 3..5) = u
     *  @param i   the row index
     *  @param jr  the column range
     *  @param u   the vector to assign
     */
    def update (i: Int, jr: Range, u: VectoD) { this(i)(jr) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in 'this' matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' matrix as copies of the values in 2D array 'u'.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [Double]])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' matrix's 'i'th row starting a column 'j' to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectoD, j: Int = 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `MatriD` into a `MatriI`.
     */
    def toInt: MatriI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' matrix to a dense matrix.
     */
    def toDense: MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over 'this' matrix row by row applying method 'f'.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Array [Double] => U)
    {
        var i = 0
        while (i < dim1) { f (this(i)().toArray); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'from' to 'end'.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix column-wise 'from' to 'end'.
     *  @param from  the start column of the slice (inclusive)
     *  @param end   the end column of the slice (exclusive)
     */
    def sliceCol (from: Int, end: Int): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix row-wise 'r_from' to 'r_end' and column-wise 'c_from' to 'c_end'.
     *  @param r_from  the start of the row slice (inclusive)
     *  @param r_end   the end of the row slice (exclusive)
     *  @param c_from  the start of the column slice (inclusive)
     *  @param c_end   the end of the column slice (exclusive)
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' matrix excluding the given 'row' and 'column'.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from 'this' matrix according to the given index/basis 'rowIndex'.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' starting 'from' in 'this' matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of 'this' matrix to vector 'u'.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectoD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from 'this' matrix according to the given index/basis 'colIndex'.
     *  Ex:  Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose 'this' matrix (rows => columns).
     */
    def t: MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (row) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (column) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last rows in new matrix
     */
    def ++ (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last columns in new matrix
     */
    def ++^ (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and matrix 'b' for any type extending `MatriD`.
     *  Note, subtypes of `MatriD` should also implement a more efficient version,
     *  e.g., `def + (b: MatrixD): MatrixD`.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def + (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def + (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and matrix 'b' for any type extending `MatriD`.
     *  Note, subtypes of `MatriD` should also implement a more efficient version,
     *  e.g., `def += (b: MatrixD): MatrixD`.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def += (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def += (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract matrix 'b' for any type extending `MatriD`.
     *  Note, subtypes of `MatriD` should also implement a more efficient version,
     *  e.g., `def - (b: MatrixD): MatrixD`.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def - (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def - (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place matrix 'b' for any type extending `MatriD`.
     *  Note, subtypes of `MatriD` should also implement a more efficient version,
     *  e.g., `def -= (b: MatrixD): MatrixD`.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def -= (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def -= (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix and matrix 'b' for any type extending `MatriD`.
     *  Note, subtypes of `MatriD` should also implement a more efficient version,
     *  e.g., `def * (b: MatrixD): MatrixD`.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def * (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u'.
     *  @param u  the vector to multiply by
     */
    def * (u: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix and matrix 'b' for any type extending `MatriD`.
     *  Note, subtypes of `MatriD` should also implement a more efficient version,
     *  e.g., `def *= (b: MatrixD): MatrixD`.
     *  @param b  the matrix to multiply by (requires 'leDimensions')
     */
    def *= (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def *= (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply (row) vector 'u' by 'this' matrix.  Note '*:' is right associative.
     *  vector = vector *: matrix
     *  @param u  the vector to multiply by
     */
    def *: (u: VectoD): VectoD = this.t * u

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def ** (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def **= (u: VectoD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: Double): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' matrix to the 'p'th power (for some integer p >= 2).
     *  @param p  the power to raise 'this' matrix to
     */
    def ~^ (p: Int): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and vector 'u', by first transposing
     *  'this' matrix and then multiplying by 'u' (i.e., 'a dot u = a.t * u').
     *  @param u  the vector to multiply by (requires same first dimensions)
     */
    def dot (u: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and matrix 'b' that results in a vector,
     *  by taking the dot product for each column 'j' of both matrices.
     *  @see www.mathworks.com/help/matlab/ref/dot.html
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def dot (b: MatriD): VectoD
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix dot product of 'this' matrix and matrix 'b', by first transposing
     *  'this' matrix and then multiplying by 'b' (i.e., 'a dot b = a.t * b').
     *  @param b  the matrix to multiply by (requires same first dimensions)
     */
    def mdot (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the magnitude of 'this' matrix, the element value farthest from zero.
     */
    def mag: Double = ABS (max ()) max ABS (min ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap the elements in rows 'i' and 'k' starting from column 'col'.
     *  @param i    the first row in the swap
     *  @param k    the second row in the swap
     *  @param col  the starting column for the swap (default 0 => whole row)
     */
    def swap (i: Int, k: Int, col: Int = 0)
    {
        val a = this; var t = 0.0
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
        val a = this; var t = 0.0
        for (i <- row until dim1) { t = a(i, l); a(i, l) = a(i, j); a(i, j) = t }
    } // swapCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the lower triangular of 'this' matrix (rest are zero).
     */ 
    def lowerT: MatriD 
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the upper triangular of 'this' matrix (rest are zero).
     */ 
    def upperT: MatriD 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud: Tuple2 [MatriD, MatriD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose in-place 'this' matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud_ip (): Tuple2 [MatriD, MatriD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' using back substitution in the equation 'u*x = y' where
     *  'this' matrix ('u') is upper triangular (see 'lud' above).
     *  @param y  the constant vector
     */
    def bsolve (y: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud' above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: MatriD, u: MatriD, b: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud' above).
     *  @param lu  the lower and upper triangular matrices
     *  @param b   the constant vector
     */
    def solve (lu: Tuple2 [MatriD, MatriD], b: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is 'this' matrix.
     *  @param b  the constant vector.
     */
    def solve (b: VectoD): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the rank of 'this' m-by-n matrix by taking the upper triangular
     *  matrix 'u' from the 'LU' Decomposition and counting the number of non-zero
     *  diagonal elements.  Implementing classes may override this method with
     *  a better one (e.g., using 'SVD' or Rank Revealing 'QR').
     *  @see http://en.wikipedia.org/wiki/Rank_%28linear_algebra%29
     *  @param upper  flag indicating whether this matrix is already upper triangular
     */
    def rank (upper: Boolean = false): Int =
    {
        val max   = if (dim1 < dim2) dim1 else dim2      // rank <= min (m, n)
        val u     = if (upper) this else lud._2          // upper triangular matrix
        var count = 0
        for (i <- 0 until max if u(i, i) !=~ 0.0) count += 1
        count
    } // rank

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine 'this' matrix with matrix 'b', placing them along the diagonal and
     *  filling in the bottom left and top right regions with zeros: '[this, b]'.
     *  @param b  the matrix to combine with this matrix
     */
    def diag (b: MatriD): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix '[Ip, this, Iq]' where 'Ir' is a 'r-by-r' identity matrix, by
     *  positioning the three matrices 'Ip', this and 'Iq' along the diagonal.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int = 0): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the 'k'th diagonal of 'this' matrix.  Assumes dim2 >= dim1.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the 'k'th diagonal of 'this' matrix to the vector 'u'.  Assumes dim2 >= dim1.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectoD, k: Int = 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of 'this' matrix to the scalar 'x'.  Assumes dim2 >= dim1.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: Double)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' matrix (requires a 'squareMatrix') and use partial pivoting.
     */
    def inverse: MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place 'this' matrix (requires a 'squareMatrix') and use partial pivoting.
     */
    def inverse_ip (): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on 'this' matrix to make the left part embed an
     *  identity matrix.  A constraint on 'this' m by n matrix is that n >= m.
     */
    def reduce: MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction in-place on 'this' matrix to make the left part
     *  embed an identity matrix.  A constraint on 'this' m by n matrix is that n >= m.
     */ 
    def reduce_ip ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' matrix at or below the threshold 'thres' by setting
     *  them to zero. Iterative algorithms give approximate values and if very close
     *  to zero, may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double, relative: Boolean = true): MatriD

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
    def nullspace: VectoD

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
    def nullspace_ip (): VectoD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of 'this' matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of 'this' matrix, i.e., the sum of its elements.
     */
    def sum: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of 'this' matrix.
     */
    def sumLower: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'abs' sum of 'this' matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the column means of this matrix.
     */
    def mean: VectoD =
    {
        var cm = this(0).zero (dim2)
        for (j <- range2) cm(j) = col (j).sum / dim1.toDouble
        cm
    } // mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 1-norm of 'this' matrix, i.e., the maximum 1-norm of the
     *  column vectors.  This is useful for comparing matrices '(a - b).norm1'.
     */
    def norm1: Double = (for (j <- range2) yield col(j).norm1).max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of 'this' matrix.
     */
    def det: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix and the other matrix 'b' have the same dimensions.
     *  @param b  the other matrix
     */
    def sameDimensions (b: MatriD): Boolean = dim1 == b.dim1 && dim2 == b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix dimensions are less than or equal to 'le' those
     *  of the other matrix 'b'.
     *  @param b  the other matrix
     */
    def leDimensions (b: MatriD): Boolean = dim1 <= b.dim1 && dim2 <= b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix and the other matrix 'b' have the same cross
     *  dimensions.
     *  @param b  the other matrix
     */
    def sameCrossDimensions (b: MatriD): Boolean = dim2 == b.dim1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is bidiagonal (has non-zero elements only in
     *  main diagonal and super-diagonal).  The method may be overriding for
     *  efficiency.
     */
    def isBidiagonal: Boolean =
    {
        for (i <- range1; j <- range2) {
            if (i != j && i != j+1 && this(i, j) !=~ 0.0) return false
        } // for
        true
    } // isBidiagonal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range1; j <- range2 if this(i, j) < 0.0) return false
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
            if (ABS (i-j) > 1 && this(i, j) !=~ 0.0) return false
        } // for
        true
    } // isTridiagonal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' matrix to a CSV-formatted text file with name 'fileName'.
     *  @param fileName  the name of file to hold the data
     */
    def write (fileName: String)

} // MatriD trait

