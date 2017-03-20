
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat May 22 12:57:45 EDT 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.gen

import scalation.math.ExtremeD.MIN_NORMAL
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Matrix` trait specifies the operations to be defined by three concrete
 *  implementing classes: `MatrixN`, `SparseMatrixN` and `SymTriMatrix`.
 */
trait Matrix [T]
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
    protected val range1 = 0 until dim1

    /** Range for the storage array on dimension 2 (columns)
     */
    protected val range2 = 0 until dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's element at the 'i,j'-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's vector at the 'i'th index position ('i'th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range 'ir' and column-wise on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range 'ir' and column-wise at index 'j'.
     *  Ex: u = a(2..4, 3)
     *  @param ir  the row range
     *  @param j   the column index
     */
    def apply (ir: Range, j: Int): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise at index 'i' and column-wise on range 'jr'.
     *  Ex: u = a(2, 3..5)
     *  @param i   the row index
     *  @param jr  the column range
     */
    def apply (i: Int, jr: Range): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's element at the 'i,j'-th index position to the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's row at the 'i'th index position to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorN [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise on range 'ir' and column-wise at index 'j'.
     *  Ex: a(2..4, 3) = u
     *  @param ir  the row range
     *  @param j   the column index
     *  @param u   the vector to assign
     */
    def update (ir: Range, j: Int, u: VectorN [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise at index 'i' and column-wise on range 'jr'.
     *  Ex: a(2, 3..5) = u
     *  @param i   the row index
     *  @param jr  the column range
     *  @param u   the vector to assign
     */
    def update (i: Int, jr: Range, u: VectorN [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in this matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in this matrix as copies of the values in 2D array 'u'.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [T]])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's 'i'th row starting a column 'j' to the vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectorN [T], j: Int = 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iterate over the matrix row by row.
     *  @param f  the function to apply
     */
    def foreach [U] (f: Array [T] => U)
    {
        var i = 0
        while (i < dim1) { f (this(i)()); i += 1 }
    } // foreach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise 'from' to 'end'.
     *  @param from  the start of the slice
     *  @param end   the end of the slice
     */
    def slice (from: Int, end: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise 'r_from' to 'r_end' and column-wise 'c_from' to 'c_end'.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix excluding the given row and column.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from this matrix according the given index.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from the matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of the matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectorN [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from this matrix according the given index.
     *  Ex:  Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this matrix (rows => columns).
     */
    def t: Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this matrix and vector 'u'.
     *  @param u  the vector to be concatenated as the new last row in matrix
     */
    def ++ (u: VectorN [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector 'u'.
     *  @param u  the vector to multiply by
     */
    def * (u: VectorN [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def *= (x: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def ** (u: VectorN [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  @param u  the vector to multiply by
     */
    def **= (u: VectorN [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: T) (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: T) (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise this matrix to the 'p'th power (for some integer 'p >= 2)'.
     *  Caveat: should be replace by a divide and conquer algorithm.
     *  @param p  the power to raise this matrix to
     */
    def ~^ (p: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the magnitude of this matrix, the element value farthest from zero.
     */
    def mag: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose this matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud (implicit fr: Fractional [T]): Tuple2 [Matrix [T], Matrix [T]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose in-place this matrix into the product of lower and upper triangular
     *  matrices '(l, u)' using the 'LU' Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud_ip (implicit fr: Fractional [T]): Tuple2 [Matrix [T], Matrix [T]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud' above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: Matrix [T], u: Matrix [T], b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'l*u*x = b' (see 'lud' above).
     *  @param lu  the lower and upper triangular matrices
     *  @param b   the constant vector
     */
    def solve (lu: Tuple2 [Matrix [T], Matrix [T]], b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is this matrix (see 'lud' above).
     *  @param b  the constant vector.
     */
    def solve (b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the rank of this 'm-by-n' matrix by taking the upper triangular
     *  matrix from the 'LU' Decomposition and counting the number of non-zero
     *  diagonal elements.  FIX:  should implement in implementing classes.
     */
    def rank (implicit fr: Fractional [T]): Int =
    {
        val max   = if (dim1 < dim2) dim1 else dim2   // rank <= min (m, n)
        val u     = lud._2                            // upper triangular matrix
        var count = 0
        for (i <- 0 until max if u(i, i) != 0.0) count += 1    // FIX: use =~
        count
    } // rank

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix '[Ip, this, Iq]' where 'Ir' is a 'r-by-r' identity matrix, by
     *  positioning the three matrices 'Ip', this and 'Iq' along the diagonal.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the 'k'th diagonal of this matrix.  Assumes 'dim2 >= dim1'.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the 'k'th diagonal of this matrix to the vector 'u'.  Assumes 'dim2 >= dim1'.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorN [T], k: Int = 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of this matrix to the scalar 'x'.  Assumes 'dim2 >= dim1'.
     */
    def setDiag (x: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a 'squareMatrix') not using partial pivoting.
     */
    def inverse_npp (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a 'squareMatrix') using partial pivoting.
     */
    def inverse (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place this matrix (requires a 'squareMatrix') using partial pivoting.
     */
    def inverse_ip (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on this matrix to make the left part embed an
     *  identity matrix.  A constraint on this 'm-by-n' matrix is that 'n >= m'.
     */
    def reduce (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction in-place on this matrix to make the left part
     *  embed an identity matrix.  A constraint on this 'm-by-n' matrix is that 'n >= m'.
     */ 
    def reduce_ip (implicit fr: Fractional [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in matrix at or below the threshold by setting them to zero.
     *  Iterative algorithms give approximate values and if very close to zero,
     *  may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: T, relative: Boolean = true) (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of this 'm-by-n' matrix (requires 'n = m + 1')
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix 'a' is "this vector 'v'
     *  times any scalar 's'", i.e., 'a*(v*s) = 0.'  The left nullspace of matrix 'a' is
     *  the same as the right nullspace of 'a.t' ('a' transpose).
     */
    def nullspace (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace in-place of this 'm-by-n' matrix (requires 'n = m + 1')
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix 'a' is "this vector 'v'
     *  times any scalar 's'", i.e., 'a*(v*s) = 0'.  The left nullspace of matrix 'a' is
     *  the same as the right nullspace of 'a.t' ('a' transpose).
     */
    def nullspace_ip (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of this matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of this matrix, i.e., the sum of its elements.
     */
    def sum: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of this matrix.
     */
    def sumLower: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'abs' sum of this matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 1-norm of this matrix, i.e., the maximum 1-norm of the
     *  column vectors.  This is useful for comparing matrices '(a - b).norm1'.
     */
    def norm1: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of this matrix.
     */
    def det: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix and the other Matrix have the same dimensions.
     *  @param b  the other matrix
     */
    def sameDimensions (b: Matrix [T]): Boolean = dim1 == b.dim1 && dim2 == b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix dimensions are less than or equal to 'le' those
     *  of the other Matrix.
     *  @param b  the other matrix
     */
    def leDimensions (b: Matrix [T]): Boolean = dim1 <= b.dim1 && dim2 <= b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix and the other matrix have the same cross dimensions.
     *  @param b  the other matrix
     */
    def sameCrossDimensions (b: Matrix [T]): Boolean = dim2 == b.dim1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is rectangular (all rows have the same number
     *  of columns).
     */
    def isRectangular: Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is square (same row and column dimensions).
     */
    def isSquare: Boolean = dim1 == dim2 && isRectangular

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is symmetric.
     */
    def isSymmetric: Boolean =
    {
        for (i <- 0 to dim1 - 2; j <- i + 1 until dim2 if this(i, j) != this(j, i)) return false
        true
    } // isSymmetric

} // Matrix class

