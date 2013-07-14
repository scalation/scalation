
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat May 22 12:57:45 EDT 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra_gen

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Matrix trait specifies the operations to be defined by three concrete
 *  implemeting classes: MatrixN, SparseMatrixN and SymTriMatrix.
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
    /** Get this matrix's element at the i,j-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's vector at the i-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's element at the i,j-th index position to the scalar x.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's row at the i-th index position to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorN [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in this matrix to the scalar x.
     *  @param x  the scalar value to assign
     */
    def set (x: T) { for (i <- range1; j <- range2) this(i, j) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the lower triangular elements in this matrix to the scalar x.
     *  @param x  the scalar value to assign
     */
    def setLower (x: T) { for (i <- range1; j <- 0 until i) this(i, j) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in this matrix as copies of the values in 2D array u.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [T]]) { for (i <- range1; j <- range2) this(i, j) = u(i)(j) }

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
    /** Get row '_row' from the matrix, returning it as a vector.
     *  @param _row  the row to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def row (_row: Int, from: Int = 0): VectorN [T]
/*
    {
        val c = new VectorN [T] (dim2 - from)
        for (j <- from until dim2) c(j - from) = this(_row, j)
        c
    } // row
*/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set row 'row' of the matrix to a vector.
     *  @param row  the column to set
     *  @param u    the vector to assign to the column
     */
    def setRow (row: Int, u: VectorN [T]) { for (j <- range2) this(row, j) = u(j) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column '_col' from the matrix, returning it as a vector.
     *  @param _col  the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (_col: Int, from: Int = 0): VectorN [T]
/*
    {
        val c = new VectorN [T] (dim1 - from)
        for (i <- from until dim1) c(i - from) = this(i, _col)
        c
    } // col
*/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of the matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectorN [T]) { for (i <- range1) this(i, col) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise from to end.
     *  @param from  the start of the slice
     *  @param end   the end of the slice
     */
    def slice (from: Int, end: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise r_from to r_end and column-wise c_from to c_end.
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
    /** Return 1 if the condition is true else 0
     *  @param cond  the condition to evaluate
     */
    def oneIf (cond: Boolean): Int = if (cond) 1 else 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this matrix (rows => columns).
     */
    def t: Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and scalar s.
     *  @param s  the scalar to add
     */
    def += (s: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place scalar s.
     *  @param s  the scalar to subtract
     */
    def -= (s: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector b.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorN [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: T): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by scalar s.
     *  @param s  the scalar to multiply by
     */
    def *= (s: T)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector b to produce another matrix (a_ij * b_j)
     *  @param b  the vector to multiply by
     */
    def ** (b: VectorN [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by vector b to produce another matrix (a_ij * b_j)
     *  @param b  the vector to multiply by
     */
    def **= (b: VectorN [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose this matrix into the product of lower and upper triangular
     *  matrices (l, u) using the LU Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud (implicit fr: Fractional [T]): Tuple2 [Matrix [T], Matrix [T]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose in-place this matrix into the product of lower and upper triangular
     *  matrices (l, u) using the LU Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud_ip (implicit fr: Fractional [T]): Tuple2 [Matrix [T], Matrix [T]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation l*u*x = b (see lud above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: Matrix [T], u: Matrix [T], b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation l*u*x = b (see lud above).
     *  @param lu  the lower and upper triangular matrices
     *  @param b   the constant vector
     */
    def solve (lu: Tuple2 [Matrix [T], Matrix [T]], b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation a*x = b where a is this matrix (see lud above).
     *  @param b  the constant vector.
     */
    def solve (b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine the rank of this m by n matrix by taking the upper triangular
     *  matrix from the LU Decomposition and counting the number of non-zero
     *  diagonal elements.
     */
    def rank (implicit fr: Fractional [T]): Double =
    {
        val max   = if (dim1 < dim2) dim1 else dim2   // rank <= min (m, n)
        val u     = lud._2                            // upper triangular matrix
        var count = 0
        for (i <- 0 until max if this(i, i) != 0.0) count += 1
        count
    } // rank

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix [Ip, this, Iq] where Ir is a r by r identity matrix, by
     *  positioning the three matrices Ip, this and Iq along the diagonal.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the main diagonal of this matrix.  Assumes dim2 >= dim1.
     */
    def getDiag (): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of this matrix to the scalar value s.  Assumes dim2 >= dim1.
     */
    def setDiag (s: T) { for (i <- range1) this(i, i) = s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a squareMatrix) and do not use partial pivoting.
     */
    def inverse_npp (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a squareMatrix) and use partial pivoting.
     */
    def inverse (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place this matrix (requires a squareMatrix) and use partial pivoting.
     */
    def inverse_ip (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on this matrix to make the left part embed an
     *  identity matrix.  A constraint on this m by n matrix is that n >= m.
     */
    def reduce (implicit fr: Fractional [T]): Matrix [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction in-place on this matrix to make the left part
     *  embed an identity matrix.  A constraint on this m by n matrix is that n >= m.
     */ 
    def reduce_ip (implicit fr: Fractional [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.0  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace (implicit fr: Fractional [T]): VectorN [T]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace in-place of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.0  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
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
    /** Compute the determinant of this matrix.
     */
    def det: T

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix and the other Matrix have the same dimensions.
     *  @param b  the other matrix
     */
    def sameDimensions (b: Matrix [T]): Boolean = dim1 == b.dim1 && dim2 == b.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix dimensions are less than or equal to (le) those
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

