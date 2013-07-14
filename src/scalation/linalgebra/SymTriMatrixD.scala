
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  Robert Davis, John Miller
 * @version 1.0
 * @date    Sun Sep 16 14:09:25 EDT 2012
 * @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import math.{abs, max, min}

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SymTriMatrixD class stores and operates on symmetric tridiagonal matrices.
 *  The elements are of type of Double.  A matrix is stored as two vectors:
 *  the diagonal vector and the sub-diagonal vector.
 *  @param d1  the first/row dimension (symmetric => d2 = d1)
 */
class SymTriMatrixD (val d1: Int)
      extends Matrix with Error with Serializable
{
    // Note: implementations for the following methods are from the Matrix trait:
    // foreach, mag, rank, sameDimensions, leDimensions, sameCrossDimensions,
    // isSquare, isSymmetric

    lazy val dim1 = d1
    lazy val dim2 = d1

    /** Size of the sub-diagonal
     */
    private val d1_1 = d1 - 1

    /** Range for the diagonal
     */
    private val range_d = 0 until d1

    /** Range for the sub-diagonal
     */
    private val range_s = 0 until d1_1

    /** Diagonal of the matrix
     */
    private var _dg: VectorD = new VectorD (d1)

    /** Sub-diagonal (also same for sup-diagonal) of the matrix
     */
    private var _sd: VectorD = new VectorD (d1_1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a symmetric tridiagonal matrix with the given diagonal and sub-diagonal.
     *  @param v1  the diagonal vector
     *  @param v2  the sub-diagonal vector
     */
    def this (v1: VectorD, v2: VectorD)
    {
        this (v1.dim)
        for (i <- range_d) _dg(i) = v1(i)
        for (i <- range_s) _sd(i) = v2(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a symmetric tridiagonal matrix from the given matrix.
     *  @param b  the matrix of values to assign
     */
    def this (b: Matrix)
    {
        this (b.dim1)
        for (i <- range_d) _dg(i) = b(i, i)
        for (i <- range_s) _sd(i) = b(i, i+1)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the diagonal of the matrix.
     */
    def dg: VectorD = _dg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the diagonal of the matrix.
     * @param v  the vector to assign to the diagonal
     */
    def dg_ (v: VectorD) { _dg = v }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the sub-diagonal of the matrix.
     */
    def sd: VectorD = _sd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the sub-diagonal of the matrix.
     *  @param v  the vector to assign to the sub-diagonal
     */
    def sd_ (v: VectorD) { _sd = v }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's element at the i,j-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): Double = 
    {
        if      (i == j)     _dg(i)       // on diagonal
        else if (i == j + 1) _sd(j)       // on sub-diagonal (below diagonal)
        else if (i + 1 == j) _sd(i)       // on sup-diagonal (above diagonal)
        else throw new Exception ("apply: element not on tridiagonal")
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's vector at the i-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorD =
    {
        val u = new VectorD (d1)
        u(i)  = _dg(i)
        if (i > 0)    u(i-1) = _sd(i-1)
        if (i < d1_1) u(i+1) = _sd(i)
        u
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range ir and column-wise on range jr.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): SymTriMatrixD = 
    {
        if (ir != jr) flaw ("apply", "requires same ranges to maintain symmetry")
        slice (ir.start, ir.end)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range ir and column-wise at index j.
     *  Ex: u = a(2..4, 3)
     *  @param ir  the row range
     *  @param j   the column index
     */
    def apply (ir: Range, j: Int): VectorD = col(j)(ir)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise at index i and column-wise on range jr.
     *  Ex: u = a(2, 3..5)
     *  @param i   the row index
     *  @param jr  the column range
     */
    def apply (i: Int, jr: Range): VectorD = this(i)(jr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's element at the i,j-th index position to the scalar x.
     *  Only store x if it is non-zero.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: Double)
    {
        if      (i == j)     _dg(i) = x
        else if (i == j + 1) _sd(j) = x
        else if (i + 1 == j) _sd(i) = x
        else flaw ("update", "element not on tridiagonal")
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's row at the i-th index position to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorD)
    {
        _dg(i) = u(i)
        if (i > 0)    _sd(i-1) = u(i-1)
        if (i < d1_1) _sd(i)   = u(i+1)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise on range ir and column-wise on range jr.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: SymTriMatrixD)
    {
        if (ir != jr) flaw ("update", "requires same ranges to maintain symmetry")
        for (i <- ir) {
            _dg(i) = b.dg(i - ir.start)
            if (i > ir.start) _sd(i-1) = b.sd(i - ir.start - 1)
        } // for
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise on range ir and column-wise at index j.
     *  Ex: a(2..4, 3) = u
     *  @param ir  the row range
     *  @param j   the column index
     *  @param u   the vector to assign
     */
    def update (ir: Range, j: Int, u: VectorD) { col(j)(ir) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise at index i and column-wise on range jr.
     *  Ex: a(2, 3..5) = u
     *  @param i   the row index
     *  @param jr  the column range
     *  @param u   the vector to assign
     */
    def update (i: Int, jr: Range, u: VectorD) { this(i)(jr) = u }

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in this matrix to the scalar x.
     *  @param x  the scalar value to assign
     */
    def set (x: Double)
    {
        for (i <- range1) {
            _dg(i) = x
            if (i > 0) _sd(i) = x
        } // for
    } // set

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in this matrix as copies of the values in 2D array u.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [Double]])
    {
        throw new NoSuchMethodException ("values for SymTriMatrixD should be diagonal")
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's ith row starting at column j to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectorD, j: Int = 0)
    {
        throw new NoSuchMethodException ("values for SymTriMatrixD should be diagonal")
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise from to end.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): SymTriMatrixD =
    {
        val c = new SymTriMatrixD (end - from)
        for (i <- c.range1) {
            c._dg(i) = _dg(i + from)
            if (i > 0) c._sd(i - 1) = _sd(i + from - 1)
        } // for
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise r_from to r_end and column-wise c_from to c_end.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): SymTriMatrixD =
    {
        throw new NoSuchMethodException ("SymTriMatrixD must be symmetric")
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix excluding the given row and column.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): SymTriMatrixD =
    {
        throw new NoSuchMethodException ("SymTriMatrixD does not support sliceExclude")
    } // sliceExclude

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from this matrix according a basis.
     *  @param basis  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (basis: Array [Int]): SymTriMatrixD =
    {
        throw new NoSuchMethodException ("SymTriMatrixD does not support selectRows")
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'c' from the matrix, returning it as a vector.
     *  @param c     the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (c: Int, from: Int = 0): VectorD =
    {
        val u = new VectorD (dim1 - from)
        for (i <- (from max c-1) until (dim1 min c+2)) u(i-from) = this(i, c)
        u
    } // col

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'c' of the matrix to a vector.
     *  @param c  the column to set
     *  @param u  the vector to assign to the column
     */
    def setCol (c: Int, u: VectorD)
    {
        _dg(c) = u(c)
        if (c > 0) _sd(c-1) = u(c-1)
    } // setCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from this matrix according a basis.
     *  @param basis  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (basis: Array [Int]): SymTriMatrixD =
    {
        throw new NoSuchMethodException ("SymTriMatrixD does not support selectCol")
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this matrix (rows => columns).  Note, since the matrix is
     *  symmetric, it returns itself.
     */
    def t: SymTriMatrixD = this

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this matrix and vector u.
     *  @param u  the vector to be concatenated as the new last row in matrix
     */
    def ++ (u: VectorD): Matrix =
    {
        throw new NoSuchMethodException ("SymTriMatrixD does not support ++")
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and matrix b.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def + (b: Matrix): Matrix = 
    {
        val trid = b.asInstanceOf [SymTriMatrixD]
	if (d1 == trid.d1) {
            new SymTriMatrixD (_dg + trid.dg, _sd + trid.sd)
        } else {
            flaw ("+", "matrix b has the wrong dimensions")
            null
        } // if
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and scalar x.
     *  @param x  the scalar to add
     */
    def + (x: Double): Matrix = new SymTriMatrixD (_dg + x, _sd + x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and matrix b.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def += (b: Matrix)
    {
        val trid = b.asInstanceOf [SymTriMatrixD]
        if (d1 == trid.d1) {
            _dg += trid.dg
            _sd += trid.sd
        } else {
            flaw ("+=", "matrix b has the wrong dimensions")
        } // if
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and scalar x.
     * @param x  the scalar to add
     */
    def += (x: Double) = { _dg += x; _sd += x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract matrix b.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def - (b: Matrix): Matrix = 
    {
        val trid = b.asInstanceOf [SymTriMatrixD]
        if (d1 == trid.d1) {
            new SymTriMatrixD (_dg - trid.dg, _sd - trid.sd)
        } else {
            flaw ("-", "matrix b has the wrong dimensions")
            null
        } // if
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract scalar x.
     *  @param x  the scalar to subtract
     */
    def - (x: Double): Matrix = new SymTriMatrixD (_dg - x, _sd - x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place matrix b.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def -= (b: Matrix)
    {
        val trid = b.asInstanceOf [SymTriMatrixD]
        if (d1 == trid.d1) {
            _dg -= trid.dg
            _sd -= trid.sd
        } else {
            flaw ("-=", "matrix b has the wrong dimensions")
        } // if
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place scalar x.
     *  @param x  the scalar to subtract
     */
    def -= (x: Double) = { _dg -= x; _sd -= x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by matrix b.
     *  @param b  the matrix to multiply by
     */
    def * (b: Matrix): Matrix = 
    {
        throw new NoSuchMethodException ("matrix multiplication not yet implemented")
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector u.
     *  @param u  the vector to multiply by
     */
    def * (u: VectorD): VectorD = 
    {
        val c = new VectorD (d1)
        c(0)  = u(0) * _dg(0) + _sd(0) * u(1)
        for (i <- 1 until d1_1) {
            c(i) = _sd(i-1) * u(i-1)
            c(i) = c(i) + _dg(i) * u(i)
            c(i) = c(i) + _sd(i+1) * u(i+1)
        } // for
        c(d1-1) = _sd(d1-2) * u(d1-2) + _dg(d1-1) * u(d1-1)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by scalar x.
     *  @param x  the scalar to multiply by
     */
    def * (x: Double): Matrix = new SymTriMatrixD (_dg * x, _sd * x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by matrix b
     *  @param b  the matrix to multiply by
     */
    def *= (b: Matrix)
    {
        throw new NoSuchMethodException ("inplace matrix multiplication not yet implemented")
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by scalar x.
     *  @param x  the scalar to multiply by
     */
    def *= (x: Double) = { _dg *= x; _sd *= x }

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector u to produce another matrix (a_ij * u_j)
     *  @param u  the vector to multiply by
     */
    def ** (u: VectorD): Matrix = 
    {
        throw new NoSuchMethodException ("matrix * vector -> matrix not implemented yet")
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by vector u to produce another matrix (a_ij * u_j)
     *  @param u  the vector to multiply by
     */
    def **= (u: VectorD)
    {
        throw new NoSuchMethodException ("inplace matrix * vector -> matrix not implemented yet")
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this matrix by scalar x.
     *  @param x  the scalar to divide by
     */
    def / (x: Double): Matrix = new SymTriMatrixD (_dg / x, _sd / x)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this matrix by scalar x.
     *  @param x  the scalar to divide by
     */
    def /= (x: Double) = { _dg /= x; _sd /= x }

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise this matrix to the pth power (for some integer p >= 2).
     *  @param p  the power to raise this matrix to
     */
    def ~^ (p: Int): MatrixD =
    {
        throw new NoSuchMethodException ("matrix power function (~^) not implemented")
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): Double = _dg(0 until e).max() max _sd(0 until e).max()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): Double = _dg(0 until e).min() min _sd(0 until e).min()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the kth diagonal of this matrix.  Assumes dim2 >= dim1.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): VectorD =
    {
        if (k == 0) _dg
        else if (abs (k) == 1) _sd
        else { flaw ("getDiag", "nothing stored for diagonal " + k); null }
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the kth diagonal of this matrix to the vector u.  Assumes dim2 >= dim1.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorD, k: Int = 0)
    {
        if (k == 0) _dg = u
        else if (abs (k) == 1) _sd = u
        else flaw ("setDiag", "nothing stored for diagonal " + k)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of this matrix to the scalar x.  Assumes dim2 >= dim1.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: Double) { _dg.set (x) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in matrix at or below the threshold by setting them to zero.
     *  Iterative algorithms give approximate values and if very close to zero,
     *  may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double, relative: Boolean = true): Matrix =
    {
        val s = if (relative) mag else 1.0             // use matrix magnitude or 1
        for (i <- range_d) if (abs (_dg(i)) <= thres * s) _dg(i) = 0.0
        for (i <- range_s) if (abs (_sd(i)) <= thres * s) _sd(i) = 0.0
        this
    } // clean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is rectangular (all rows have the same number
     *  of columns).
     */
    def isRectangular: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of this matrix.
     */
    def det: Double = detHelper (d1 - 1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation a*x = d where a is this matrix
     *  @param d  the constant vector.
     */
    def solve (d: VectorD): VectorD =
    {
        val x = new VectorD (d1)
        val c = new VectorD (d1)
        val a = new VectorD (d1)
        val b = new VectorD (d1)
        for (i <- range_s) { c(i) = _sd(i); a(i) = _sd(i) }
        for (i <- range_d) b(i) = _dg(i)

        c(0) = c(0) / b(0)
        d(0) = d(0) / b(0)
        for (i <- 1 until d1) {
            val id = 1.0 / (b(i) - c(i-1) * a(i))
            c(i)   = c(i) * id
            d(i)   = (d(i) - d(i-1) * a(i)) * id
        } // for
        x(d1-1) = d(d1-1)
        for (i <- d1 - 2 to 0 by -1) x(i) = d(i) - c(i) * x(i+1)
        x
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this symmetric tridiagonal matrix to a string.
     */
    override def toString: String = 
    {
        "\nSymTriMatrixD (\t" + _dg + ", \n\t\t\t" + _sd + ")"
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Helper method for computing the determinant of this matrix.
     *  @param n  the current dimension
     */
    private def detHelper (n: Int): Double =
    {
        if (n == 0)      _dg(0)
        else if (n == 1) _dg(0) * _dg(1) - _sd(0) * _sd(0)
	else             _dg(n) * detHelper (n - 1) - _sd(n-1) * _sd(n-1) * detHelper (n - 2)
    } // detHelper

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.0  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace: VectorD =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce.col(dim2 - 1) * -1.0 ++ 1.0
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace in-place of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.0  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace_ip: VectorD =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce_ip
        col(dim2 - 1) * -1.0 ++ 1.0
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of this matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: Double = _dg.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of this matrix, i.e., the sum of its elements.
     */
    def sum: Double = _dg.sum + _sd.sum + _sd.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of this matrix.
     */
    def sumLower: Double = _sd.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean = _dg.isNonnegative && _sd.isNonnegative

    //--------------------------------------------------------------------------
    // The following methods are not useful for Symmetric Tridiagonal matrices:
    //--------------------------------------------------------------------------

    def lud: Tuple2 [Matrix, Matrix] =
    {
        throw new NoSuchMethodException ("lud not implemented")
    } // lud

    def lud_ip: Tuple2 [Matrix, Matrix] = 
    {
        throw new NoSuchMethodException ("lud_ip not implemented")
    } // lud_ip

    def solve (l: Matrix, u: Matrix, b: VectorD): VectorD = 
    {
        throw new NoSuchMethodException ("solve lu not implemented")
    } // solve

    def solve (lu: Tuple2 [Matrix, Matrix], b: VectorD): VectorD = 
    {
        throw new NoSuchMethodException ("solve lu not implemented")
    } // solve

    def diag (b: Matrix): Matrix = 
    {
        throw new NoSuchMethodException ("diag not implemented")
    } // diag

    def diag (p: Int, q: Int): Matrix = 
    {
        throw new NoSuchMethodException ("diag not implemented")
    } // diag

    def inverse: Matrix = 
    {
        throw new NoSuchMethodException ("inverse: not implemented")
    } // inverse

    def inverse_ip: Matrix = 
    {
        throw new NoSuchMethodException ("inverse_ip not implemented")
    } // inverse_ip

    def reduce: Matrix = 
    {
        throw new NoSuchMethodException ("reduce not implemented")
    } // reduce

    def reduce_ip
    {
        throw new NoSuchMethodException ("reduce_ip not implemented")
    } // reduce_ip

} // SymTriMatrixD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SymTriMatrixD class.
 */
object SymTriMatrixDTest extends App
{
    val a = new SymTriMatrixD (new VectorD (1.0, 2.0, 3.0),
                               new VectorD (4.0, 5.0))

    val b = new SymTriMatrixD (new VectorD (2.0, 3.0, 4.0),
                               new VectorD (5.0, 6.0))

    println ("a     = " + a)
    println ("b     = " + b)
    println ("a.det = " + a.det)	
    println ("a + b = " + (a + b))	

} // SymTriMatrixDTest object

