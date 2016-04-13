
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @builder scalation.linalgebra.bld.BldBidMatrix
 *  @version 1.2
 *  @date    Mon May 19 15:52:24 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import io.Source.fromFile

import math.{abs => ABS}

import scalation.math.{int_exp, oneIf}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BidMatrixI` class stores and operates on square (upper) bidiagonal matrices.
 *  The elements are of type of `Int`.  A matrix is stored as two vectors:
 *  the diagonal vector and the sup-diagonal vector.
 *  @param d1  the first/row dimension (square => d2 = d1)
 */
class BidMatrixI (val d1: Int)
      extends MatriI with Error with Serializable
{
    /** Dimension 1
     */
    lazy val dim1 = d1

    /** Dimension 2
     */
    lazy val dim2 = d1

    /** Size of the sup-diagonal
     */
    private val n = d1 - 1

    /** Range for the diagonal
     */
    private val range_d = 0 until d1

    /** Range for the sup-diagonal
     */
    private val range_s = 0 until n

    /** Diagonal of the matrix
     */
    private var _dg: VectorI = new VectorI (d1)

    /** Sup-diagonal of the matrix
     */
    private var _sd: VectorI = new VectorI (n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a bidiagonal matrix with the given diagonal and sup-diagonal.
     *  @param v1  the diagonal vector
     *  @param v2  the sup-diagonal vector
     */
    def this (v1: VectorI, v2: VectorI)
    {
        this (v1.dim)
        for (i <- range_d) _dg(i) = v1(i)
        for (i <- range_s) _sd(i) = v2(i)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a bidiagonal matrix from the given matrix.
     *  @param b  the matrix of values to assign
     */
    def this (b: MatriI)
    {
        this (b.dim1)
        for (i <- range_d) _dg(i) = b(i, i)
        for (i <- range_s) _sd(i) = b(i, i+1)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the diagonal of 'this' bidiagonal matrix.
     */
    def dg: VectorI = _dg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the diagonal of 'this' bidiagonal matrix.
     * @param v  the vector to assign to the diagonal
     */
    def dg_ (v: VectorI) { _dg = v }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the sup-diagonal of this bidiagonal matrix.
     */
    def sd: VectorI = _sd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the sup-diagonal of 'this' bidiagonal matrix.
     *  @param v  the vector to assign to the sup-diagonal
     */
    def sd_ (v: VectorI) { _sd = v }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' bidiagonal matrix's element at the 'i,j'-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): Int = 
    {
        if      (i == j)     _dg(i)       // on diagonal
        else if (i + 1 == j) _sd(i)       // on sup-diagonal (above diagonal)
        else throw new Exception ("BidMatrixI.apply: element not on diagonals")
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' bidiagonal matrix's element at the 'i,j'-th index position,
     *  returning 0, if off bidiagonal.
     *  @param i  the row index
     *  @param j  the column index
     */
    def at (i: Int, j: Int): Int =
    {
        if (i < 0 || j < 0 || i >= d1 || j >= d1) 0
        else if (i == j)     _dg(i)       // on diagonal
        else if (i + 1 == j) _sd(i)       // on sup-diagonal (above diagonal)
        else 0
    } // at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' bidiagonal matrix's vector at the 'i'-th index position ('i'-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorI =
    {
        val u = new VectorI (d1)
        u(i)  = _dg(i)
        if (i > 0) u(i-1) = _sd(i-1)
        if (i < n) u(i+1) = _sd(i)
        u
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' bidiagonal matrix row-wise on range 'ir' and column-wise
     *  on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): BidMatrixI = 
    {
        if (ir != jr) flaw ("apply", "requires same ranges to maintain squareness")
        slice (ir.start, ir.end)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' bidiagonal matrix's element at the 'i,j'-th index position to
     *  the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: Int)
    {
        if      (i == j)     _dg(i) = x
        else if (i == j + 1) _sd(j) = x
        else if (i + 1 == j) _sd(i) = x
        else flaw ("update", "element not on bidiagonal")
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' bidiagonal matrix's row at the 'i'-th index position to the
     *  vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorI)
    {
        _dg(i) = u(i)
        if (i > 0) _sd(i-1) = u(i-1)
        if (i < n) _sd(i)   = u(i+1)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice 'this' bidiagonal matrix row-wise on range 'ir' and column-wise
     *  on range 'jr'.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: MatriI)
    {
        if (ir != jr) flaw ("update", "requires same ranges to maintain squareness")
        if (b.isInstanceOf [BidMatrixI]) {
            val bb = b.asInstanceOf [BidMatrixI]
            for (i <- ir) {
                _dg(i) = bb.dg(i - ir.start)
                if (i > ir.start) _sd(i-1) = bb.sd(i - ir.start - 1)
            } // for
        } else {
            flaw ("update", "must convert b to a BidMatrixI first")
        } // if
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in 'this' bidiagonal matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: Int)
    {
        for (i <- range1) {
            _dg(i) = x
            if (i > 0) _sd(i) = x
        } // for
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in 'this' bidiagonal matrix as copies of the values in 2D array u.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [Int]])
    {
        throw new NoSuchMethodException ("values for BidMatrixI should be diagonal")
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' bidiagonal matrix's 'i'th row starting at column 'j' to the
     *  vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectorI, j: Int = 0)
    {
        if (i >= j)   _dg(i) = u(i)
        if (i-1 >= j) _sd(i-1) = u(i+1)
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' bidiagonal matrix row-wise 'from' to 'end'.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): BidMatrixI =
    {
        val c = new BidMatrixI (end - from)
        for (i <- c.range1) {
            c._dg(i) = _dg(i + from)
            if (i > 0) c._sd(i - 1) = _sd(i + from - 1)
        } // for
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' bidiagonal matrix column-wise 'from' to 'end'.
     *  @param from  the start column of the slice (inclusive)
     *  @param end   the end column of the slice (exclusive)
     */
    def sliceCol (from: Int, end: Int): BidMatrixI =
    {
        val c = new BidMatrixI (end - from)
        for (j <- c.range2) {
            c._dg(j) = _dg(j + from)
            if (j > 0) c._sd(j - 1) = _sd(j + from - 1)
        } // for
        c
    } // sliceCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' bidiagonal matrix row-wise 'r_from' to 'r_end' and column-wise
     *  'c_from' to 'c_end'.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI must be square")
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' bidiagonal matrix excluding the given 'row' and 'col'umn.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support sliceExclude")
    } // sliceExclude

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from 'this' bidiagonal matrix according to the given index/basis.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support selectRows")
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from 'this' bidiagonal matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): VectorI =
    {
        val u = new VectorI (d1 - from)
        for (i <- (from max col-1) until (d1 min col+2)) u(i-from) = this(i, col)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of 'this' bidiagonal matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectorI)
    {
        _dg(col) = u(col)
        if (col > 0) _sd(col-1) = u(col-1)
    } // setCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from 'this' bidiagonal matrix according to the given index/basis.
     *  Ex: Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support selectCols")
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose 'this' bidiagonal matrix (rows => columns).  
     */
    def t: BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support transpose")
    } // t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support +:")
    } // +:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support +^:")
    } // +^:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (row) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support :+")
    } // :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (column) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support :^+")
    } // :^+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last rows in new matrix
     */
    def ++ (b: MatriI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support ++")
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last columns in new matrix
     */
    def ++^ (b: MatriI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support ++^")
    } // ++^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' bidiagonal matrix and matrix 'b'.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def + (b: MatriI): BidMatrixI = 
    {
        val bid = b.asInstanceOf [BidMatrixI]
	if (d1 == bid.d1) {
            new BidMatrixI (_dg + bid.dg, _sd + bid.sd)
        } else {
            flaw ("+", "matrix b has the wrong dimensions")
            null
        } // if
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' bidiagonal matrix and (row) vector u.
     *  @param u  the vector to add
     */
    def + (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support + with VectorI")
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' bidiagonal matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: Int): BidMatrixI =
    {
        new BidMatrixI (_dg + x, _sd + x)
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' bidiagonal matrix and matrix 'b'.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def += (b: MatriI): BidMatrixI =
    {
        val bid = b.asInstanceOf [BidMatrixI]
        if (d1 == bid.d1) {
            _dg += bid.dg
            _sd += bid.sd
        } else {
            flaw ("+=", "matrix b has the wrong dimensions")
        } // if
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' bidiagonal matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def += (u: VectorI): MatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support += with VectorI")
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' bidiagonal matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: Int): BidMatrixI =
    {
        _dg += x; _sd += x; this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' bidiagonal matrix subtract matrix 'b'.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def - (b: MatriI): BidMatrixI = 
    {
        val bid = b.asInstanceOf [BidMatrixI]
        if (d1 == bid.d1) {
            new BidMatrixI (_dg - bid.dg, _sd - bid.sd)
        } else {
            flaw ("-", "matrix b has the wrong dimensions")
            null
        } // if
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' bidiagonal matrix subtract (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def - (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support - with VectorI")
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' bidiagonal matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: Int): BidMatrixI =
    {
        new BidMatrixI (_dg - x, _sd - x)
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' bidiagonal bidiagonal matrix subtract in-place matrix 'b'.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def -= (b: MatriI): BidMatrixI =
    {
        val bid = b.asInstanceOf [BidMatrixI]
        if (d1 == bid.d1) {
            _dg -= bid.dg
            _sd -= bid.sd
        } else {
            flaw ("-=", "matrix b has the wrong dimensions")
        } // if
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' bidiagonal matrix subtract in-place (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def -= (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("BidMatrixI does not support -= with VectorI")
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' bidiagonal matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: Int): BidMatrixI =
    {
        _dg -= x; _sd -= x; this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' bidiagonal matrix by matrix 'b'.
     *  @param b  the matrix to multiply by
     */
    def * (b: MatriI): BidMatrixI = 
    {
        throw new NoSuchMethodException ("BidMatrixI does not support * with general matrices")
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' bidiagonal matrix by matrix 'b'.  Requires 'b' to have
     *  type BidMatrixI, but returns a more general type of matrix.
     *  @param b  the matrix to multiply by
     */
    def * (b: BidMatrixI): MatrixI = 
    {
        val c = new MatrixI (d1)
        for (i <- 0 until d1; j <- (i-2 max 0) to (i+2 min n)) {
            var sum = 0
            val k1 = ((i min j) - 1) max 0
            val k2 = ((i max j) + 1) min n
            for (k <- k1 to k2) sum += at(i, k) * b.at(k, j)
            c(i, j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' bidiagonal matrix by vector 'u'.
     *  @param u  the vector to multiply by
     */
    def * (u: VectorI): VectorI = 
    {
        val c = new VectorI (d1)
        for (i <- 0 until n) c(i) = _dg(i) * u(i) + _sd(i) * u(i+1)
        c(n) = _dg(d1-1) * u(d1-1)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' bidiagonal matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: Int): BidMatrixI =
    {
        new BidMatrixI (_dg * x, _sd * x)
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' bidiagonal matrix by matrix 'b'.
     *  @param b  the matrix to multiply by
     */
    def *= (b: MatriI): BidMatrixI =
    {
        throw new NoSuchMethodException ("inplace matrix multiplication not implemented")
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' bidiagonal matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def *= (x: Int): BidMatrixI =
    {
        _dg *= x; _sd *= x; this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix and vector 'u', by conceptually
     *  transposing 'this' matrix and then multiplying by 'u' (ie., 'a dot u = a.t * u').
     *  @param u  the vector to multiply by (requires same first dimensions)
     */
    def dot (u: VectorI): VectorI =
    {
        if (dim1 != u.dim) flaw ("dot", "matrix dot vector - incompatible first dimensions")

        val c = new VectorI (d1)
        c(0)  = _dg(0) * u(0)
        for (i <- 1 until d1) c(i) = _sd(i-1) * u(i-1) + _dg(i) * u(i)
        c
    } // dot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' bidiagonal matrix by vector 'u' to produce another matrix
     *  '(a_ij * u_j)'.
     *  @param u  the vector to multiply by
     */
    def ** (u: VectorI): BidMatrixI = 
    {
        throw new NoSuchMethodException ("matrix * vector -> matrix not implemented")
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' bidiagonal matrix by vector 'u' to produce another
     *  matrix '(a_ij * u_j)'.
     *  @param u  the vector to multiply by
     */
    def **= (u: VectorI): BidMatrixI =
    {
        throw new NoSuchMethodException ("inplace matrix * vector -> matrix not implemented")
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' bidiagonal matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: Int): BidMatrixI =
    {
        new BidMatrixI (_dg / x, _sd / x)
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' bidiagonal matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: Int): BidMatrixI =
    {
        _dg /= x; _sd /= x; this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' bidiagonal matrix to the 'p'th power (for some integer 'p' >= 2).
     *  @param p  the power to raise 'this' matrix to
     */
    def ~^ (p: Int): BidMatrixI =
    {
        throw new NoSuchMethodException ("matrix power function (~^) not implemented")
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' bidiagonal matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): Int = _dg(0 until e).max() max _sd(0 until e).max()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' bidiagonal matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): Int = _dg(0 until e).min() min _sd(0 until e).min()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is 'this' bidiagonal matrix.
     *  @param b  the constant vector
     */
    def solve (b: VectorI): VectorI =
    {
        val d = _dg                           // diagonal
        val e = _sd                           // superdiagonal

        val x = new VectorI (d1)
        x(n) = b(n) / d(n)
        for (i <- n-1 to 0 by -1) x(i) = (b(i) - e(i) * x(i+1)) / d(i)
        x
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine 'this' bidiagonal matrix with matrix 'b', placing them along the
     *  diagonal and filling in the bottom left and top right regions with zeros:
     *  '[this, b]'.
     *  @param b  the matrix to combine with 'this' bidiagonal matrix
     */
    def diag (b: MatriI): MatriI =
    {
        val m = d1 + b.dim1
        val n = d1 + b.dim2
        val c = new MatrixI (m, n)

        c(0, 0) = _dg(0)
        c(0, 1) = _sd(0)
        for (i <- 1 until m) {
            if (i < d1) {
                c(i, i)   = _dg(i)
                if (i < n) c(i, i+1) = _sd(i)
            } else {
                for (j <- d1 until n) c(i, j) = b(i-d1, j-d1)
            } // if
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
    def diag (p: Int, q: Int): SymTriMatrixI =
    {
        val nn = d1 + p + q
        val dd = new VectorI (nn)
        val ss = new VectorI (nn-1)

        for (i <- 0 until p)  dd(i) = 1              // Ip
        for (i <- 0 until d1) dd(i+p) = _dg(i)         // this
        for (i <- 0 until n)  ss(i+p) = _sd(i)         // this
        for (i <- p + d1 until nn) dd(i) = 1         // Iq
        new SymTriMatrixI (dd, ss)
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the 'k'th diagonal of 'this' bidiagonal matrix.  Assumes 'dim2 >= dim1'.
     *  @param k  how far above the main diagonal, e.g., (0, 1) for (main, super)
     */
    def getDiag (k: Int = 0): VectorI =
    {
        if (k == 0) _dg
        else if (k == 1) _sd
        else { flaw ("getDiag", "nothing stored for diagonal " + k); null }
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the 'k'th diagonal of 'this' bidiagonal matrix to the vector 'u'.
     *  Assumes 'dim2 >= dim1'.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorI, k: Int = 0)
    {
        if (k == 0) _dg = u
        else if (k == 1) _sd = u
        else flaw ("setDiag", "nothing stored for diagonal " + k)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of 'this' bidiagonal matrix to the scalar 'x'.
     *  Assumes 'dim2 >= dim1'.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: Int) { _dg.set (x) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' bidiagonal matrix.
     */
    def inverse: MatriI =
    {
        val d = _dg                           // diagonal
        val e = _sd                           // augmented superdiagonal

        val b = new MatrixI (d1, d1)
        for (i <- 0 until d1) b(i, i) = 1 / d(i)
        for (i <- n to 1 by -1; j <- i+1 until d1) {
            b(i, j) = -(e(j-1) / d(j)) * b(i, j-1)
        } // for
        b
     } // inverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' bidiagonal matrix at or below the threshold by setting
     *  them to zero.  Iterative algorithms give approximate values and if very close
     *  to zero, may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double, relative: Boolean = true): BidMatrixI =
    {
        val s = if (relative) mag else 1            // use matrix magnitude or 1
        for (i <- range_d) if (ABS (_dg(i)) <= thres * s) _dg(i) = 0 
        for (i <- range_s) if (ABS (_sd(i)) <= thres * s) _sd(i) = 0
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
    def nullspace: VectorI =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce.col(dim2 - 1) * -1 ++ 1
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
    def nullspace_ip: VectorI =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce_ip
        col(dim2 - 1) * -1 ++ 1
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of 'this' bidiagonal matrix, i.e., the sum of the elements
     *  on the main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: Int = _dg.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of 'this' bidiagonal matrix, i.e., the sum of its elements.
     */
    def sum: Int = _dg.sum + _sd.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the abs sum of 'this' bidiagonal matrix, i.e., the sum of the absolute
     *  value of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: Int = _dg.sumAbs + _sd.sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of 'this' bidiagonal matrix.
     */
    def sumLower: Int = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of 'this' bidiagonal matrix.
     */
    def det: Int = detHelper (n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Helper method for computing the determinant of 'this' bidiagonal matrix. FIX
     *  @param nn  the current dimension
     */
    private def detHelper (nn: Int): Int =
    {
        if (nn == 0)      _dg(0)
        else if (nn == 1) _dg(0) * _dg(1) - _sd(0) * _sd(0)
	else              _dg(n) * detHelper (nn-1) - _sd(nn-1) * _sd(nn-1) * detHelper (nn-2)
    } // detHelper

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is bidiagonal (has non-zreo elements only in
     *  main diagonal and superdiagonal).
     */
    override def isBidiagonal: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' bidiagonal matrix is nonnegative (has no negative elements).
     */
    override def isNonnegative: Boolean = _dg.isNonnegative && _sd.isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' bidiagonal matrix is rectangular (all rows have the same
     *  number  of columns).
     */
    def isRectangular: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' matrix is bidiagonal (has non-zreo elements only in
     *  main diagonal and superdiagonal).
     */
    override def isTridiagonal: Boolean = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' bidiagonal matrix to a string showing the diagonal
     *  vector followed by the sup-diagonal vector.
     */
    override def toString: String = "\nBidMatrixI(\t" + _dg + ", \n\t\t" + _sd + ")"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' matrix to a CSV-formatted text file with name 'fileName'.
     *  @param fileName  the name of file to hold the data
     */
    def write (fileName: String)
    {
        // FIX - implement write method
    } // write

    //--------------------------------------------------------------------------
    // The following methods are currently not implemented for Bidiagonal matrices:
    //--------------------------------------------------------------------------

    def lud: Tuple2 [MatriI, MatriI] =
    {
        throw new NoSuchMethodException ("lud not implemented since it's already an upper matrix")
    } // lud

    def lud_ip: Tuple2 [MatriI, MatriI] = 
    {
        throw new NoSuchMethodException ("lud_ip not implemented since it's already an upper matrix")
    } // lud_ip

    def solve (l: MatriI, u: MatriI, b: VectorI): VectorI = 
    {
        throw new NoSuchMethodException ("solve lu not implemented, since lud not needed")
    } // solve

    def inverse_ip: BidMatrixI = 
    {
        throw new NoSuchMethodException ("inverse_ip not implemented since result may not be BidMatrix")
    } // inverse_ip

    def reduce: BidMatrixI = 
    {
        throw new NoSuchMethodException ("reduce not yet implemented")
    } // reduce

    def reduce_ip ()
    {
        throw new NoSuchMethodException ("reduce_ip not implemented since result may not be BidMatrix")
    } // reduce_ip

} // BidMatrixI class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BidMatrixI` object is the companion object for the `BidMatrixI` class.
 */
object BidMatrixI extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the array of vectors 'u'.
     *  @param u           the array of vectors to assign
     *  @param columnwise  whether the vectors are treated as column or row vectors
     */
    def apply (u: Array [VectorI], columnwise: Boolean = true): BidMatrixI =
    {
        var x: BidMatrixI = null
        val u_dim = u(0).dim
        if (u_dim != u.length) flaw ("apply", "symmetric matrices must be square")
        if (columnwise) {
            x = new BidMatrixI (u_dim)
            for (j <- 0 until u_dim) x.setCol (j, u(j))       // assign column vectors
        } else {
            x = new BidMatrixI (u_dim)
            for (i <- 0 until u_dim) x(i) = u(i)              // assign row vectors
        } // if
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the Scala `Vector` of vectors 'u'.
     *  Assumes vectors are columwise.
     *  @param u  the Vector of vectors to assign
     */
    def apply (u: Vector [VectorI]): BidMatrixI =
    {
        val u_dim = u(0).dim
        val x = new BidMatrixI (u_dim)
        for (j <- 0 until u.length) x.setCol (j, u(j))        // assign column vectors
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix by reading from a text file, e.g., a CSV file.
     *  @param fileName  the name of file holding the data
     */
    def apply (fileName: String): BidMatrixI =
    {
        val sp     = ','                                          // character separating the values
        val lines  = fromFile (fileName).getLines.toArray         // get the lines from file
        val (m, n) = (lines.length, lines(0).split (sp).length)
        if (m != n) flaw ("apply", "symmetric matrices must be square")
        val x      = new BidMatrixI (m)
        for (i <- 0 until m) x(i) = VectorI (lines(i).split (sp))
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an 'm-by-m' identity matrix I (ones on main diagonal, zeros elsewhere).
     *  @param m  the row and column dimensions of the matrix
     */
    def eye (m: Int): BidMatrixI =
    {
        val c = new BidMatrixI (m)
        for (i <- 0 until m) c(i, i) = 1
        c
    } // eye

} // BidMatrixI object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BidMatrixITest` object is used to test the `BidMatrixI` class.
 *  > run-main scalation.linalgebra.BidMatrixITest
 */
object BidMatrixITest extends App
{
    val a = new BidMatrixI (VectorI (3, 4, 5),
                            VectorI (2, 1))

    val b = new BidMatrixI (VectorI (2, 3, 4),
                            VectorI (5, 6))

    val v = VectorI (5, 3, 6)

    val c = new MatrixI ((3, 3), 3, 1, 0,
                                 0, 4, 2,
                                 0, 0, 5)

    val d = new MatrixI ((3, 3), 2, 5, 0,
                                 5, 3, 6,
                                 0, 6, 4)

    println ("a     = " + a)
    println ("b     = " + b)
    println ("a + b = " + (a + b))	
    println ("a - b = " + (a - b))	
    println ("a * b = " + (a * b))	
    println ("a * v = " + (a * v))	
    println ("c * d = " + (c * d))	

    println ("a.det = " + a.det)

    val x2 = a.solve (v)
    println ("a.solve (v)       = " + x2)
    println ("a * x2 = " + a * x2)

    println ("a.inverse     = " + a.inverse)
    println ("a.inverse * a = " + a.inverse * a)

} // BidMatrixITest object

