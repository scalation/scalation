
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon May 25 17:57:02 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

import java.io.{File, PrintWriter}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldSymTriMatrix` object is used to build symmetric tridiagonal matrix
 *  classes for various base types.
 *  > runMain scalation.linalgebra.bld.BldSymTriMatrix
 */
object BldSymTriMatrix extends App with BldParams
{
    println ("BldSymTriMatrix: generate code for Symmetric Tridiagonal Matrix classes")

    for (i <- 0 until kind.length-1) {        // do not generate `SymTriMatrixD`
        val VECTO     = kind(i)._1
        val VECTOR    = kind(i)._1.replace ("o", "or")
        val BASE      = kind(i)._2
        val VECTOR2   = kind(i)._3
        val BASE2     = kind(i)._4
        val FORMAT    = kind(i)._5
        val MATRI     = kind(i)._6
        val ZERO      = kind(i)._8
        val ONE       = kind(i)._9
        val BASE_LC   = BASE.toLowerCase
        val MATRIX    = { val m = MATRI.splitAt (MATRI.size-1); m._1 + "x" + m._2 }
        val IMPORT    = if (CUSTOM contains BASE) s"scalation.math.$BASE.{abs => ABS, _}"
                        else "scala.math.{abs => ABS}"
        val IMPORT2   = if (CUSTOM contains BASE) s"scalation.math.{$BASE, oneIf}"
                        else s"scalation.math.{${BASE_LC}_exp, oneIf}"

// Beginning of string holding code template -----------------------------------

        val code = raw"""
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Robert Davis, John Miller
 *  @builder scalation.linalgebra.bld.BldSymTriMatrix
 *  @version 1.6
 *  @date    Sun Sep 16 14:09:25 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.io.Source.fromFile

import $IMPORT

import $IMPORT2
import scalation.util.Error

import $MATRIX.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SymTri$MATRIX` class stores and operates on symmetric tridiagonal matrices.
 *  The elements are of type of `$BASE`.  A matrix is stored as two vectors:
 *  the diagonal vector and the sub-diagonal vector.
 *  @param d1  the first/row dimension (symmetric => d2 = d1)
 */
class SymTri$MATRIX (val d1: Int)
      extends $MATRI with Error with Serializable
{
    /** Dimension 1
     */
    lazy val dim1 = d1

    /** Dimension 2
     */
    lazy val dim2 = d1

    /** Size of the sub-diagonal
     */
    private val n = d1 - 1

    /** Range for the diagonal
     */
    private val range_d = 0 until d1

    /** Range for the sub-diagonal
     */
    private val range_s = 0 until n

    /** Diagonal of the matrix
     */
    private var _dg: $VECTOR = new $VECTOR (d1)

    /** Sub-diagonal (also same for sup-diagonal) of the matrix
     */
    private var _sd: $VECTOR = new $VECTOR (n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a symmetric tridiagonal matrix with the given diagonal and sub-diagonal.
     *  @param v1  the diagonal vector
     *  @param v2  the sub-diagonal vector
     */
    def this (v1: $VECTO, v2: $VECTO)
    {
        this (v1.dim)
        for (i <- range_d) _dg(i) = v1(i)
        for (i <- range_s) _sd(i) = v2(i)
    } // constructor
"""; val code2 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a symmetric tridiagonal matrix from the given matrix.
     *  @param b  the matrix of values to assign
     */
    def this (b: $MATRI)
    {
        this (b.dim1)
        for (i <- range_d) _dg(i) = b(i, i)
        for (i <- range_s) _sd(i) = b(i, i+1)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an exact copy of 'this' m-by-n symmetric tridiagonal matrix.
     */
    def copy (): SymTri$MATRIX = new SymTri$MATRIX (dim1)             // FIX - copy the diagonals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an m-by-n symmetric tridiagonal matrix with all elements initialized to zero.
     *  @param m  the number of rows
     *  @param n  the number of columns
     */
    def zero (m: Int = dim1, n: Int = dim2): SymTri$MATRIX = new SymTri$MATRIX (m)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the diagonal of 'this' tridiagonal matrix.
     */
    def dg: $VECTOR = _dg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the diagonal of 'this' tridiagonal matrix.
     * @param v  the vector to assign to the diagonal
     */
    def dg_ (v: $VECTOR) { _dg = v }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the sub-diagonal of 'this' tridiagonal matrix.
     */
    def sd: $VECTOR = _sd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the sub-diagonal of 'this' tridiagonal matrix.
     *  @param v  the vector to assign to the sub-diagonal
     */
    def sd_ (v: $VECTOR) { _sd = v }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' tridiagonal matrix's element at the 'i,j'-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): $BASE = 
    {
        if      (i == j)     _dg(i)       // on diagonal
        else if (i == j + 1) _sd(j)       // on sub-diagonal (below diagonal)
        else if (i + 1 == j) _sd(i)       // on sup-diagonal (above diagonal)
        else throw new Exception ("SymTri$MATRIX.apply: element not on tridiagonal")
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' tridiagonal matrix's element at the 'i,j'-th index position,
     *  returning 0, if off tridiagonal.
     *  @param i  the row index
     *  @param j  the column index
     */
    def at (i: Int, j: Int): $BASE =
    {
        if (i < 0 || j < 0 || i >= d1 || j >= d1) $ZERO
        else if (i == j)     _dg(i)       // on diagonal
        else if (i == j + 1) _sd(j)       // on sub-diagonal (below diagonal)
        else if (i + 1 == j) _sd(i)       // on sup-diagonal (above diagonal)
        else $ZERO
    } // at

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get 'this' tridiagonal matrix's vector at the 'i'-th index position
     *  ('i'-th row).
     *  @param i  the row index
     */
    def apply (i: Int): $VECTOR =
    {
        val u = new $VECTOR (d1)
        u(i)  = _dg(i)
        if (i > 0) u(i-1) = _sd(i-1)
        if (i < n) u(i+1) = _sd(i)
        u
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice 'this' tridiagonal matrix row-wise on range 'ir' and column-wise
     *  on range 'jr'.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): SymTri$MATRIX = 
    {
        if (ir != jr) flaw ("apply", "requires same ranges to maintain symmetry")
        slice (ir.start, ir.end)
    } // apply
"""; val code3 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' tridiagonal matrix's element at the 'i,j'-th index position to
     *  the scalar 'x'.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: $BASE)
    {
        if      (i == j)     _dg(i) = x
        else if (i == j + 1) _sd(j) = x
        else if (i + 1 == j) _sd(i) = x
        else flaw ("update", "element not on tridiagonal")
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' tridiagonal matrix's row at the 'i'-th index position to the
     *  vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: $VECTO)
    {
        _dg(i) = u(i)
        if (i > 0) _sd(i-1) = u(i-1)
        if (i < n) _sd(i)   = u(i+1)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice 'this' tridiagonal matrix row-wise on range 'ir' and column-wise
     *  on range 'jr'.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: $MATRI)
    {
        if (ir != jr) flaw ("update", "requires same ranges to maintain symmetry")
        if (b.isInstanceOf [SymTri$MATRIX]) {
            val bb = b.asInstanceOf [SymTri$MATRIX]
            for (i <- ir) {
                _dg(i) = bb.dg(i - ir.start)
                if (i > ir.start) _sd(i-1) = bb.sd(i - ir.start - 1)
            } // for
        } else {
            flaw ("update", "must convert b to a SymTri$MATRIX first")
        } // if
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in 'this' tridiagonal matrix to the scalar 'x'.
     *  @param x  the scalar value to assign
     */
    def set (x: $BASE)
    {
        for (i <- range1) {
            _dg(i) = x
            if (i > 0) _sd(i) = x
        } // for
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in 'this' tridiagonal matrix as copies of the values in
     *  2D array 'u'.  Ignore parts of array not corresponding to tridiagonal.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [$BASE]])
    {
        for (i <- 0 until d1) {
            _dg(i) = u(i)(i)                   // set diagonal
            if (i < n) _sd(i) = u(i+1)(i)      // set sub-diagonal
        } // for
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the values in 'this' matrix as copies of the values in matrix 'u'.
     *  @param u  the matrix of values to assign
     */
    def set (u: $MATRI)
    {
        if (u.isInstanceOf [SymTri$MATRIX]) {
            val uu = u.asInstanceOf [SymTri$MATRIX]
            for (i <- range_d) _dg(i) = uu._dg(i)
            for (j <- range_s) _sd(j) = uu._sd(j)
        } else {
            flaw ("set", "must convert u to a SymTri$MATRIX first")
        } // if
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set 'this' tridiagonal matrix's 'i'th row starting at column 'j' to the
     *  vector 'u'.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: $VECTO, j: Int = 0)
    {
        if (i >= j)   _dg(i) = u(i)
        if (i-1 >= j) _sd(i-1) = u(i-1)
    } // set
"""; val code4 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' `SymTri$MATRIX` into a SymTriMatrixI`.
     */
    def toInt: SymTriMatrixI = new SymTriMatrixI (_dg.toInt, _sd.toInt)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' tridiagonal matrix to a dense matrix.
     */
    def toDense: $MATRIX =
    {
        val c = new $MATRIX (dim1, dim1)
        for (i <- range1) {
            c(i, i) = _dg(i) 
            if (i > 0)      c(i, i-1) = _sd(i-1)
            if (i < dim1-1) c(i, i+1) = _sd(i)
        } // for
        c
    } // for

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' tridiagonal matrix row-wise 'from' to 'end'.
     *  @param from  the start row of the slice (inclusive)
     *  @param end   the end row of the slice (exclusive)
     */
    def slice (from: Int, end: Int): SymTri$MATRIX =
    {
        val c = new SymTri$MATRIX (end - from)
        for (i <- c.range1) {
            c._dg(i) = _dg(i + from)
            if (i > 0) c._sd(i - 1) = _sd(i + from - 1)
        } // for
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' tridiagonal matrix column-wise 'from' to 'end'.
     *  @param from  the start column of the slice (inclusive)
     *  @param end   the end column of the slice (exclusive)
     */
    def sliceCol (from: Int, end: Int): SymTri$MATRIX =
    {
        val c = new SymTri$MATRIX (end - from)
        for (j <- c.range1) {
            c._dg(j) = _dg(j + from)
            if (j > 0) c._sd(j - 1) = _sd(j + from - 1)
        } // for
        c
    } // sliceCol

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' tridiagonal matrix row-wise 'r_from' to 'r_end' and column-wise
     *  'c_from' to 'c_end'.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX must be symmetric")
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice 'this' tridiagonal matrix excluding the given 'row' and 'col'umn.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceEx (row: Int, col: Int): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support sliceEx")
    } // sliceEx

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from 'this' tridiagonal matrix according to the given index/basis.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support selectRows")
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from 'this' tridiagonal matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): $VECTOR =
    {
        val u = new $VECTOR (d1 - from)
        for (i <- (from max col-1) until (d1 min col+2)) u(i-from) = this(i, col)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of 'this' tridiagonal matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: $VECTO)
    {
        _dg(col) = u(col)
        if (col > 0) _sd(col-1) = u(col-1)
    } // setCol
"""; val code5 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from 'this' tridiagonal matrix according to the given index/basis.
     *  Ex: Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support selectCols")
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose 'this' tridiagonal matrix (rows => columns).  Note, since the
     *  matrix is symmetric, it returns itself.
     */
    def t: SymTri$MATRIX = this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first row in new matrix
     */
    def +: (u: $VECTO): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support +:")
    } // +:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column) vector 'u' and 'this' matrix, i.e., prepend 'u' to 'this'.
     *  @param u  the vector to be prepended as the new first column in new matrix
     */
    def +^: (u: $VECTO): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support +^:")
    } // +^:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (row) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last row in new matrix
     */
    def :+ (u: $VECTO): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support :+")
    } // :+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate 'this' matrix and (column) vector 'u', i.e., append 'u' to 'this'.
     *  @param u  the vector to be appended as the new last column in new matrix
     */
    def :^+ (u: $VECTO): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support :^+")
    } // :^+

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (row-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last rows in new matrix
     */
    def ++ (b: $MATRI): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support ++")
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate (column-wise) 'this' matrix and matrix 'b'.
     *  @param b  the matrix to be concatenated as the new last columns in new matrix
     */
    def ++^ (b: $MATRI): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support ++^")
    } // ++^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' tridiagonal matrix and matrix 'b'.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def + (b: $MATRI): SymTri$MATRIX = 
    {
        val trid = b.asInstanceOf [SymTri$MATRIX]
	if (d1 == trid.d1) {
            new SymTri$MATRIX (_dg + trid.dg, _sd + trid.sd)
        } else {
            flaw ("+", "matrix b has the wrong dimensions")
            null
        } // if
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' tridiagonal matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def + (u: $VECTO): $MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support + for $VECTO")
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add 'this' tridiagonal matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def + (x: $BASE): SymTri$MATRIX =
    {
        new SymTri$MATRIX (_dg + x, _sd + x)
    } // +
"""; val code6 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' tridiagonal matrix and matrix 'b'.
     *  @param b  the matrix to add (requires 'leDimensions')
     */
    def += (b: $MATRI): SymTri$MATRIX =
    {
        val trid = b.asInstanceOf [SymTri$MATRIX]
        if (d1 == trid.d1) {
            _dg += trid.dg
            _sd += trid.sd
        } else {
            flaw ("+=", "matrix b has the wrong dimensions")
        } // if
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' tridiagonal matrix and (row) vector 'u'.
     *  @param u  the vector to add
     */
    def += (u: $VECTO): $MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support += for $VECTO")
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place 'this' tridiagonal matrix and scalar 'x'.
     *  @param x  the scalar to add
     */
    def += (x: $BASE): SymTri$MATRIX =
    {
        _dg += x; _sd += x; this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tridiagonal matrix subtract matrix 'b'.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def - (b: $MATRI): SymTri$MATRIX = 
    {
        val trid = b.asInstanceOf [SymTri$MATRIX]
        if (d1 == trid.d1) {
            new SymTri$MATRIX (_dg - trid.dg, _sd - trid.sd)
        } else {
            flaw ("-", "matrix b has the wrong dimensions")
            null
        } // if
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tridiagonal matrix subtract (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def - (u: $VECTO): $MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support - for $VECTO")
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tridiagonal matrix subtract scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def - (x: $BASE): SymTri$MATRIX =
    {
        new SymTri$MATRIX (_dg - x, _sd - x)
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tridiagonal matrix subtract in-place matrix 'b'.
     *  @param b  the matrix to subtract (requires 'leDimensions')
     */
    def -= (b: $MATRI): SymTri$MATRIX =
    {
        val trid = b.asInstanceOf [SymTri$MATRIX]
        if (d1 == trid.d1) {
            _dg -= trid.dg
            _sd -= trid.sd
        } else {
            flaw ("-=", "matrix b has the wrong dimensions")
        } // if
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tridiagonal matrix subtract in-place (row) vector 'u'.
     *  @param u  the vector to subtract
     */
    def -= (u: $VECTO): $MATRIX =
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support -= for $VECTO")
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From 'this' tridiagonal matrix subtract in-place scalar 'x'.
     *  @param x  the scalar to subtract
     */
    def -= (x: $BASE): SymTri$MATRIX =
    {
        _dg -= x; _sd -= x; this
    } // -=
"""; val code7 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' tridiagonal matrix by matrix 'b'.
     *  @param b  the matrix to multiply by
     */
    def * (b: $MATRI): SymTri$MATRIX = 
    {
        throw new UnsupportedOperationException ("SymTri$MATRIX does not support * with general matrices")
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' tridiagonal matrix by matrix 'b'.  Requires 'b' to have
     *  type `SymTri$MATRIX`, but returns a more general type of matrix.
     *  @param b  the matrix to multiply by
     */
    def * (b: SymTri$MATRIX): $MATRIX = 
    {
        val c = new $MATRIX (d1)
        for (i <- 0 until d1; j <- (i-2 max 0) to (i+2 min n)) {
            var sum = $ZERO
            val k1 = ((i min j) - 1) max 0
            val k2 = ((i max j) + 1) min n
            for (k <- k1 to k2) sum += at(i, k) * b.at(k, j)
            c(i, j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' tridiagonal matrix by vector 'u'.
     *  @param u  the vector to multiply by
     */
    def * (u: $VECTO): $VECTOR = 
    {
        val c = new $VECTOR (d1)
        c(0)  = _dg(0) * u(0) + _sd(0) * u(1)
        for (i <- 1 until n) {
            c(i) = _sd(i-1) * u(i-1) + _dg(i) * u(i) + _sd(i) * u(i+1)
        } // for
        c(n) = _sd(d1-2) * u(d1-2) + _dg(n) * u(n)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' tridiagonal matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def * (x: $BASE): SymTri$MATRIX =
    {
        new SymTri$MATRIX (_dg * x, _sd * x)
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' tridiagonal matrix by matrix 'b'.
     *  @param b  the matrix to multiply by
     */
    def *= (b: $MATRI): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("inplace matrix multiplication not implemented")
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' tridiagonal matrix by scalar 'x'.
     *  @param x  the scalar to multiply by
     */
    def *= (x: $BASE): SymTri$MATRIX =
    {
        _dg *= x; _sd *= x; this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' tridiagonal matrix and vector 'u', by
     *  first transposing 'this' tridiagonal matrix and then multiplying by 'u'
     *  (i.e., 'a dot u = a.t * u').
     *  Since 'this' is symmetric, the result is the same as 'a * u'.
     *  @param u  the vector to multiply by (requires same first dimensions)
     */
    def dot (u: $VECTO): $VECTOR = this * u

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix with matrix 'b' to produce a vector.
     *  @param b  the second matrix of the dot product
     */
    def dot (b: SymTri$MATRIX): $VECTOR = 
    {
        if (dim1 != b.dim1) flaw ("dot", "matrix dot matrix - incompatible first dimensions")
        if (dim1 == 1) return $VECTOR (_dg(0) * b._dg(0))
        
        var c = new $VECTOR (dim2)
        c(0)  = _dg(0) * b._dg(0) + _sd(0) * b._sd(0)
        for (i <- 1 until dim1-1) c(i) = _sd(i-1) * b.sd(i-1) + _dg(i) * b._dg(i) + _sd(i) * b.sd(i)
        c(dim1-1) = _dg(dim1-1) * b._dg(dim1-1) + _sd(dim1-2) * b._sd(dim1-2)
        c
    } // dot 
"""; val code8 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the dot product of 'this' matrix with matrix 'b' to produce a vector.
     *  @param b  the second matrix of the dot product
     */
    def dot (b: $MATRI): $VECTOR = 
    {
        if (dim1 != b.dim1) flaw ("dot", "matrix dot matrix - incompatible first dimensions")
        if (dim1 == 1) return $VECTOR (_dg(0) * b(0, 0))
        
        var c = new $VECTOR (dim2)
        c(0) = _dg(0) * b(0, 0) + _sd(0) * b(1, 0)
        for (i <- 1 until dim1 - 1) c(i) = _sd(i-1) * b(i-1, i) + _dg(i) * b(i, i) + _sd(i) * b(i+1, i)
        c(dim1-1) = _dg(dim1-1) * b(dim1-1, dim1-1) + _sd(dim1-2) * b(dim1-2, dim1-1)
        c
    } // dot 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix dot product of 'this' matrix with matrix 'b' to produce a matrix.
     *  @param b  the second matrix of the dot product
     */
    def mdot (b: SymTri$MATRIX): $MATRIX = 
    {
        if (dim1 != b.dim1) flaw ("mdot", "matrix mdot matrix - incompatible first dimensions")
        
        val c = new $MATRIX (dim2, b.dim2)           
        if (dim1 == 1) {
            c(0, 0) = _dg(0) * b._dg(0)
            return c
        } // if
        c(0, 0) = _dg(0) * b._dg(0) + _sd(0) * b._sd(0)
        c(1, 0) = _sd(0) * b._dg(0) + _dg(1) * b._sd(0)
        c(0, 1) = _dg(0) * b._sd(0) + _sd(0) * b._dg(1)
        if (dim1 == 2) {
            c(1, 1) = _dg(1) * b._dg(1) + _sd(0) * b._sd(0)
            return c
        } // if
        c(1, 1) = _dg(1) * b._dg(1) + _sd(0) * b._sd(0) + _sd(1) * b._sd(1)
        for (i <- 2 until dim1) {
            c(i, i)   = if (i != dim1-1) _sd(i-1) * b._sd(i-1) + _dg(i) * b._dg(i) + _sd(i) * b._sd(i)
                        else             _sd(i-1) * b._sd(i-1) + _dg(i) * b._dg(i)
            c(i-1, i) = _dg(i - 1) * b._sd(i-1) + _sd(i - 1) * b._dg(i)
            c(i-2, i) = _sd(i - 2) * b._sd(i-1)
            c(i, i-1) = _sd(i - 1) * b._dg(i-1) + _dg(i) * b._sd(i-1)
            c(i, i-2) = _sd(i - 1) * b._sd(i-2)
        } // for
        c
    } // mdot
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the matrix dot product of 'this' matrix with matrix 'b' to produce a matrix.
     *  @param b  the second matrix of the dot product
     */
    def mdot (b: $MATRI): $MATRIX = 
    {
        if (dim1 != b.dim1) flaw ("mdot", "matrix mdot matrix - incompatible first dimensions")
        
        var c = new $MATRIX (dim2, b.dim2)
        if (dim1 == 1) {
            for (j <- 0 until b.dim2) c(0, j) = _dg(0) * b(0, j)
            return c
        } // if
        for (j <- 0 until b.dim2) {
            c(0, j) = _dg(0) * b(0, j) + _sd(0) * b(1, j)
            for (i <- 1 until dim1 - 1) {
                c(i, j) = _sd(i-1) * b(i-1, j) + _dg(i) * b(i, j) + _sd(i) * b(i+1, j)
            } // for
            c(dim1-1, j) = _dg(dim1-1) * b(dim1-1, j) + _sd(dim1-2) * b(dim1-2, j)
        } // for 
        c
    } // mdot

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply 'this' tridiagonal matrix by vector 'u' to produce another matrix 'a_ij * u_j'.
     *  E.g., multiply a diagonal matrix represented as a vector by a matrix.
     *  @param u  the vector to multiply by
     */
    def ** (u: $VECTO): $MATRIX = this * SymTri$MATRIX (u)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place 'this' tridiagonal matrix by vector 'u' to produce another
     *  matrix 'a_ij * u_j'.
     *  E.g., multiply a diagonal matrix represented as a vector by a matrix.
     *  @param u  the vector to multiply by
     */
    def **= (u: $VECTO): $MATRIX =
    {
        throw new UnsupportedOperationException ("inplace matrix * vector -> matrix not implemented")
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply vector 'u' by 'this' tridiagonal matrix to produce another matrix 'u_i * a_ij'.
     *  E.g., multiply a diagonal matrix represented as a vector by a matrix.
     *  This operator is right associative.
     *  @param u  the vector to multiply by
     */
    def **: (u: $VECTO): $MATRIX = SymTri$MATRIX (u) * this
"""; val code9 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide 'this' tridiagonal matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def / (x: $BASE): SymTri$MATRIX =
    {
        new SymTri$MATRIX (_dg / x, _sd / x)
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place 'this' tridiagonal matrix by scalar 'x'.
     *  @param x  the scalar to divide by
     */
    def /= (x: $BASE): SymTri$MATRIX =
    {
        _dg /= x; _sd /= x; this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' tridiagonal matrix to the 'p'th power (for some integer 'p' >= 2).
     *  @param p  the power to raise this tridiagonal matrix to
     */
    def ~^ (p: Int): SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("matrix power function (~^) not implemented")
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in 'this' tridiagonal matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): $BASE = _dg(0 until e).max() max _sd(0 until e).max()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in 'this' tridiagonal matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): $BASE = _dg(0 until e).min() min _sd(0 until e).min()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor 'this' tridiagonal matrix into the product of lower and
     *  upper triangular matrices '(l, u)' using the 'LU' Factorization algorithm.
     *  'l' is lower bidiagonal and 'u' is upper bidiagonal.
     *  FIX: would be more efficient to use tridiagonal matrices than dense matrices.
     *  @see www.webpages.uidaho.edu/~barannyk/Teaching/LU_factorization_tridiagonal.pdf
     */
    def lud_npp: ($MATRI, $MATRI) =
    {
        val l  = eye (d1)                  // lower triangular matrix
        val u  = new $MATRIX (d1, d1)      // upper triangular matrix
        val ls = new $VECTOR (n)           // subdiagonal of l
        val ud = new $VECTOR (d1)          // diagonal of u

        ud(0) = _dg(0)
        for (i <- 1 until d1) {
            ls(i-1) = _sd(i-1) / ud(i-1)
            ud(i)   = _dg(i) - ls(i-1) * _sd(i-1)
        } // for

        l setDiag (ls, -1)                 // set subdiagonal for l
        u setDiag (_sd, 1)                 // set super-diagonal for u
        u setDiag (ud)                     // set diagonal for u
        (l, u)
    } // lud_npp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = l*u*x = b' (see 'lud_npp' above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: $MATRI, u: $MATRI, b: $VECTO): $VECTO = 
    {
        if (! l.isInstanceOf [$MATRIX] || ! u.isInstanceOf [$MATRIX]) {
            throw new IllegalArgumentException ("'l.solve (u)' is only implemented for dense matrices")
        } // if
        l.solve (l, u, b)
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for 'x' in the equation 'a*x = b' where 'a' is 'this' tridiagonal matrix,
     *  using the Thomas Algorithm.
     *  Caveat:  Stability vs. diagonal dominance.
     *  This method is more efficient, since a 'lud_npp' creates dense matrices.
     *  @see en.wikibooks.org/wiki/Algorithm_Implementation/Linear_Algebra/Tridiagonal_matrix_algorithm
     *  @param b  the constant vector
     */
    def solve (b: $VECTO): $VECTO =
    {
        val j = d1 - 2
        val x = new $VECTOR (b)               // solution vector, start with copy of b
        val c = _sd                           // subdiagonal
        val d = _dg                           // diagonal
        val e = new $VECTOR (_sd ++ $ZERO)      // augmented super-diagonal
 
        e(0) /= d(0)
        x(0) /= d(0)
 
        for (i <- 1 until n) {
            val t = $ONE / (d(i) - c(i-1) * e(i-1))
            e(i) *= t
            x(i)  = (x(i) - c(i-1) * x(i-1)) * t;
        } // for

        x(n) = (x(n) - c(j) * x(j)) / (d(n) - c(j)* e(j))
        for (i <- j to 0 by -1) x(i) -= e(i) * x(i+1)
        x
    } // solve
"""; val code10 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine 'this' tridiagonal matrix with matrix 'b', placing them along the
     *  diagonal and filling in the bottom left and top right regions with zeros:
     *  '[this, b]'.
     *  @param b  the matrix to combine with 'this' tridiagonal matrix
     */
    def diag (b: $MATRI): $MATRI = 
    {
        val m = d1 + b.dim1
        val n = d1 + b.dim2
        val c = new $MATRIX (m, n)

        c(0, 0) = _dg(0)
        c(0, 1) = _sd(0)
        for (i <- 1 until m) {
            if (i < d1) {
                c(i, i-1) = _sd(i-1)
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
    def diag (p: Int, q: Int): SymTri$MATRIX = 
    {
        val nn = d1 + p + q 
        val dd = new $VECTOR (nn)
        val ss = new $VECTOR (nn-1)

        for (i <- 0 until p)  dd(i) = $ONE              // Ip
        for (i <- 0 until d1) dd(i+p) = _dg(i)         // this
        for (i <- 0 until n)  ss(i+p) = _sd(i)         // this
        for (i <- p + d1 until nn) dd(i) = $ONE         // Iq
        new SymTri$MATRIX (dd, ss)
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the 'k'th diagonal of 'this' tridiagonal matrix.  Assumes 'dim2 >= dim1'.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): $VECTOR =
    {
        if (k == 0) _dg
        else if (ABS (k) == 1) _sd
        else { flaw ("getDiag", "nothing stored for diagonal " + k); null }
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the 'k'th diagonal of 'this' tridiagonal matrix to the vector 'u'.
     *  Assumes 'dim2 >= dim1'.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: $VECTO, k: Int = 0)
    {
        if (k == 0) _dg = u.toDense
        else if (ABS (k) == 1) _sd = u.toDense
        else flaw ("setDiag", "nothing stored for diagonal " + k)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of 'this' tridiagonal matrix to the scalar 'x'.
     *  Assumes 'dim2 >= dim1'.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: $BASE) { _dg.set (x) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert 'this' tridiagonal matrix.
     *  @see www.amm.shu.edu.cn/EN/article/downloadArticleFile.do?attachType=PDF&id=4339
     */
    def inverse: $MATRI =
    {
        val c = _sd                           // subdiagonal
        val d = _dg                           // diagonal
        val e = new $VECTOR (_sd  ++ $ZERO)     // augmented super-diagonal

        val a = new $VECTOR (d1)              // alpha
        val g = new $VECTOR (d1)              // gamma
        val t = new $VECTOR (d1)              // tau

        a(0) = d(0)
        for (i <- 1 until d1) {
            t(i-1) = e(i-1) / a(i-1)
            a(i)   = d(i) - c(i-1) * t(i-1)
            g(i)   = c(i-1) / a(i-1)
            if (a(i) =~ $ZERO) {
                if (i == n) flaw ("inverse", "this matrix is singular")
                else        flaw ("inverse", "failed a(" + i + ") is 0")
                return null
            } // if
        } // for

        val b = new $MATRIX (d1, d1)
        b(n, n) = $ONE / a(n)
        for (i <- n-1 to 0 by -1) b(i, i) = ($ONE / a(i)) + t(i) * g(i+1) * b(i+1, i+1)
        for (i <- n to 1 by -1; j <- i-1 to 0 by -1) {
            b(i, j) = -g(j+1) * b(i, j+1)
            b(j, i) = -t(j) * b(j+1, i)
        } // for
        b
    } // inverse
"""; val code11 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clean values in 'this' tridiagonal matrix at or below the threshold by setting
     *  them to zero.  Iterative algorithms give approximate values and if very close
     *  to zero,  may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: Double, relative: Boolean = true): SymTri$MATRIX =
    {
        val s = if (relative) mag else $ONE            // use matrix magnitude or 1
        for (i <- range_d) if (ABS (_dg(i)) <= thres * s) _dg(i) = $ZERO 
        for (i <- range_s) if (ABS (_sd(i)) <= thres * s) _sd(i) = $ZERO
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
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce_ip ()
        col(dim2 - 1) * -$ONE ++ $ONE
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of 'this' tridiagonal matrix, i.e., the sum of the elements
     *  on the main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: $BASE = _dg.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of 'this' tridiagonal matrix, i.e., the sum of its elements.
     */
    def sum: $BASE = _dg.sum + _sd.sum + _sd.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 'abs' sum of 'this' tridiagonal matrix, i.e., the sum of the absolute
     *  value of its elements.  This is useful for comparing matrices '(a - b).sumAbs'.
     */
    def sumAbs: $BASE = _dg.norm1 + _sd.norm1 + _sd.norm1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of 'this' tridiagonal matrix.
     */
    def sumLower: $BASE = _sd.sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of 'this' tridiagonal matrix.
     */
    def det: $BASE = detHelper (n)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Helper method for computing the determinant of 'this' tridiagonal matrix.
     *  @param nn  the current dimension
     */
    private def detHelper (nn: Int): $BASE =
    {
        if (nn == 0)      _dg(0)
        else if (nn == 1) _dg(0) * _dg(1) - _sd(0) * _sd(0)
	else              _dg(nn) * detHelper (nn-1) - _sd(nn-1) * _sd(nn-1) * detHelper (nn-2)
    } // detHelper
"""; val code12 = raw"""
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' tridiagonal matrix is nonnegative (has no negative elements).
     */
    override def isNonnegative: Boolean = _dg.isNonnegative && _sd.isNonnegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' tridiagonal matrix is rectangular (all rows have the same
     *  number of columns).
     */
    def isRectangular: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' tridiagonal matrix is symmetric.
     */
    override def isSymmetric: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether 'this' tridiagonal matrix is bidiagonal (has non-zero elements
     *  only in main diagonal and super-diagonal).  The method may be overriding for
     *  efficiency.
     */
    override def isTridiagonal: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert 'this' tridiagonal matrix to a string showing the diagonal
     *  vector followed by the sub-diagonal vector.
     */
    override def toString: String = "\nSymTri$MATRIX(\t" + _dg + ", \n\t\t" + _sd + ")"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write 'this' tridiagonal matrix to a CSV-formatted text file with name
     *  'fileName'.
     *  @param fileName  the name of file to hold the data
     */
    def write (fileName: String)
    {
        // FIX - implement write method
    } // write

    //--------------------------------------------------------------------------
    // The following methods are currently not implemented for Symmetric Tridiagonal matrices:
    //--------------------------------------------------------------------------

    def lowerT: SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("lowerT not implemented since result may not be SymTriMatrix")
    } // lowerT

    def upperT: SymTri$MATRIX =
    {
        throw new UnsupportedOperationException ("lowerT not implemented since result may not be SymTriMatrix")
    } // upperT

    def lud_ip (): ($MATRI, $MATRI) = 
    {
        throw new UnsupportedOperationException ("lud_ip not implemented since result may not be SymTriMatrix")
    } // lud_ip

    def bsolve (y: $VECTO): $VECTOR =
    {
        throw new UnsupportedOperationException ("bsolve not implemented since upper triangular is symmetric")
    } // bsolve

    def inverse_ip (): SymTri$MATRIX = 
    {
        throw new UnsupportedOperationException ("inverse_ip not implemented since result may not be SymTriMatrix")
    } // inverse_ip

    def reduce: SymTri$MATRIX = 
    {
        throw new UnsupportedOperationException ("reduce not yet implemented")
    } // reduce

    def reduce_ip ()
    {
        throw new UnsupportedOperationException ("reduce_ip not implemented since results may not be SymTriMatrix")
    } // reduce_ip

} // SymTri$MATRIX class
"""; val code13 = raw"""

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SymTri$MATRIX` object is the companion object for the `SymTri$MATRIX` class.
 */
object SymTri$MATRIX extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the array of vectors 'u'.
     *  @param u           the array of vectors to assign
     *  @param columnwise  whether the vectors are treated as column or row vectors
     */
    def apply (u: Array [$VECTO], columnwise: Boolean = true): SymTri$MATRIX =
    {
        var x: SymTri$MATRIX = null
        val u_dim = u(0).dim
        if (u_dim != u.length) flaw ("apply", "symmetric matrices must be square")
        if (columnwise) {
            x = new SymTri$MATRIX (u_dim)
            for (j <- 0 until u_dim) x.setCol (j, u(j))       // assign column vectors
        } else {
            x = new SymTri$MATRIX (u_dim)
            for (i <- 0 until u_dim) x(i) = u(i)              // assign row vectors
        } // if
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix and assign values from the Scala `Vector` of vectors 'u'.
     *  Assumes vectors are column-wise.
     *  @param u  the Vector of vectors to assign
     */
    def apply (u: Vector [$VECTO]): SymTri$MATRIX =
    {
        val u_dim = u(0).dim
        val x = new SymTri$MATRIX (u_dim)
        for (j <- 0 until u.length) x.setCol (j, u(j))        // assign column vectors
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a diagonal symtri matrix and assign values from vector 'u'.
     *  @param u  the vector to assign
     */ 
    def apply (u: $VECTO): SymTri$MATRIX =
    {   
        val x = new SymTri$MATRIX (u.dim)
        for (i <- 0 until u.dim) x(i, i) = u(i)
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a matrix by reading from a text file, e.g., a CSV file.
     *  @param fileName  the name of file holding the data
     */
    def apply (fileName: String): SymTri$MATRIX =
    {
        val sp     = ','                                          // character separating the values
        val lines  = fromFile (fileName).getLines.toArray         // get the lines from file
        val (m, n) = (lines.length, lines(0).split (sp).length)
        if (m != n) flaw ("apply", "symmetric matrices must be square")
        val x      = new SymTri$MATRIX (m)
        for (i <- 0 until m) x(i) = $VECTOR (lines(i).split (sp))
        x
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an 'm-by-m' identity matrix I (ones on main diagonal, zeros elsewhere).
     *  @param m  the row and column dimensions of the matrix
     */
    def eye (m: Int): SymTri$MATRIX =
    {
        val c = new SymTri$MATRIX (m)
        for (i <- 0 until m) c(i, i) = $ONE
        c
    } // eye

} // SymTri$MATRIX object
"""; val code14 = raw"""


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SymTri${MATRIX}Test` object is used to test the `SymTri$MATRIX` class.
 *  > runMain scalation.linalgebra.SymTri${MATRIX}Test
 */
object SymTri${MATRIX}Test extends App
{
    val a = new SymTri$MATRIX ($VECTOR (3, 4, 5),
                               $VECTOR (1, 2))

    val b = new SymTri$MATRIX ($VECTOR (2, 3, 4),
                               $VECTOR (5, 6))

    val v = $VECTOR (5, 3, 6)

    val c = new $MATRIX ((3, 3), 3, 1, 0,
                                 1, 4, 2,
                                 0, 2, 5)

    val d = new $MATRIX ((3, 3), 2, 5, 0,
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

    val (l, u) = a.lud_npp
    println ("l  = " + l)
    println ("u  = " + u)
    println ("lu = " + (l * u))
    println ("v  = " + v)

    val x1 = a.solve (l, u, v)
    val x2 = a.solve (v)
    println ("a.solve (l, u, v) = " + x1)
    println ("a.solve (v)       = " + x2)
    println ("a * x1 = " + a * x1)
    println ("a * x2 = " + a * x2)

    println ("a.inverse     = " + a.inverse)
    println ("a.inverse * a = " + a.inverse * a)

} // SymTri${MATRIX}Test object

"""

// Ending of string holding code template --------------------------------------

//      println (code); println (code2); println (code3); println (code4)
//      println (code5); prinln (code6); println (code7); println (code8)
//      println (code9); println (code10); println (code11); println (code12)
//      println (code13); println (code14)

        val writer = new PrintWriter (new File (DIR + _l + "SymTri" + MATRIX + ".scalaa"))
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
        writer.close ()
    } // for

} // BldSymTriMatrix object

