
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/**:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 * @author  Robert Davis, John Miller
 * @version 1.0
 * @date    Sun Sep 16 14:09:25 EDT 2012
 * @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import math.{max, min}

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
     *  @param a  the matrix of values to assign
     */
    def this (a: Matrix)
    {
        this (a.dim1)
        for (i <- range_d) _dg(i) = a(i, i)
        for (i <- range_s) _sd(i) = a(i, i+1)
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
        val v = new VectorD (d1)
        v(i)  = _dg(i)
        if (i > 0)    v(i-1) = _sd(i-1)
        if (i < d1_1) v(i+1) = _sd(i)
        v
    } // apply

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
    /** Transpose this matrix (rows => columns).  Note, since the matrix is
     *  symmetric, it returns itself.
     */
    def t: SymTriMatrixD = this

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
    /** Add this matrix and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: Double): Matrix = new SymTriMatrixD (_dg + s, _sd + s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and scalar s.
     * @param s  the scalar to add
     */
    def += (s: Double) = { _dg += s; _sd += s }

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
    /** From this matrix subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: Double): Matrix = new SymTriMatrixD (_dg - s, _sd - s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place scalar s.
     *  @param s  the scalar to subtract
     */
    def -= (s: Double) = { _dg -= s; _sd -= s }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector b.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorD): VectorD = 
    {
        val c = new VectorD (d1)
        c(0)  = b(0) * _dg(0) + _sd(0) * b(1)
        for (i <- 1 until d1_1) {
            c(i) = _sd(i-1) * b(i-1)
            c(i) = c(i) + _dg(i) * b(i)
            c(i) = c(i) + _sd(i+1) * b(i+1)
        } // for
        c(d1-1) = _sd(d1-2) * b(d1-2) + _dg(d1-1) * b(d1-1)
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): Matrix = new SymTriMatrixD (_dg * s, _sd * s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by scalar s.
     *  @param s  the scalar to multiply by
     */
    def *= (s: Double) = { _dg *= s; _sd *= s }

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
     * @param d  the constant vector.
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
            val id = 1. / (b(i) - c(i-1) * a(i))
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
    /** Get row 'r' from the matrix, returning it as a vector.
     *  @param r     the row to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def row (r: Int, from: Int = 0): VectorD =
    {
        val u = new VectorD (dim2 - from)
        for (j <- max (from, r-1) until min (dim2, r+2)) u(j-from) = this(r, j)
        u
    } // row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'c' from the matrix, returning it as a vector.
     *  @param c     the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (c: Int, from: Int = 0): VectorD =
    {
        val u = new VectorD (dim1 - from)
        for (i <- max (from, c-1) until min (dim1, c+2)) u(i-from) = this(i, c)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., s*v*a = 0.  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace: VectorD =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce.col(dim2 - 1) * -1. ++ 1.
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace in-place of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., s*v*a = 0.  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace_ip: VectorD =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce_ip
        col(dim2 - 1) * -1. ++ 1.
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the main diagonal of this matrix.  Assumes dim2 >= dim1.
     */
    def getDiag (): VectorD = _dg

    //--------------------------------------------------------------------------
    // The following methods are not useful for Symmetric Tridiagonal matrices:
    //--------------------------------------------------------------------------

    def slice (from: Int, end: Int): Matrix = 
    {
        throw new NoSuchMethodException ("convert to other matrix type first")
    } // slice

    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): Matrix = 
    {
        throw new NoSuchMethodException ("convert to other matrix type first")
    } // slice

    def sliceExclude (row: Int, col: Int): Matrix =
    {
        throw new NoSuchMethodException ("convert to other matrix type first")
    } // sliceExclude

    def * (b: Matrix): Matrix = 
    {
        throw new Exception("convert to other matrix type first")
    } // *

    def *= (b: Matrix)
    {
        throw new Exception("convert to other matrix type first")
    } // *=

    def ** (b: VectorD): Matrix = 
    {
        throw new NoSuchMethodException ("convert to other matrix type first")
    } // **

    def **= (b: VectorD)
    {
        throw new NoSuchMethodException ("convert to other matrix type first")
    } // **=

    def lud: Tuple2 [Matrix, Matrix] =
    {
        throw new NoSuchMethodException ("not implemented")
    } // lud

    def lud_ip: Tuple2 [Matrix, Matrix] = 
    {
        throw new NoSuchMethodException ("not implemented")
    } // lud_ip

    def solve (l: Matrix, u: Matrix, b: VectorD): VectorD = 
    {
        throw new NoSuchMethodException ("not implemented")
    } // solve

    def solve (lu: Tuple2 [Matrix, Matrix], b: VectorD): VectorD = 
    {
        throw new NoSuchMethodException ("not implemented")
    } // solve

    def diag (b: Matrix): Matrix = 
    {
        throw new NoSuchMethodException ("not implemented")
    } // diag

    def diag (p: Int, q: Int): Matrix = 
    {
        throw new NoSuchMethodException("not implemented")
    } // diag

    def inverse_npp: Matrix = 
    {
        throw new NoSuchMethodException("not implemented")
    } // inverse_npp

    def inverse: Matrix = 
    {
        throw new NoSuchMethodException("not implemented")
    } // inverse

    def inverse_ip: Matrix = 
    {
        throw new NoSuchMethodException("not implemented")
    } // inverse_ip

    def reduce: Matrix = 
    {
        throw new NoSuchMethodException("not implemented")
    } // reduce

    def reduce_ip
    {
        throw new NoSuchMethodException("not implemented")
    } // reduce_ip

} // SymTriMatrixD class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SymTriMatrixD class.
 */
object SymTriMatrixDTest extends App
{
    val a = new SymTriMatrixD (new VectorD (1., 2., 3.),
                               new VectorD (4., 5.))

    val b = new SymTriMatrixD (new VectorD (2., 3., 4.),
                               new VectorD (5., 6.))

    println ("a     = " + a)
    println ("b     = " + b)
    println ("a.det = " + a.det)	
    println ("a + b = " + (a + b))	

} // SymTriMatrixDTest object

