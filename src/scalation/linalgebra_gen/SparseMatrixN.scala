
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat May 22 15:24:17 EDT 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra_gen

import collection.mutable.ListMap
import math.abs

import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convenience definitions for commonly used types of sparse matrices.
 */
object SparseMatrices
{
    type SparseMatrixI = SparseMatrixN [Int]
    type SparseMatrixL = SparseMatrixN [Long]
    type SparseMatrixF = SparseMatrixN [Float]
//  type SparseMatrixD = SparseMatrixN [Double]   // see linalgebra package for efficient impl
//  type SparseMatrixC = SparseMatrixN [Complex]  // see linalgebra package for efficient impl

} // SparseMatrices object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SparseMatrixN class stores and operates on Numeric Matrices of various
 *  sizes and types.  The element type may be any subtype of Numeric.  Rather
 *  than storing the matrix as a 2 dimensional array, it is stored as an array
 *  of list-maps, which record all the non-zero values for each particular row,
 *  along with their j-index.
 *  @param d1  the first/row dimension
 *  @param d2  the second/column dimension
 *  @param _0  the value zero for type T
 */
class SparseMatrixN [T: ClassManifest: Numeric] (d1: Int,
                                                 d2: Int,
                                                 _0: T)
      extends Matrix [T] with Error with Serializable
{
    lazy val dim1 = d1
    lazy val dim2 = d2

    /** Store the matrix as an array of list-maps (j-index, value)
     */
    private val v = new Array [ListMap [Int, T]] (dim1)

    for (i <- range1) v(i) = new ListMap [Int, T] ()

    /** Import Numeric evidence (nu value from superclass)
     */
    val nu = implicitly [Numeric [T]]
    import nu._

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim1 square sparse matrix.
     *  @param dim1  the row and column dimension
     *  @param _0    the value zero for type T
     */
    def this (dim1: Int, _0: T) { this (dim1, dim1, _0) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim2 sparse matrix and assign each element the value x.
     *  @param dim1  the row dimension
     *  @param dim2  the column dimesion
     *  @param x     the scalar value to assign
     *  @param _0    the value zero for type T
     */
    def this (dim1: Int, dim2: Int, x: T, _0: T)
    {
        this (dim1, dim2, _0)                      // invoke primary constructor
        if (x != _0) for (i <- range1; j <- range2.reverse) v(i)(j) = x
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim1 square sparse matrix with x assigned on the diagonal
     *  and 0 assigned off the diagonal.  To obtain an identity matrix, let x = 1.
     *  @param dim1  the row and column dimension
     *  @param x     the scalar value to assign on the diagonal
     *  @param _0    the value zero for type T
     */
    def this (dim1: Int, x: T, _0: T)
    {
        this (dim1, dim1, _0)                      // invoke primary constructor
        if (x != _0) for (i <- range1) v(i)(i) = x
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a sparse matrix and assign values from matrix u.
     *  @param u   the matrix of values to assign
     *  @param _0  the value zero for type T
     */
    def this (u: SparseMatrixN [T], _0: T)
    {
        this (u.dim1, u.dim2, _0)                  // invoke primary constructor
        for (i <- range1; j <- range2) this(i, j) = u(i, j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a sparse matrix and assign values from (MatrixN) matrix u.
     *  @param u   the matrix of values to assign
     *  @param _0  the value zero for type T
     */
    def this (u: MatrixN [T], _0: T)
    {
        this (u.dim1, u.dim2, _0)                  // invoke primary constructor
        for (i <- range1; j <- range2) this(i, j) = u(i, j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a sparse matrix and assign values from (SymTriMatrixN) matrix u.
     *  @param u   the matrix of values to assign
     *  @param _0  the value zero for type T
     */
    def this (u: SymTriMatrixN [T], _0: T)
    {
        this (u.d1, u.d1, _0)                      // invoke primary constructor
        this(0, 0) = u.dg(0)
        this(0, 1) = u.sd(0)
        for (i<-1 until dim1-1) {
            this(i, i - 1) = u.sd(i - 1)
            this(i, i)     = u.dg(i)
            this(i, i + 1) = u.sd(i)
        } // for
        this(dim1 - 1, dim1 - 2) = u.sd(dim1 - 2)
        this(dim1 - 1, dim1 - 1) = u.dg(dim1 - 1)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's element at the i,j-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): T =
    {
        var v_ij = _0
        try v_ij = v(i)(j)
        catch { case nsee: NoSuchElementException => }
        v_ij
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's vector at the i-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorN [T] =
    {
        val a = Array.ofDim [T] (dim2)
        for (j <- 0 until dim2) {
            try a(j) = v(i)(j)
            catch { case nsee: NoSuchElementException => a(j) = _0 }
        } // for
        new VectorN [T] (a)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's element at the i,j-th index position to the scalar x.
     *  Only store x if it is non-zero.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: T) { if (x != _0) v(i)(j) = x else v(i) -= j }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's row at the i-th index position to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorN [T])
    {
        for (j <- 0 until u.dim) {
            val x = u(j)
            if (x != _0) v(i)(j) = x else v(i) -= j
        } // for
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's row at the i-th index position to the list-map u.
     *  @param i  the row index
     *  @param u  the list-map of non-zreo values to assign
     */
    def update (i: Int, u: ListMap [Int, T]) { v(i) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get row 'r' from the matrix, returning it as a vector.
     *  @param r     the row to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def row (r: Int, from: Int = 0): VectorN [T] =
    {
        val u = new VectorN [T] (dim2 - from)
        for (j <- from until dim2) u(j-from) = this(r, j)
        u
    } // row

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'c' from the matrix, returning it as a vector.
     *  @param c     the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (c: Int, from: Int = 0): VectorN [T] =
    {
        val u = new VectorN [T] (dim1 - from)
        for (i <- from until dim1) u(i-from) = this(i, c)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise from to end.
     *  @param from  the start row of the slice
     *  @param end   the end row of the slice
     */
    def slice (from: Int, end: Int): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (end - from, dim2, _0)
        for (i <- 0 until c.dim1) c(i) = this(i + from)
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix row-wise r_from to r_end and column-wise c_from to c_end.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (r_end - r_from, c_end - c_from, _0)
        for (i <- 0 until c.dim1; j <- 0 until c.dim2) c(i, j) = this(i + r_from, j + c_from)
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this matrix excluding the given row and column.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1 - 1, dim2 - 1, _0)
        for (i <- range1 if i != row) for (j <- range2 if j != col) {
            c.v(i - oneIf (i > row))(j - oneIf (j > col)) = this(i, j)
        } // for
        c
    } // sliceExclude

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this matrix (rows => columns).
     */
    def t: SparseMatrixN [T] =
    {
        val b = new SparseMatrixN [T] (dim2, dim1, _0)
        for (i <- b.range1; j <- b.range2) b(i, j) = this(j, i)
        b
    } // t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and matrix b.
     *  @param b  the matrix to add (requires leDimensions)
     */
    def + (b: Matrix [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) + b(i, j)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and matrix b.
     * @param b  the matrix to add (requires leDimensions)
     */
    def += (b: Matrix [T])
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) + b(i, j)
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this matrix and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: T): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) + s
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and scalar s.
     *  @param s  the scalar to add
     */
    def += (s: T)
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) + s
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract matrix b.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def - (b: Matrix [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) - b(i, j)
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place matrix b.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def -= (b: Matrix [T])
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) - b(i, j)
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract scalar s.
     *  @param s  the scalar to subtract
     */
    def - (s: T): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) - s
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place scalar s.
     *  @param s  the scalar to subtract
     */
    def -= (s: T)
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) - s
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by matrix b.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     *
    def * (b: Matrix [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, b.dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = row(i) dot b.col(j)
        c
    } // *
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by matrix b.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: Matrix [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, b.dim2, _0)
        for (i <- c.range1; j <- c.range2) {
            var sum = _0
            for ((k, v_ik) <- v(i)) sum += v_ik * b(k, j)
            c(i, j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by matrix b.  If b and this reference the
     *  same matrix (b == this), a copy of the this matrix is made.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def *= (b: Matrix [T])
    {
        val c = if (b == this) new SparseMatrixN [T] (this, _0) else b
        for (i <- range1) {
            val row_i = new VectorN [T] (row(i))          // save so not overwritten
            for (j <- range2) this(i, j) = row_i dot c.col(j)
        } // for
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector b.
     *  @param b  the vector to multiply by
     */
    def * (b: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim1)
        for (i <- range1) c(i) = row(i) dot b
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: T): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) * s
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by scalar s.
     *  @param s  the scalar to multiply by
     */
    def *= (s: T)
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) * s
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector b to produce another matrix (a_ij * b_j)
     *  @param b  the vector to multiply by
     */
    def ** (b: VectorN [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) * b(j)
        c
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by vector b to produce another matrix (a_ij * b_j)
     *  @param b  the vector to multiply by
     */
    def **= (b: VectorN [T])
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) * b(j)
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose this matrix into the product of lower and upper triangular
     *  matrices (l, u) using the LU Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud (implicit fr: Fractional [T]): Tuple2 [SparseMatrixN [T], SparseMatrixN [T]] =
    {
        import fr._
        val l = new SparseMatrixN [T] (dim1, dim2, _0)  // lower triangular matrix
        val u = new SparseMatrixN [T] (this, _0)        // upper triangular matrix (a copy of this)

        for (i <- u.range1) {
            var pivot = u(i, i)
            if (pivot == 0.) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                swap (u, i, k, i)                // swap rows i and k from column k
                pivot = u(i, i)                  // reset the pivot
            } // if
            l(i, i) = one
            for (j <- i + 1 until u.dim2) l(i, j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u(k, i) / pivot
                l(k, i) = mul
                for (j <- u.range2) u(k, j) = u(k, j) - mul * u(i, j)
            } // for
        } // for
        Tuple2 (l, u)
    } // lud

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose in-place this matrix into the product of lower and upper triangular
     *  matrices (l, u) using the LU Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud_ip (implicit fr: Fractional [T]): Tuple2 [SparseMatrixN [T], SparseMatrixN [T]] =
    {
        import fr._
        val l = new SparseMatrixN [T] (dim1, dim2, _0)  // lower triangular matrix
        val u = this                                    // upper triangular matrix (this)

        for (i <- u.range1) {
            var pivot = u(i, i)
            if (pivot == 0.) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                swap (u, i, k, i)                // swap rows i and k from column k
                pivot = u(i, i)                  // reset the pivot
            } // if
            l(i, i) = one
            for (j <- i + 1 until u.dim2) l(i, j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u(k, i) / pivot
                l(k, i) = mul
                for (j <- u.range2) u(k, j) = u(k, j) - mul * u(i, j)
            } // for
        } // for
        Tuple2 (l, u)
    } // lud_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use partial pivoting to find a maximal non-zero pivot and return its row
     *  index, i.e., find the maximum element (k, i) below the pivot (i, i).
     *  @param a  the matrix to perform partial pivoting on
     *  @param i  the row and column index for the current pivot
     */
    private def partialPivoting (a: SparseMatrixN [T], i: Int) (implicit fr: Fractional [T]): Int =
    {
        import fr._
        var max  = a(i, i)   // initially set to the pivot
        var kMax = i         // initially the pivot row

        for (k <- i + 1 until a.dim1 if fr.abs (a(k, i)) > max) {
            max  = fr.abs (a(k, i))
            kMax = k
        } // for
        if (kMax == i) flaw ("partialPivoting", "unable to find a non-zero pivot")
        kMax
    } // partialPivoting

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap the elements in rows i and k starting from column col.
     *  @param a    the matrix containing the rows to swap
     *  @param i    the higher row  (e.g., contains a zero pivot)
     *  @param k    the lower row (e.g., contains max element below pivot)
     *  @param col  the starting column for the swap
     */
    private def swap (a: SparseMatrixN [T], i: Int, k: Int, col: Int)
    {
        for (j <- col until a.dim2) {
            val tmp = a(k, j); a(k, j) = a(i, j); a(i, j) = tmp
        } // for
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation l*u*x = b (see lud above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: Matrix [T], u: Matrix [T], b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T] =
    {
        import fr._
        val y = new VectorN [T] (l.dim2)
        for (k <- 0 until y.dim) {                   // solve for y in l*y = b
            var sum = _0
            for (j <- 0 until k) sum = sum + l(k, j) * y(j)
            y(k) = b(k) - sum
        } // for

        val x = new VectorN [T] (u.dim2)
        for (k <- x.dim - 1 to 0 by -1) {            // solve for x in u*x = y
            var sum = _0
            for (j <- k + 1 until u.dim2) sum = sum + u(k, j) * x(j)
            x(k) = (y(k) - sum) / u(k, k)
        } // for
        x
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation l*u*x = b (see lud above).
     *  @param lu  the lower and upper triangular matrices
     *  @param b   the constant vector
     */
    def solve (lu: Tuple2 [Matrix [T], Matrix [T]], b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T] =
    {
        solve (lu._1, lu._2, b)
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation a*x = b where a is this matrix (see lud above).
     *  @param b  the constant vector.
     */
    def solve (b: VectorN [T])
        (implicit fr: Fractional [T]): VectorN [T] =
    {
        solve (lud, b)
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine this matrix with matrix b, placing them along the diagonal and
     *  filling in the bottom left and top right regions with zeroes; [this, b].
     *  @param b  the matrix to combine with this matrix
     */
    def diag (b: Matrix [T]): SparseMatrixN [T] =
    {
        val m = dim1 + b.dim1
        val n = dim2 + b.dim2
        val c = new SparseMatrixN [T] (m, n, _0)

        for (i <- 0 until m; j <- 0 until n) {
            c(i, j) = if (i <  dim1 && j <  dim2) this(i, j)
                 else if (i >= dim1 && j >= dim2) b(i-dim1, j-dim2)
                    else                          _0
        } // for
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form a matrix [Ip, this, Iq] where Ir is a r by r identity matrix, by
     *  positioning the three matrices Ip, this and Iq along the diagonal.
     *  @param p  the size of identity matrix Ip
     *  @param q  the size of identity matrix Iq
     */
    def diag (p: Int, q: Int): SparseMatrixN [T] =
    {
        if (! isSymmetric) flaw ("diag", "this matrix must be symmetric")
        val n  = dim1 + p + q
        val c  = new SparseMatrixN [T] (n, n, _0)

        for (i <- 0 until n; j <- 0 until n) {
            c(i, j) = if (i < p || i > p + dim1) if (i == j) one else _0
                    else                         this(i-p, j-p)
        } // for
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the main diagonal of this matrix.  Assumes dim2 >= dim1.
     */
    def getDiag (): VectorN [T] =
    {
        val c = new VectorN [T] (dim1)
        for (i <- range1) c(i) = this(i, i)
        c
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a squareMatrix) and do not uses partial pivoting.
     */
    def inverse_npp (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        val b = new SparseMatrixN [T] (this, _0)        // copy this matrix into b
        val c = new SparseMatrixN [T] (dim1, one, _0)   // let c represent the augmentation

        for (i <- b.range1) {
            val pivot = b(i, i)
            if (pivot == 0.) flaw ("inverse_npp", "use inverse since you have a zero pivot")
            for (j <- b.range2) {
                b(i, j) = b(i, j) / pivot
                c(i, j) = c(i, j) / pivot
            } // for
            for (k <- 0 until b.dim1 if k != i) {
                val mul = b(k, i)
                for (j <- b.range2) {
                     b(k, j) = b(k, j) - mul * b(i, j)
                     c(k, j) = c(k, j) - mul * c(i, j)
                } // for
            } // for
        } // for
        c
    } // inverse_npp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a squareMatrix) and use partial pivoting.
     */
    def inverse (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        val b = new SparseMatrixN [T] (this, _0)        // copy this matrix into b
        val c = new SparseMatrixN [T] (dim1, one, _0)   // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == 0.) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                swap (b, i, k, i)               // in b, swap rows i and k from column i
                swap (c, i, k, 0)               // in c, swap rows i and k from column 0
                pivot = b(i, i)                 // reset the pivot
            } // if
            for (j <- b.range2) {
                b(i, j) = b(i, j) / pivot
                c(i, j) = c(i, j) / pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                for (j <- b.range2) {
                     b(k, j) = b(k, j) - mul * b(i, j)
                     c(k, j) = c(k, j) - mul * c(i, j)
                } // for
            } // for
        } // for
        c
    } // inverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place this matrix (requires a squareMatrix).  This version uses
     *  partial pivoting.
     */
    def inverse_ip (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        val b = this                                    // use this matrix for b
        val c = new SparseMatrixN [T] (dim1, one, _0)   // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == 0.) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                swap (b, i, k, i)               // in b, swap rows i and k from column i
                swap (c, i, k, 0)               // in c, swap rows i and k from column 0
                pivot = b(i, i)                 // reset the pivot
            } // if
            for (j <- b.range2) {
                b(i, j) = b(i, j) / pivot
                c(i, j) = c(i, j) / pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                for (j <- b.range2) {
                     b(k, j) = b(k, j) - mul * b(i, j)
                     c(k, j) = c(k, j) - mul * c(i, j)
                } // for
            } // for
        } // for
        c
    } // inverse_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction on this matrix to make the left part embed an
     *  identity matrix.  A constraint on this m by n matrix is that n >= m.
     */
    def reduce (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")
        val b = new SparseMatrixN [T] (this, _0)    // copy this matrix into b

        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == 0.) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                swap (b, i, k, i)               // in b, swap rows i and k from column i
                pivot = b(i, i)                 // reset the pivot
            } // if
            for (j <- b.range2) {
                b(i, j) = b(i, j) / pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                for (j <- b.range2) {
                     b(k, j) = b(k, j) - mul * b(i, j)
                } // for
            } // for
        } // for
        b
    } // reduce

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Gauss-Jordan reduction in-place on this matrix to make the left part
     *  embed an identity matrix.  A constraint on this m by n matrix is that n >= m.
     */
    def reduce_ip (implicit fr: Fractional [T])
    {
        import fr._
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")
        val b = this         // use this matrix for b

        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == 0.) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                swap (b, i, k, i)               // in b, swap rows i and k from column i
                pivot = b(i, i)                 // reset the pivot
            } // if
            for (j <- b.range2) {
                b(i, j) = b(i, j) / pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                for (j <- b.range2) {
                     b(k, j) = b(k, j) - mul * b(i, j)
                } // for
            } // for
        } // for
    } // reduce_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace (implicit fr: Fractional [T]): VectorN [T] =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce.col(dim2 - 1) * negate (one) ++ one
    } // nullspace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace in-place of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace_ip (implicit fr: Fractional [T]): VectorN [T] =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce_ip
        col(dim2 - 1) * negate (one) ++ one
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of this matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: T =
    {
        if ( ! isSquare) flaw ("trace", "trace only works on square matrices")
        var sum = zero
        for (i <- range1) sum += this(i, i)
        sum
    } // trace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of this matrix, i.e., the sum of its elements.
     */
    def sum: T =
    {
        var sum = zero
        for (i <- range1; j <- range2) sum += this(i, j)
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of this matrix.
     */
    def sumLower: T =
    {
        var sum = zero
        for (i <- range1; j <- 0 until i) sum += this(i, j)
        sum
    } // sumLower

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of this matrix.
     */
    def det: T =
    {
        if ( ! isSquare) flaw ("det", "determinant only works on square matrices")
        var sum = _0
        var b: SparseMatrixN [T] = null

        for (j <- range2) {
            b = sliceExclude (0, j)   // the submatrix that excludes row 0 and column j
            if (j % 2 == 0) {
                sum = sum + this(0, j) * (if (b.dim1 == 1) b(0, 0) else b.det)
            } else {
                sum = sum - this(0, j) * (if (b.dim1 == 1) b(0, 0) else b.det)
            } // if
        } // for
        sum
    } // det

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is rectangular (all rows have the same number
     * of columns).
     */
    def isRectangular: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this matrix is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range1; j <- range2 if this(i, j) < zero) return false
        true
    } // isNonegative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the non-zero elements in this sparse matrix.
     */
    override def toString: String =
    {
        var sb = new StringBuilder ("\nSparseMatrixN(\t")
        for (i <- range1) {
            sb.append (v(i).toString)
            sb.append (if (i < dim1 - 1) ",\n\t\t" else ")")
        } // for
        sb.mkString

    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show all elements in this sparse matrix.
     */
    def showAll
    {
        print ("SparseMatrixN(")
        for (i <- range1) {
            if (i > 0) print ("\t")
            print ("\t(")
            for (j <- range2) print (this(i, j).toString + (if (j < dim2 - 1) ", " else ")\n"))
        } // for
        println (")")
    } // showAll

} // SparseMatrixN class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SparseMatrixN class.
 */
object SparseMatrixNTest extends App
{
     import SparseMatrices._

     val y = new SparseMatrixF (3, 3, 7.f, 0.f)
     val z = new SparseMatrixF (3, 8.f, 0.f)
     val a = y + z
     val b = y - z
     val c = y * z

     println ("y     = " + y); y.showAll
     println ("z     = " + z); z.showAll
     println ("y + z = " + a); a.showAll
     println ("y - z = " + b); b.showAll
     println ("y * z = " + c); c.showAll

     val x = new SparseMatrixF (12, 0.f)
     x(0)  = ListMap ((1, 1.f), (5, 1.f), (6, 1.f), (9, 1.f))
     x(1)  = ListMap ((0, 1.f), (2, 1.f), (4, 1.f))
     x(2)  = ListMap ((1, 1.f), (3, 1.f), (4, 1.f))
     x(3)  = ListMap ((2, 1.f), (7, 1.f), (8, 1.f), (10, 1.f))
     x(4)  = ListMap ((1, 1.f), (2, 1.f), (6, 1.f), (7, 1.f))
     x(5)  = ListMap ((0, 1.f), (9, 1.f))
     x(6)  = ListMap ((0, 1.f), (4, 1.f), (9, 1.f))
     x(7)  = ListMap ((3, 1.f), (4, 1.f), (8, 1.f), (10, 1.f))
     x(8)  = ListMap ((3, 1.f), (7, 1.f), (10, 1.f), (11, 1.f))
     x(9)  = ListMap ((0, 1.f), (5, 1.f), (6, 1.f))
     x(10) = ListMap ((3, 1.f), (7, 1.f), (8, 1.f), (11, 1.f))
     x(11) = ListMap ((8, 1.f))

     println ("x     = " + x); x.showAll

} // SparseMatrixNTest object

