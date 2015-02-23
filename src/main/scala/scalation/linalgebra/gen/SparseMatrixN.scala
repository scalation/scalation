
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Sat May 22 15:24:17 EDT 2010
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.gen

import collection.mutable.LinkedEntry
import reflect.ClassTag

import scalation.math.Basic.oneIf
import scalation.util.{Error, SortedLinkedHashMap}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SparseMatrices` object contains convenience definitions for commonly used
 *  types of sparse matrices.
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
/** The `SparseMatrixN` class stores and operates on Numeric Matrices of various
 *  sizes and types.  The element type may be any subtype of Numeric.  Rather
 *  than storing the matrix as a 2 dimensional array, it is stored as an array
 *  of list-maps, which record all the non-zero values for each particular row,
 *  along with their j-index.
 *  @param d1  the first/row dimension
 *  @param d2  the second/column dimension
 *  @param _0  the value zero for type T
 */
class SparseMatrixN [T: ClassTag: Numeric] (d1: Int,
                                            d2: Int,
                                            _0: T)
      extends Matrix [T] with Error with Serializable
{
    /** Dimension 1
     */
    lazy val dim1 = d1

    /** Dimension 2
     */
    lazy val dim2 = d2

    /** Type definition for matrix rows
     */
    type RowMap = SortedLinkedHashMap [Int, T]

    /** Store the matrix as an array of sorted-linked-maps {(j, v)}
     *  where j is the second index and v is value to store
     */
    private val v = new Array [RowMap] (d1)
    for (i <- 0 until d1) v(i) = new RowMap ()

    /** Import Numeric evidence (nu value from superclass)
     */
    val nu = implicitly [Numeric [T]]
    import nu._

    /** Numeric one (1)
     */
    val _1 = nu.one 
     
    /** Numeric minus one (-1)
     */
    val _1n = -_1 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim2 sparse matrix from an array of sorted-linked-maps.
     *  @param dim1  the row dimension
     *  @param dim2  the column dimension
     *  @param u     the array of sorted-linked-maps
     *  @param _0    the value zero for type T
     */
    def this (dim1: Int, dim2: Int, u: Array [SortedLinkedHashMap [Int, T]], _0: T)
    {
        this (dim1, dim2, _0: T)
        if (u.length != dim1) flaw ("contructor", "dimension is not matched!")
        for (i <- 0 until dim1) v(i) = u(i)
    } // constructor

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
    def apply (i: Int, j: Int): T = if (v(i) contains j) v(i)(j) else _0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this matrix's vector at the i-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorN [T] =
    {
        val a = Array.ofDim [T] (dim2)
        for (j <- 0 until dim2) a(j) = this(i, j)
        new VectorN [T] (a)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range ir and column-wise on range jr.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): SparseMatrixN [T] = slice (ir.start, ir.end, jr.start, jr.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range ir and column-wise at index j.
     *  Ex: u = a(2..4, 3)
     *  @param ir  the row range
     *  @param j   the column index
     */
    def apply (ir: Range, j: Int): VectorN [T] = col(j)(ir)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise at index i and column-wise on range jr.
     *  Ex: u = a(2, 3..5)
     *  @param i   the row index
     *  @param jr  the column range
     */
    def apply (i: Int, jr: Range): VectorN [T] = this(i)(jr)

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
    def update (i: Int, u: RowMap) { v(i) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise on range ir and column-wise on range jr.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: SparseMatrixN [T])
    {
        for (i <- ir; j <- jr) this(i, j) = b(i-ir.start, j-jr.start)
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise on range ir and column-wise at index j.
     *  Ex: a(2..4, 3) = u
     *  @param ir  the row range
     *  @param j   the column index
     *  @param u   the vector to assign
     */
    def update (ir: Range, j: Int, u: VectorN [T]) { col(j)(ir) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise at index i and column-wise on range jr.
     *  Ex: a(2, 3..5) = u
     *  @param i   the row index
     *  @param jr  the column range
     *  @param u   the vector to assign
     */
    def update (i: Int, jr: Range, u: VectorN [T]) { this(i)(jr) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in this matrix to the scalar x.
     *  @param x  the scalar value to assign
     */
    def set (x: T)
    {
        throw new NoSuchMethodException ("use a dense matrix instead")
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in this matrix as copies of the values in 2D array u.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [T]])
    {
        for (i <- range1; j <- range2) this(i, j) = u(i)(j)
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's ith row starting at column j to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectorN [T], j: Int = 0)
    {
        for (k <- 0 until u.dim) this(i, k+j) = u(k)
    } // set

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
            if (v(i) contains j) c.v(i - oneIf (i > row))(j - oneIf (j > col)) = this(i, j)
        } // for
        c
    } // sliceExclude

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from this matrix according to the given index/basis.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (rowIndex.length, dim2, _0)
        for (i <- c.range1) c(i) = this(rowIndex(i))
        c
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from the matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): VectorN [T] =
    {
        val u = new VectorN [T] (dim1 - from)
        for (i <- from until dim1) u(i-from) = this(i, col)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of the matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectorN [T]) { for (i <- range1) this(i, col) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from this matrix according to the given index/basis.
     *  Ex: Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, colIndex.length, _0)
        for (j <- c.range2) c.setCol (j, col(colIndex(j)))
        c
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this matrix (rows => columns).
     */
    def t: SparseMatrixN [T] =
    {
        val b = new SparseMatrixN [T] (dim2, dim1, _0)
        for (i <- b.range2; e <- v(i)) b(e._1, i) = e._2
        b
    } // t

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this sparse matrix and vector u.
     *  @param u  the vector to be concatenated as the new last row in matrix
     */
    def ++ (u: VectorN [T]): SparseMatrixN [T] =
    {
        if (u.dim != dim2) flaw ("++", "vector does not match row dimension")

        val c = new SparseMatrixN [T] (dim1 + 1, dim2, _0)
        for (i <- c.range1) c(i) = if (i < dim1) this(i) else u
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this sparse matrix and sparse matrix b.
     *  @param b  the matrix to add (requires sameCrossDimensions)
     */
    def + (b: SparseMatrixN [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (this, _0)
        for (i <- range1; e <- b.v(i)) c(i, e._1) += e._2
        c
    } // +

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
    /** Add this matrix and scalar x.
     *  @param x  the scalar to add
     */
    def + (x: T): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) + x
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this sparse matrix and sparse matrix b.
     *  @param b  the matrix to add (requires sameCrossDimensions)
     */
    def += (b: SparseMatrixN [T]): SparseMatrixN [T] =
    {
        for (i <- range1; e <- b.v(i)) this(i, e._1) = this(i, e._1) + e._2
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and matrix b.
     * @param b  the matrix to add (requires leDimensions)
     */
    def += (b: Matrix [T]): SparseMatrixN [T] =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) + b(i, j)
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this matrix and scalar x.
     *  @param x  the scalar to add
     */
    def += (x: T): SparseMatrixN [T] =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) + x
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix substract matrix b.
     *  @param b  the sparse matrix to subtract (requires sameCrossDimensions)
     */
    def - (b: SparseMatrixN [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (this, _0)
        for (i <- range1; e <- b.v(i)) c(i, e._1) -= e._2
        c
    } // -

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
    /** From this matrix subtract scalar x.
     *  @param x  the scalar to subtract
     */
    def - (x: T): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) - x
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix substract in-place sparse matrix b.
     *  @param b  the sparse matrix to subtract (requires sameCrossDimensions)
     */
    def -= (b: SparseMatrixN [T]): SparseMatrixN [T] =
    {
        for (i <- range1; e <- b.v(i)) this(i, e._1) = this(i, e._1) - e._2
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place matrix b.
     *  @param b  the matrix to subtract (requires leDimensions)
     */
    def -= (b: Matrix [T]): SparseMatrixN [T] =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) - b(i, j)
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this matrix subtract in-place scalar x.
     *  @param x  the scalar to subtract
     */
    def -= (x: T): SparseMatrixN [T] =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) - x
        this
    } // -=

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this sparse matrix by sparse matrix b, by performing a merge
     *  operation on the rows on this sparse matrix and the transpose of the
     *  b matrix.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: SparseMatrixN [T]): SparseMatrixN [T] =
    {
        if (dim2 != b.dim1) flaw ("*", "matrix * matrix - incompatible cross dimensions")

        val c  = new SparseMatrixN [T] (dim1, b.dim2, _0)
        val bt = b.t                            // transpose the b matrix (for row access)
        for (i <- c.range1) {
            var ea: LinkedEntry [Int, T] = null
            var eb: LinkedEntry [Int, T] = null
            for (j <- c.range2) {
                ea = v(i).getFirstEntry()
                eb = bt.v(j).getFirstEntry()
                var cont = false
                var itaNext = false               // more elements in row of this matrix?
                var itbNext = false               // more elements in row of bt matrix?
                var sum = _0
                if (ea != null && eb != null) cont = true
                while (cont) {
                    if (itaNext) ea = ea.later
                    if (itbNext) eb = eb.later
                    if (ea.key == eb.key) {            // matching indexes
                        sum += ea.value * eb.value
                        itaNext = true; itbNext = true
                    } else if (ea.key > eb.key) {
                        itaNext = false; itbNext = true
                    } else if (ea.key < eb.key) {
                        itaNext = true; itbNext = false
                    } // if
                    if (itaNext && ea.later == null) cont = false
                    if (itbNext && eb.later == null) cont = false
                } // while
                if (sum != _0) c(i, j) = sum         // assign if non-zero
            } // for
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by matrix b.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: Matrix [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, b.dim2, _0)
        for (i <- c.range1; j <- c.range2) {
            var sum = _0
            for (e <- v(i)) sum += e._2 * b(e._1, j)
            c(i, j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector u.
     *  @param u  the vector to multiply by
     */
    def * (u: VectorN [T]): VectorN [T] =
    {
        val c = new VectorN [T] (dim1)
        for (i <- range1) {
            var sum = _0
            for (e <- v(i)) sum += e._2 * u(e._1)
            c(i) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by scalar x.
     *  @param x  the scalar to multiply by
     */
    def * (x: T): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- range1; e <- v(i)) c(i, e._1) = x * e._2
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this sparse matrix by sparse matrix b, by performing a
     *  merge operation on the rows on this sparse matrix and the transpose of
     *  the b matrix.
     *  @param b  the matrix to multiply by (requires square and sameCrossDimensions)
     */
    def *= (b: SparseMatrixN [T]): SparseMatrixN [T] =
    {
        if (! b.isSquare)   flaw ("*=", "matrix b must be square")
        if (dim2 != b.dim1) flaw ("*=", "matrix *= matrix - incompatible cross dimensions")

        val bt = b.t                              // transpose the b matrix (for row access)
        for (i <- range1) {
            var ea: LinkedEntry[Int, T] = null
            var eb: LinkedEntry[Int, T] = null
            val temp = new RowMap ()
            for (e <- v(i)) temp(e._1) = e._2     // copy a new SortedLinkedHashMap
            for (j <- range2) {
                ea = temp.getFirstEntry ()
                eb = bt.v(j).getFirstEntry ()
                var cont = false
                var itaNext = false               // more elements in row of this matrix?
                var itbNext = false               // more elements in row of bt matrix?
                var sum = _0
                if (ea != null && eb != null) cont = true
                while (cont) {
                    if (itaNext) ea = ea.later
                    if (itbNext) eb = eb.later
                    if (ea.key == eb.key) {            // matching indexes
                        sum += ea.value * eb.value
                        itaNext = true; itbNext = true
                    } else if (ea.key > eb.key) {
                        itaNext = false; itbNext = true
                    } else if (ea.key < eb.key) {
                        itaNext = true; itbNext = false
                    } // if
                    if (itaNext && ea.later == null) cont = false
                    if (itbNext && eb.later == null) cont = false
                } // while
                this(i, j) = sum         // assign if non-zero
            } // for
        } // for
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by matrix b.  If b and this reference the
     *  same matrix (b == this), a copy of the this matrix is made.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def *= (b: Matrix [T]): SparseMatrixN [T] =
    {
        if (! b.isSquare)   flaw ("*=", "matrix b must be square")
        if (dim2 != b.dim1) flaw ("*=", "matrix *= matrix - incompatible cross dimensions")

        for (i <- range1) {
            val temp = new RowMap ()   // save so not overwritten
            for (e <- v(i)) temp(e._1) = e._2
            for (j <- range2) {
                var sum = _0
                for (e <- temp) sum += e._2 * b(e._1, j)
                this(i, j) = sum
            } // for
        } // for
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by scalar x.
     *  @param x  the scalar to multiply by
     */
    def *= (x: T): SparseMatrixN [T] =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) * x
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this matrix by vector u to produce another matrix (a_ij * b_j)
     *  @param u  the vector to multiply by
     */
    def ** (u: VectorN [T]): SparseMatrixN [T] =
    {
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) * u(j)
        c
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this matrix by vector u to produce another matrix (a_ij * b_j)
     *  @param u  the vector to multiply by
     */
    def **= (u: VectorN [T]): SparseMatrixN [T] =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) * u(j)
        this
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this sparse matrix by scalar x.
     *  @param x  the scalar to divide by
     */
    def / (x: T) (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        val c = new SparseMatrixN [T] (dim1, dim2, _0)
        for (i <- range1; e <- v(i)) c(i, e._1) = e._2 / x
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this sparse matrix by scalar x.
     *  @param x  the scalar to divide by
     */
    def /= (x: T) (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        for (i <- range1; e <- v(i)) this(i, e._1) = e._2 / x
        this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise this sparse matrix to the pth power (for some integer p >= 2).
     *  Caveat: should be replace by a divide and conquer algorithm.
     *  @param p  the power to raise this matrix to
     */
    def ~^ (p: Int): SparseMatrixN [T] =
    {
        if (p < 2)      flaw ("~^", "p must be an integer >= 2")
        if (! isSquare) flaw ("~^", "only defined on square matrices")

        var c = new SparseMatrixN [T] (this, _0)
        for (i <- 0 until p-1) c *= c
        c
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this sparse matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): T =
    {
        var x = getMaxVal(v(0))
        for (i <- 1 until e) {
            val max = getMaxVal (v(i))
            if (max > x) x = max
        } // for
        x
    } // max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in SortedLinkHashMap.
     *  @param u  the SortedLinkHashMap for the search
     */
    private def getMaxVal (u: RowMap): T =
    {
        var x = if (u contains 0) u(0) else _0
        for (e <- u) if (e._2 > x) x = e._2
        x
    } // getMaxVal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this sparse matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): T =
    {
        var x = getMinVal(v(0))
        for (i <- 1 until e) {
            val min = getMinVal (v(i))
            if (min < x) x = min
        } // for
        x
    } // min

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in SortedLinkHashMap.
     *  @param u  the SortedLinkHashMap for the search
     */
    private def getMinVal (u: RowMap): T =
    {
        var x = if (u contains 0) u(0) else _0
        for (e <- u) if (e._2 < x) x = e._2
        x
    } // getMinVal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the magnitude of this matrix, the element value farthest from zero.
     */
    def mag: T = nu.abs (max ()) max nu.abs (min ())

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
            if (pivot == _0) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                swap (u, i, k, i)                // swap rows i and k from column k
                pivot = u(i, i)                  // reset the pivot
            } // if
            l(i, i) = _1
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
            if (pivot == _0) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                swap (u, i, k, i)                // swap rows i and k from column k
                pivot = u(i, i)                  // reset the pivot
            } // if
            l(i, i) = _1
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
     *  filling in the bottom left and top right regions with zeros; [this, b].
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
            c(i, j) = if (i < p || i > p + dim1) if (i == j) _1 else _0
                    else                         this(i-p, j-p)
        } // for
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the kth diagonal of this matrix.  Assumes dim2 >= dim1.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): VectorN [T] =
    {
        val mm = dim1 - math.abs (k)
        val c  = new VectorN [T] (mm)
        for (i <- 0 until mm) c(i) = this(i, i+k)
        c
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the kth diagonal of this matrix to the vector u.  Assumes dim2 >= dim1.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorN [T], k: Int = 0)
    {
        val mm = dim1 - math.abs (k)
        for (i <- 0 until mm) this(i, i+k) = u(i)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of this matrix to the scalar x.  Assumes dim2 >= dim1.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: T) { for (i <- range1) this(i, i) = x }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this matrix (requires a squareMatrix) and not using partial pivoting.
     */
    def inverse_npp (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        val b = new SparseMatrixN [T] (this, _0)        // copy this matrix into b
        val c = new SparseMatrixN [T] (dim1, _1, _0)    // let c represent the augmentation

        for (i <- b.range1) {
            val pivot = b(i, i)
            if (pivot == _0) flaw ("inverse_npp", "use inverse since you have a zero pivot")
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
    /** Invert this matrix (requires a squareMatrix) using partial pivoting.
     */
    def inverse (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        import fr._
        val b = new SparseMatrixN [T] (this, _0)        // copy this matrix into b
        val c = new SparseMatrixN [T] (dim1, _1, _0)    // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == _0) {
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
        val c = new SparseMatrixN [T] (dim1, _1, _0)    // let c represent the augmentation

        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == _0) {
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
            if (pivot == _0) {
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
            if (pivot == _0) {
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
    /** Clean values in matrix at or below the threshold by setting them to zero.
     *  Iterative algorithms give approximate values and if very close to zero,
     *  may throw off other calculations, e.g., in computing eigenvectors.
     *  @param thres     the cutoff threshold (a small value)
     *  @param relative  whether to use relative or absolute cutoff
     */
    def clean (thres: T, relative: Boolean = true)
              (implicit fr: Fractional [T]): SparseMatrixN [T] =
    {
        val s = if (relative) mag else _1             // use matrix magnitude or 1
        for (i <- range1; j <- range2) if (nu.abs (this(i, j)) <= thres * s) this(i, j) = _0
        this
    } // clean

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
        reduce.col(dim2 - 1) * _1n ++ _1
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
        col(dim2 - 1) * _1n ++ _1
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of this matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: T =
    {
        if ( ! isSquare) flaw ("trace", "trace only works on square matrices")
        var sum = _0
        for (i <- range1) sum += this(i, i)
        sum
    } // trace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of this matrix, i.e., the sum of its elements.
     */
    def sum: T =
    {
        var sum = _0
        for (i <- range1; j <- range2) sum += this(i, j)
        sum
    } // sum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of this matrix.
     */
    def sumLower: T =
    {
        var sum = _0
        for (i <- range1; j <- 0 until i) sum += this(i, j)
        sum
    } // sumLower

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the abs sum of this matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices (a - b).sumAbs
     */
    def sumAbs: T =
    {
        var sum = _0 
        for (i <- range1; j <- range2) sum += abs (this(i, j))
        sum
    } // sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 1-norm of this matrix, i.e., the maximum 1-norm of the
     *  column vectors.  This is useful for comparing matrices (a - b).norm1
     */
    def norm1: T =
    {
        val c = new VectorN [T] (dim2)
        for (j <- range2) c(j) = col(j).norm1
        c.max ()
    } // norm1

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
        for (i <- range1; j <- range2 if this(i, j) < _0) return false
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
/** The `SparseMatrixNTest` object is used to test the `SparseMatrixN` class.
 */
object SparseMatrixNTest extends App
{
     import SparseMatrices._

     val y = new SparseMatrixF (3, 3, 7.0f, 0.0f)
     val z = new SparseMatrixF (3, 8.0f, 0.0f)
     val a = y + z
     val b = y - z
     val c = y * z

     println ("y     = " + y); y.showAll
     println ("z     = " + z); z.showAll
     println ("y + z = " + a); a.showAll
     println ("y - z = " + b); b.showAll
     println ("y * z = " + c); c.showAll

     type RowMap = SortedLinkedHashMap [Int, Float]

     val x = new SparseMatrixF (12, 0.0f)
     x(0)  = new RowMap ((1, 1.0f), (5, 1.0f), (6, 1.0f), (9, 1.0f))
     x(1)  = new RowMap ((0, 1.0f), (2, 1.0f), (4, 1.0f))
     x(2)  = new RowMap ((1, 1.0f), (3, 1.0f), (4, 1.0f))
     x(3)  = new RowMap ((2, 1.0f), (7, 1.0f), (8, 1.0f), (10, 1.0f))
     x(4)  = new RowMap ((1, 1.0f), (2, 1.0f), (6, 1.0f), (7, 1.0f))
     x(5)  = new RowMap ((0, 1.0f), (9, 1.0f))
     x(6)  = new RowMap ((0, 1.0f), (4, 1.0f), (9, 1.0f))
     x(7)  = new RowMap ((3, 1.0f), (4, 1.0f), (8, 1.0f), (10, 1.0f))
     x(8)  = new RowMap ((3, 1.0f), (7, 1.0f), (10, 1.0f), (11, 1.0f))
     x(9)  = new RowMap ((0, 1.0f), (5, 1.0f), (6, 1.0f))
     x(10) = new RowMap ((3, 1.0f), (7, 1.0f), (8, 1.0f), (11, 1.0f))
     x(11) = new RowMap ((8, 1.0f))

     println ("x     = " + x); x.showAll

} // SparseMatrixNTest object

