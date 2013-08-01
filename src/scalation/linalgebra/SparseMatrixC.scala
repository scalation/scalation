
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yung Long Li 
 *  @version 1.0
 *  @date    Sat Nov 10 19:05:18 EST 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scala.collection.mutable.LinkedEntry

import scalation.math.Complex
import scalation.math.Complex.{abs, _0}
import scalation.util.{Error, SortedLinkedHashMap}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for the SparseMatrixC class.
 */
object SparseMatrixC
{
    type RowMap = SortedLinkedHashMap [Int, Complex]

} // SparseMatrixC object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The SparseMatrixC class stores and operates on Matrices of Complex numbers.  Rather
 *  than storing the matrix as a 2 dimensional array, it is stored as an array
 *  of sorted-linked-maps, which record all the non-zero values for each particular
 *  row, along with their j-index as (j, v) pairs.
 *  Note: _npp versions of methods are not appropriate for sparse matrices
 *       (i.e., always use partial pivoting).
 *  @param d1  the first/row dimension
 *  @param d2  the second/column dimension
 */
class SparseMatrixC (val d1: Int, 
                     val d2: Int)
      extends Matric with Error with Serializable
{
    /** Dimension 1
     */
    lazy val dim1 = d1

    /** Dimension 2
     */
    lazy val dim2 = d2

    import SparseMatrixC.RowMap
    
    /** Store the matrix as an array of sorted-linked-maps {(j, v)}
     *  where j is the second index and v is value to store
     */
    private val v = new Array [RowMap] (d1)
    for (i <- 0 until d1) v(i) = new RowMap ()
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim2 sparse matrix from an array of sorted-linked-maps.
     *  @param dim1  the row dimension
     *  @param dim2  the column dimension
     *  @param u     the array of sorted-linked-maps
     */
    def this (dim1: Int, dim2: Int, u: Array [SparseMatrixC.RowMap])
    {
        this (dim1, dim2)
        if (u.length != dim1) flaw ("contructor", "dimension is not matched!")
        for (i <- 0 until dim1) v(i) = u(i)
    } // constructor
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim1 square sparse matrix.
     *  @param dim1  the row and column dimension
     */
    def this (dim1: Int) { this (dim1, dim1) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim2 sparse matrix and assign each element the value x.
     *  @param dim1  the row dimension
     *  @param dim2  the column dimesion
     *  @param x     the scalar value to assign
     */
    def this (dim1: Int, dim2: Int, x: Complex)
    {
        this (dim1, dim2)                      // invoke primary constructor
        if (x != _0) for (i <- range1; j <- range2.reverse) v(i)(j) = x
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim1 by dim1 square sparse matrix with x assigned on the diagonal
     *  and 0 assigned off the diagonal.  To obtain an identity matrix, let x = 1.
     *  @param dim1  the row and column dimension
     *  @param x     the scalar value to assign on the diagonal
     */
    def this (dim1: Int, x: Complex)
    {
        this (dim1, dim1)                      // invoke primary constructor
        if (x != _0) for (i <- range1) v(i)(i) = x
    } // constructor

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix from repeated values.
     *  @param dim  the (row, column) dimensions
     *  @param u    the repeated values
     */
    def this (dim: Tuple2 [Int, Int], u: Complex*)
    {
        this (dim._1, dim._2)                      // invoke primary constructor
        for (i <- range1; j <- range2) v(i)(j) = u(i * dim2 + j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a matrix from repeated real values.
     *  @param dim  the (row, column) dimensions
     *  @param u00  the first value (necessary due to type erasure)
     *  @param u    the rest of the repeated values
     */
    def this (dim: Tuple2 [Int, Int], u00: Double, u: Double*)
    {
        this (dim._1, dim._2)                      // invoke primary constructor
        for (i <- range1; j <- range2) v(i)(j) = if (j == 0 && j == 0) Complex (u00)
                                               else Complex (u(i * dim2 + j - 1))
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a sparse matrix and assign values from matrix u.
     *  @param u   the matrix of values to assign
     */
    def this (u: SparseMatrixC)
    {
        this (u.dim1, u.dim2)                  // invoke primary constructor
        for (i <- range1; e <- u.v(i)) this(i, e._1) = e._2
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a sparse matrix and assign values from (MatrixN) matrix u.
     *  @param u   the matrix of values to assign
     */
    def this (u: MatrixC)
    {
        this (u.dim1, u.dim2)                  // invoke primary constructor
        for (i <- range1; j <- range2) this(i, j) = u(i, j)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a sparse matrix and assign values from (SymTriMatrixN) matrix u.
     *  @param u   the matrix of values to assign
     */
    def this (u: SymTriMatrixC)
    {
        this (u.d1, u.d1)                      // invoke primary constructor
        v(0)(0) = u.dg(0)
        v(0)(1) = u.sd(0)
        for (i <- 1 until dim1 - 1) {
            v(i)(i-1) = u.sd(i-1)
            v(i)(i)   = u.dg(i)
            v(i)(i+1) = u.sd(i)
        } // for
        v(dim1-1)(dim1-2) = u.sd(dim1-2)
        v(dim1-1)(dim1-1) = u.dg(dim1-1)
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this sparse matrix's element at the i,j-th index position.
     *  @param i  the row index
     *  @param j  the column index
     */
    def apply (i: Int, j: Int): Complex = if (v(i) contains j) v(i)(j) else _0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this sparse matrix's vector at the i-th index position (i-th row).
     *  @param i  the row index
     */
    def apply (i: Int): VectorC =
    {
        val a = Array.ofDim [Complex] (dim2)
        for (j <- 0 until dim2) a(j) = this(i, j)
        new VectorC (a)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get this sparse matrix's vector at the i-th index position (i-th row).
     *  @param i  the row index
     *
    def apply (i: Int): VectorC =
    {
        val a = Array.ofDim [Complex] (dim2)
        for (j <- 0 until dim2) {
            try a(j) = v(i)(j)
            catch { case nsee: NoSuchElementException => a(j) = _0 }
        } // for
        new VectorC (a)
    } // apply
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range ir and column-wise on range jr.
     *  Ex: b = a(2..4, 3..5)
     *  @param ir  the row range
     *  @param jr  the column range
     */
    def apply (ir: Range, jr: Range): SparseMatrixC = slice (ir.start, ir.end, jr.start, jr.end)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise on range ir and column-wise at index j.
     *  Ex: u = a(2..4, 3)
     *  @param ir  the row range
     *  @param j   the column index
     */
    def apply (ir: Range, j: Int): VectorC = col(j)(ir)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get a slice this matrix row-wise at index i and column-wise on range jr.
     *  Ex: u = a(2, 3..5)
     *  @param i   the row index
     *  @param jr  the column range
     */
    def apply (i: Int, jr: Range): VectorC = this(i)(jr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this sparse matrix's element at the i,j-th index position to the scalar x.
     *  Only store x if it is non-zero.
     *  @param i  the row index
     *  @param j  the column index
     *  @param x  the scalar value to assign
     */
    def update (i: Int, j: Int, x: Complex) { if (x != _0) v(i)(j) = x else v(i) -= j }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this sparse matrix's row at the i-th index position to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     */
    def update (i: Int, u: VectorC)
    {
        for (j <- 0 until u.dim) {
            val x = u(j)
            if (x != _0) v(i)(j) = x else v(i) -= j
        } // for
    } // update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this sparse matrix's row at the i-th index position to the sorted-linked-map u.
     *  @param i  the row index
     *  @param u  the sorted-linked-map of non-zreo values to assign
     */
    def update (i: Int, u: RowMap) { v(i) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise on range ir and column-wise on range jr.
     *  Ex: a(2..4, 3..5) = b
     *  @param ir  the row range
     *  @param jr  the column range
     *  @param b   the matrix to assign
     */
    def update (ir: Range, jr: Range, b: SparseMatrixC)
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
    def update (ir: Range, j: Int, u: VectorC) { col(j)(ir) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set a slice this matrix row-wise at index i and column-wise on range jr.
     *  Ex: a(2, 3..5) = u
     *  @param i   the row index
     *  @param jr  the column range
     *  @param u   the vector to assign
     */
    def update (i: Int, jr: Range, u: VectorC) { this(i)(jr) = u }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the elements in this matrix to the scalar x.
     *  @param x  the scalar value to assign
     */
    def set (x: Complex)
    {
        throw new NoSuchMethodException ("use a dense matrix instead")
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the values in this matrix as copies of the values in 2D array u.
     *  @param u  the 2D array of values to assign
     */
    def set (u: Array [Array [Complex]])
    {
        for (i <- range1; j <- range2) this(i, j) = u(i)(j)
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set this matrix's ith row starting at column j to the vector u.
     *  @param i  the row index
     *  @param u  the vector value to assign
     *  @param j  the starting column index
     */
    def set (i: Int, u: VectorC, j: Int = 0)
    {
        for (k <- 0 until u.dim) this(i, k+j) = u(k)
    } // set

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this sparse matrix row-wise from to end.
     *  @param from  the start row of the slice
     *  @param end   the end row of the slice
     */
    def slice (from: Int, end: Int): SparseMatrixC =
    {
        val c = new SparseMatrixC (end - from, dim2)
        for (i <- 0 until c.dim1) c(i) = this(i + from)
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this sparse matrix row-wise r_from to r_end and column-wise c_from to c_end.
     *  @param r_from  the start of the row slice
     *  @param r_end   the end of the row slice
     *  @param c_from  the start of the column slice
     *  @param c_end   the end of the column slice
     */
    def slice (r_from: Int, r_end: Int, c_from: Int, c_end: Int): SparseMatrixC =
    {
        val c = new SparseMatrixC (r_end - r_from, c_end - c_from)
        for (i <- 0 until c.dim1; e <- v(i+r_from)) { 
            if (c_from <= e._1 && e._1 < c_end) c.v(i)(e._1-c_from) = e._2
        } // for
        c
    } // slice

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Slice this sparse matrix excluding the given row and column.
     *  @param row  the row to exclude
     *  @param col  the column to exclude
     */
    def sliceExclude (row: Int, col: Int): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1 - 1, dim2 - 1)
        for (i <- range1 if i != row) for (j <- range2 if j != col) {
            if (v(i) contains j) c.v(i - oneIf (i > row))(j - oneIf (j > col)) = this(i, j)
        } // for
        c
    } // sliceExclude

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select rows from this matrix according to the given index/basis.
     *  @param rowIndex  the row index positions (e.g., (0, 2, 5))
     */
    def selectRows (rowIndex: Array [Int]): SparseMatrixC =
    {
        val c = new SparseMatrixC (rowIndex.length, dim2)
        for (i <- c.range1) c(i) = this(rowIndex(i))
        c
    } // selectRows

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get column 'col' from the matrix, returning it as a vector.
     *  @param col   the column to extract from the matrix
     *  @param from  the position to start extracting from
     */
    def col (col: Int, from: Int = 0): VectorC =
    {
        val u = new VectorC (dim1 - from)
        for (i <- from until dim1) u(i-from) = this(i, col)
        u
    } // col

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set column 'col' of the matrix to a vector.
     *  @param col  the column to set
     *  @param u    the vector to assign to the column
     */
    def setCol (col: Int, u: VectorC) { for (i <- range1) this(i, col) = u(i) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select columns from this matrix according to the given index/basis.
     *  Ex: Can be used to divide a matrix into a basis and a non-basis.
     *  @param colIndex  the column index positions (e.g., (0, 2, 5))
     */
    def selectCols (colIndex: Array [Int]): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, colIndex.length)
        for (j <- c.range2) c.setCol (j, col(colIndex(j)))
        c
    } // selectCols

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transpose this sparse matrix (rows => columns).
     */
    def t: SparseMatrixC =
    {
        val b = new SparseMatrixC (dim2, dim1)
        for (i <- b.range1; e <- v(i)) b(e._1, i) = e._2
        b
    } // t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Concatenate this sparse matrix and vector u.
     *  @param u  the vector to be concatenated as the new last row in matrix
     */
    def ++ (u: VectorC): SparseMatrixC =
    {
        if (u.dim != dim2) flaw ("++", "vector does not match row dimension")
        val c = new SparseMatrixC (dim1 + 1, dim2)
        for (i <- c.range1) c(i) = if (i < dim1) this(i) else u
        c
    } // ++

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this sparse matrix and sparse matrix b.
     *  @param b  the matrix to add (requires sameCrossDimensions)
     */
    def + (b: SparseMatrixC): SparseMatrixC =
    {
        val c = new SparseMatrixC (this)
        for (i <- range1; e <- b.v(i)) c(i, e._1) += e._2
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this sparse matrix and sparse matrix b.
     *  @param b  the matrix to add (requires sameCrossDimensions)
     */
    def += (b: SparseMatrixC): SparseMatrixC =
    {
        for (i <- range1; e <- b.v(i)) this(i, e._1) = this(i, e._1) + e._2
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this sparse matrix and matrix b.
     *  @param b  the matrix to add (requires sameCrossDimensions)
     */
    def + (b: MatrixC): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, dim2)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) + b(i, j)
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this sparse matrix and matrix b.
     *  @param b  the matrix to add (requires sameCrossDimensions)
     */
    def += (b: MatrixC): SparseMatrixC =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) + b(i, j)
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this sparse matrix and scalar x. Note: every element will be likely 
     *  filled, hence the return type is a dense matrix.
     *  @param x  the scalar to add
     */
    def + (x: Complex): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c(i, j) = this(i, j) + x
        c
    } // +

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add in-place this sparse matrix and scalar x.
     *  @param x  the scalar to add
     */
    def += (x: Complex): SparseMatrixC =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) + x
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix substract matrix b.
     *  @param b  the sparse matrix to subtract (requires sameCrossDimensions)
     */
    def - (b: SparseMatrixC): SparseMatrixC =
    {
        val c = new SparseMatrixC (this)
        for (i <- range1; e <- b.v(i)) c(i, e._1) -= e._2
        c
    } // -

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix substract in-place sparse matrix b.
     *  @param b  the sparse matrix to subtract (requires sameCrossDimensions)
     */
    def -= (b: SparseMatrixC): SparseMatrixC =
    {
        for (i <- range1; e <- b.v(i)) this(i, e._1) = this(i, e._1) - e._2
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix substract matrix b.
     *  @param b  the matrix to subtract (requires sameCrossDimensions)
     */
    def - (b: MatrixC): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, dim2)
        for (i <- c.range1; j <- c.range2) c(i, j) = this(i, j) - b(i, j)
        c
    } // -
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix substract in-place matrix b.
     *  @param b  the matrix to subtract (requires sameCrossDimensions)
     */
    def -= (b: MatrixC): SparseMatrixC =
    {
        for (i <- range1; j <- range2) this(i, j) = this(i, j) - b(i, j)
        this
    } // -=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix subtract scalar x. Note: every element will be likely 
     *  filled, hence the return type is a dense matrix.
     *  @param x  the scalar to subtract
     */
    def - (x: Complex): MatrixC =
    {
        val c = new MatrixC (dim1, dim2)
        for (i <- range1; j <- range2) c(i, j) = this(i, j) - x
        c
    } // -
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this sparse matrix subtract in-place scalar x.
     *  @param x  the scalar to subtract
     */
    def -= (x: Complex): SparseMatrixC =
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
    def * (b: SparseMatrixC): SparseMatrixC =
    {
        val c  = new SparseMatrixC (dim1, b.dim2)
        val bt = b.t                            // transpose the b matrix (for row access)
        for (i <- c.range1) {
            var ea: LinkedEntry [Int, Complex] = null
            var eb: LinkedEntry [Int, Complex] = null
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
    /** Multiply in-place this sparse matrix by sparse matrix b, by performing a
     *  merge operation on the rows on this sparse matrix and the transpose of
     *  the b matrix.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def *= (b: SparseMatrixC): SparseMatrixC =
    {
        val c = if (b == this) new SparseMatrixC (this) else b
        val bt = b.t                            // transpose the b matrix (for row access)
        for (i <- c.range1) {
            var ea: LinkedEntry[Int, Complex] = null
            var eb: LinkedEntry[Int, Complex] = null
            val temp = new RowMap ()  
            for (e <- v(i)) temp(e._1) = e._2   // copy a new SortedLinkedHashMap
            for (j <- c.range2) {
                ea = temp.getFirstEntry()
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
                this(i, j) = sum         // assign if non-zero
            } // for
        } // for
        this
    } // *=
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this sparse matrix by matrix b.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def * (b: MatrixC): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, b.dim2)
        for (i <- c.range1; j <- c.range2) {
            var sum = _0
            for (e <- v(i)) sum += e._2 * b(e._1, j)
            c(i, j) = sum
        } // for
        c
    } // *

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this sparse matrix by matrix b.
     *  @param b  the matrix to multiply by (requires sameCrossDimensions)
     */
    def *= (b: MatrixC): SparseMatrixC =
    {
        val c = if (b == this) new SparseMatrixC (this) else b
        for (i <- range1) {
            val temp = new RowMap ()   // save so not overwritten
            for (e <- v(i)) temp(e._1) = e._2
            for (j <- range1) {
                var sum = _0
                for (e <- temp) sum += e._2 * b(e._1, j)
                this(i, j) = sum
            } // for
        } // for
        this
    } // *=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this sparse matrix by sparse matrix b using the Strassen matrix
     *  multiplication algorithm.  Both matrices (this and b) must be square.
     *  Although the algorithm is faster than the traditional cubic algorithm,
     *  its requires more memory and is often less stable (due to round-off errors).
     *  FIX:  could be make more efficient using a virtual slice (vslice) method.
     *  @see http://en.wikipedia.org/wiki/Strassen_algorithm
     *  @param b  the matrix to multiply by (it has to be a square matrix)
     */
    def strassenMult (b: SparseMatrixC): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, dim1)  // allocate result matrix
        var d = dim1 / 2                        // half dim1
        if (d + d < dim1) d += 1                // if not even, increment by 1
        val evenDim = d + d                     // equals dim1 if even, else dim1 + 1

        // decompose to blocks (use vslice method if available)
        val a11 = slice (0, d, 0, d)
        val a12 = slice (0, d, d, evenDim)
        val a21 = slice (d, evenDim, 0, d)
        val a22 = slice (d, evenDim, d, evenDim)
        val b11 = b.slice (0, d, 0, d)
        val b12 = b.slice (0, d, d, evenDim)
        val b21 = b.slice (d, evenDim, 0, d)
        val b22 = b.slice (d, evenDim, d, evenDim)

        // compute intermediate sub-matrices
        val p1 = (a11 + a22) * (b11 + b22)
        val p2 = (a21 + a22) * b11
        val p3 = a11 * (b12 - b22)
        val p4 = a22 * (b21 - b11)
        val p5 = (a11 + a12) * b22
        val p6 = (a21 - a11) * (b11 + b12)
        val p7 = (a12 - a22) * (b21 + b22)

        for (i <- c.range1; j <- c.range2) {
            c(i, j) = if (i < d && j < d)    p1(i, j) + p4(i, j)- p5(i, j) + p7(i, j)
                   else if (i < d)           p3(i, j-d) + p5(i, j-d)
                   else if (i >= d && j < d) p2(i-d, j) + p4(i-d, j)
                   else                      p1(i-d, j-d) - p2(i-d, j-d) + p3(i-d, j-d) + p6(i-d, j-d)
        } // for
        c                                    // return result matrix
    } // strassenMult

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this sparse matrix by vector u.
     *  @param u  the vector to multiply by
     */
    def * (u: VectorC): VectorC =
    {
        val c = new VectorC (dim1)
        for (i <- range1) {
            var sum = _0
            for (e <- v(i)) sum += e._2 * u(e._1)
            c(i) = sum
        } // for
        c
    } // *
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this sparse matrix by scalar x.
     *  @param x  the scalar to multiply by
     */
    def * (x: Complex): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, dim2)
        for (i <- range1; e <- v(i)) c(i, e._1) = x * e._2
        c
    } // *
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this sparse matrix by scalar x.
     *  @param x  the scalar to multiply by
     */
    def *= (x: Complex): SparseMatrixC =
    {
        for (i <- range1; e <- v(i)) this(i, e._1) = x * e._2
        this
    } // *=
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this sparse matrix by vector u to produce another matrix (a_ij * b_j)
     *  @param u  the vector to multiply by
     */
    def ** (u: VectorC): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, dim2)
        for (i <- c.range1; e <- v(i)) c(i, e._1) = e._2 * u(e._1)
        c
    } // **

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply in-place this sparse matrix by vector u to produce another matrix (a_ij * b_j)
     *  @param u  the vector to multiply by
     */
    def **= (u: VectorC): SparseMatrixC =
    {
        for (i <- range1; e <- v(i)) this(i, e._1) = e._2 * u(e._1)
        this
    } // **=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide this sparse matrix by scalar x.
     *  @param x  the scalar to divide by
     */
    def / (x: Complex): SparseMatrixC =
    {
        val c = new SparseMatrixC (dim1, dim2)
        for (i <- range1; e <- v(i)) c(i, e._1) = e._2 / x
        c
    } // /

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Divide in-place this sparse matrix by scalar x.
     *  @param x  the scalar to divide by
     */
    def /= (x: Complex): SparseMatrixC =
    {
        for (i <- range1; e <- v(i)) this(i, e._1) = e._2 / x
        this
    } // /=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise this sparse matrix to the pth power (for some integer p >= 2).
     *  Caveat: should be replace by a divide and conquer algorithm.
     *  @param p  the power to raise this matrix to
     */
    def ~^ (p: Int): SparseMatrixC =
    {
        if (p < 2)      flaw ("~^", "p must be an integer >= 2")
        if (! isSquare) flaw ("~^", "only defined on square matrices")
        var c = new SparseMatrixC (this)
        for (i <- 0 until p-1) c *= c
        c  
    } // ~^

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the maximum element in this sparse matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def max (e: Int = dim1): Complex =
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
    private def getMaxVal (u: RowMap): Complex =
    {
        var x = if (u contains 0) u(0) else _0
        for (e <- u) if (e._2 > x) x = e._2
        x
    } // getMaxVal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the minimum element in this sparse matrix.
     *  @param e  the ending row index (exclusive) for the search
     */
    def min (e: Int = dim1): Complex =
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
    private def getMinVal (u: RowMap): Complex =
    {
        var x = if (u contains 0) u(0) else _0 
        for (e <- u) if (e._2 < x) x = e._2
        x
    } // getMinVal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose this sparse matrix into the product of lower and upper triangular
     *  matrices (l, u) using the LU Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud: Tuple2 [SparseMatrixC, SparseMatrixC] =
    {
        val l = new SparseMatrixC (dim1, dim2)   // lower triangular matrix
        val u = new SparseMatrixC (this)         // upper triangular matrix (a copy of this)
        for (i <- u.range1) {
            var pivot = u(i, i)
            if (pivot == _0) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                swap (u, i, k, i)                // swap rows i and k from column k
                pivot = u(i, i)                  // reset the pivot
            } // if
            l(i, i) = Complex._1
            for (j <- i + 1 until u.dim2) l(i, j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u(k, i) / pivot
                l(k, i) = mul
                for (j <- u.range2) u(k, j) -= mul * u(i, j)
            } // for
        } // for
        Tuple2 (l, u)
    } // lud

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose in-place this sparse matrix into the product of lower and upper triangular
     *  matrices (l, u) using the LU Decomposition algorithm.  This version uses
     *  partial pivoting.
     */
    def lud_ip: Tuple2 [SparseMatrixC, SparseMatrixC] =
    {
        val l = new SparseMatrixC (dim1, dim2)   // lower triangular matrix
        val u = this                             // upper triangular matrix (this)
        for (i <- u.range1) {
            var pivot = u(i, i)
            if (pivot == _0) {
                val k = partialPivoting (u, i)   // find the maxiumum element below pivot
                swap (u, i, k, i)                // swap rows i and k from column k
                pivot = u(i, i)                  // reset the pivot
            } // if
            l(i, i) = Complex._1
            for (j <- i + 1 until u.dim2) l(i, j) = _0
            for (k <- i + 1 until u.dim1) {
                val mul = u(k, i) / pivot
                l(k, i) = mul
                for (j <- u.range2) u(k, j) -= mul * u(i, j)
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
    private def partialPivoting (a: SparseMatrixC, i: Int): Int =
    {
        var max  = a(i, i)        // initially set to the pivot
        var kMax = i              // initially the pivot row
        for (k <- i + 1 until a.dim1 if Complex.abs (a(k, i)) > max) {
            max  = Complex.abs (a(k, i))
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
    private def swap (a: SparseMatrixC, i: Int, k: Int, col: Int)
    {
        for (j <- col until a.dim2) {
            val tmp = a(k, j); a.v(k)(j) = a(i, j); a(i, j) = tmp
        } // for
    } // swap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation l*u*x = b (see lud above).
     *  @param l  the lower triangular matrix
     *  @param u  the upper triangular matrix
     *  @param b  the constant vector
     */
    def solve (l: Matric, u: Matric, b: VectorC): VectorC =
    {
        val y = new VectorC (l.dim2)       
        for (k <- 0 until y.dim) {                   // solve for y in l*y = b
            var sum = _0
            for (j <- 0 until k) sum = sum + l(k, j) * y(j)
            y(k) = b(k) - sum
        } // for
        val x = new VectorC (u.dim2)
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
    def solve (lu: Tuple2 [Matric, Matric], b: VectorC): VectorC = solve (lu._1, lu._2, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x in the equation a*x = b where a is this sparse matrix (see lud above).
     *  @param b  the constant vector.
     */
    def solve (b: VectorC): VectorC = solve (lud, b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Combine this sparse matrix with matrix b, placing them along the diagonal and
     *  filling in the bottom left and top right regions with zeroes; [this, b].
     *  @param b  the matrix to combine with this matrix
     */
    def diag (b: MatrixC): SparseMatrixC =
    {
        val m = dim1 + b.dim1
        val n = dim2 + b.dim2
        val c = new SparseMatrixC (m, n)
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
    def diag (p: Int, q: Int): SparseMatrixC =
    {
        if (! isSymmetric) flaw ("diag", "this matrix must be symmetric")
        val n  = dim1 + p + q
        val c  = new SparseMatrixC (n, n)
        for (i <- 0 until n; j <- 0 until n) {
            c(i, j) = if (i < p || i > p + dim1) if (i == j) Complex._1 else _0
                    else                         this(i-p, j-p)
        } // for
        c
    } // diag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the kth diagonal of this sparse matrix.  Assumes dim2 >= dim1.
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def getDiag (k: Int = 0): VectorC =
    {
        val mm = dim1 - math.abs (k)
        val c = new VectorC (mm)
        for (i <- 0 until mm) c(i) = this(i, i+k)
        c
    } // getDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the kth diagonal of this matrix to the vector u.  Assumes dim2 >= dim1.
     *  @param u  the vector to set the diagonal to
     *  @param k  how far above the main diagonal, e.g., (-1, 0, 1) for (sub, main, super)
     */
    def setDiag (u: VectorC, k: Int = 0)
    {
        val mm = dim1 - math.abs (k)
        for (i <- 0 until mm) this(i, i+k) = u(i)
    } // setDiag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the main diagonal of this matrix to the scalar x.  Assumes dim2 >= dim1.
     *  @param x  the scalar to set the diagonal to
     */
    def setDiag (x: Complex) { for (i <- range1) this(i, i) = x }
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert this sparse matrix (requires a squareMatrix) and use partial pivoting.
     */
    def inverse: SparseMatrixC =
    {
        val b = new SparseMatrixC (this)              // copy this matrix into b
        val c = new SparseMatrixC (dim1, Complex._1)  // let c represent the augmentation
        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == _0) {
                val k = partialPivoting (b, i)     // find the maxiumum element below pivot
                swap (b, i, k, i)                  // in b, swap rows i and k from column i
                swap (c, i, k, 0)                  // in c, swap rows i and k from column 0
                pivot = b(i, i)                    // reset the pivot
            } // if
            for (j <- b.range2) {
                b(i, j) = b(i, j) / pivot
                c(i, j) = c(i, j) / pivot
            } // for
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                if (mul != _0) {
                    for (j <- b.range2) {
                        val bval = b(i, j)
                        val cval = c(i, j)
                        if (bval != _0) b(k, j) -= mul * bval
                        if (cval != _0) c(k, j) -= mul * cval
                    } // for
                } // if
            } // for
        } // for
        c
    } // inverse
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Invert in-place this sparse matrix (requires a squareMatrix).  This version uses
     *  partial pivoting.
     */
    def inverse_ip: SparseMatrixC =
    {
        val b = this                                   // use this matrix for b
        val c = new SparseMatrixC (dim1, Complex._1)   // let c represent the augmentation
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
                if (mul != _0) {
                    for (j <- b.range2) {
                        b(k, j) = b(k, j) - mul * b(i, j)
                        c(k, j) = c(k, j) - mul * c(i, j)
                    } // for
                } // if
            } // for
        } // for
        c
    } // inverse_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Guass-Jordan reduction on this sparse matrix to make the left part embed an
     *  identity matrix.  A constraint on this m by n matrix is that n >= m.
     */
    def reduce: SparseMatrixC =
    {
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")
        val b = new SparseMatrixC (this)        // copy this matrix into b
        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == _0) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                swap (b, i, k, i)               // in b, swap rows i and k from column i
                pivot = b(i, i)                 // reset the pivot
            } // if
            for (j <- b.range2) b(i, j) = b(i, j) / pivot
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                if (mul != 0) {
                    for (j <- b.range2) b(k, j) = b(k, j) - mul * b(i, j)
                } // if
            } // for
        } // for
        b
    } // reduce

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use Guass-Jordan reduction in-place on this sparse matrix to make the left part
     *  embed an identity matrix.  A constraint on this m by n matrix is that n >= m.
     */
    def reduce_ip 
    {
        if (dim2 < dim1) flaw ("reduce", "requires n (columns) >= m (rows)")
        val b = this                            // use this matrix for b
        for (i <- b.range1) {
            var pivot = b(i, i)
            if (pivot == _0) {
                val k = partialPivoting (b, i)  // find the maxiumum element below pivot
                swap (b, i, k, i)               // in b, swap rows i and k from column i
                pivot = b(i, i)                 // reset the pivot
            } // if
            for (j <- b.range2) b(i, j) = b(i, j) / pivot
            for (k <- 0 until dim1 if k != i) {
                val mul = b(k, i)
                if (mul != _0) {
                    for (j <- b.range2) b(k, j) = b(k, j) - mul * b(i, j)
                } // if
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
    def clean (thres: Double, relative: Boolean = true): SparseMatrixC =
    {
        val s = if (relative) mag else Complex._1             // use matrix magnitude or 1
        for (i <- range1; j <- range2) if (abs (this(i, j)) <= s * thres) this(i, j) = _0
        this
    } // clean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace: VectorC =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        //reduce.col(dim2 - 1) * Complex._1n ++ Complex._1
        var r = reduce.col(dim2 - 1) 
        r = r * Complex._1n
        r ++ Complex._1
    } // nullspace
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the (right) nullspace in-place of this m by n matrix (requires n = m + 1)
     *  by performing Gauss-Jordan reduction and extracting the negation of the
     *  last column augmented by 1.  The nullspace of matrix a is "this vector v
     *  times any scalar s", i.e., a*(v*s) = 0.  The left nullspace of matrix a is
     *  the same as the right nullspace of a.t (a transpose).
     */
    def nullspace_ip: VectorC =
    {
        if (dim2 != dim1 + 1) flaw ("nullspace", "requires n (columns) = m (rows) + 1")
        reduce_ip
        var c = col(dim2 - 1) 
        c = c *  Complex._1n
        c ++ Complex._1
    } // nullspace_ip

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the trace of this sparse matrix, i.e., the sum of the elements on the
     *  main diagonal.  Should also equal the sum of the eigenvalues.
     *  @see Eigen.scala
     */
    def trace: Complex =
    {
        if ( ! isSquare) flaw ("trace", "trace only works on square matrices")
        var sum = _0
        for (i <- range1) sum += this(i, i)
        sum
    } // trace
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of this sparse matrix, i.e., the sum of its elements.
     */
    def sum: Complex =
    {
        var sum = _0
        for (i <- range1; j <- range2) sum += this(i, j)
        sum
    } // sum
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of the lower triangular region of this sparse matrix.
     */
    def sumLower: Complex =
    {
        var sum = _0
        for (i <- range1; j <- 0 until i) sum += this(i, j)
        sum
    } // sumLower

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the abs sum of this matrix, i.e., the sum of the absolute value
     *  of its elements.  This is useful for comparing matrices (a - b).sumAbs
     */
    def sumAbs: Complex =
    {
        var sum = _0
        for (i <- range1; j <- range2) sum += abs (this(i, j))
        sum
    } // sumAbs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the 1-norm of this matrix, i.e., the maximum 1-norm of the
     *  column vectors.  This is useful for comparing matrices (a - b).norm1
     */
    def norm1: Complex =
    {
        val c = new VectorC (dim2)
        for (j <- range2) c(j) = col(j).norm1
        c.max ()
    } // norm1

    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the determinant of this sparse matrix.
     */
    def det: Complex =
    {
        if ( ! isSquare) flaw ("det", "determinant only works on square matrices")
        var sum = _0
        for (j <- range2) {
            val b = sliceExclude (0, j)   // the submatrix that excludes row 0 and column j
            sum += (if (j % 2 == 0) this(0, j) * (if (b.dim1 == 1) b(0, 0) else b.det)
                    else          - this(0, j) * (if (b.dim1 == 1) b(0, 0) else b.det))
        } // for
        sum
    } // det

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this sparse matrix is rectangular (all rows have the same number
     * of columns).
     */
    def isRectangular: Boolean = true

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether this sparse matrix is nonnegative (has no negative elements).
     */
    def isNonnegative: Boolean =
    {
        for (i <- range1; e <- v(i) if e._2 < _0) return false
        true
    } // isNonegative
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the non-zero elements in this sparse matrix.
     */
    override def toString: String =
    {
        var s = new StringBuilder ("\nSparseMatrixC(\t")
        for (i <- range1) {
            s ++= v(i).toString
            s ++= (if (i < dim1 - 1) ",\n\t\t" else ")")
        } // for
        s.toString
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show all elements in this sparse matrix.
     */
    def showAll
    {
        print ("SparseMatrixC(")
        for (i <- range1) {
            if (i > 0) print ("\t")
            print ("\t(")
            for (j <- range2) print (this(i, j).toString + (if (j < dim2 - 1) ", " else ")\n"))
        } // for
        println (")")
    } // showAll
    
} // SparseMatrixC class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the SparseMatrixC class.
 */
object SparseMatrixCTest extends App
{
    for (l <- 1 to 4) {
        println ("\n\tTest SparseMatrixC on real matrices of dim " + l)
        val x = new SparseMatrixC (l, l)
        val y = new SparseMatrixC (l, l)
        x.set (2.)
        y.set (3.)
        println ("x + y  = " + (x + y))
        println ("x - y  = " + (x - y))
        println ("x * y  = " + (x * y))
        println ("x * 4. = " + (x * Complex (4.)))
    } // for

    println ("\n\tTest SparseMatrixC on additional operations")

    val z  = new SparseMatrixC ((2, 2), 1., 2.,
                                        3., 2.)
    val b  = VectorC (8., 7.)
    val lu = z.lud

    println ("z         = " + z)
    println ("z.t       = " + z.t)
    println ("z.lud     = " + lu)
    println ("z.solve   = " + z.solve (lu._1, lu._2, b))
    println ("z.inverse = " + z.inverse)
    println ("z.inv * b = " + z.inverse * b)
    println ("z.det     = " + z.det)
    println ("z         = " + z)

    val w = new SparseMatrixC ((2, 3), 2., 3., 5.,
                                      -4., 2., 3.)
    val v = new MatrixC ((3, 2), 2., -4.,
                                 3.,  2.,
                                 5.,  3.)
    
    println ("w         = " + w)
    println ("v         = " + v)
    println ("w.reduce  = " + w.reduce)

    println ("right:    w.nullspace = " + w.nullspace)
    println ("check right nullspace = " + w * w.nullspace)

    println ("left:   v.t.nullspace = " + v.t.nullspace)
    println ("check left  nullspace = " + v.t.nullspace * v)

    for (row <- z) println ("row = " + row.deep)
     
} // SparseMatrixCTest object

