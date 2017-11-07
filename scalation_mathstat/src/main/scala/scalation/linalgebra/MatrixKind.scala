
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixKind` object defines an enumeration for the kinds of matrices
 *  supported in ScalaTion.  Combined with the base type (e.g., D for `Double`),
 *  a particular type of matrix is defined (e.g., `SparseMatrixD`).
 */
object MatrixKind extends Enumeration
{
        type MatrixKind = Value
        val DENSE, SPARSE, SYM_TRIDIAGONAL, BIDIAGONAL, COMPRESSED = Value

} // MatrixKind object
 
