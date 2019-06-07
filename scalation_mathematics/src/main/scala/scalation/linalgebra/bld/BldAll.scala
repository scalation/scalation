
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Jun 12 15:53:24 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.bld

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BldAll` object calls all the builders to generate code for the vector
 *  and matrix classes/traits in the `scalation.linalgbra` package.
 *  Note, generated files have the suffix ".scalaa".
 *------------------------------------------------------------------------------
 *  vector traits         - base traits for several kinds of vectors
 *  vector classes        - regular (dense) mathematical vectors
 *  sparse vector classes - sparse mathematical vectors
 *  RLE vector classes    - compressed (RLE) mathematical vectors
 *------------------------------------------------------------------------------
 *  matrix traits         - base traits for several kinds of matrices
 *  matrix classes        - regular (dense) matrices
 *  sparse matrix classes - sparse matrices (for high fraction of zeroes)
 *  symtri matrix classes - symmetric tridiagonal matrices (for eigenvalue algorithms)
 *  bid matrix classes    - square (upper) bidiagonal matrices (for SVD algorithms)
 *  RLE matrix classes    - compressed (RLE) matrices
 *------------------------------------------------------------------------------
 *  To see the differences between the current code and 'new generated code',
 *  run the 'check.sh' script.
 *------------------------------------------------------------------------------
 *  To install the 'new generated code' in the package replacing the current code,
 *  run the 'install.sh' script.
 *------------------------------------------------------------------------------
 *  > runMain scalation.linalgebra.bld.BldAll
 */
object BldAll extends App
{
    private val build_vectors  = true
    private val build_matrices = true

    if (build_vectors) {
        BldVecto.main (null)              // build vector traits
        BldVector.main (null)             // build vector classes
        BldSparseVector.main (null)       // build sparse vector classes
        BldRleVector.main (null)          // build RLE vector classes
    } // if

    if (build_matrices) {
        BldMatri.main (null)              // build matrix traits
        BldMatrix.main (null)             // build matrix classes
        BldSparseMatrix.main (null)       // build sparse matrix classes
        BldSymTriMatrix.main (null)       // build symtri matrix classes
        BldBidMatrix.main (null)          // build bid matrix classes
        BldRleMatrix.main (null)          // build RLE matrix classes
    } // if

    println ("Next ---------------------------------------")
    println ("BldAll: development phase: type './check.sh'")
    println ("BldAll: production phase:  type './install.sh'")

} // BldAll object

