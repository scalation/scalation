
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed May 28 16:06:12 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SVDDecomp` trait specifies the major methods for Singular Value
 *  Decomposition implementations.
 */
trait SVDecomp
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate the matrix by iteratively turning elements not in the main
     *  diagonal to zero. Then return the vector of singular values (i.e., the main
     *  diagonal).
     */
    def factor (): Tuple3 [MatrixD, VectorD, MatrixD]

} // SVDecomp trait

