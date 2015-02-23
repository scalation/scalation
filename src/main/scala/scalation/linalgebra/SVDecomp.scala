
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
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
    def deflate (): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Deflate the matrix by iteratively turning elements not in the main
     *  diagonal to zero. Then return the vector of singular values and the
     *  matrices of singular vectors.
     */
    def deflateV (): Product

} // SVDecomp trait

