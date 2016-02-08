
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Dec 23 12:55:13 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Factorization` trait the template for classes implementing various forms
 *  of matrix factorization.
 */
trait Factorization
{
    protected var raw = true       // whether the matrix has yet to be factored

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * u',
     *  returning both the first and second matrices.
     */
    def factor (): Tuple2 [MatriD, MatriD]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t',
     *  returning only the first matrix.
     */
    def factor1 (): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t',
     *  returning only the second matrix.
     */
    def factor2 (): MatriD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve a system of equations, e,g., 'a * x = b' for 'x', using the factored
     *  matrices, by first performing forward substitution and then backward substitution.
     *  @param b  the constant vector
     */
    def solve (b: VectoD): VectoD

} // Factorization trait

