
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Tue Dec 23 12:55:13 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Factorization` trait is the template for classes implementing various forms
 *  of matrix factorization.
 */
trait Factorization
{
    /** Flag indicating whether the matrix has been factored
     */
    protected var factored = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices without returning the
     *  two factored matrices.  Allows for example skipping the computation of the
     *  Q matrix in QR factorization when it is not needed, e.g., for regression.
     *  Class implementing the 'factor' method should set 'factored = true'.
     */
    def factor (): Factorization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the two factored matrices.
     */
    def factors: (MatriD, MatriD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t' or
     *  a = q * r, returning both the first and second matrices.
     */
    def factor12 (): (MatriD, MatriD) = { if (! factored) factor (); factors }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t',
     *  returning only the first matrix.
     */
    def factor1 (): MatriD = { if (! factored) factor (); factors._1 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Factor a matrix into the product of two matrices, e.g., 'a = l * l.t',
     *  returning only the second matrix.
     */
    def factor2 (): MatriD = { if (! factored) factor (); factors._2 }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve a system of equations, e,g., 'a * x = b' for 'x', using the factored
     *  matrices, by first performing forward substitution and then backward substitution.
     *  Must factor before calling 'solve'.
     *  @param b  the constant vector
     */
    def solve (b: VectoD): VectoD

} // Factorization trait

