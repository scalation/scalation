
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sat Jan 31 20:59:02 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, MatrixD, VectoD}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Centering` object is used to center the input matrix 'x'.  This is done by
 *  subtracting the column means from each value.
 */
object Centering
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center the input matrix 'x' to zero mean, column-wise, by subtracting the mean.
     *  @param x     the input matrix to center
     *  @param mu_x  the vector of column means of matrix x
     */
    def center (x: MatriD, mu_x: VectoD): MatrixD =
    {
        val x_c = new MatrixD (x.dim1, x.dim2)
        for (j <- 0 until x.dim2) {
            x_c.setCol (j, x.col(j) - mu_x(j))       // subtract column means
        } // for
        x_c
    } // center

} // Centering object

