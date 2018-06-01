
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Wed Mar 14 11:42:40 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.math.round

import scalation.linalgebra.{MatriD, MatriI, MatrixI, VectoD, VectoI, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Round` object provides methods to round double vectors and matrices
 *  into integer vectors and matrices.
 */
object Round
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Round a double vector into an integer vector.
     *  @param x  the double vector to round
     */
    def roundVec (y: VectoD): VectoI =
    {
        val yr = new VectorI (y.dim)
        for (i <- y.range) yr(i) = round (y(i)).toInt
        yr
    } // roundVec

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Round a scaled double vector into an integer vector.
     *  @param x  the double vector to round
     *  @param s  the scale factor
     */
    def roundScaledVec (y: VectoD, s: Double): VectoI = roundVec (y * s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Round a double matrix into an integer matrix.
     *  @param x  the double matrix to round
     */
    def roundMat (x: MatriD): MatriI =
    {
        val xr = new MatrixI (x.dim1, x.dim2)
        for (i <- x.range1; j <- x.range2) xr(i, j) = round (x(i, j)).toInt
        xr
    } // roundMat

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Round a scaled double matrix into an integer matrix.
     *  @param x  the double matrix to round
     *  @param s  the scale factor
     */
    def roundScaledMat (x: MatriD, s: Double): MatriI = roundMat (x * s)

} // Round object

