
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sat Jan 31 20:59:02 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.sqrt

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixTransform` object is used to transform the columns of a data matrix 'x'.
 *  Such pre-processing of the data is required by some modeling techniques.
 */
object MatrixTransform
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center the matrix 'x' to zero mean, column-wise, by subtracting the mean.
     *  @param x     the matrix to center
     *  @param mu_x  the vector of column means of matrix x
     */
    def center (x: MatriD, mu_x: VectoD): MatriD =
    {
        val x_c = new MatrixD (x.dim1, x.dim2)
        for (j <- x.range2) x_c.setCol (j, x.col(j) - mu_x(j))     // subtract column means
        x_c
    } // center

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Uncenter the matrix 'x_c' from zero mean, column-wise, by adding the mean.
     *  @param x_c   the matrix to uncenter
     *  @param mu_x  the vector of column means of matrix x_c
     */
    def uncenter (x_c: MatriD, mu_x: VectoD): MatriD =
    {
        val x = new MatrixD (x_c.dim1, x_c.dim2)
        for (j <- x.range2) x.setCol (j, x_c.col(j) + mu_x(j))     // add column means
        x
    } // uncenter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale the vector 'x' to the range 'lb' to 'ub'.
     *  @param x      the matrix to scale
     *  @param min_x  the vector of column minima of matrix x
     *  @param max_x  the vector of column maxima of matrix x
     *  @param lb     the lower bounds
     *  @param ub     the upper bounds
     */
    def scaleV (x: VectoD, min_x: Double, max_x: Double,
               lb: Double = 0.0, ub: Double = 1.0): VectoD =
    {
        val scale = (ub - lb) / (max_x - min_x)
        (x - min_x) * scale + lb                                   // shift and scale
    } // scaleV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unscale the vector 'x' from the range 'lb' to 'ub'.
     *  @param x      the matrix to scale
     *  @param min_x  the vector of column minima of matrix x
     *  @param max_x  the vector of column maxima of matrix x
     *  @param lb     the lower bounds
     *  @param ub     the upper bounds
     */
    def unscaleV (x: VectoD, min_x: Double, max_x: Double,
               lb: Double = 0.0, ub: Double = 1.0): VectoD =
    {
        val scale = (ub - lb) / (max_x - min_x)
        (x - lb) / scale + min_x                                   // shift and scale
    } // unscaleV

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Scale the matrix 'x' to the range 'lb to 'ub', column-wise.
     *  @param x      the matrix to scale
     *  @param min_x  the vector of column minima of matrix x
     *  @param max_x  the vector of column maxima of matrix x
     *  @param lb     the lower bounds
     *  @param ub     the upper bounds
     */
    def scale (x: MatriD, min_x: VectoD, max_x: VectoD,
               lb: Double = 0.0, ub: Double = 1.0): MatriD =
    {
        val x_s = new MatrixD (x.dim1, x.dim2)
        for (j <- x.range2) {
            val scale = (ub - lb) / (max_x(j) - min_x(j))
            x_s.setCol (j, (x.col(j) - min_x(j)) * scale + lb)     // shift and scale
        } // for
        x_s
    } // scale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unscale the matrix 'x_s' from the range 'lb' to 'ub', column-wise.
     *  @param x_s    the matrix to unscale
     *  @param min_x  the vector of column minima of matrix x_s
     *  @param max_x  the vector of column maxima of matrix x_s
     *  @param lb     the lower bounds
     *  @param ub     the upper bounds
     */
    def unscale (x_s: MatriD, min_x: VectoD, max_x: VectoD,
                 lb: Double = 0.0, ub: Double = 1.0): MatriD =
    {
        val x = new MatrixD (x_s.dim1, x_s.dim2)
        for (j <- x.range2) {
            val scale = (ub - lb) / (max_x(j) - min_x(j))
            x.setCol (j, (x_s.col(j) - lb) / scale + min_x(j))     // scale and shift
        } // for
        x
    } // unscale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Normalize the matrix 'x' to zero mean and unit standard deviation,
     *  column-wise, by subtracting the mean and dividing by standard deviation
     *  @param x      the matrix to normalize
     *  @param mu_x   the vector of column means of matrix x
     *  @param sig_x  the vector of column standard deviations of matrix x
     */
    def normalize (x: MatriD, mu_x: VectoD, sig_x: VectoD): MatriD =
    {
        val x_n = new MatrixD (x.dim1, x.dim2)
        for (j <- x.range2) x_n.setCol (j, (x.col(j) - mu_x(j)) / sig_x)
        x_n
    } // normalize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Denormalize the matrix 'x_n' from zero mean and unit standard deviation,
     *  column-wise, by multiplying by the standard deviation and adding the mean.
     *  @param x_n    the matrix to normalize
     *  @param mu_x   the vector of column means of matrix x_n
     *  @param sig_x  the vector of column standard deviations of matrix x_n
     */
    def denormalize (x_n: MatriD, mu_x: VectoD, sig_x: VectoD): MatriD =
    {
        val x = new MatrixD (x_n.dim1, x_n.dim2)
        for (j <- x.range2) x.setCol (j, x_n.col(j) * sig_x + mu_x(j))
        x
    } // denormalize

} // MatrixTransform object

import MatrixTransform._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatrixTransformTest` is used to test the `MatrixTransform` object.
 *  > runMain scalation.analytics.MatrixTransformTest
 */
object MatrixTransformTest extends App
{
    val x = new MatrixD ((3, 3), 1, 2, 3,
                                 4, 5, 6,
                                 7, 8, 9)

    val mu_x  = x.mean
    val min_x = VectorD (for (j <- x.range2) yield x.col(j).min ())
    val max_x = VectorD (for (j <- x.range2) yield x.col(j).max ())
    val sig_x = VectorD (for (j <- x.range2) yield sqrt (x.col(j).variance))

    val x_c  = center (x, mu_x)
    val x_s  = scale (x, min_x, max_x)
    val x_s2 = scale (x, min_x, max_x, -1.0, 1.0)
    val x_n  = normalize (x, mu_x, sig_x)

    println ("x    = " + x)
    println ("x_c  = " + x_c)
    println ("x_s  = " + x_s)
    println ("x_s2 = " + x_s2)
    println ("x_n  = " + x_n)

    assert (uncenter (x_c, mu_x) == x, "uncenter")
    assert (unscale (x_s, min_x, max_x) == x, "unscale")
    assert (unscale (x_s2, min_x, max_x, -1.0, 1.0) == x, "unscale")
    assert (denormalize (x_n, mu_x, sig_x) == x, "denormalize")

} // MatrixTransformTest object

