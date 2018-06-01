
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Mon May 21 15:07:29 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Iterative KLT Algorithm for 2D
 *  @see http://web.yonsei.ac.kr/jksuhr/articles/Kanade-Lucas-Tomasi%20Tracker.pdf
 */

//  U N D E R   D E V E L O P M E N T

package scalation.image

import scala.util.control.Breaks.{break, breakable}
import scala.math.{max, min, round}

import scalation.linalgebra.{MatriD, MatrixD, VectoI, VectorI, VectoD, VectorD}
import scalation.math.double_exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KLT` class is used for image tracking based on the idea of minimizing
 *  the difference between a sub-image of 'a'  subsequent displaced sub-image of 'b'.
 *  @param a    the initial image
 *  @param b    the subsequent image
 *  @param u    the point in image 'a' for which a matching point 'v' is to found in 'b'
 *  @param w    the window size for window around point 'u'
 *  @param lim  the multi-dimensional size of the image, e.g. (1024, 1024)
 */
class KLT (a: MatriD, b: MatriD, u: VectoI, w: VectoI, lim: VectoI)
{
    private val DEBUG = true                                               // debug flag
    private val K_MAX = 10                                                 // k-loop max iterations
    private val L_MAX = 10                                                 // l-loop max iterations
    private val THR   = 1E-4                                               // early termination threshold
    private var g     = new VectorI (2)                                    // guess vector
    private val d     = new VectorI (2)                                    // displacement vector
    private val x_r   = max (1, u(0) - w(0)) to min (lim(0), u(0) + w(0))  // x range
    private val y_r   = max (1, u(1) - w(1)) to min (lim(1), u(1) + w(1))  // y range

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value for the residual/error function that measures the difference
     *  between a sub-image of 'a' and a displaced sub-image of 'b'.
     *  @param d  the displacement vector
     */
    def err (d: VectoD): Double =
    {
        var sum = 0.0
        for (x <- x_r; y <- y_r) sum = sum + (a(x, y) - b(x + d(0).toInt, y + d(1).toInt))~^2
        sum
    } // sim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal displacement vector 'd', so that point 'u' in image 'a'
     *  matches point 'v = u + d' in image 'b', returning vectors 'd' and 'v'.
     */
    def optimize: (VectoI, VectoI) =
    {
        for (l <- L_MAX to 0 by -1) {
            d.set (0)
            val gm = gradM
            val gmi = gm.inverse
            if (DEBUG) println (s"optimize ($l): gm = $gm,\n gmi = $gmi,\n gm * gmi = ${gm * gmi}")
            breakable { for (k <- 1 to K_MAX) {
                val b   = mismatchV
                val eta = gmi * b
                if (DEBUG) println (s"oprimize ($l, $k): b = $b, eta = $eta")
                if (eta.norm < THR) break
                for (i <- eta.range) d(i) += round (eta(i)).toInt
//              d += b.toInt
            }} // breakable for
            g = (g + d) * 2
            if (DEBUG) println (s"optimize ($l): d = $d, g = $g")
        } // for
        (g + d, u + d)
    } // optimize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the derivative of image a w.r.t. 'x'.
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    private def ix (x: Int, y: Int): Double = (a(x+1, y) - a(x-1, y)) / 2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the derivative of image a w.r.t. 'y'.
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    private def iy (x: Int, y: Int): Double = (a(x, y+1) - a(x, y-1)) / 2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the image difference of 'a' with a displaced 'b'.
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    private def di (x: Int, y: Int): Double =
    {
        val xd = x + g(0) + d(0)
        val yd = y + g(1) + d(1)
        if (0 <= xd && xd <= lim(0) + 1 && 0 <= yd && yd <= lim(1) + 1)
            a(x, y) - b(xd, yd)
        else
            0.0
    } // di

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the spatial gradient matrix.
     */
    private def gradM: MatriD =
    {
        var g1, g2, g3 = 0.0
        for (x <-x_r; y <- y_r) {
            val ixy = (ix(x, y), iy(x, y))
            g1 += ixy._1 * ixy._1
            g2 += ixy._1 * ixy._2
            g3 += ixy._2 * ixy._2
        } // for
        MatrixD ((2, 2), g1, g2,
                         g2, g3)
    } // gradM 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the image mismatch vector.
     */
    private def mismatchV: VectoD =
    {
        var b1, b2 = 0.0
        for (x <- x_r; y <- y_r) {
            val dixy = di(x, y)
            b1 += dixy * ix(x, y)
            b2 += dixy * iy(x, y)
        } // for
        VectorD (b1, b2)
    } // bk

} // KLT class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KLTTest` object is used test the KLT class.
 *  > runMain scalation.image.KLTTest
 */
object KLTTest extends App
{
    // the initial image
    val a = MatrixD ((10, 10), 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 4, 6, 4, 1, 1, 1, 1, 1,
                               1, 1, 6, 8, 6, 1, 1, 1, 1, 1,
                               1, 1, 4, 6, 4, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    // the sebsequent image
    val b = MatrixD ((10, 10), 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 4, 6, 4, 1, 1,
                               1, 1, 1, 1, 1, 6, 8, 6, 1, 1,
                               1, 1, 1, 1, 1, 4, 6, 4, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    val u   = VectorI (3, 3)                                 // reference point in 'a'
    val w   = VectorI (3, 3)                                 // window size
    val lim = VectorI (8, 8)                                 // image size - largest coordinate

    val klt = new KLT (a, b, u, w, lim)
    println ("(d, v) = " + klt.optimize)

} // KLTTest object


