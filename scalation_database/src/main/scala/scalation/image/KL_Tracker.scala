
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon May 21 15:07:29 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Iterative KL 2D Tracker Algorithm
 *  @see web.yonsei.ac.kr/jksuhr/articles/Kanade-Lucas-Tomasi%20Tracker.pdf
 */

package scalation.image

import scala.math.{ceil, max, min, round}

import scalation.linalgebra._
import scalation.util.banner

import Tracker._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KL_Tracker` class is used for image tracking based on the idea of minimizing
 *  the difference between a sub-image of 'a'  subsequent displaced sub-image of 'b'.
 *  @param a  the initial image
 *  @param b  the subsequent image
 *  @param w  the window size
 */
class KL_Tracker (a: MatriI, b: MatriI, w: VectoI)
      extends Tracker (a, b)
{
    private val DEBUG    = true                                         // debug flag
    private val ITER_MAX = 3                                            // maximum number of iterations
    private val THR      = 1                                            // early termination threshold
    private var d: VectoI = new VectorI (2)                             // displacement vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal displacement vector 'd', so that point 'u' in image 'a'
     *  matches point 'v = u + d' in image 'b', returning vector 'd' and its error 'e'.
     *  Solve for 'd' in the following system of equations.
     *  <p>
     *      Z d = df
     *  <p>
     *  where 'Z' is the gradient outer product matrix, 'd' is the displacement
     *  @see www.comp.nus.edu.sg/~cs4243/lecture/motion.pdf - p. 36
     *  and 'df' gradient adjusted image difference.
     *  @param u  the point in image 'a' for which a matching point 'v' is to found in 'b'
     */
    def optimize (u: VectoI): (VectoI, Double) =
    {
        val x_r = max (1, u(0)-w(0)) to min (limit(0), u(0)+w(0))       // x-coordinate range
        val y_r = max (1, u(1)-w(1)) to min (limit(1), u(1)+w(1))       // y-coordinate range
        var e   = err (d, x_r, y_r)                                     // compute initial error for d = 0
        if (DEBUG) println (s"optimize (0): (d, e) = ($d, $e)")

        val z  = grad_outer (x_r, y_r)                                  // gradient outer product
        for (it <- 1 to ITER_MAX) {
            if (DEBUG) banner (s"begin interation $it")
            val df = mismatch (x_r, y_r)                                // gradient weighted difference
            val dd = z.inverse * df                                     // solve for dd
            d      = makeInt (dd)                                       // convert to integer vector ceil/round
            e      = err (d, x_r, y_r)                                  // compute the new error
            if (DEBUG) println (s"optimize ($it): dd = $dd, (d, e) = ($d, $e)")
            if (e < THR) return (d, e)
        } // for

        (d, e)                                                           // return final displacement and its error
    } // optimize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the vector of `Double`s 'v' to a vector of `Int`s using ceil/round.
     *  @param v  the vector of `Double`s to convert
     */
    def makeInt (v: VectoD): VectoI = VectorI (for (i <- v.range) yield ceil (v(i)).toInt)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the gradient outer product matrix for 'a', summed over the window 'Z'.
     *  @param x_r  the range of x-coordinate values (e.g., 2, 3, 4)
     *  @param y_r  the range of y-coordinate values (e.g., 4, 5, 6)
     */
    private def grad_outer (x_r: Range, y_r: Range): MatriD =
    {
        val z = new MatrixD (2, 2)
        for (x <-x_r; y <- y_r) {
            val g = grad (a, x, y)
            if (DEBUG) println (s"grad (x, $x, $y) = $g")
            z += MatrixD.outer (g, g)
        } // for
        if (DEBUG) println (s"grad_outer: z = $z")
        z
    } // grad_outer

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the image difference of 'a' with a displaced 'b'.
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    private def diff (x: Int, y: Int): Double =
    {
        val (xd, yd) = (x + d(0), y + d(1))                             // displaced (x,y)-coordinate
        if (outside (xd, yd)) return Double.MaxValue
        a(x, y) - b(xd, yd)
    } // diff

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the image mismatch vector, summed over the window 'df'
     *  @param x_r  the range of x-coordinate values (e.g., 2, 3, 4)
     *  @param y_r  the range of y-coordinate values (e.g., 4, 5, 6)
     */
    private def mismatch (x_r: Range, y_r: Range): VectoD =
    {
        val df = new VectorD (2)
        for (x <- x_r; y <- y_r) {
            val dfxy = diff (x, y)
            df(0) += dfxy * d_dx(a, x, y)
            df(1) += dfxy * d_dy(a, x, y)
            if (DEBUG) println (s"($x, $y) => $df")
        } // for
        if (DEBUG) println (s"mismatch: df = $df")
        df
    } // mismatch

} // KL_Tracker class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KL_TrackerTest` object is used test the `KL_Tracker` class.
 *  For this case, the algorithm makes improvement, but is not optimal.
 *  > runMain scalation.image.KL_TrackerTest
 */
object KL_TrackerTest extends App
{
    val u = VectorI (3, 3)                                              // location of interest
    val w = VectorI (2, 2)                                              // size of window around 'u'

    val tr     = new KL_Tracker (a1, a2, w)                             // KL Tracker
    val (d, e) = tr.optimize (u)
    banner ("KL Tracker")
    println (s"(d, e) = ($d, $e)")
    println ("optimal solution: (d, e) = (VectorI (2, 2), 0.0)")

} // KL_TrackerTest object

