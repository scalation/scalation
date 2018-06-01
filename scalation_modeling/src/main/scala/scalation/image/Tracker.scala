
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Thu May 24 16:16:26 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Object Tracking Algorithm Template
 *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.112.8588&rep=rep1&type=pdf
 *  @see http://web.yonsei.ac.kr/jksuhr/articles/Kanade-Lucas-Tomasi%20Tracker.pdf
 */

package scalation.image

import scala.util.control.Breaks.{break, breakable}
import scala.math.{max, min, round}

import scalation.linalgebra._
import scalation.linalgebra.MatrixD.outer
import scalation.math.double_exp
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tracker` abstract class is used for image tracking algorithms based on
 *  the idea of minimizing the difference between a sub-image of 'a' and a subsequent
 *  displaced sub-image of 'b'.
 *  @param a  the initial image
 *  @param b  the subsequent image
 */
abstract class Tracker (a: MatriI, b: MatriI)
         extends Error
{
    private   val DEBUG = true                                         // debug flag
    protected val limit = VectorI (a.dim1, a.dim2)                     // the image size, e.g. (1024, 1024)

    if (b.dim1 < limit(0) || b.dim2 < limit(1)) flaw ("constructor", "image b is smaller than image a")
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value for the residual/error function that measures the difference
     *  between a sub-image of 'a' and a displaced sub-image of 'b'.
     *  @param dd   the displacement vector
     *  @param x_r  the range of x-coordinate values (e.g., 2, 3, 4) 
     *  @param y_r  the range of y-coordinate values (e.g., 4, 5, 6) 
     */
    def err (dd: VectoI, x_r: Range, y_r: Range): Double =
    {
        var sum = 0.0
        for (x <- x_r; y <- y_r) {
            val (xd, yd) = (x + dd(0), y + dd(1))                      // displaced (x,y)-coordinate
            if (outside (xd, yd)) return Double.MaxValue
            sum += (a(x, y) - b(xd, yd))~^2
        } // for
        if (DEBUG) println (s"err ($dd) = $sum")
        sum
    } // err

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the point (x, y) is outside the image (e.g., x = -1).
     *  @param x  the x-coordinate/pixel position
     *  @param y  the y-coordinate/pixel position
     */
    def outside (x: Int, y: Int): Boolean =
    {
        (x < 0 || x > limit(0)-1) ||                                   // check x-coordinate
        (y < 0 || y > limit(1)-1)                                      // check y-coordinate
    } // outside

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal displacement vector 'd', so that points near 'u' in image 'a'
     *  match points near 'v = u + d' in image 'b', returning the vectors 'd' and 'v'.
     *  @param u  the point in image 'a' for which a matching point 'v' is sought in 'b'
     */
    def optimize (u: VectoI): (VectoI, Double)

} // Tracker abstract class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tracker` object provides directions and discrete derivatives (differences)
 *  for images
 */
object Tracker
{
    /** Rook directions
     */
    val r_directions = (for (x <- -1 to 1 by 2) yield VectorI (x, 0)) ++
                       (for (y <- -1 to 1 by 2) yield VectorI (0, y))

    /** Queen directions
     */
    val q_directions = for (x <- -1 to 1; y <- -1 to 1 if x != 0 || y != 0) yield VectorI (x, y)

    // the initial image
    val a1 = new MatrixI ((10, 10), 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 4, 6, 4, 1, 1, 1, 1, 1,
                                   1, 1, 6, 8, 6, 1, 1, 1, 1, 1,
                                   1, 1, 4, 6, 4, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

    // the subsequent images
    val a2 = shift (a1, 2, 2)
    val a3 = shift (a1, 2, 3)
    val a4 = shift (a1, 3, 3)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the image by the given displacements.
     *  @param a     the 2D image matrix
     *  @param dx    the x-coordinate displacement
     *  @param dy    the y-coordinate displacement
     *  @param fill  the pixel value to fill with
     */
    def shift (a: MatriI, dx: Int, dy: Int, fill: Int = 1): MatriI =
    {
        val b = new MatrixI (a.dim1, a.dim2)
        for (x <- b.range1; y <- b.range2) {
            val (xx, yy) = (x - dx, y - dy)
            b(x, y) = if (0 < xx && xx < a.dim1 && 0 < yy && yy < a.dim2) a(xx, yy) else fill
        } // for
        b
    } // shift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the discrete derivative of image 'a' w.r.t. 'x'.
     *  @param a  the 2D image matrix
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    def d_dx (a: MatriI, x: Int, y: Int): Double = (a(x+1, y) - a(x-1, y)) / 2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the discrete derivative of image 'a' w.r.t. 'y'.
     *  @param a  the 2D image matrix
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    def d_dy (a: MatriI, x: Int, y: Int): Double = (a(x, y+1) - a(x, y-1)) / 2.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the discrete gradient of image 'a' w.r.t. 'x' and 'y'.
     *  @param a  the 2D image matrix
     *  @param x  the x-coordinate/pixel
     *  @param y  the y-coordinate/pixel
     */
    def grad (a: MatriI, x: Int, y: Int): VectoD = VectorD (d_dx(a, x, y), d_dy(a, x, y))

} // Tracker object

