
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Fri May 25 14:55:48 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Chess 2D Object Tracking Algorithm
 *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.112.8588&rep=rep1&type=pdf
 *  @see http://web.yonsei.ac.kr/jksuhr/articles/Kanade-Lucas-Tomasi%20ChessTracker.pdf
 */

package scalation.image

import scala.math.{max, min}

import scalation.linalgebra.{MatriI, MatrixI, VectoI, VectorI}
import scalation.util.banner

import Tracker._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ChessTracker` class is used for image tracking algorithms based on
 *  the idea of minimizing the difference between a sub-image of 'a' and a subsequent
 *  displaced sub-image of 'b'.
 *  This algorithm searches for improvements in displacement 'd' in multiple directions
 *  and with multiple iterations.
 *  It may be viewed as a form of Generating Set Search (GSS).
 *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.112.8588&rep=rep1&type=pdf
 *  @param a  the initial image
 *  @param b  the subsequent image
 *  @param w  the window size
 *  @param q  use false for Rook (4 directional) or true for Queen (8 directional) tracking
 */
class ChessTracker (a: MatriI, b: MatriI, w: VectoI, q: Boolean)
      extends Tracker (a, b)
{
    private val DEBUG     = true                                         // debug flag
    private val MAX_ITER  = 3                                            // maximum number of iterations
    private val MAX_STEPS = 4                                            // maximum number of steps
    private val THRES     = 1                                            // early termination threshold

    private val directions = if (q) q_directions else r_directions       // Queen or Rook search directions
    if (DEBUG) println (s"directions = $directions")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal displacement vector 'd', so that points near 'u' in image 'a'
     *  match points near 'v = u + d' in image 'b', returning the vectors 'd' and 'v'.
     *  @param u  the point in image 'a' for which a matching point 'v' is sought in 'b'
     */
    def optimize (u: VectoI): (VectoI, Double) =
    {
        var d: VectoI = new VectorI (2)                                  // set optimal displacement to zero
        val x_r  = max (0, u(0)-w(0)) to min (limit(0)-1, u(0) + w(0))   // x-coordinate range
        val y_r  = max (0, u(1)-w(1)) to min (limit(1)-1, u(1) + w(1))   // y-coordinate range
        var best = (d, err (d, x_r, y_r))

        for (it <- 1 to MAX_ITER) {                                      // perform iteration it
            if (DEBUG) banner (s"begin interation $it")

            for (dir <- directions) {                                    // try all directions
                val (dl, e) = linesearch (d, dir, x_r, y_r)              // best displacement, error in direction 'dir'
                if (DEBUG) println (s"optimize: dl = $dl, e = $e")
                if (e < best._2) { best = (dl, e); d = dl }              // record if best of all directions
            } // for

            if (DEBUG) println (s"best = $best")
            if (best._2 < THRES) return best                             // terminate early if error is small
        } // for

        best
    } // optimize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal displacement vector along the given direction 'dir'.
     *  @param d    the current displacement vector
     *  @param dir  the current direction to search in
     *  @param x_r  the range of x-coordinates
     *  @param y_r  the range of y-coordinates
     */
    def linesearch (d: VectoI, dir: VectoI, x_r: Range, y_r: Range): (VectoI, Double) =
    {
        var best = (d, err (d, x_r, y_r))                                // initialize best displacement and its error
        var dd   = d                                                     // initialize candidate displacement

        for (steps <- 1 to MAX_STEPS) {                                  // iteterate over steps in direction 'dir'
            dd    = dd + dir                                             // take one more step in direction 'dir'
            val e = err (dd, x_r, y_r)                                   // compute the error for this candidate
            if (DEBUG) println (s"linesearch: dd = $dd, e = $e")
            if (e < best._2) best = (dd.copy, e)                         // record if best in direction 'dir'
        } // for

        if (DEBUG) print (s"best = $best")
        best
    } // linesearch

} // ChessTracker class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ChessTrackerTest` object is used test the `ChessTracker` class.
 *  This test case works for both Rook and Queen directions.
 *  > runMain scalation.image.ChessTrackerTest
 */
object ChessTrackerTest extends App
{
    val u  = VectorI (3, 3)                                              // location of interest
    val w  = VectorI (1, 1)                                              // size of window around 'u'

    val tr_r     = new ChessTracker (a1, a2, w, false)                   // Rook
    val (dr, er) = tr_r.optimize (u)
    banner ("Rook Tracker")
    println (s"(dr, er) = ($dr, $er)")
    println ("optimal solution: (d, e) = (VectorI (2, 2), 0.0)")

    val tr_q     = new ChessTracker (a1, a2, w, true)                    // Queen
    val (dq, eq) = tr_q.optimize (u)
    banner ("Queen Tracker")
    println (s"(dq, eq) = ($dq, $eq)")
    println ("optimal solution: (d, e) = (VectorI (2, 2), 0.0)")

} // ChessTrackerTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ChessTrackerTest2` object is used test the `ChessTracker` class.
 *  This test case works for both Rook and Queen directions.
 *  > runMain scalation.image.ChessTrackerTest2
 */
object ChessTrackerTest2 extends App
{
    val u  = VectorI (3, 3)
    val w  = VectorI (1, 1)

    val tr_r     = new ChessTracker (a1, a3, w, false)                   // Rook
    val (dr, er) = tr_r.optimize (u)
    banner ("Rook Tracker")
    println (s"(dr, er) = ($dr, $er)")
    println ("optimal solution: (d, e) = (VectorI (2, 3), 0.0)")

    val tr_q     = new ChessTracker (a1, a3, w, true)                    // Queen
    val (dq, eq) = tr_q.optimize (u)
    banner ("Queen Tracker")
    println (s"(dq, eq) = ($dq, $eq)")
    println ("optimal solution: (d, e) = (VectorI (2, 3), 0.0)")

} // ChessTrackerTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ChessTrackerTest3` object is used test the `ChessTracker` class.
 *  This test case works only for Queen directions.
 *  > runMain scalation.image.ChessTrackerTest3
 */
object ChessTrackerTest3 extends App
{
    val u  = VectorI (3, 3)
    val w  = VectorI (1, 1)

    val tr_r     = new ChessTracker (a1, a4, w, false)                   // Rook
    val (dr, er) = tr_r.optimize (u)
    banner ("Rook Tracker")
    println (s"(dr, er) = ($dr, $er)")
    println ("optimal solution: (d, e) = (VectorI (3, 3), 0.0)")

    val tr_q     = new ChessTracker (a1, a4, w, true)                    // Queen
    val (dq, eq) = tr_q.optimize (u)
    banner ("Queen Tracker")
    println (s"(dq, eq) = ($dq, $eq)")
    println ("optimal solution: (d, e) = (VectorI (3, 3), 0.0)")

} // ChessTrackerTest3 object

