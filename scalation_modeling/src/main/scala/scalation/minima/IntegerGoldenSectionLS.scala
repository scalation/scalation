
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scala.math.{abs, ceil, floor, max, pow, round, sqrt}
import scala.util.control.Breaks.{breakable, break}

import scalation.calculus.Differential.{gradient, gradientD}
import scalation.linalgebra.VectorD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntegerGoldenSectionLS` class performs a line search on 'f(x)' to find a
 *  minimal value for 'f'.  It requires no derivatives and only one functional
 *  evaluation per iteration.  A search is conducted from 'x1' (often 0) to 'xmax'.
 *  A guess for 'xmax' must be given, but can be made larger during the expansion phase,
 *  that occurs before the recursive golden section search is called.  It works on
 *  scalar functions
 *  @see IntegerGoldenSectionLSTest.
 *  If starting with a vector function 'f(x)', simply  define a new function
 *  'g(y) = x0 + direction * y'.
 *  @see IntegerGoldenSectionLSTest2.
 *  @param f  the scalar objective function to minimize
 */
class IntegerGoldenSectionLS (f: Int => Double)
{
    private val DEBUG     = true                        // debug flag
    private val MAX_ITER  = 10                          // maximum number of expansion iterations
    private val G_RATIO   = (1.0 + sqrt (5.0)) / 2.0    // the golden ratio (1.618033988749895)
    private val G_SECTION = G_RATIO / (1.0 + G_RATIO)   // the golden section number (0.6180339887498949)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A recursive golden section search requiring only one functional evaluation
     *  per call.  It works by comparing two center points x2 (given) and x4 computed.
     *  @param left  whether to search left (true) or right (false) side of last interval
     *  @param x1    the left-most point
     *  @param x2    the center point (.618 across for left and .382 across for right)
     *  @param x3    the right-most point
     *  @param f2    the functional value for the x2 center point
     */
    def gsection (left: Boolean, x1: Int, x2: Int, x3: Int, f2: Double): Int =
    {
        var x4 = x2
        var f4 = 0.0

        if (DEBUG) println ("gsection: left = " + left + ", x1 = " + x1 + ", x2 = " + x2 +
                                                         ", x3 = " + x3 + ", f2 = " + f2)
        val dist = x3 - x1
        if (dist <= 1) return x2
        if (left) {
            x4 = x3 - (ceil (G_SECTION * dist)).toInt                // search left:  x1 < x4 < x2 < x3
            f4 = f(x4)
            if (f4 < f2) gsection (true,  x1, x4, x2, f4)
            else         gsection (false, x4, x2, x3, f2)
        } else {
            x4 = x1 + (floor (G_SECTION * dist)).toInt               // search right: x1 < x2 < x4 < x3
            f4 = f(x4)
            if (f2 < f4) gsection (true,  x1, x2, x4, f2)
            else         gsection (false, x2, x4, x3, f4)
        } // if
    } // gsection

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform a Line Search (LS) using the Integer Golden Search Algorithm.  Two
     *  phases are used:  an expansion phase (moving the end-point) to find a
     *  down-up pattern, followed by a traditional golden section search.
     *  @param xmax  a rough guess for the right end-point of the line search
     *  @param x1    the left (smallest) anchor point for the search (usually 1)
     */
    def solve (xmax: Int = 5, x1: Int = 1): Int =
    {
        val f1 = f(x1)
        var x2 = x1
        var f2 = 0.0
        var x3 = xmax
        var f3 = 0.0
        var matched = false
        breakable { for (k <- 1 to MAX_ITER) {   // expand right to try to find a down-up pattern
            val dist = x3 - x1
            x2 = x1 + (round (G_SECTION * dist)).toInt
            f2 = f(x2)
            f3 = f(x3)
            if (DEBUG) println ("solve: x1 = " + x1 + ", f1 = " + f1 +
                                      ", x2 = " + x2 + ", f2 = " + f2 +
                                      ", x3 = " + x3 + ", f3 = " + f3)
            if (f1 > f2 && f2 < f3) { matched = true; break }     // found down-up pattern, e.g., 20, 10, 30
            x2 = x3
            x3 = x1 + (round (G_RATIO * dist)).toInt     // increase upper bound
        }} // for
        if (matched) {
            gsection (true, x1, x2, x3, f2)      // apply golden section search on expanded interval
        } else {
            x2 = x1 + (G_SECTION * (xmax - x1)).toInt
            f2 = f(x2)
            gsection (true, x1, x2, xmax, f2)    // apply golden section search on original interval
        } // if
    } // solve
 
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the golden ratio and the golden section.
     */
    def printGolden ()
    {
        println ("GOLDEN_RATIO   = " + G_RATIO)
        println ("GOLDEN_SECTION = " + G_SECTION)
    } // printGolden

} // IntegerGoldenSectionLS class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntegerGoldenSectionLSTest` object is used to test the `IntegerGoldenSectionLS`
 *  class on scalar functions.
 */
object IntegerGoldenSectionLSTest extends App
{
    def f (x: Int): Int = (x - 40) * (x - 40) + 1         // requires expansion phase
    val optimizer = new IntegerGoldenSectionLS (f)
    println ("\nProblem 1: (x - 4)^2 + 1") 
    println ("optimal solution = " + optimizer.solve (10))

} // IntegerGoldenSectionLSTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntegerGoldenSectionLSTest2` object is used to test the `IntegerGoldenSectionLS`
 *  class on vector functions.
 */
//object IntegerGoldenSectionLSTest2 extends App
//{
//  val zo   = VectorD (0.0, 0.0)                       // zero vector, the origin
//  val dir  = VectorD (1.0, 1.0)                       // direction to search in
//  val ymax = 5.0
//  var y    = 0.0
//  var x    = zo
//
//  def f (x: VectorD): Double  = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
//  def g (y: Double): Double = f(zo + dir * y)
//  def f2 (x: VectorD): Double = x(0)/4.0 + 5.0*x(0)*x(0) + pow(x(0),4) -
//                                  9.0*x(0)*x(0)*x(1) + 3.0*x(1)*x(1) + 2.0*pow(x(1),4)
//  def g2 (y: Double): Double = f2(zo + dir * y)
//
//  val solver  = new IntegerGoldenSectionLS (g)
//  val solver2 = new IntegerGoldenSectionLS (g2)
//
//  println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
//  y = solver.search (ymax)
//  println ("optimal y solution = " + y)
//  x = zo + dir * y
//  println ("optimal x solution = " + x)
//  println ("optimal f solution = " + f(x))
//
//  println ("\nProblem 4: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
//  // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
//  y = solver2.search (ymax)
//  println ("optimal y solution = " + y)
//  x = zo + dir * y
//  println ("optimal x solution = " + x)
//  println ("optimal f solution = " + f(x))
//
//} // IntegerGoldenSectionLSTest2 object

