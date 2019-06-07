
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Apr 18 14:24:14 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @see www.ias.ac.in/article/fulltext/reso/023/04/0439-0464
 */
 
//  U N D E R   D E V E L O P M E N T

package scalation.analytics

import scala.util.control.Breaks._
import scala.math.{abs, max}

import scalation.linalgebra._
import scalation.math.sign
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
class CoordDescentLasso (x: MatriD, y: VectoD, val lambda: Double = 0.1)
{
    private val DEBUG   = true
    private val maxIter = 200
    private val EPSILON = 1E-5

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def optimize (): VectoD =
    {
        var b: VectoD = null
        val b2 = new VectorD (x.dim2)

        breakable { for (k <- 0 until maxIter) {
            b = b2.copy ()
            for (j <- x.range2) b2(j) = lasso_coord (b, j)
            if (DEBUG) println (s"b = $b, b2 = $b2")
            if ((b - b2).normSq < EPSILON) break
        }} // breakable for
        b
    } // optimize

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def lasso_coord (b: VectoD, j: Int): Double =
    {
        val x_noj = x.sliceEx (x.dim1, j)                   // all columns of x except j
        val b_noj = b.sliceEx (j to j)                      // all elements of b except j
        val rj    = y - x_noj * b_noj                       // jth partial residual
        val x_j   = x.col (j)                               // jth column vector in x
        val b_ols = (rj dot x_j) / x.dim1                   // compute OLS estimate for b_j
        fast_sthresh (b_ols, lambda/2)                      // return Lasso estimate for b_j
    } // lasso_coord

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fast soft thresholding function.
     *  @param v    the value to threshold
     *  @param thr  the threshold
     */
    def fast_sthresh (v: Double, thr: Double): Double =
    {
        sign (max (abs (v) - thr, 0.0), v)
    } // fast_sthresh

} // CoordDescentLasso object

import MatrixTransform._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CoordDescentLassoTest` object tests `CoordDescentLasso` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2.
 *  <p>
 *  It compares `RidgeRegression` with `Regression`
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.CoordDescentLassoTest
 */
object CoordDescentLassoTest extends App
{
    // 5 data points:             x_0    x_1
    val x = new MatrixD ((5, 2), 36.0,  66.0,               // 5-by-2 matrix
                                 37.0,  68.0,
                                 47.0,  64.0,
                                 32.0,  53.0,
                                  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    // Compute centered (zero mean) versions of x and y

    val mu_x = x.mean                                       // columnwise mean of x
    val sig_x = stddev (x)
    val x_n  = normalize (x, (mu_x, sig_x)) 
    val mu_y = y.mean                                       // mean of y
    val sig_y = y.stddev
    val y_n  = y.standardize                                     // centered y

    println (s"x_n = $x_n \ny_n = $y_n")

    banner ("Regression")
    val ox = VectorD.one (y.dim) +^: x
    val rg = new Regression (ox, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    val oz = VectorD (1.0, 20.0, 80.0)
    var yp = rg.predict (oz)                                // predict z and add y's mean
    println (s"predict ($z) = $yp")

    banner ("CoordDescentLasso")
    val cdl = new CoordDescentLasso (x_n, y_n)
    println (s"optimize = ${cdl.optimize}")
/*
    val = new RidgeRegression (x_c, y_c)
    rrg.train ().eval ()
    println (rrg.report)
    println (rrg.summary)

    val z_c = z - mu_x                                      // center z first
    yp = rrg.predict (z_c) + mu_y                           // predict z_c and add y's mean
    println (s"predict ($z) = $yp")
*/

} // CoordDescentLassoTest object

