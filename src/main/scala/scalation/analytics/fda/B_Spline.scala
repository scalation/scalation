
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Sep 22 21:45:58 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see en.wikipedia.org/wiki/B-spline
 *  @see cran.r-project.org/web/packages/crs/vignettes/spline_primer.pdf
 */

package scalation.analytics.fda

import scala.math.max

import scalation.linalgebra.VectorD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `B_Spline` class provides B-Spline basis functions for various orders 'm',
 *  where the order is one more than the degree.  A spline function is a piecewise
 *  polynomial function where the pieces are stitched together at knots with the
 *  goal of maintaining continuity and differentability.  B-Spline basis functions
 *  form a popular form of basis functions used in Functional Data Analysis.
 *  @see open.uct.ac.za/bitstream/item/16664/thesis_sci_2015_essomba_rene_franck.pdf?sequence=1
 *-----------------------------------------------------------------------------
 *  @param ττ    the time-points of the original knots in the time dimension
 *  @param mMax  the maximum order, allowing splines orders from 1 to nMax
 */
class B_Spline (ττ: VectorD, mMax: Int = 4)
{
    private val DEBUG = true                                 // debug flag
    private val l     = ττ.dim - 1                           // the number of intervals
    private val τ     = ττ ++ VectorD.fill (mMax-1)(ττ(l))   // augment on right to avoid index error

    if (DEBUG) println ("τ = " + τ)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order one 'm = 1' B-Spline basis functions (flat functions).
     *  @param j  indicates which spline function 0 to l-1
     *  @param t  the time parameter
     */
    def b1 (j: Int, t: Double): Double =
    {
        val k = max (0, j)                                   // no negative indices
        if (τ(k) <= t && t < τ(k+1)) 1.0 else 0.0
    } // b_1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order two 'm = 2' B-Spline basis functions (linear functions).
     *  @param j  indicates which spline function: 0 to l-2 
     *  @param t  the time parameter
     */
    def b2 (j: Int, t: Double): Double =
    {
        val k = max (0, j)
        (t - τ(k)) / (τ(k+1) - τ(k)) * b1 (k, t) +
        (τ(k+2) - t) / (τ(k+2) - τ(k+1)) * b1 (k+1, t)
    } // b2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order three 'm = 3' B-Spline basis functions (quadratic functions).
     *  @param j  indicates which spline function: 0 to l-3
     *  @param t  the time parameter
     */
    def b3 (j: Int, t: Double): Double =
    {
        val k = max (0, j)
        (t - τ(k)) / (τ(k+2) - τ(k)) * b2 (k, t) +
        (τ(k+3) - t) / (τ(k+3) - τ(k+1)) * b2 (k+1, t)
    } // b3

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order four 'm = 4' B-Spline basis functions (cubic functions).
     *  @param j  indicates which spline function: 0 to l-4
     *  @param t  the time parameter
     */
    def b4 (j: Int, t: Double): Double =
    {
        val k = max (0, j)
        (t - τ(k)) / (τ(k+3) - τ(k)) * b3 (k, t) +
        (τ(k+4) - t) / (τ(k+4) - τ(k+1)) * b3 (k+1, t)
    } // b4

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order 'm' B-Spline basis functions (general recurrence).
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function: 0 to l-m
     *  @param t  the time parameter
     */
    def bb (m: Int)(j: Int, t: Double): Double =
    {
        val k = max (0, j)
        if (m == 1) return  b1 (k, t)

        val km = k + m
        (t - τ(k)) / (τ(km-1) - τ(k)) * bb (m-1)(k, t) +
        (τ(km) - t) / (τ(km) - τ(k+1)) * bb (m-1)(k+1, t)
    } // bb

} // B_Spline class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `B_SplineTest` object is used to test the `B_Spline` class.
 *  It tests the B-Spline functions for specific orders.
 *  > run-main scalation.analytics.fda.B_SplineTest
 */
object B_SplineTest extends App
{
    import scalation.plot.Plot

    val mM = 4                                               // maximum order to test
    val τ  = VectorD (0.0, 20.0, 40.0, 60.0, 80.0, 100)      // knot time-points
    val bs = new B_Spline (τ, mM)                            // B-Spline generator
    val n  = 100                                             // number of time-points for plotting
    val t  = VectorD.range (0, n)                            // time-points for plotting
    val k  = 0                                               // index for initial B-Spline

    //-------------------------------------------------------------------------
    // order 1 B-Splines (flat functions, degree 0)

    val y1 = new VectorD (n)                                 // vector to hold initial B-Spline
    val y2 = new VectorD (n)                                 // vector to hold next B-Spline

    for (i <- 0 until n) {                                   // order 1 B-Splines
        y1(i) = bs.b1 (k, i)                                 // initial B-Spline
        y2(i) = bs.b1 (k+1, i)                               // next B-Spline
    } // for
    new Plot (t, y1, y2, "B-Spline order " + 1)

    //-------------------------------------------------------------------------
    // order 2 B-Splines (linear functions, degree 1)

    val y3 = new VectorD (n)                                 // vector to hold initial B-Spline
    val y4 = new VectorD (n)                                 // vector to hold next B-Spline

    for (i <- 0 until n) {                                   // order 2 B-Splines
        y3(i) = bs.b2 (k, i)                                 // initial B-Spline
        y4(i) = bs.b2 (k+1, i)                               // next B-Spline
    } // for
    new Plot (t, y3, y4, "B-Spline order " + 2)

    //-------------------------------------------------------------------------
    // order 3 B-Splines (quadratic functions, degree 2)

    val y5 = new VectorD (n)                                 // vector to hold initial B-Spline
    val y6 = new VectorD (n)                                 // vector to hold next B-Spline

    for (i <- 0 until n) {                                   // order 3 B-Splines
        y5(i) = bs.b3 (k, i)                                 // initial B-Spline
        y6(i) = bs.b3 (k+1, i)                               // next B-Spline
    } // for
    new Plot (t, y5, y6, "B-Spline order " + 3)

    //-------------------------------------------------------------------------
    // order 4 B-Splines (cubic functions, degree 3)

    val y7 = new VectorD (n)                                 // vector to hold initial B-Spline
    val y8 = new VectorD (n)                                 // vector to hold next B-Spline

    for (i <- 0 until n) {                                   // order 4 B-Splines
        y7(i) = bs.b4 (k, i)                                 // initial B-Spline
        y8(i) = bs.b4 (k+1, i)                               // next B-Spline
    } // for
    new Plot (t, y7, y8, "B-Spline order " + 4)

} // B_SplineTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `B_SplineTest2` object is used to test the `B_Spline` class.
 *  It tests the B-Spline functions using the general recurrence.
 *  > run-main scalation.analytics.fda.B_SplineTest2
 */
object B_SplineTest2 extends App
{
    import scalation.plot.Plot

    val mM = 4                                               // maximum order to test
    val τ  = VectorD (0.0, 20.0, 40.0, 60.0, 80.0, 100.0)    // knot time-points
    val bs = new B_Spline (τ, mM)                            // B-Spline generator
    val n  = 100                                              // number of time-points for plotting
    val t  = VectorD.range (0, n)                            // time-points for plotting
    val k  = 0                                               // index for initial B-Spline

    for (m <- 1 to mM) {

        //---------------------------------------------------------------------
        // order m B-Splines (polynomial functions)

        val y1 = new VectorD (n)                             // vector to hold initial B-Spline
        val y2 = new VectorD (n)                             // vector to hold next B-Spline

        for (i <- 0 until n) {
            y1(i) = bs.bb (m)(k, i)                          // initial B-Spline
            y2(i) = bs.bb (m)(k+1, i)                        // next B-Spline
        } // for
        new Plot (t, y1, y2, "B-Spline order " + m)

   } // for

} // B_SplineTest2 object

