
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Thu Sep 22 21:45:58 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see open.uct.ac.za/bitstream/item/16664/thesis_sci_2015_essomba_rene_franck.pdf?sequence=1
 *  @see www.jstatsoft.org/article/view/v051i04/v51i04.pdf
 *  @see Functional Data Analysis, Second Edition, Chapter 4
 *  @see http://link.springer.com/book/10.1007%2Fb98888
 */

package scalation.analytics.fda

import scalation.analytics.RidgeRegression
import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.plot.{FPlot, Plot}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Smoothing_F` class fits a time-dependent data vector 'y' to B-Splines.
 *  <p>
 *      y(t(i)) = x(t(i)) + ε(t(i))
 *      x(t) = cΦ(t)
 *  <p>
 *  where 'x' is the signal, 'ε' is the noise, 'c' is a coefficient vector and
 *  'Φ(t)' is a vector of basis functions. 
 *-----------------------------------------------------------------------------
 *  @param y    the (raw) data points/vector
 *  @param t    the data time points/vector
 *  @param τ    the time points/vector for the knots
 *  @param n    the number of basis functions to use
 *  @param ord  the order (degree+1) of B-Splines (2, 3, 4, 5 or 6)
 */
class Smoothing_F (y: VectorD, t: VectorD, private var τ: VectorD = null, ord: Int = 4)
      extends Error
{
    private val DEBUG = true                   // debug flag
    private val GAP   = 5                      // gap between time points and knots
    private val m     = t.dim                  // number of data time points
    if (τ == null) τ  = makeKnots
    private val n     = τ.dim                  // number of time points for the knots

    if (y.dim != m) flaw ("constructor", "require # data points == # data time points")
    if (n > m)      flaw ("constructor", "require # knot points <= # data time points")

    private val phi = new MatrixD (m, n)       // at time ti, value of jth spline
    private var c: VectoD = null               // coefficient vector

    private val bs = new B_Spline (τ, ord)     // use B-Spline basis functions

    if (DEBUG) println (s"m = $m, n = $n")

    def makeKnots: VectorD = VectorD.range (0, (m/GAP + ord)) / (m/GAP.toDouble)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model, i.e., determine the optimal cofficients 'c' for the
     *  basis functions.
     */
    def train (): VectoD =
    {
        form_phi ()
/*
        if (DEBUG) println ("phi = " + phi)
        c = (phi.t * phi).inverse * phi.t * y            // may produce NaN
*/
        val lambda = 0.01
        val rrg = new RidgeRegression (phi, y, lambda)   
        rrg.train ()
        c = rrg.coefficient
        c
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the y-value at time point 'tt'.
     *  @param tt  the given time point
     */
    def predict (tt: Double): Double =
    {
        var sum = 0.0
        for (j <- 0 until n) sum += c(j) * bs.bb (ord) (j, tt)
        sum
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the y-values at all time points in vector 'tv'.
     *  @param tv  the given vector of time points
     */
    def predict (tv: VectorD): VectorD = tv.map (predict (_))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form the phi matrix by evaluating the basis functions at the time points.
     */
    private def form_phi ()
    {
        for (i <- t.indices; j <- 0 until n) phi (i, j) = bs.bb (ord) (j, t(i))
    } // form_phi

} // Smoothing_F class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Smoothing_FTest` is used to test the `Smoothing_F` class.
 *  > runMain scalation.analytics.fda.Smoothing_FTest
 */
object Smoothing_FTest extends App
{
    import scalation.random.Normal

    val normal = Normal ()                                           // normal random variate generator
    val t = VectorD.range (0, 100) / 100.0                           // time points
    val y = t.map ((x: Double) => 3.0 + 2.0 * x * x + normal.gen)    // raw data points

    for (ord <- 2 to 6) {
//      val τ   = VectorD.range (0, 20 + ord) / 20.0                 // time points for knots
        val τ   = null                                               // let `Smoothing_F` nake the knots
        val moo = new Smoothing_F (y, t, τ, ord)                     // smoother
        val c   = moo.train ()                                       // train -> set coefficients

        println (s"y = $y \nt = $t \nc = $c")

        val x = moo.predict (t)                                      // predict for all time points
        new Plot (t, y, x, s"B-Spline Fit: ord = $ord")
    } // for

} // Smoothing_FTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Smoothing_FTest2` is used to test the `Smoothing_F` class.
 *  > runMain scalation.analytics.fda.Smoothing_FTest2
 */
object Smoothing_FTest2 extends App
{
    import scalation.random.Normal

    val normal = Normal ()
    val t  = VectorD.range (0, 100) / 100.0
    val t2 = VectorD.range (0, 1000) / 1000.0
    val y  = t.map ((x: Double) => 3.0 + 2.0 * x * x + normal.gen)

    for (ord <- 2 to 6) {
        val τ   = VectorD.range (0, 20 + ord) / 20.0
        val moo = new Smoothing_F (y, t, τ, ord)
        val c   = moo.train ()

        println (s"y = $y \nt = $t \nc = $c")

        new FPlot (t, y, t2, moo.predict, s"B-Spline Fit: ord = $ord")
    } // for

} // Smoothing_FTest2 object

