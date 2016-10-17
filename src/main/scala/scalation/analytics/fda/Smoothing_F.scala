
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
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
import scalation.plot.Plot
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
 *  @param y    the data points/vector
 *  @param t    the data time points/vector
 *  @param τ    the time points/vector for the knots
 *  @param n    the number of basis functions to use
 *  @param ord  the order (degree+1) of B-Splines (2, 3, 4, 5 or 6)
 */
class Smoothing_F (y: VectorD, t: VectorD, τ: VectorD, ord: Int = 4)
      extends Error
{
    private val DEBUG = true                   // debug flag
    private val m     = t.dim                  // number of data time points
    private val n     = τ.dim                  // number of time points for the knots

    if (y.dim != m) flaw ("constructor", "require # data points == # data time points")
    if (n > m)      flaw ("constructor", "require # knot points <= # data time points")

    private val phi = new MatrixD (m, n)       // at time ti, value of jth spline
    private var c: VectoD = null               // coefficient vector

    private val bs = new B_Spline (τ, ord)     // use B-Spline basis functions

    if (DEBUG) println (s"m = $m, n = $n")

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
    /** Form the phi matrix by evaluating the basis functions at the time points.
     */
    private def form_phi ()
    {
        for (i <- t.indices; j <- 0 until n) phi (i, j) = bs.bb (ord) (j, t(i))
    } // form_phi

} // Smoothing_F class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Smoothing_FTest` is used to test the `Smoothing_F` class.
 *  > run-main scalation.analytics.fda.Smoothing_FTest
 */
object Smoothing_FTest extends App
{
    import scalation.random.Normal

    val normal = Normal ()
    val t   = VectorD.range (0, 100) / 100.0
    val y   = t.map ((x: Double) => 3.0 + 2.0 * x * x + normal.gen)

    for (ord <- 2 to 6) {
        val τ   = VectorD.range (0, 20 + ord) / 20.0
        val fr = new Smoothing_F (y, t, τ, ord)
        val c  = fr.train ()

        println (s"y = $y \nt = $t \nc = $c")

        val yp = VectorD (for (i <- t.indices) yield fr.predict (t(i)))
        new Plot (t, y, yp, s"B-Spline Fit: ord = $ord")
    } // for

} // Smoothing_FTest object

