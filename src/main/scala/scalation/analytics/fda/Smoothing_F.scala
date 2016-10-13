
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Sep 22 21:45:58 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see open.uct.ac.za/bitstream/item/16664/thesis_sci_2015_essomba_rene_franck.pdf?sequence=1
 *  @see www.jstatsoft.org/article/view/v051i04/v51i04.pdf
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
 *  @param y  the data points/vector
 *  @param t  the time points/vector
 *  @param n  the number of basis functions to use
 */
class Smoothing_F (y: VectorD, t: VectorD, n: Int = 2)
      extends Error
{
    private val m = y.dim                   // number of data points
    if (t.dim != m) flaw ("constructor", "time points must agree with data points")

    private val phi = new MatrixD (m, n)    // at time ti, value of jth spline
    private var c: VectoD = null            // coefficient vector

    private val bs = new B_Spline (t)       // use B-Spline basis functions

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model, i.e., determine the optimal cofficients 'c' for the
     *  basis functions.
     */
    def train (): VectoD =
    {
        form_phi ()
//      c = (phi.t * phi).inverse * phi.t * y            // solution without smoothness
        val lambda = 0.5                                 // smoothness parameter
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
        for (j <- 0 until n) sum += c(j) * bs.b3 (j, tt)
        sum
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form the phi matrix by evaluating the basis functions at the time points.
     */
    private def form_phi ()
    {
        for (i <- t.indices; j <- 0 until n) phi (i, j) = bs.b3 (j, t(i))
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
    val t = VectorD.range (0, 80) / 40.0
    val y = t.map ((x: Double) => 3.0 + 2.0 * x * x + normal.gen)

    val fr = new Smoothing_F (y, t, t.dim-3)
    val c  = fr.train ()

    println (s"y = $y")
    println (s"t = $t")
    println (s"c = $c")

    val yp = VectorD (for (i <- t.indices) yield fr.predict (t(i)))

    new Plot (t, y, yp, "B-Spline Fit")

} // Smoothing_FTest object

