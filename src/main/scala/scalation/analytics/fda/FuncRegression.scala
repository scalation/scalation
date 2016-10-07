
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Sep 22 21:45:58 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 * open.uct.ac.za/bitstream/item/16664/thesis_sci_2015_essomba_rene_franck.pdf?sequence=1
 */

package scalation.analytics.fda

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FuncRegression` class fits a data vector to B-Splines.
 *  @param y  the data points/vector
 *  @param t  the time points/vector
 *  @param the number of basis functions to use
 */
class FuncRegression (y: VectorD, t: VectorD, n: Int = 2)
      extends Error
{
    private val m   = y.dim
    if (t.dim != m) flaw ("constructor", "time points must agree with data points")

    private val phi = new MatrixD (m, n)
    private var c: VectorD = null

    private val bs = new B_Spline (t)
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form the phi matrix by evaluating the basis functions at the time points.
     */
    def form_phi ()
    {
        for (i <- t.indices; j <- 0 until n) phi (i, j) = bs.b3 (j, t(i))
    } // form_phi

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model, i.e., determine the optimal cofficients 'c' for the
     *  basis functions
     *  FIX: need more basis functions and a roughness penalty parameter and term
     */
    def train (): VectorD =
    {
        c = (phi.t * phi).inverse * phi.t * y
        c
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the y-value at time point 'tt'.
     *  @param tt  the given time point.
     */
    def predict (tt: Double): Double =
    {
        var sum = 0.0
        for (j <- 0 until n) sum += c(j) * bs.b3 (j, tt)
        sum
    } // predict

} // FuncRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FuncRegressionTest` is used to test the `FuncRegression` class.
 *  > run-main scalation.analytics.fda.FuncRegressionTest
 */
object FuncRegressionTest extends App
{
    val y = VectorD (1)
    val t = VectorD (1)

    val fr = new FuncRegression (y, t)
    fr.form_phi ()
    val c = fr.train ()

    println (s"y = $y")
    println (s"t = $t")
    println (s"c = $c")

    val yp = VectorD (for (i <- t.indices) yield fr.predict (t(i)))

    new Plot (t, y, yp, "B-Spline Fit")

} // FuncRegressionTest object

