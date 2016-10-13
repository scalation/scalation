
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Oct 11 16:12:54 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see orfe.princeton.edu/~jqfan/papers/07/WuFanMueller1.pdf
 *  @see www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=4&cad=rja&uact=8&ved=0ahUKEwizk5f2q9PPAhWCSCYKHVF2Be8QFggxMAM&url=http%3A%2F%2Fanson.ucdavis.edu%2F~mueller%2Fhandbook-final1.pdf&usg=AFQjCNHS96onDE2qFFynU1L1xAx27wh0lA&sig2=PLLilZsXqGaI-GV8g5njQA
 */

// U N D E R   D E V E L O P M E N T

package scalation.analytics.fda

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_F` class performs functional linear regression.
 *  <p>
 *      y = b0 + b1 * x(t) + ε             ??
 *  <p>
 *  @param y  the response vector
 *  @param x  the covariate vector - treated as functional
 *  @param t  the time vector
 *  @param n  the number of basis functions to use
 */
class Regression_F (y: VectorD, x: VectorD, t: VectorD, n: Int = 2)
{
    private val DEBUG = true

    private var b: VectoD = null                         // regression coefficients
    private var c: VectoD = null                         // smoothing coefficients

    private val bs  = new B_Spline (t)                   // use B-Spline basis functions
    private val moo = new Smoothing_F (x, t, t.dim-3)
    private val xx  = new MatrixD (x.dim, 2)             // 2-column data matrix [1, xs]
    xx.setCol (0, x.one ())                              // column of all ones
    xx.setCol (1, smooth (x))                            // column of smoothed x, i.e., xs

    if (DEBUG) println ("data matrix xx = " + xx)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model using the smoothed data to find the regression coefficients 'b'.
     */
    def train (): VectoD =
    {
        b = (xx.t * xx).inverse * xx.t * y               // solution without smoothness
/*
        val lambda = 0.1                                 // smoothness parameter
        val rrg = new RidgeRegression (xx, y, lambda)
        rrg.train ()
        b = rrg.coefficient
*/
        b
    } // train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the y-value at time point 'tt'.
     *  @param tt  the given time point
     */
    def predict (tt: Double): Double =
    {
        var sum = 0.0
        println ("b = " + b)
        println ("c = " + c)
        val xt = VectorD (1.0, moo.predict (tt))
        for (j <- xt.indices) sum += b(j) * xt(j)            // c(j) * bs.b3 (j, tt)
        sum
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the data vector 'x' using B-Spline expansion
     *  @param x  the data vector to smooth
     */
    private def smooth (x: VectorD): VectorD =
    {
        val xs = new VectorD (x.dim)                      // smoothed version of x
        c      = moo.train ()                             // smoothing coefficients
        if (DEBUG) println ("c = " + c)
        for (j <- t.range) xs(j) = moo.predict (t(j))
        xs
    } // smooth

} // Regression_F class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_FTest` object is used to test the `Regression_F` class.
 *  > run-main scalation.analytics.fda.Regression_FTest
 */
object Regression_FTest extends App
{
    val y = VectorD (2.1, 4.3, 5.9, 7.7, 10.3, 11.8, 14.1, 15.9, 18.1, 20.0) 
    val x = VectorD.range (1, 11)
    val t = VectorD.range (0, 10) / 10.0

    println ("y = " + y)
    println ("x = " + x)
    println ("t = " + t)

    val rgf = new Regression_F (y, x, t, t.dim-3)

    println ("b = " + rgf.train ())

    val yp = t.map (rgf.predict (_))

    new Plot (t, y, yp)

} // Regression_FTest object

