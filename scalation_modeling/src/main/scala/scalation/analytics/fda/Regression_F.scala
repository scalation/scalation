
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Oct 11 16:12:54 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see orfe.princeton.edu/~jqfan/papers/07/WuFanMueller1.pdf
 *  @see www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=4&cad=rja&uact=8&ved=0ahUKEwizk5f2q9PPAhWCSCYKHVF2Be8QFggxMAM&url=http%3A%2F%2Fanson.ucdavis.edu%2F~mueller%2Fhandbook-final1.pdf&usg=AFQjCNHS96onDE2qFFynU1L1xAx27wh0lA&sig2=PLLilZsXqGaI-GV8g5njQA
 *  @see Functional Data Analysis, Second Edition, Chapter 12
 *  @see http://link.springer.com/book/10.1007%2Fb98888
 */

package scalation.analytics
package fda

import scalation.calculus.DB_Spline
import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_F` class performs functional linear regression.
 *  <p>
 *      y = b0 + b1 * x(t) + ε
 *  <p>
 *  @param y    the response vector
 *  @param x    the covariate vector - treated as functional
 *  @param t    the time vector
 *  @param τ    the knot vector
 *  @param ord  the order (degree+1) of the B-Splines (2 to 6)
 */
class Regression_F (y: VectorD, x: VectorD, t: VectorD, τ: VectorD, ord: Int = 4)
{
    private val DEBUG = true                             // debug flag

    private var b: VectoD = null                         // regression coefficients
    private var c: VectoD = null                         // smoothing coefficients

//  private val bs  = new B_Spline (t)                   // use B-Spline basis functions
//  private val moo = new Smoothing_F (x, t, τ, ord)
    private val bs  = new DB_Spline (t)                  // use derivative B-Spline basis functions
    private val moo = new Smoothing_F (x, t, bs)
    private val xx  = new MatrixD (x.dim, 2)             // 2-column data matrix [1, xs]
    xx.setCol (0, x.one ())                              // column of all ones
    xx.setCol (1, smooth (x))                            // column of smoothed x, i.e., xs

    if (DEBUG) println ("data matrix xx = " + xx)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model using the smoothed data to find the regression coefficients 'b'.
     */
    def train (): VectoD =
    {
/*
        b = (xx.t * xx).inverse * xx.t * y               // direct solution, may produce NaN
*/
        val hp = RidgeRegression.hp.updateReturn ("lambda", 0.01)
        val rrg = new RidgeRegression (xx, y, null, hp)
        rrg.train ()
        b = rrg.parameter
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
        for (j <- xt.indices) sum += b(j) * xt(j)        // c(j) * bs.b3 (j, tt)
        sum
    } // predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the data vector 'x' using B-Spline expansion
     *  @param x  the data vector to smooth
     */
    private def smooth (x: VectorD): VectorD =
    {
        val xs = new VectorD (x.dim)                     // smoothed version of x
        c      = moo.train ()                            // smoothing coefficients
        if (DEBUG) println ("c = " + c)
        for (j <- t.range) xs(j) = moo.predict (t(j))
        xs
    } // smooth

} // Regression_F class

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_FTest` object is used to test the `Regression_F` class.
 *  > runMain scalation.analytics.fda.Regression_FTest
 */
object Regression_FTest extends App
{
    import scalation.random.Normal

    val normal = Normal (0.0, 0.2)
    val t = VectorD.range (0, 100) / 100.0
    val x = t.map ((x: Double) => x * x)
    val y = t.map ((x: Double) => 3.0 + 5.0 * x * x + normal.gen)

    println (s"y = $y \nx = $x \n t = $t")

    for (ord <- 2 to 6) {
        val τ   = VectorD.range (0, 20 + ord) / 20.0
        val rgf = new Regression_F (y, x, t, τ, ord)
        println ("b = " + rgf.train ())
        val yp  = t.map (rgf.predict (_))
        new Plot (t, y, yp, s"RegressionF - ord = $ord")
    } // for

} // Regression_FTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression_FTest2` object is used to test the `Regression_F` class.
 *  > runMain scalation.analytics.fda.Regression_FTest2
 */
object Regression_FTest2 extends App
{
    val ord = 4
    val y   = VectorD ( 2.1,  4.3,  5.9,  7.7, 10.3, 11.8, 14.1, 15.9, 18.1, 20.0, 
                       22.1, 24.3, 25.9, 27.7, 30.3, 31.8, 34.1, 35.9, 38.1, 40.0) 
    val x   = VectorD.range (1, 21)
    val t   = VectorD.range (0, 20) / 20.0
    val τ   = VectorD.range (0, 10 + ord) / 10.0

    println (s"y = $y \nx = $x \n t = $t")

    val rgf = new Regression_F (y, x, t, τ)
    println ("b = " + rgf.train ())
    val yp = t.map (rgf.predict (_))
    new Plot (t, y, yp, "Regression - ord = default")

} // Regression_FTest2 object

