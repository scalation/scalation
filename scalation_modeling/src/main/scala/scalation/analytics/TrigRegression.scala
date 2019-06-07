
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Feb  2 18:18:15 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.{ListMap, Map, Set}
import scala.math.{cos, Pi, sin}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.plot.Plot
import scalation.stat.Statistic
import scalation.util.{banner, Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrigRegression` class supports trigonometric regression.  In this case,
 *  't' is expanded to '[1, sin (wt), cos (wt), sin (2wt), cos (2wt), ...]'.
 *  Fit the parameter vector 'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 sin (wt)  + b_2 cos (wt)  +
 *                                  b_3 sin (2wt) + b_4 cos (2wt) + ... + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y
 *      b  =  fac.solve (.)
 *  <p>
 *  @see link.springer.com/article/10.1023%2FA%3A1022436007242#page-1
 *  @param t          the input vector: t_i expands to x_i
 *  @param y          the response vector
 *  @param ord        the order (k), maximum multiplier in the trig function (kwt)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class TrigRegression (t: VectoD, y: VectoD, ord: Int, technique: RegTechnique = QR)
      extends PredictorVec (t, y, ord)
{
    private val w = (2.0 * Pi) / (t.max() - t.min())           // base displacement angle in radians
    private val x = new MatrixD (t.dim, 1 + 2 * ord)           // data matrix built from t
    for (i <- t.range) x(i) = expand (t(i))

    rg = new Regression (x, y, null, null, technique)          // regular multiple linear regression

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand the scalar 't' into a vector of powers of  trig terms/columns:
     *  '[1, sin (wt), cos (wt), sin (2wt), cos (2wt), ...]'.
     *  @param t  the scalar to expand into the vector
     */
    def expand (t: Double): VectoD = 
    {
        val wt = w * t
        val v = new VectorD (1 + 2 * ord)
        v(0) = 1.0
        for (j <- 1 to ord) {
            v(2*j-1) = sin (j * wt)
            v(2*j)   = cos (j * wt)
        } // for
        v
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot expand (z),
     *  e.g., (b_0, b_1, b_2) dot (1, z, z^2).
     *  @param z  the new scalar to predict
     */
    def predict (z: Double): Double = rg.predict (expand (z))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param ord    the maximum multiplier in the trig function (kwt)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (ord: Int, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((t: VectoD, y: VectoD, ord) => new TrigRegression (t, y, ord, technique), k, rando)
    } // crossVal

} // TrigRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrigRegressionTest` object tests `TrigRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1 sin wt + b_2 cos wt + ... b_2k-1 sin kwt + b_2k cos kwt + e
 *  <p>
 *  The data is generated from a noisy cubic function.
 *  > runMain scalation.analytics.TrigRegressionTest
 */
object TrigRegressionTest extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 10000.0)
    val t     = VectorD.range (0, 100)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 100) { val x = (i - 40)/2.0; y(i) = 1000.0 + x + x*x + x*x*x + noise.gen }

    println ("t = " + t)
    println ("y = " + y)

    val harmonics = 8
    val trg   = new TrigRegression (t, y, harmonics)
    trg.train ().eval ()
    println ("parameter = " + trg.parameter)
    println ("fitMap    = " + trg.fitMap)

    val z   = 10.5                                  // predict y for one point
    val yp1 = trg.predict (z)
    println ("predict (" + z + ") = " + yp1)

    banner ("test predictions")
    val yp = t.map (trg.predict (_))
    println (s" y = $y \n yp = $yp")
    new Plot (t, y, yp, "TrigRegression")

} // TrigRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrigRegressionTest2` object tests `TrigRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1 sin wt + b_2 cos wt + ... b_2k-1 sin kwt + b_2k cos kwt + e
 *  <p>
 *  The data is generated from periodic noisy cubic functions.
 *  > runMain scalation.analytics.TrigRegressionTest2
 */
object TrigRegressionTest2 extends App
{
    import scalation.random.Normal

    val noise = Normal (0.0, 10.0)
    val t     = VectorD.range (0, 200)
    val y     = new VectorD (t.dim)
    for (i <- 0 until 5) {
        for (j <- 0 until 20) { val x = j - 4;  y(40*i+j) = 100.0 + x + x*x + x*x*x + noise.gen }
        for (j <- 0 until 20) { val x = 16 - j; y(40*i+20+j) = 100.0 + x + x*x + x*x*x + noise.gen }
    } // for

    println ("t = " + t)
    println ("y = " + y)

    val harmonics = 16
    val trg   = new TrigRegression (t, y, harmonics)
    trg.train ().eval ()
    println ("parameter = " + trg.parameter)
    println ("fitMap    = " + trg.fitMap)

    val z   = 10.5                                  // predict y for one point
    val yp1 = trg.predict (z)
    println ("predict (" + z + ") = " + yp1)

    banner ("test predictions")
    val yp = t.map (trg.predict (_))
    println (s" y = $y \n yp = $yp")
    new Plot (t, y, yp, "TrigRegression")

} // TrigRegressionTest2 object

