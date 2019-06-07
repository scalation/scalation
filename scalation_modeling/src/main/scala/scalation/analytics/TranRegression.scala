
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 1.6
 *  @date    Sat Jan 20 15:41:27 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @see data.princeton.edu/wws509/notes/c2s10.html
 */

package scalation.analytics

import scala.math.{abs, exp, log, sqrt}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.math.{double_exp, FunctionS2S, sq}
import scalation.random.Normal
import scalation.plot.{Plot, PlotM}
import scalation.stat.Statistic
import scalation.util.banner

import MatrixTransform._
import PredictorMat.pullResponse
import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegression` class supports transformed multiple linear regression.
 *  In this case, 'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter
 *  vector 'b' in the transformed regression equation
 *  <p>
 *      transform (y)  =  b dot x + e  =  b_0 + b_1 * x_1 +  b_2 * x_2 ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model) and
 *  'transform' is the function (defaults to log) used to transform the response vector 'y'.
 *  Common transforms include 'log (y)', 'sqrt (y)' when 'y > 0', or even 'sq (y)', 'exp (y)'.
 *  More generally, a Box-Cox Transformation may be applied.
 *  @see citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.469.7176&rep=rep1&type=pdf
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector 'b'
 *  Note: this class does not provide transformations on columns of matrix 'x'.
 *  @see www.ams.sunysb.edu/~zhu/ams57213/Team3.pptx
 *  @param x          the data/input matrix
 *  @param y          the response/output vector
 *  @param fname_     the feature/variable names
 *  @param tran       the transformation function (defaults to log)
 *  @param itran      the inverse transformation function to rescale predictions to original y scale (defaults to exp)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class TranRegression (x: MatriD, y: VectoD, fname_ : Strings = null,
                      tran: FunctionS2S = log, itran: FunctionS2S = exp,
                      technique: RegTechnique = QR)
      extends Regression (x, y.map (tran), fname_, null, technique)
{
    private val DEBUG = true                                       // debug flag

    if (! y.isNonnegative)
        throw new IllegalArgumentException ("y must be positive for transformed regression (log, sqrt")
                                            // FIX - may work for other transformations
    private val inf = findInfinity (getY)
    if (! inf.isEmpty) flaw ("constructor", s"the transformed response vector has infinite elements at $inf")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics based on transformed data using
     *  the 'eval' method from the superclass `Regression`.
     *  @param xx  the data/input matrix
     *  @param yy  the response/output vector
     */
    def eval0 (xx: MatriD = x, yy: VectoD = getY): TranRegression =
    {
        super.eval (xx, yy)
        this
    } // eval0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics based on original (untransformed)
     *  data.
     *  @param xx  the data/input matrix
     *  @param yy  the response/output vector
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): TranRegression =
    {
        val yp = (xx * b).map (itran)                     // y predicted for xx (test/full)
        e = yy - yp                                       // residual/error vector for original data
        diagnose (e, yy)                                  // compute diagnostics
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = itran (b dot z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    override def predict (z: MatriD = x): VectoD = (z * b).map (itran)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    override def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new TranRegression (x, y, fname, tran, itran, technique),
                                                 xx, k, rando)
    } // crossVal

} // TranRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegression` companion object provides transformation and inverse
 *  transformation function based on the parameter 'lambda'.
 *  It support the family of Box-Cox transformations.
 */
object TranRegression
{
    private val DEBUG  = true                       // debug flag
    private var lambda = 0.5                        // the power parameter for Box-Cox tranformations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the value for the 'lambda' parameter.
     *  @param lambda_  the new value for the 'lambda' parameter
     */
    def set_lambda (lambda_ : Double) { lambda = lambda_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform 'y' using the Box-Cox transformation.
     *  @param y  the value to be transformed
     */
    def box_cox (y: Double): Double =
    {
        if (lambda == 0.0) log (y)
        else               (y ~^ lambda - 1.0) / lambda
    } // box_cox

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Inverse transform 'z' using the Box-Cox transformation.
     *  @param z  the value to be inverse transformed
     */
    def cox_box (z: Double): Double =
    {
        if (lambda == 0.0) exp (z)
        else               (lambda * z + 1.0) ~^ (1.0 / lambda)
    } // cox_box

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Create a `TranRegression` object that uses a Box-Cox transformation.
     *  @param x          the data/input matrix
     *  @param y          the response/output vector
     *  @param fname      the feature/variable names
     *  @param lamdba_    the Box-Cox power parameter
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
     */
    def apply (x: MatriD, y: VectoD, fname: Strings = null, lambda_ : Double = 0.5,
               technique: RegTechnique = QR): TranRegression =
    {
        set_lambda (lambda_)
        new TranRegression (x, y, fname, box_cox, cox_box, technique)
    } // apply

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `TranRegression` with automatic rescaling from a combined data matrix.
     *  @param xy         the combined data/input and response/output matrix
     *  @param fname      the feature/variable names
     *  @param tran       the transformation function (defaults to log)
     *  @param itran      the inverse transformation function to rescale predictions to original y scale (defaults to exp)
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
     *  @param bounds     the bounds for rescaling
     *  @param rescale    the whether to rescale response vector y to match transform function
     */
    def apply (xy: MatriD, fname: Strings,
               tran: FunctionS2S, itran: FunctionS2S,
               technique: RegTechnique,
               bounds: PairD, rescale: Boolean): TranRegression =
    {
        val (x, y) = pullResponse (xy)

        val y_s =                                                   // scaled version of y
            if (rescale) {
                if (bounds != null) {                               // scale to bounds
                   val extrem = extreme (y)
                   scaleV (extrem, bounds)(y)
               } else {                                             // normalize
                  val (mu_y, sig_y) = (y.mean, stddev (y))
                  normalizeV (mu_y, sig_y)(y)
               } // if
           } else {                                                 // do not rescale
               y
           } // if

        if (DEBUG) println ("scaled: \nscaled y_s = " + y_s)
        new TranRegression (x, y_s, fname, tran, itran, technique)
    } // apply

} // TranRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionEx` provides a sample dataset for testing purposes.
 *  Move the comments on the line used to generate the reposnse 'y(k)' to test
 *  1D and 2D cases.
 */
object TranRegressionEx
{
    private val cap    = 30
    private val rng    = 0 until cap
    private val (m, n) = (cap * cap, 3)
    private val err    = Normal (0, cap)

    val x = new MatrixD (m, n)
    val y = new VectorD (m)
    for (i <- rng; j <- rng) x(cap * i + j) = VectorD (1, i, j)
    for (k <- y.range) y(k) = sq (10 + 2 * x(k, 1) + err.gen)
//  for (k <- y.range) y(k) = sq (10 + 2 * x(k, 1) + 0.3 * x(k, 2) + err.gen)
    val t = VectorD.range (0, y.dim)

} // TranRegressionEx


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      log (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest
 */
object TranRegressionTest extends App
{
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x)
    println ("y = " + y)

    val trg = new TranRegression (x, y)
    trg.train ().eval ()

    banner ("Parameter Estimation and Quality of Fit")
    println (trg.report)
    println (trg.summary)

    banner ("Quality of Fit - based on original data")
    trg.eval0 ()
    println ("fitMap = " + trg.fitMap)

    banner ("Prediction")
    val yp = trg.predict (x)
    println (s"predict (x) = $yp")

    val yp2 = trg.predict (z)
    println (s"predict ($z) = $yp2")

} // TranRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest2` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      sqrt (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest2
 */
object TranRegressionTest2 extends App
{
    // 9 data points:        Constant    x1    x2     y
    val xy = new MatrixD ((9, 4), 1.0,  1.0,  1.0,  0.04,
                                  1.0,  2.0,  1.0,  0.05,
                                  1.0,  3.0,  1.0,  0.06,

                                  1.0,  1.0,  2.0,  0.10,
                                  1.0,  2.0,  2.0,  0.11,
                                  1.0,  3.0,  2.0,  0.12,

                                  1.0,  1.0,  3.0,  0.20,
                                  1.0,  2.0,  3.0,  0.21,
                                  1.0,  3.0,  3.0,  0.22)

    val (x, y) = pullResponse (xy)
    val xtx    = x.t * x
    val yy   = y.map (sqrt _)
    val xtyy = x.t * yy
    val b    = xtx.inverse * xtyy

    banner ("parameters")
    println (s"xtx  = $xtx")
    println (s"xtyy = $xtyy")
    println (s"b    = $b")

    val yyp  = x * b                      // transformed
    val sst  = (yy - yy.mean).normSq
    val e    = yy - yyp
    val sse  = e.normSq
    val rSq  = 1.0 - sse / sst

    banner ("transformed")
    println (s"yy   = $yy")
    println (s"yyp  = $yyp")
    println (s"e    = $e")
    println (s"sst  = $sst")
    println (s"sse  = $sse")
    println (s"rSq  = $rSq")

    banner ("original")
    val yp   = yyp.map (sq _)             // orginal
    val sst2 = (y - y.mean).normSq
    val e2   = y - yp
    val sse2 = e2.normSq
    val rSq2  = 1.0 - sse2 / sst2

    println (s"y    = $y")
    println (s"yp   = $yp")
    println (s"e2   = $e2")
    println (s"sst2 = $sst2")
    println (s"sse2 = $sse2")
    println (s"rSq2 = $rSq2")

    val trg = new TranRegression (x, y, null, sqrt _, sq _)
    trg.train ().eval ()

    banner ("Parameter Estimation and Quality of Fit")
    println (trg.report)
    println (trg.summary)

} // TranRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest3` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      sqrt (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest3
 */
object TranRegressionTest3 extends App
{
    import TranRegressionEx.{x, y, t}

    // Phase 1 ================================================================
    banner ("Regression prediction yp")
    val rg = new Regression (x, y)
    rg.train ().eval ()
    val sst = rg.fit (rg.index_sst)
    println (rg.report)
    println (rg.summary)

    val yp = rg.predict ()
    val e  = y - yp

    new Plot (t, y, yp, "Original Regression y and yp vs. t")
    new Plot (t, e, null, "Original e vs. t")

    // Phase 2 ================================================================
    banner ("Transform y to y2")
    val y2  = y.map (sqrt _)
    val trg = new Regression (x, y2)
    trg.train ().eval ()
    println (trg.report)
    println (trg.summary)

    val yp2 = trg.predict ()
    val e2  = y2 - yp2

    new Plot (t, y2, yp2, "Transformed Regression y2 and yp2 vs. t")
    new Plot (t, e2, null, "Transformed e2 vs. t")

    // Phase 3 ================================================================
    banner ("Inverse Transform yp2 to yp3")
    val yp3 = yp2.map (sq _)
    val e3  = y - yp3

    val sse = e3 dot e3
    println (s"R^2 = ${1 - (sse / sst)}")

    new Plot (t, y, yp3, "Tran-back Regression y and yp3 vs. t")
    new Plot (t, e3, null, "Tran-back e3 vs. t")

    val ys2 = MatrixD (Seq (y2, yp2))
    val ys3 = MatrixD (Seq (y, yp3, yp))
    new PlotM (t, ys2.t, null, "Transformed")
    new PlotM (t, ys3.t, null, "Tran-back")

} // TranRegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest4` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      sqrt (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest4
 */
object TranRegressionTest4 extends App
{
    import TranRegressionEx.{x, y, t}

    banner ("Regession")
    val rg = new Regression (x, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    val t  = VectorD.range (0, y.dim)
    val yp = rg.predict ()
    val e  = rg.residual

    banner ("TranRegession")
    val trg = new TranRegression (x, y, null, sqrt _, sq _)
    trg.train ().eval ()
    println (trg.report)
    println (trg.summary)

    trg.eval0 ()
    println (trg.report)
    println (trg.summary)

    val yp2 = trg.predict ()
    val e2  = trg.residual

    val ys = MatrixD (Seq (y, yp, yp2))
    new PlotM (t, ys.t)

    new Plot (t, e, null, "e vs. t")
    new Plot (t, e2, null, "e2 vs. t")

} // TranRegressionTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TranRegressionTest5` object tests `TranRegression` class using the following
 *  regression equation.
 *  <p>
 *      sqrt (y)  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.TranRegressionTest5
 */
object TranRegressionTest5 extends App
{
    import scalation.util.Sorting.{iqsort, reorder}
    import ExampleAutoMPG._

    def f (u: Double): Double  = -log (1/u - 1)            // transform
    def fi (t: Double): Double = 1 / (1 + exp (-t))        // inverse transform

    val extrem = extreme (y)                               // (min, max) for y
    val bounds = (0.01, 0.99)                              // transform function domain bounds
 
    val yy = scaleV (extrem, bounds)(y)                    // rescale to domain of transform
    println (s"yy = $yy")

    banner ("Regession")
    val rg = new Regression (ox, yy)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    val t  = VectorD.range (0, yy.dim)
    val yp = rg.predict ()
    val e  = yy - yp

    banner ("TranRegession")
//  val trg = new Regression (ox, yy.map (f _))                        // rescale & transform
//  val trg = new TranRegression (ox, yy, null, f _, fi _)             // rescale
    val trg = TranRegression (oxy, null, f _, fi _, QR, bounds, true)  // automated
    trg.train ().eval ()
    println (trg.report)
    println (trg.summary)

    trg.eval0 ()
    println (trg.report)
    println (trg.summary)

    val yp2 = trg.predict ()
    val e2  = yy - yp2

    val yp_  = scaleV (bounds, extrem)(yp)
    val yp2_ = scaleV (bounds, extrem)(yp2)

    val rnk  = y.rank                                      // rank order for vector y
    val ry   = y.reorder (rnk)                             // actual - red
    val ryp  = yp_.reorder (rnk)                           // Regression - green
    val ryp2 = yp2_.reorder (rnk)                          // TranRegression - blue

    val ys = MatrixD (Seq (ry, ryp, ryp2))
    new PlotM (t, ys.t)

    new Plot (t, e, null, "e vs. t")
    new Plot (t, e2, null, "e2 vs. t")

} // TranRegressionTest5 object

