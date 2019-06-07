
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 1.6
 *  @date    Sat Jan 20 16:05:52 EST 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.linalgebra.VectorD.one
import scalation.math.double_exp
import scalation.plot.PlotM
import scalation.stat.Statistic
import scalation.util.banner

import RegTechnique._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegression` class uses multiple regression to fit a quadratic
 *  surface to the data.  For example in 2D, the quadratic regression equation is
 *  <p>
 *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_1, x_1^2] + e
 *  <p>
 *  Has no interaction/cross-terms and adds an a constant term for intercept
 *  (must not include intercept, column of ones in initial data matrix).
 *  @see scalation.metamodel.QuadraticFit
 *  @param x_         the initial data/input matrix (before quadratic term expansion)
 *  @param y          the response/output vector
 *  @param fname_     the feature/variable names
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class QuadRegression (x_ : MatriD, y: VectoD, fname_ : Strings = null, 
                      technique: RegTechnique = QR)
      extends Regression (QuadRegression.allForms (x_), y, fname_, null, technique)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    override def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new QuadRegression (x, y, fname, technique),
                                                 xx, k, rando)
    } // crossVal

} // QuadRegression class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegression` companion object provides methods for creating
 *  functional forms.
 */
object QuadRegression
{

    def apply (xy: MatriD, fname: Strings = null, technique: RegTechnique = QR): QuadRegression =
    {
        val (x, y) = PredictorMat.pullResponse (xy)
        new QuadRegression (x, y, fname, technique)
    } // apply 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of quadratic, linear and constant forms/terms (1, 3, 5, 7, ...).
     *  when there are no cross-terms.
     *  @param n  number of features/predictor variables (not counting intercept)
     */
    def numTerms (n: Int) =  2 * n + 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create all forms/terms for each point placing them in a new matrix.
     *  @param x  the input data matrix
     */
    def allForms (x_ : MatriD): MatriD =
    {
        val n  = x_.dim2
        val nt = numTerms (n)
        if (x_.dim1 < nt) throw new IllegalArgumentException ("not enough data rows in matrix to use regression")
        val x = new MatrixD (x_.dim1, nt)
        for (i <- x.range1) x(i) = qForms (x_(i), nt)           // vector values for all quadratic forms
        x
    } // allForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a vector/point 'v', compute the values for all of its quadratic,
     *  linear and constant forms/terms, returning them as a vector.
     *  No interaction/cross-terms.
     *  for 1D: v = (x_0)      => 'VectorD (1, x_0, x_0^2)'
     *  for 2D: p = (x_0, x_1) => 'VectorD (1, x_0, x_1, x_0^2, x_1^2)'
     *  @param v   the vector/point (i-th row of x_) for creating forms/terms
     *  @param nt  the number of terms
     *  @param n   the number of predictors
     */
    def qForms (v: VectoD, nt: Int): VectoD =
    {
        VectorD (for (j <- 0 until nt) yield
            if (j == 0)          1.0                            // intercept term
            else if (j % 2 == 1) v(j/2)                         // linear terms
            else                 v((j-1)/2)~^2                  // quadratic terms
        ) // for
    } // qForms

} // QuadRegression object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegressionTest` object is used to test the `QuadRegression` class.
 *  > runMain scalation.analytics.QuadRegressionTest
 */
object QuadRegressionTest extends App
{
    import ExampleBPressure.{x01 => x, y}

    val qrg = new QuadRegression (x, y)
    qrg.train ().eval ()
    val nTerms = QuadRegression.numTerms (2)
    println (s"x = ${qrg.getX}")
    println (s"y = $y")

    println (s"nTerms = $nTerms")
    println (qrg.report)
    println (qrg.summary)

    banner ("Forward Selection Test")
    val fcols = Set (0)
    for (l <- 1 until nTerms) {
        val (x_j, b_j, fit_j) = qrg.forwardSel (fcols)          // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        fcols += x_j
    } // for

    banner ("Backward Elimination Test")
    val bcols = Set (0) ++ Array.range (1, nTerms)
    for (l <- 1 until nTerms) {
        val (x_j, b_j, fit_j) = qrg.backwardElim (bcols)       // eliminate least predictive variable
        println (s"backward model: remove x_j = $x_j with b = $b_j \n fit = $fit_j")
        bcols -= x_j
    } // for

} // QuadRegressionTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegressionTest2` object is used to test the `QuadRegression` class.
 *  > runMain scalation.analytics.QuadRegressionTest2
 */
object QuadRegressionTest2 extends App
{
    import scalation.random.Normal
    import scalation.plot.Plot

    val (m, n) = (400, 1)
    val noise = new Normal (0, 10 * m * m)
    val x = new MatrixD (m, n)
    val y = new VectorD (m)
    val t = VectorD.range (0, m)

    for (i <- x.range1) { 
        x(i, 0) = i
        y(i) = i*i + i + noise.gen
    } // for

    banner ("Regression")
    val ox = VectorD.one (y.dim) +^: x
    val rg = new Regression (ox, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)
    val yp = rg.predict ()
    val e  = rg.residual

    banner ("QuadRegression")
    val qrg = new QuadRegression (x, y)
    qrg.train ().eval ()
    println (qrg.report)
    println (qrg.summary)
    val qyp = qrg.predict ()
    val qe  = qrg.residual

    val x0 = x.col(0)
    new Plot (x0, y, null, "y vs x")
    new Plot (t, y, yp, "y and yp vs t")
    new Plot (t, y, qyp, "y and qyp vs t")
    new Plot (x0, e, null, "e vs x")
    new Plot (x0, qe, null, "qe vs x")

} // QuadRegressionTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegressionTest3` object is used to test the `QuadRegression` class.
 *  > runMain scalation.analytics.QuadRegressionTest3
 */
object QuadRegressionTest3 extends App
{
    import scalation.random.Normal
    import scalation.stat.StatVector.corr

    val s      = 20
    val grid   = 1 to s
    val (m, n) = (s*s, 2)
    val noise  = new Normal (0, 10 * s * s)
    val x = new MatrixD (m, n)
    val y = new VectorD (m)

    var k = 0
    for (i <- grid; j <- grid) {
        x(k) = VectorD (i, j)
        y(k) = x(k, 0)~^2 + 2 * x(k, 1) +  noise.gen
        k += 1
    } // for

    banner ("Regression")
    val ox = VectorD.one (y.dim) +^: x
    val rg = new Regression (ox, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    banner ("QuadRegression")
    val qrg = new QuadRegression (x, y)
    qrg.train ().eval ()
    println (qrg.report)
    println (qrg.summary)

    banner ("Multi-collinearity Check")
    val qx = qrg.getX
    println (corr (qx.asInstanceOf [MatrixD]))
    println (s"vif = ${qrg.vif}")

} // QuadRegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegressionTest4` object tests the `QuadRegression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.analytics.QuadRegressionTest4
 */
object QuadRegressionTest4 extends App
{
    import scalation.columnar_db.Relation

    banner ("auto_mpg relation")
    val auto_tab = Relation (BASE_DIR + "auto-mpg.csv", "auto_mpg", null, -1)
    auto_tab.show ()

    banner ("auto_mpg (x, y) dataset")
    val (x, y) = auto_tab.toMatriDD (1 to 6, 0)
    println (s"x = $x")
    println (s"y = $y")

    banner ("auto_mpg regression")
    val qrg = new QuadRegression (x, y)
    qrg.train ().eval ()
    val n  = x.dim2                                                  // number of variables
    val nt = QuadRegression.numTerms (n)                             // number of terms
    println (s"n = $n, nt = $nt")
    println (qrg.report)
    println (qrg.summary)
    
    banner ("Forward Selection Test")
    val qx  = qrg.getX                                               // get matrix with all columns
    val rg  = new Regression (qx, y)                                 // regression on all columns
    val rSq = new MatrixD (qx.dim2-1, 3)                             // R^2, R^2 Bar,  R^2 cv
    
    val fcols = Set (0)                                              // start with x_0 in model
    for (l <- 1 until nt) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)                // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else { 
            fcols += x_j                                             // add variable x_j
            val x_cols = qx.selectCols (fcols.toArray)               // qx projected onto cols_j columns
            rSq(l-1)   = Fit.qofVector (fit_j, rg.crossVal (x_cols))   // use main model, 'rg'
//          val rg_j   = new Regression (x_cols, y)                  // new model, regress with x_j added
//          rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())     // use new model, rg_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until nt) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                     // instance index
    new PlotM (t, rSq.t, lines = true)

} // QuadRegressionTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegressionTest5` object tests `QuadRegression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.QuadRegressionTest5
 */
object QuadRegressionTest5 extends App
{
    // 4 data points:             x1 x2  y
    val xy = new MatrixD ((9, 3), 2, 1, 0.4,               // 4-by-3 matrix
                                  3, 1, 0.5,
                                  4, 1, 0.6,
                                  2, 2, 1.0,
                                  3, 2, 1.1,
                                  4, 2, 1.2,
                                  2, 3, 2.0,
                                  3, 3, 2.1,
                                  4, 3, 2.2)

    println ("model: y = b0 + b1*x1 b2*x1^2 + b3*x2 + b4*x2^2")
    println (s"xy = $xy")

    val oxy = VectorD.one (xy.dim1) +^: xy
    val xy_ = oxy.selectCols (Array (0, 2, 3))
    println (s"xy_ = $xy_")

    banner ("SimpleRegression")
    val srg  = SimpleRegression (xy_)
    srg.train ().eval ()
    println (srg.report)
    println (srg.summary)
    println (s"predict = ${srg.predict ()}")

    banner ("Regression")
    val rg  = Regression (oxy)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)
    println (s"predict = ${rg.predict ()}")

    banner ("QuadRegression")
    val qrg = QuadRegression (xy)
    qrg.train ().eval ()
    println (qrg.report)
    println (qrg.summary)
    println (s"predict = ${qrg.predict ()}")

} // QuadRegressionTest5 object

