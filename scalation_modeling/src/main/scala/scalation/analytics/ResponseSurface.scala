
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
/** The `ResponseSurface` class uses multiple regression to fit a quadratic/cubic
 *  surface to the data.  For example in 2D, the quadratic regression equation is
 *  <p>
 *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_1, x_0*x_1, x_1^2] + e
 *  <p>
 *  Adds an a constant term for intercept (must not include intercept, column of ones
 *  in initial data matrix).
 *  @see scalation.metamodel.QuadraticFit
 *  @param x_         the input vectors/points
 *  @param y          the response vector
 *  @param fname_     the feature/variable names
 *  @param cubic      the order of the surface (defaults to quadratic, else cubic)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ResponseSurface (x_ : MatriD, y: VectoD, fname_ : Strings = null, cubic: Boolean = false,
                       technique: RegTechnique = QR)
      extends Regression (ResponseSurface.allForms (x_, cubic), y, fname_, null, technique)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    override def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new ResponseSurface (x, y, fname, cubic, technique),
                                                 xx, k, rando)
    } // crossVal

} // ResponseSurface class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurface` companion object provides methods for creating
 *  functional forms.
 */
object ResponseSurface
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of quadratic, linear and constant forms/terms (3, 6, 10, 15, ...)
     *  of cubic, quadratic, linear and constant forms/terms (4, 10, 20, 35, ...)
     *  @param n      the number of parameters/predictors
     *  @param cubic  the order of the surface (2 for quadratic, 3 for cubic)
     */
    def numTerms (n: Int, cubic: Boolean = false) =
    {
        if (cubic) (n + 1) * (n + 2) * (n + 3) / 6
        else       (n + 1) * (n + 2) / 2
    } // numTerms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create all forms/terms for each point placing them in a new matrix.
     *  @param x      the input data matrix
     *  @param cubic  the order of the surface (2 for quadratic, 3 for cubic)
     */
    def allForms (x: MatriD, cubic: Boolean): MatriD =
    {
        val n  = x.dim2
        val nt = numTerms(n, cubic)
        if (x.dim1 < nt) throw new IllegalArgumentException ("not enough data rows in matrix to use regression")
        val xa = new MatrixD (x.dim1, nt)
        for (i <- 0 until x.dim1)
            xa(i) = if (cubic) cForms (x(i), nt, n)    // vector values for all cubic forms
                    else       qForms (x(i), nt, n)    // vector values for all quadratic forms
        xa
    } // allForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a vector/point 'p', compute the values for all of its quadratic,
     *  linear and constant forms/terms, returning them as a vector.
     *  for 1D: p = (x_0)      => 'VectorD (1, x_0, x_0^2)'
     *  for 2D: p = (x_0, x_1) => 'VectorD (1, x_0, x_0^2, x_0*x_1, x_1, x_1^2)'
     *  @param p    the source vector/point for creating forms/terms
     *  @param nt   the number of terms
     *  @param n    the number of predictors
     */
    def qForms (p: VectoD, nt: Int, n: Int): VectoD =
    {
        val q = one (1) ++ p          // augmented vector: [ 1., p(0), ..., p(n-1) ]
        val z = new VectorD (nt)      // vector of all forms/terms
        var l = 0
        for (i <- 0 to n; j <- i to n) { z(l) = q(i) * q(j); l += 1 }
        z
    } // qForms

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a vector/point 'p', compute the values for all of its cubic, quadratic,
     *  linear and constant forms/terms, returning them as a vector.
     *  for 1D: p = (x_0)      => 'VectorD (1, x_0, x_0^2, x_0^3)'
     *  for 2D: p = (x_0, x_1) => 'VectorD (1, x_0, x_0^2, x_0^3,
     *                                        x_0*x_1, x_0^2*x_1, x_0*x_1^2,
     *                                        x_1, x_1^2, x_1^3)'
     *  @param p    the source vector/point for creating forms/terms
     *  @param nt   the number of terms
     *  @param n    the number of predictors
     */
    def cForms (p: VectoD, nt: Int, n: Int): VectoD =
    {
        val q = one (1) ++ p          // augmented vector: [ 1., p(0), ..., p(n-1) ]
        val z = new VectorD (nt)      // vector of all forms/terms
        var l = 0
        for (i <- 0 to n; j <- i to n; k <- j to n) { z(l) = q(i) * q(j) * q(k); l += 1 }
        z
    } // cForms

} // ResponseSurface object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurfaceTest` object is used to test the `ResponseSurface` class.
 *  > runMain scalation.analytics.ResponseSurfaceTest
 */
object ResponseSurfaceTest extends App
{
    import ExampleBPressure.{x01 => x, y}

    val rsr = new ResponseSurface (x, y)
    rsr.train ().eval ()
    val nTerms = ResponseSurface.numTerms (2)
    println (s"x = ${rsr.getX}")
    println (s"y = $y")

    println ("nTerms    = " + nTerms)
    println (rsr.report)
    println (rsr.summary)

    banner ("Forward Selection Test")
    val fcols = Set (0)
    for (l <- 1 until nTerms) {
        val (x_j, b_j, fit_j) = rsr.forwardSel (fcols)        // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        fcols += x_j
    } // for

    banner ("Backward Elimination Test")
    val bcols = Set (0) ++ Array.range (1, nTerms)
    for (l <- 1 until nTerms) {
        val (x_j, b_j, fit_j) = rsr.backwardElim (bcols)     // eliminate least predictive variable
        println (s"backward model: remove x_j = $x_j with b = $b_j \n fit = $fit_j")
        bcols -= x_j
    } // for

} // ResponseSurfaceTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurfaceTest2` object is used to test the `ResponseSurface` class.
 *  > runMain scalation.analytics.ResponseSurfaceTest2
 */
object ResponseSurfaceTest2 extends App
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
        y(k) = x(k, 0)~^2 + 2 * x(k, 1) + x(k, 0) * x(k, 1) +  noise.gen
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

    banner ("ResponseSurface")
    val rsr = new ResponseSurface (x, y)
    rsr.train ().eval ()
    println (rsr.report)
    println (rsr.summary)

    banner ("Multi-collinearity Check")
    val rx = rsr.getX
    println (corr (rx.asInstanceOf [MatrixD]))
    println (s"vif = ${rsr.vif}")

} // ResponseSurfaceTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurfaceTest3` object tests the `ResponseSurface` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.analytics.ResponseSurfaceTest3
 */
object ResponseSurfaceTest3 extends App
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
    val qrg = new ResponseSurface (x, y)
    qrg.train ().eval ()
    val n  = x.dim2                                                // number of variables
    val nt = ResponseSurface.numTerms (n)                          // number of terms
    println (s"n = $n, nt = $nt")
    println (qrg.report)
    println (qrg.summary)

    banner ("Forward Selection Test")
    val qx  = qrg.getX                                             // get matrix with all columns
    val rg  = new Regression (qx, y)                               // regression on all columns
    val rSq = new MatrixD (qx.dim2-1, 3)                           // R^2, R^2 Bar,  R^2 cv

    val fcols = Set (0)                                            // start with x_0 in model
    for (l <- 1 until nt) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)              // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                           // add variable x_j
            val x_cols = qx.selectCols (fcols.toArray)             // qx projected onto cols_j columns
            rSq(l-1)   = Fit.qofVector (fit_j, rg.crossVal (x_cols))   // use main model, 'rg'
//          val rg_j   = new Regression (x_cols, y)                // regress with x_j added
//          rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())   // use new model, rg_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until nt) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                   // instance index
    new PlotM (t, rSq.t, lines = true)

} // ResponseSurfaceTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ResponseSurfaceTest4` object tests the `ResponseSurface` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.  Runs the cubic case.
 *  > runMain scalation.analytics.ResponseSurfaceTest4
 */
object ResponseSurfaceTest4 extends App
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
    val qrg = new ResponseSurface (x, y, cubic = true)
    qrg.train ().eval ()
    val n  = x.dim2                                                // number of variables
    val nt = ResponseSurface.numTerms (n, cubic = true)            // number of terms
    println (s"n = $n, nt = $nt")
    println (qrg.report)
    println (qrg.summary)

    banner ("Forward Selection Test")
    val qx  = qrg.getX                                             // get matrix with all columns
    val rg  = new Regression (qx, y)                               // regression on all columns
    val rSq = new MatrixD (qx.dim2-1, 3)                           // R^2, R^2 Bar,  R^2 cv

    val fcols = Set (0)                                            // start with x_0 in model
    for (l <- 1 until nt) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)              // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                           // add variable x_j
            val x_cols = qx.selectCols (fcols.toArray)             // qx projected onto cols_j columns
            rSq(l-1)   = Fit.qofVector (fit_j, rg.crossVal (x_cols))   // use main model, 'rg'
//          val rg_j   = new Regression (x_cols, y)                // regress with x_j added
//          rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())   // use new model, rg_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until nt) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                   // instance index
    new PlotM (t, rSq.t, lines = true)

} // ResponseSurfaceTest4 object

