
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 1.6
 *  @date    Tue Apr 18 14:24:14 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
import scalation.minima._
import scalation.plot.{Plot, PlotM}
import scalation.stat.Statistic
import scalation.random.CDF.studentTCDF
import scalation.util.{banner, Error}
import scalation.util.Unicode.sub

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in
 *  the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x       the data/input m-by-n matrix
 *  @param y       the response/output m-vector
 *  @param fname_  the feature/variable names
 *  @param hparam  the shrinkage hyper-parameter, lambda (0 => OLS) in the penalty term 'lambda * b dot b'
 */
class LassoRegression (x: MatriD, y: VectoD,
                       fname_ : Strings = null, hparam: HyperParameter = LassoRegression.hp)
      extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG = false                                          // debug flag
    private var λ     = hparam ("lambda")                              // weight to put on regularization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squares error + λ * sum of the magnitude  of coefficients.
     *  This is the objective function to be minimized.
     *  @param yy  the response/output vector
     *  @param b   the vector of coefficients/parameters
     * 
    def f (yy: VectoD)(b: VectoD): Double =
    {
        e = yy - x * b                                                 // calculate the residuals/error
        e dot e + λ * b.norm1 
    } // f
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  regularized by the sum of magnitudes of the coefficients.
     *  @see pdfs.semanticscholar.org/969f/077a3a56105a926a3b0c67077a57f3da3ddf.pdf
     *  @see `scalation.minima.LassoAdmm`
     *  @param yy  the response/output vector
     */
    def train (yy: VectoD = y): LassoRegression =
    {
//      val g       = f(yy) _
//      val optimer = new CoordinateDescent (g)                        // Coordinate Descent optimizer
//      val optimer = new GradientDescent (g)                          // Gradient Descent optimizer
//      val optimer = new ConjugateGradient (g)                        // Conjugate Gradient optimizer
//      val optimer = new QuasiNewton (g)                              // Quasi-Newton optimizer
//      val optimer = new NelderMeadSimplex (g, x.dim2)                // Nelder-Mead optimizer
//      val b0 = new VectorD (k+1)                                     // initial guess for coefficient vector
//      b = optimer.solve (b0, 0.5)                                    // find an optimal solution for coefficients

        b = LassoAdmm.solve (x.asInstanceOf [MatrixD], yy, λ)          // Alternating Direction Method of Multipliers
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to add the most predictive variable to the existing
     *  model, returning the variable to add, the new parameter vector and the new
     *  Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean = true): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq         // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                                 // index of variable to add
        var b_max =  b                                                 // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)        // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                      // try adding x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new LassoRegression (x_cols, y, null, hparam)   // regress with x_j added
            rg_j.train ().eval ()                                      // train model, evaluate QoF
            val bb  = rg_j.parameter
            val ft  = rg_j.fit
            val qof = ft(ir)                                           // rSqBar/rSq
            if (DEBUG) println (s"forwardSel: cols_$j = $cols_j, qof_$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) println (s"forwardSel: add variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        (j_max, b_max, ft_max)
    } // forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable from
     *  the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     *  @param first     first variable to consider for elimination
     *                       (default (1) assume intercept x_0 will be in any model)
     */
    def backwardElim (cols: Set [Int], adjusted: Boolean = true, first: Int = 1): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq         // fit(ir) is rSqBar/rSq
        var j_max = -1                                                 // index of variable to eliminate
        var b_max =  b                                                 // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)        // optimize on quality of fit

        for (j <- first until x.dim2 if cols contains j) {
            val cols_j = cols - j                                      // try removing x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new LassoRegression (x_cols, y, null, hparam)   // regress with x_j removed
            rg_j.train ().eval ()                                      // train model, evaluate QoF
            val bb  = rg_j.parameter
            val ft  = rg_j.fit
            val qof = ft(ir)                                           // rSqBar/rSq
            if (DEBUG) println (s"backwardElim: cols_$j = $cols_j, qof$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) println (s"backwardElim: remove variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        (j_max, b_max, ft_max)
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new LassoRegression (x, y, fname, hparam),
                                                 xx, k, rando)
    } // crossVal

} // LassoRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegression` companion object provides factory methods for the
 *  `LassoRegression` class.
 */
object LassoRegression extends Error
{
    import PredictorMat.pullResponse

    /** Base hyper-parameter specification for `LassoRegression`
     */
    val hp = new HyperParameter; hp += ("lambda", 0.01, 0.01)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Lasso Regression from a combined data matrix.
     *  @param xy       the combined data matrix
     *  @param lambda0  the initial value for the regularization weight
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = hp): LassoRegression =
    {
        val n = xy.dim2
        if (n < 2) {
            flaw ("apply", s"dim2 = $n of the 'xy' matrix must be at least 2")
            null
        } else {
            val (x, y) = pullResponse (xy)
            new LassoRegression (x, y, fname, hparam)
       } // if
    } // apply

} // LassoRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegressionTest` object tests `LassoRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  It comapres `LassoRegression` to `Regression`.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.LassoRegressionTest
 */
object LassoRegressionTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,                 // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

//  println ("model: y = b_0 + b_1*x_1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    banner ("Regression")
    val rg = new Regression (x, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    var yp = rg.predict (z)                                        // predict y for one point
    println (s"predict ($z) = $yp")

    banner ("LassoRegression")
    val lrg = new LassoRegression (x, y)
    lrg.train ().eval ()
    println (lrg.report)
    println (lrg.summary)

    yp = lrg.predict (z)                                           // predict y for one point
    println (s"predict ($z) = $yp")

    val yyp = rg.predict (x)                                       // predict y for several points
    println (s"predict (x) = $yyp")

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

} // LassoRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegressionTest2` object tests `LassoRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  Try non-default value for the 'lambda' hyper-parameter.
 *  > runMain scalation.analytics.LassoRegressionTest2
 */
object LassoRegressionTest2 extends App
{
    import LassoRegression.hp

    println (s"hp = $hp")
    val hp2 = hp.updateReturn ("lambda", 1.0)                         // try different values
    println (s"hp2 = $hp2")

    // 5 data points:            one   x_1    x_2
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,                    // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    banner ("LassoRegression")
    val lrg = new LassoRegression (x, y, hparam = hp2)
    lrg.train ().eval ()
    println (lrg.report)
    println (lrg.summary)

    val yp  = lrg.predict (z)                                         // predict response for z
    println (s"predict ($z) = $yp")

} // LassoRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegressionTest3` object tests `LassoRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  Test regression, forward selection and backward elimination.
 *  > runMain scalation.analytics.LassoRegressionTest3
 */
object LassoRegressionTest3 extends App
{
    // 5 data points:            one   x_1    x_2
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,                    // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    banner ("LassoRegression")
    val lrg = new LassoRegression (x, y)
    lrg.train ().eval ()
    println (lrg.report)

    banner ("Forward Selection Test")
    val fcols = Set (0)                                               // start with x_0 in model
    for (l <- 1 until x.dim2) {
        val (x_j, b_j, fit_j) = lrg.forwardSel (fcols)                // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        fcols += x_j
    } // for

    banner ("Backward Elimination Test")
    val bcols = Set (0) ++ Array.range (1, x.dim2)                    // start with all x_j in model
    for (l <- 1 until x.dim2  - 1) {
        val (x_j, b_j, fit_j) = lrg.backwardElim (bcols)              // eliminate least predictive variable
        println (s"backward model: remove x_j = $x_j with b = $b_j \n fit = $fit_j")
        bcols -= x_j
    } // for

} // LassoRegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegressionTest4` object tests the `LassoRegression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.analytics.LassoRegressionTest4
 */
object LassoRegressionTest4 extends App
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
    val rg = new LassoRegression (x, y)
    rg.train ().eval ()
    val n = x.dim2                                                    // number of parameters/variables
    println (rg.report)
    println (rg.summary)

    banner ("Forward Selection Test")
    val rSq = new MatrixD (x.dim2-1, 3)                               // R^2, R^2 Bar,  R^2 cv

    val fcols = Set (0)                                               // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)                 // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                              // add variable x_j
            val x_cols = x.selectCols (fcols.toArray)                 // x projected onto cols_j columns
            rSq(l-1)   = Fit.qofVector (fit_j, rg.crossVal (x_cols))  // use main model, 'rg'
//          val rg_j   = new LassoRegression (x_cols, y)              // regress with x_j added
//          rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())      // use new model, rg_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                      // instance index
    new PlotM (t, rSq.t, lines = true)

} // LassoRegressionTest4 object

