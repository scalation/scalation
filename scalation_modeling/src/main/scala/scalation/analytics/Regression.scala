
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.Set

import scalation.linalgebra._
import scalation.plot.{Plot, PlotM}
import scalation.stat.Statistic
import scalation.stat.StatVector.corr
import scalation.random.CDF.studentTCDF
import scalation.util.{banner, Error}
import scalation.util.Unicode.sub

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegTechnique` object defines the implementation techniques available.
 */
object RegTechnique extends Enumeration
{
    type RegTechnique = Value
    val QR, Cholesky, SVD, LU, Inverse = Value
    val techniques = Array (QR, Cholesky, SVD, LU, Inverse)
   
} // RegTechnique

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in
 *  the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y 
 *      b  =  fac.solve (.)
 *  <p>
 *  Five factorization techniques are provided:
 *  <p>
 *      'QR'         // QR Factorization: slower, more stable (default)
 *      'Cholesky'   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      'SVD'        // Singular Value Decomposition: slowest, most robust
 *      'LU'         // LU Factorization: better than Inverse
 *      'Inverse'    // Inverse/Gaussian Elimination, classical textbook technique
 *  <p>
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  Note, not intended for use when the number of degrees of freedom 'df' is negative.
 *  @see en.wikipedia.org/wiki/Degrees_of_freedom_(statistics)
 *  @param x          the data/input m-by-n matrix
 *                        (augment with a first column of ones to include intercept in model)
 *  @param y          the response/output m-vector
 *  @param fname_     the feature/variable names
 *  @param hparam     the hyper-parameters (it doesn't have any, but may be used by derived classes)
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class Regression (x: MatriD, y: VectoD,
                  fname_ : Strings = null, hparam: HyperParameter = null,
                  technique: RegTechnique = QR)
      extends PredictorMat (x, y, fname_, hparam)
{
    private val DEBUG = true                                           // debug flag

    type Fac_QR = Fac_QR_H [MatriD]                                    // change as needed (H => Householder)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the data matrix 'x'.  Mainly for derived classes where 'x' is expanded
     *  from the given columns in 'x_', e.g., `QuadRegression` add squared columns.
     */
    def getX = x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the response vector 'y'.  Mainly for derived classes where 'y' is
     *  transformed, e.g., `TranRegression`.
     */
    def getY = y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a solver for the Normal Equations using the selected factorization technique.
     */
    private def solver (): Factorization =
    {
        technique match {                                              // select the factorization technique
        case QR       => new Fac_QR (x, false)                         // QR Factorization
        case Cholesky => new Fac_Cholesky (x.t * x)                    // Cholesky Factorization
        case SVD      => new SVD (x)                                   // Singular Value Decomposition
        case LU       => new Fac_LU (x.t * x)                          // LU Factorization
        case _        => new Fac_Inv (x.t * x)                         // Inverse Factorization
        } // match
    } // solver

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  using the ordinary least squares 'OLS' method.
     *  @param yy  the response/output vector to work with (defaults to y)
     */
    def train (yy: VectoD = y): Regression =
    {
        val fac = solver ()                                            // create selected factorization technique
        fac.factor ()                                                  // factor the matrix, either X or X.t * X
        b = technique match {                                          // solve for parameter/coefficient vector b
            case QR       => fac.solve (yy)                            // R * b = Q.t * yy
            case Cholesky => fac.solve (x.t * yy)                      // L * L.t * b = X.t * yy
            case SVD      => fac.solve (yy)                            // b = V * Σ^-1 * U.t * yy
            case LU       => fac.solve (x.t * yy)                      // b = (X.t * X) \ X.t * yy
            case _        => fac.solve (x.t * yy)                      // b = (X.t * X)^-1 * X.t * yy
        } // match
        if (DEBUG) (s"train: parameter b = $b")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error (difference between actual and predicted) and useful
     *  diagnostics for the test dataset.  Overridden for efficiency.
     *  @param xx  the test data/input matrix (defaults to full x)
     *  @param yy  the test response/output vector (defaults to full y)
     */
    override def eval (xx: MatriD = x, yy: VectoD = y): Regression =
    {
        val yp = xx * b                                                // y predicted for xx (test/full)
        e = yy - yp                                                    // compute residual/error vector e
        diagnose (e, yy)                                               // compute diagnostics
        this
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add, the new parameter vector and
     *  the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x currently included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean = true): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq         // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                                 // index of variable to add
        var b_max =  b                                                 // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)        // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                      // try adding variable/column x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new Regression (x_cols, y, null, null, technique)   // regress with x_j added
            rg_j.train ().eval ()                                      // train model, evaluate QoF
            val bb  = rg_j.parameter
            val ft  = rg_j.fit
            val qof = ft(ir)                                           // rSqBar/rSq
            if (DEBUG) println (s"forwardSel: cols_$j = $cols_j, qof_$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) {
            val k = cols.size + 1
            println (s"forwardSel: add (#$k) variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        } // if
        (j_max, b_max, ft_max)                                         // should add j_max variable 
    } // forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to find the least predictive variable to remove
     *  from the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x currently included in the existing model
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
            val cols_j = cols - j                                      // try removing variable/column x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new Regression (x_cols, y, null, null, technique)   // regress with x_j removed
            rg_j.train ().eval ()                                      // train model, evaluate QoF
            val bb  = rg_j.parameter
            val ft  = rg_j.fit
            val qof = ft(ir)                                           // rSqBar/rSq
            if (DEBUG) println (s"backwardElim: cols_$j = $cols_j, qof$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) {
            val k = cols.size - 1
            println (s"backwardElim: remove (#$k) variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        } // if
        (j_max, b_max, ft_max)
    } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'x_j' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'x_j' can be predicted
     *  from the other variables, so 'x_j' is a candidate for removal from the model.
     */
    def vif: VectoD =
    {
        val ir   = index_rSq                                           // fit(ir) is rSq
        val vifV = new VectorD (k)                                     // VIF vector

        for (j <- 1 until x.dim2) {
            val x_j   = x.col(j)                                       // x_j is jth column in x
            val x_noj = x.sliceEx (m, j)                               // x matrix without column j
            val rg_j  = new Regression (x_noj, x_j, null, null, technique)   // regress with x_j removed
            rg_j.train ().eval ()                                      // train model, evaluate QoF
            val rSq_j = rg_j.fit(ir)
            if (DEBUG) println (s"vif: for variable x_$j, rSq_$j = $rSq_j")
            vifV(j-1) =  1.0 / (1.0 - rSq_j)                           // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation.
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new Regression (x, y, fname, null, technique),
                                                 xx, k, rando) 
    } // crossVal

} // Regression class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression` companion object provides factory apply functions and a testing method.
 */
object Regression extends Error
{
    import PredictorMat.pullResponse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Regression` object from a combined data matrix.
     *  The last column is assumed to be the response column.
     *  @param xy         the combined data matrix (predictors and response)
     *  @param fname      the feature/variable names
     *  @param hparam     the hyper-parameters
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = null,
               technique: RegTechnique = QR): Regression = 
    {
        val n = xy.dim2
        if (n < 2) {
            flaw ("apply", s"dim2 = $n of the 'xy' matrix must be at least 2")
            null
        } else {
            val (x, y) = pullResponse (xy)
            new Regression (x, y, fname, hparam, technique)
       } // if
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the various regression techniques.
     *  @param x      the data/input matrix
     *  @param y      the response/output vector
     *  @param z      a vector to predict
     *  @param fname  the names of features/variable
     */
    def test (x: MatriD, y: VectoD, z: VectoD, fname: Strings = null)
    {
        println (s"x = $x")
        println (s"y = $y")

        for (tec <- techniques) {                                      // use 'tec' Factorization
            banner (s"Fit the parameter vector b using $tec")
            val rg = new Regression (x, y, fname, null, tec)           // use null for hyper-parameters
            rg.train ().eval ()                                        // train model, evaluate QoF
            println (rg.report)
            println (rg.summary)

            val yp = rg.predict (x)                                    // predict y for several points
            println (s"predict (x) = $yp")
            new Plot (y, yp, null, tec.toString)

            val yp1 = rg.predict (z)                                   // predict y for one point
            println (s"predict ($z) = $yp1")
        } // for
    } // test

} // Regression object

import Regression.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest` object tests `Regression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.RegressionTest
 */
object RegressionTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,                     // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

//  println ("model: y = b_0 + b_1*x_1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")

    test (x, y, z)

} // RegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest2` object tests `Regression` class using the following
 *  regression equation, which has a perfect fit.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.RegressionTest2
 */
object RegressionTest2 extends App
{
    // 4 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((4, 3), 1.0, 1.0, 1.0,                        // 4-by-3 matrix
                                 1.0, 1.0, 2.0,
                                 1.0, 2.0, 1.0,
                                 1.0, 2.0, 2.0)
    val y = VectorD (6.0, 8.0, 7.0, 9.0)
    val z = VectorD (1.0, 2.0, 3.0)

//  println ("model: y = b_0 + b_1*x1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")

    test (x, y, z)

} // RegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest3` object tests the multi-collinearity method in the
 *  `Regression` class using the following regression equation on the Blood
 *  Pressure dataset.  Also performs Collinearity Diagnostics.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  > runMain scalation.analytics.RegressionTest3
 */
object RegressionTest3 extends App
{
    import ExampleBPressure._

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")

    val z = VectorD (1.0, 46.0, 97.5, 7.0, 95.0)

//  test (x, y, z, fname)                                            // no intercept
    test (ox, y, z, ofname)                                          // with intercept

    banner ("Collinearity Diagnostics")
    println ("corr (x) = " + corr (x))                               // correlations of column vectors in x

} // RegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest4` object tests the multi-collinearity method in the
 *  `Regression` class using the following regression equation on the Blood
 *  Pressure dataset.  It also applies forward selection and backward elimination.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 *  > runMain scalation.analytics.RegressionTest4
 */
object RegressionTest4 extends App
{
    import ExampleBPressure._
    val x = ox                                                       // use ox for intercept

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    banner ("Parameter Estimation and Quality of Fit")
    val rg = new Regression (x, y, ofname)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    banner ("Collinearity Diagnostics")
    println ("corr (x) = " + corr (x))                               // correlations of column vectors in x

    banner ("Multi-collinearity Diagnostics")
    println ("vif      = " + rg.vif)                                 // test multi-colinearity (VIF)

    banner ("Forward Selection Test")
    val fcols = Set (0)                                              // start with x_0 in model
    for (l <- 1 until x.dim2) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)          // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        fcols += x_j
    } // for

    banner ("Backward Elimination Test")
    val bcols = Set (0) ++ Array.range (1, x.dim2)                   // start with all x_j in model
    for (l <- 1 until x.dim2  - 1) {
        val (x_j, b_j, fit_j) = rg.backwardElim (bcols)              // eliminate least predictive variable
        println (s"backward model: remove x_j = $x_j with b = $b_j \n fit = $fit_j")
        bcols -= x_j
    } // for

} // RegressionTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest5` object tests the cross-validation for the `Regression`
 *  class using the following regression equation on the Blood Pressure dataset.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  > runMain scalation.analytics.RegressionTest5
 */
object RegressionTest5 extends App
{
    import ExampleBPressure._
    val x = ox                                                       // use ox for intercept

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    banner ("Cross-Validation")
    val rg  = new Regression (x, y, fname)
    val results = rg.crossVal ()
    banner ("rSq Statistics")
    println (Statistic.labels)
    println (results(rg.index_rSq))

} // RegressionTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest6` object tests `Regression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.RegressionTest6
 */
object RegressionTest6 extends App
{
    // 4 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((7, 3), 1.0, 1.0, 1.0,                      // 4-by-3 matrix
                                 1.0, 1.0, 2.0,
                                 1.0, 2.0, 1.0,
                                 1.0, 2.0, 2.0,
                                 1.0, 2.0, 3.0,
                                 1.0, 3.0, 2.0,
                                 1.0, 3.0, 3.0)
    val y = VectorD (6.0, 8.0, 9.0, 11.0, 13.0, 13.0, 16.0)
    val z = VectorD (1.0, 1.0, 3.0)

//  println ("model: y = b_0 + b_1*x1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    test (x, y, z)

} // RegressionTest6 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest7` object tests `Regression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.RegressionTest7
 */
object RegressionTest7 extends App
{
    val xy = new MatrixD ((9, 4), 1, 0, 0, 0,
                                  1, 0, 1, 0,
                                  1, 0, 2, 0,
                                  1, 1, 0, 0,
                                  1, 1, 1, 1,
                                  1, 1, 2, 1,
                                  1, 2, 0, 0,
                                  1, 2, 1, 1,
                                  1, 2, 2, 1)
    val rg = Regression (xy)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

} // RegressionTest7 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest8` object tests the `Regression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  > runMain scalation.analytics.RegressionTest8
 */
object RegressionTest8 extends App
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
    val rg = new Regression (x, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    banner ("auto_mpg cross-validation")
    rg.crossVal ()

} // RegressionTest8 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest9` object is used to test the `Regression` class.
 *  <p>
 *      y = b dot x = b0 + b1 * x1
 *  <p>
 *  This version uses gradient descent to search for the optimal solution for 'b'.
 *  Try normalizing the data first.
 *  @see `MatrixTransform`
 *  > runMain scalation.analytics.RegressionTest9
 */
object RegressionTest9 extends App
{
    import scala.util.control.Breaks.{breakable, break}
    import MatrixTransform.golden
    import ExampleBPressure._
    val x = ox                                                        // use ox for intercept

    val stop = new StoppingRule ()

    println ("x = " + x)

    val ITER = 100                                                    // number of iterations/epochs
    val eta  = 0.000006                                               // try different values for the learning rate (g)
//  val eta  = 0.00012                                                // try different values for the learning rate (gg)
    val rg   = new Regression (x, y)                                  // create a regression model, don't train
    val mu   = y.mean                                                 // mean of y
    val sst  = (y dot y) - mu * mu * x.dim1                           // sum of squares total
    var b    = new VectorD (x.dim2)                                   // starting point [0, 0] for parameter vector b

    banner (s"Regression Model: gradient descent: eta = $eta")
    breakable { for (it <- 1 to ITER) {
        val yp = x * b                                                // y predicted
        val e  = y - yp                                               // error
        val g  = x.t * e                                              // - gradient
        val gg = golden (g)                                           // - golden gradient
        println (s"g = $g, gg = $gg")
        b     += g * eta                                              // update parameter b
//      b     += gg * eta                                             // update parameter b (golden gradient)
        val sse = e dot e                                             // sum of squares error
        val rSq = 1.0 - sse / sst                                     // coefficient of determination
        println (s"for iteration $it, b = $b, sse = $sse, rSq = $rSq")
        val (b_best, sse_best) = stop.stopWhen (b, sse) 
        if (b_best != null) {
            val rSq_best = 1.0 - sse_best / sst                       // best coefficient of determination
            println (s"solution b_best = $b_best, sse_best = $sse_best, rSq_best = $rSq_best")
            break
        } // if
    }} // breakable for

} // RegressionTest9 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest10` object tests the `Regression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  Show effects of increasing collinearity.
 *  > runMain scalation.analytics.RegressionTest10
 */
object RegressionTest10 extends App
{
//                               c x1 x2
    val x = new MatrixD ((4, 3), 1, 1, 1,
                                 1, 2, 2,
                                 1, 3, 3,
                                 1, 4, -1)

    val y = VectorD (1, 3, 3, 4)
    val z = VectorD (1, 1.5, 2, 2.5, 3, 3.5, 4)

    val v = x.sliceCol (0, 2)
    banner (s"Test without column x2")
    println (s"v = $v")
    var rg = new Regression (v, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    for (i <- z.range) {
        x(3, 2) = z(i)
        banner (s"Test Increasing Collinearity: x_32 = ${z(i)}")
        println (s"x = $x")
        println (s"corr (x) = ${corr (x)}")
        rg = new Regression (x, y)
        rg.train ().eval ()
        println (rg.report)
        println (rg.summary)
   } // for

} // RegressionTest10 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTest11` object tests the `Regression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.analytics.RegressionTest11
 */
object RegressionTest11 extends App
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
    val rg = new Regression (x, y)
    rg.train ().eval ()
    val n = x.dim2                                                   // number of parameters/variables
    println (rg.report)
    println (rg.summary)

    banner ("Forward Selection Test")
    val rSq = new MatrixD (x.dim2-1, 3)                              // R^2, R^2 Bar, R^2 cv

    val fcols = Set (0)                                              // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)                // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                             // add variable x_j
            val x_cols = x.selectCols (fcols.toArray)                // x projected onto cols_j columns
            rSq(l-1)   = Fit.qofVector (fit_j, rg.crossVal (x_cols))   // use main model, 'rg'
//          val rg_j   = new Regression (x_cols, y)                  // new model, regress with x_j added
//          rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())     // use new model, rg_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                     // instance index
    new PlotM (t, rSq.t, lines = true)

} // RegressionTest11 object

