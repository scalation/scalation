
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun May 12 17:59:00 EDT 2019
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

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_Regression` class supports multiple linear regression.  In this case,
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
 *  @param x          the data/input m-by-nx matrix
 *                        (augment with a first column of ones to include intercept in model)
 *  @param y          the response/output m-by-ny matrix
 *  @param fname_     the feature/variable names
 *  @param hparam     the hyper-parameters (it doesn't have any, but may be used by derived classes)
 *  @param technique  the technique used to solve for b_k in x.t*x*b_k = x.t*y_k
 */
class MV_Regression (x: MatriD, y: MatriD,
                     fname_ : Strings = null, hparam: HyperParameter = null,
                     technique: RegTechnique = QR)
      extends PredictorMat2 (x, y, fname_, hparam)
{
    private val DEBUG = true                                           // debug flag
    private val b     = new NetParam (new MatrixD (nx, ny))            // parameters (weights, bias implicit) in to out

    type Fac_QR = Fac_QR_H [MatriD]                                    // change as needed (H => Householder)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameters 'b' (weight matrix 'b.w') (array of 1).
     */
    def parameters: NetParams = Array (b)

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
     *  This version only works with the first response.
     *  @param yy  the response/output vector to work with (defaults to y)
     */
    def train0 (yy: MatriD = y): MV_Regression =
    {
        val fac = solver ()                                            // create selected factorization technique
        fac.factor ()                                                  // factor the matrix, either X or X.t * X
        val y0 = yy.col (0)                                            // first column of response matrix yy
        b.w.setCol(0, technique match {                                // solve for parameter/coefficient vector b (column k)
            case QR       => fac.solve (y0)                            // R * b = Q.t * yy
            case Cholesky => fac.solve (x.t * y0)                      // L * L.t * b = X.t * yy
            case SVD      => fac.solve (y0)                            // b = V * Σ^-1 * U.t * yy
            case LU       => fac.solve (x.t * y0)                      // b = (X.t * X) \ X.t * yy
            case _        => fac.solve (x.t * y0)                      // b = (X.t * X)^-1 * X.t * yy
        }) // match
        if (DEBUG) (s"train: parameter b = $b")
        this
    } // train0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter matrix (b-matrix) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  using the ordinary least squares 'OLS' method.
     *  @param yy  the response/output vector to work with (defaults to y)
     */
    def train (yy: MatriD = y): MV_Regression =
    {
        val fac = solver ()                                            // create selected factorization technique
        fac.factor ()                                                  // factor the matrix, either X or X.t * X
        for (k <- y.range2) {	
            val yk = yy.col (k)                                        // k-th column of response matrix yy
            b.w.setCol (k, technique match {                           // solve for parameter/coefficient vector b (column k)
                case QR       => fac.solve (yk)                        // R * b = Q.t * yy
                case Cholesky => fac.solve (x.t * yk)                  // L * L.t * b = X.t * yy
                case SVD      => fac.solve (yk)                        // b = V * Σ^-1 * U.t * yy
                case LU       => fac.solve (x.t * yk)                  // b = (X.t * X) \ X.t * yy
                case _        => fac.solve (x.t * yk)                  // b = (X.t * X)^-1 * X.t * yy
            }) // match
        } // for
        if (DEBUG) (s"train: parameter b = $b")
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add, the new parameter vector and
     *  the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for 'ir' index of QoF measures.
     *  @param cols      the columns of matrix x currently included in the existing model
     *  @param adjusted  whether to use rSqBar or rSq as the criterion
     */
    def forwardSel (cols: Set [Int], adjusted: Boolean = true): (Int, NetParams, VectoD) =
    {
        val ft0   = fitA(0)
        val ir    =  if (adjusted) Fit.index_rSqBar else Fit.index_rSq   // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                                 // index of variable to add
        var b_max =  b.w                                               // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)        // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                      // try adding variable/column x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new MV_Regression (x_cols, y, null, null, technique)   // regress with x_j added
            rg_j.train ().eval ()                                      // train model, evaluate QoF
            val bb  = rg_j.parameters(0).w
            val ft  = rg_j.fitA(0).fit
            val qof = ft(ir)                                           // rSqBar/rSq
            if (DEBUG) println (s"forwardSel: cols_$j = $cols_j, qof_$j = $qof")
            if (qof > ft_max(ir)) { j_max = j; b_max = bb; ft_max = ft }
        } // for
        if (DEBUG) {
            val k = cols.size + 1
            println (s"forwardSel: add (#$k) variable $j_max, parameter b = $b_max, qof = ${ft_max(ir)}")
        } // if
        (j_max, Array (NetParam (b_max)), ft_max)                      // should add j_max variable 
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
     * 
    def backwardElim (cols: Set [Int], adjusted: Boolean = true, first: Int = 1): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) Fit.index_rSqBar else Fit.index_rSq   // fit(ir) is rSqBar/rSq
        var j_max = -1                                                 // index of variable to eliminate
        var b_max =  b                                                 // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)        // optimize on quality of fit

        for (j <- first until x.dim2 if cols contains j) {
            val cols_j = cols - j                                      // try removing variable/column x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new MV_Regression (x_cols, y, null, null, technique)   // regress with x_j removed
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
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'x_j' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'x_j' can be predicted
     *  from the other variables, so 'x_j' is a candidate for removal from the model.
     */
    def vif: VectoD =
    {
        val ir   = Fit.index_rSq                                       // fit(ir) is rSq
        val vifV = new VectorD (x.dim1 - 1)                            // VIF vector

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
    /** Given a new input vector 'z', predict the output/response vector 'f(z)'.
     *  @param z  the new input vector
     */
    def predictV (z: VectoD): VectoD = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix 'z', predict the output/response matrix 'f(z)'.
     *  @param z  the input matrix
     */
    def predict (z: MatriD = x): MatriD = b * z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation.
     */
//  def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    def crossVal (k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: MatriD) => new MV_Regression (x, y, fname, null, technique),
                                                 k, rando) 
//                                               xx, k, rando)         // FIX - PredictorMat2
    } // crossVal

} // MV_Regression class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_Regression` companion object provides factory apply functions and a testing method.
 */
object MV_Regression extends Error
{
    import PredictorMat.pullResponse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `MV_Regression` object from a combined data matrix.
     *  The last column is assumed to be the response column.
     *  @param xy         the combined data matrix (predictors and responses)
     *  @param fname      the feature/variable names
     *  @param hparam     the hyper-parameters
     *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = null,
               technique: RegTechnique = QR): MV_Regression = 
    {
        val n = xy.dim2
        if (n < 2) {
            flaw ("apply", s"dim2 = $n of the 'xy' matrix must be at least 2")
            null
        } else {
            val (x, y) = pullResponse (xy)
            new MV_Regression (x, MatrixD (Seq (y)), fname, hparam, technique)
       } // if
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the various regression techniques.
     *  @param x      the data/input matrix
     *  @param y      the response/output matrix
     *  @param z      a vector to predict
     *  @param fname  the names of features/variable
     */
    def test (x: MatriD, y: MatriD, z: VectoD, fname: Strings = null)
    {
        println (s"x = $x")
        println (s"y = $y")

        for (tec <- techniques) {                                      // use 'tec' Factorization
            banner (s"Fit the parameter vector b using $tec")
            val rg = new MV_Regression (x, y, fname, null, tec)        // use null for hyper-parameters
            rg.train ().eval ()                                        // train model, evaluate QoF
            println (rg.report)
//          println (rg.summary)

            val yp = rg.predict (x)                                    // predict y for several points
            println (s"predict (x) = $yp")
            new Plot (y.col(0), yp.col(0), null, tec.toString)

            val yp1 = rg.predict (z)                                   // predict y for one point
            println (s"predict ($z) = $yp1")
        } // for
    } // test

} // MV_Regression object

import MV_Regression.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest` object tests `MV_Regression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.MV_RegressionTest
 */
object MV_RegressionTest extends App
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

    test (x, MatrixD (Seq (y)), z)

} // MV_RegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest2` object tests `MV_Regression` class using the following
 *  regression equation, which has a perfect fit.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.MV_RegressionTest2
 */
object MV_RegressionTest2 extends App
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

    test (x, MatrixD (Seq (y)), z)

} // MV_RegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest3` object tests the multi-collinearity method in the
 *  `MV_Regression` class using the following regression equation on the Blood
 *  Pressure dataset.  Also performs Collinearity Diagnostics.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  > runMain scalation.analytics.MV_RegressionTest3
 */
object MV_RegressionTest3 extends App
{
    import ExampleBPressure._

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")

    val z = VectorD (1.0, 46.0, 97.5, 7.0, 95.0)

//  test (x, MatrixD (Seq (y)), z, fname)                              // no intercept
    test (ox, MatrixD (Seq (y)), z, ofname)                            // with intercept

    banner ("Collinearity Diagnostics")
    println ("corr (x) = " + corr (x))                                 // correlations of column vectors in x

} // MV_RegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest4` object tests the multi-collinearity method in the
 *  `MV_Regression` class using the following regression equation on the Blood
 *  Pressure dataset.  It also applies forward selection and backward elimination.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 *  > runMain scalation.analytics.MV_RegressionTest4
 */
object MV_RegressionTest4 extends App
{
    import ExampleBPressure._
    val x = ox                                                         // use ox for intercept

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    banner ("Parameter Estimation and Quality of Fit")
    val rg = new MV_Regression (x, MatrixD (Seq (y)), ofname)
    rg.train ().eval ()
    println (rg.report)
//  println (rg.summary)

    banner ("Collinearity Diagnostics")
    println ("corr (x) = " + corr (x))                                 // correlations of column vectors in x

    banner ("Multi-collinearity Diagnostics")
    println ("vif      = " + rg.vif)                                   // test multi-colinearity (VIF)

    banner ("Forward Selection Test")
    val fcols = Set (0)                                                // start with x_0 in model
    for (l <- 1 until x.dim2) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)                  // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        fcols += x_j
    } // for

} // MV_RegressionTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest5` object tests the cross-validation for the `MV_Regression`
 *  class using the following regression equation on the Blood Pressure dataset.
 *  <p>
 *      y = b dot x = b_0 + b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  > runMain scalation.analytics.MV_RegressionTest5
 */
object MV_RegressionTest5 extends App
{
    import ExampleBPressure._
    val x = ox                                                         // use ox for intercept

//  println ("model: y = b_0 + b_1*x1 + b_2*x_ + b3*x3 + b4*x42")
    println ("model: y = b₀ + b₁∙x₁ + b₂∙x₂ + b₃∙x₃ + b₄∙x₄")
    println ("x = " + x)
    println ("y = " + y)

    banner ("Cross-Validation")
    val rg  = new MV_Regression (x, MatrixD (Seq (y)), fname)
    val results = rg.crossVal ()
    banner ("rSq Statistics")
    println (Statistic.labels)
    println (results(Fit.index_rSq))

} // MV_RegressionTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest6` object tests `MV_Regression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.MV_RegressionTest6
 */
object MV_RegressionTest6 extends App
{
    // 4 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((7, 3), 1.0, 1.0, 1.0,                        // 4-by-3 matrix
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

    test (x, MatrixD (Seq (y)), z)

} // MV_RegressionTest6 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest7` object tests `MV_Regression` class using the following
 *  regression equation.
 *  <p>
 *      y = b dot x = b_0 + b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.MV_RegressionTest7
 */
object MV_RegressionTest7 extends App
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
    val rg = MV_Regression (xy)
    rg.train ().eval ()
    println (rg.report)
//  println (rg.summary)

} // MV_RegressionTest7 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest8` object tests the `MV_Regression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  > runMain scalation.analytics.MV_RegressionTest8
 */
object MV_RegressionTest8 extends App
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
    val rg = new MV_Regression (x, MatrixD (Seq (y)))
    rg.train ().eval ()
    println (rg.report)
//  println (rg.summary)

    banner ("auto_mpg cross-validation")
    rg.crossVal ()

} // MV_RegressionTest8 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MV_RegressionTest9` object tests the `MV_Regression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.analytics.MV_RegressionTest9
 */
object MV_RegressionTest9 extends App
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
    val rg = new MV_Regression (x, MatrixD (Seq (y)))
    rg.train ().eval ()
    val n = x.dim2                                                     // number of parameters/variables
    println (rg.report)
//  println (rg.summary)

    banner ("Forward Selection Test")
    val rSq = new MatrixD (x.dim2-1, 3)                                // R^2, R^2 Bar,  R^2 cv

    val fcols = Set (0)                                                // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = rg.forwardSel (fcols)                  // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                               // add variable x_j
            val x_cols = x.selectCols (fcols.toArray)                  // x projected onto cols_j columns
            val rg_j   = new MV_Regression (x_cols, MatrixD (Seq (y)))   // regress with x_j added
            rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())       // collect qof results
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                       // instance index
    new PlotM (t, rSq.t, lines = true)

} // MV_RegressionTest9 object

