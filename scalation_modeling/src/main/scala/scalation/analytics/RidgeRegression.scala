
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Jan 31 20:59:02 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Ridge Regression using QR
 *  @see math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression
 *  Ridge Regression using SVD
 *  @see Hastie, T., Tibshirani, R., & Friedman, J. (2009). The Elements of Statistical Learning
 */

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.{log, sqrt}

import scalation.linalgebra._
import scalation.minima.GoldenSectionLS
import scalation.plot.{Plot, PlotM}
import scalation.stat.Statistic
import scalation.util.{banner, Error}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegression` class supports multiple linear ridge regression.
 *  In this case, 'x' is multi-dimensional [x_1, ... x_k].  Ridge regression puts
 *  a penalty on the L2 norm of the parameters b to reduce the chance of them taking
 *  on large values that may lead to less robust models.  Both the input matrix 'x'
 *  and the response vector 'y' are centered (zero mean).  Fit the parameter vector
 *  'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the regularized Normal Equations:
 *  <p>
 *      b  =  fac.solve (.)  with regularization  x.t * x + λ * I
 *  <p>
 *  Five factorization techniques are provided:
 *  <p>
 *      'QR'         // QR Factorization: slower, more stable (default)
 *      'Cholesky'   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      'SVD'        // Singular Value Decomposition: slowest, most robust
 *      'LU'         // LU Factorization: similar, but better than inverse
 *      'Inverse'    // Inverse/Gaussian Elimination, classical textbook technique 
 *  <p>
 *  @see statweb.stanford.edu/~tibs/ElemStatLearn/
 *  @param x          the centered data/input m-by-n matrix NOT augmented with a first column of ones
 *  @param y          the centered response/output m-vector
 *  @param fname_     the feature/variable names
 *  @param hparam     the shrinkage hyper-parameter, lambda (0 => OLS) in the penalty term 'lambda * b dot b'
 *  @param technique  the technique used to solve for b in (x.t*x + lambda*I)*b = x.t*y
 */
class RidgeRegression (x: MatriD, y: VectoD,
                       fname_ : Strings = null, hparam: HyperParameter = RidgeRegression.hp,
                       technique: RegTechnique = Cholesky)
      extends PredictorMat (x, y, fname_, hparam)
{
    resetDF (x.dim2, x.dim1 - x.dim2)                                  // no intercept => correct degrees of freedom

    private val DEBUG  = true                                          // debug flag
    private val xtx    = x.t * x                                       // precompute X.t * X
    private val ey     = MatrixD.eye (x.dim1, x.dim2)                  // identity matrix
    private var lambda = if (hparam ("lambda") <= 0.0) gcv () else hparam ("lambda")

    type Fac_QR = Fac_QR_H [MatriD]                                    // change as needed

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a solver for the Modified Normal Equations using the selected
     *  factorization technique.
     */
    private def solver (): Factorization =
    {
        val xtx_ = xtx.copy ().asInstanceOf [MatriD]                   // copy xtx (X.t * X)
        for (i <- xtx_.range1) xtx_(i, i) += lambda                    // add lambda to the diagonal

        technique match {                                              // select the factorization technique
        case QR       => val x_ = x ++ (ey * sqrt (lambda))
                         new Fac_QR (x_) // , false)                   // QR Factorization
        case Cholesky => new Fac_Cholesky (xtx_)                       // Cholesky Factorization
//      case SVD      => new SVD (x)                                   // Singular Value Decomposition - FIX
        case LU       => new Fac_LU (xtx_)                             // LU Factorization
        case _        => new Fac_Inv (xtx_)                            // Inverse Factorization
        } // match
    } // solver

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      yy  =  b dot x + e  =  [b_1, ... b_k] dot [x_1, ... x_k] + e
     *  <p>
     *  using the least squares method.
     *  @param yy  the response/ouput vector
     */
    def train (yy: VectoD = y): RidgeRegression =
    {
        val fac = solver ()                                            // create selected factorization technique
        fac.factor ()                                                  // factor the matrix, either X or X.t * X

        b = technique match {                                          // solve for coefficient vector b
            case QR       => fac.solve (yy ++ new VectorD (y.dim))     // FIX - give formula
            case Cholesky => fac.solve (x.t * yy)                      // L * L.t * b = X.t * yy
//          case SVD      => fac.solve (yy)                            // FIX - give formula
            case LU       => fac.solve (x.t * yy)                      // b = (X.t * X) \ X.t * yy
            case _        => fac.solve (x.t * yy)                      // b = (X.t * X)^-1 * X.t * yy
        } // match
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal value for the shrinkage parameter 'λ' using Generalized
     *  Cross Validation (GCV).
     *  FIX - try other QoF measures, e.g., sse_cv
     *  @param xx  the data/input matrix (full or test)
     *  @param yy  the response/output vector (full or test)
     */
    def gcv (xx: MatriD = x, yy: VectoD = y): Double =
    {
        def f_sse (λ: Double): Double = 
        {
            lambda = λ
            train (yy)
            e = yy - xx * b
            val sse = e dot e
            if (sse.isNaN) throw new ArithmeticException ("sse is NaN")
            if (DEBUG) println (s"gcv: for lambda = $λ, sse = $sse")
            sse
        } // f_sse

        val gs = new GoldenSectionLS (f_sse _)
        gs.search ()
    } // gcv

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
            val rg_j   = new RidgeRegression (x_cols, y, null, hparam, technique)   // regress with x_j added
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
     */
    def backwardElim (cols: Set [Int], adjusted: Boolean = true, first: Int = 0): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq         // fit(ir) is rSqBar/rSq
        var j_max = -1                                                 // index of variable to eliminate
        var b_max =  b                                                 // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)        // optimize on quality of fit

        for (j <- first until x.dim2 if cols contains j) {
            val cols_j = cols - j                                      // try removing x_j
            val x_cols = x.selectCols (cols_j.toArray)                 // x projected onto cols_j columns
            val rg_j   = new RidgeRegression (x_cols, y, null, hparam, technique)   // regress with x_j removed
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
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     */
    def vif: VectoD =
    {
        val ir   = index_rSq                                           // fit(ir) is rSq
        val vifV = new VectorD (k)                                     // VIF vector

        for (j <- 1 to k) {
            val x_j   = x.col(j)                                       // x_j is jth column in x
            val x_noj = x.sliceEx (m, j)                               // x without column j
            val rg_j  = new RidgeRegression (x_noj, x_j, null, hparam, technique)   // regress with x_j removed
            rg_j.train ().eval ()
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
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new RidgeRegression (x, y, fname, hparam, technique),
                                                 xx, k, rando)
    } // crossVal

} // RidgeRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegression` companion object defines hyper-paramters and provides
 *  factory functions for the `RidgeRegression` class.
 */
object RidgeRegression extends Error
{
    import PredictorMat.pullResponse

    /** Base hyper-parameter specification for `RidgeRegression`
     */
    val hp = new HyperParameter; hp += ("lambda", 0.01, 0.01)          // shrinkage parameter

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Ridge Regression from a combined data matrix.
     *  @param xy         the centered data/input m-by-n matrix, NOT augmented with a first column of ones
     *                        and the centered response m-vector (combined)
     *  @param fname      the feature/varaiable names
     *  @param hparam     the shrinkage hyper-parameter (0 => OLS) in the penalty term 'lambda * b dot b'
     *  @param technique  the technique used to solve for b in (x.t*x + lambda*I)*b = x.t*y
     */
    def apply (xy: MatriD, fname: Strings = null, hparam: HyperParameter = hp,
               technique: RegTechnique = Cholesky): RidgeRegression =
    {
        val n = xy.dim2
        if (n < 2) {
            flaw ("apply", s"dim2 = $n of the 'xy' matrix must be at least 2")
            null
        } else {
            val (x, y) = pullResponse (xy)
            val mu_x = x.mean                                          // columnwise mean of x
            val mu_y = y.mean                                          // mean of y
            val x_c  = x - mu_x                                        // centered x (columnwise)
            val y_c  = y - mu_y                                        // centered y
            new RidgeRegression (x_c, y_c, fname, hparam, technique)
       } // if
    } // apply


  //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Ridge Regression from a combined data matrix.
     *  @param x          the centered data/input m-by-n matrix, NOT augmented with a first column of ones
     *  @param y          the centered repsonse/output vector
     *  @param fname      the feature/varaiable names
     *  @param hparam     the shrinkage hyper-parameter (0 => OLS) in the penalty term 'lambda * b dot b'
     *  @param technique  the technique used to solve for b in (x.t*x + lambda*I)*b = x.t*y
     */
    def apply (x: MatriD, y: VectoD, fname: Strings, hparam: HyperParameter,
               technique: RegTechnique): RidgeRegression =
    {
        val n = x.dim2
        if (n < 1) {
            flaw ("apply", s"dim2 = $n of the 'x' matrix must be at least 1")
            null
        } else {
            val mu_x = x.mean                                          // columnwise mean of x
            val mu_y = y.mean                                          // mean of y
            val x_c  = x - mu_x                                        // centered x (columnwise)
            val y_c  = y - mu_y                                        // centered y
            new RidgeRegression (x_c, y_c, fname, hparam, technique)
       } // if
    } // apply

} // RidgeRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2.
 *  <p>
 *  It compares `RidgeRegression` with `Regression`
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.RidgeRegressionTest
 */
object RidgeRegressionTest extends App
{
    // 5 data points:             x_0    x_1
    val x = new MatrixD ((5, 2), 36.0,  66.0,                          // 5-by-2 matrix
                                 37.0,  68.0,
                                 47.0,  64.0,
                                 32.0,  53.0,
                                  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    // Compute centered (zero mean) versions of x and y

    val mu_x = x.mean                                                  // columnwise mean of x
    val mu_y = y.mean                                                  // mean of y
    val x_c  = x - mu_x                                                // centered x (columnwise)
    val y_c  = y - mu_y                                                // centered y

    println (s"x_c = $x_c \ny_c = $y_c")

    banner ("Regression")
    val ox = VectorD.one (y.dim) +^: x
    val rg = new Regression (ox, y)
    rg.train ().eval ()
    println (rg.report)
    println (rg.summary)

    val oz = VectorD (1.0, 20.0, 80.0)
    var yp = rg.predict (oz)                                           // predict z and add y's mean
    println (s"predict ($z) = $yp")

    banner ("RidgeRegression")
    val rrg = new RidgeRegression (x_c, y_c)
    rrg.train ().eval ()
    println (rrg.report)
    println (rrg.summary)

    val z_c = z - mu_x                                                 // center z first
    yp = rrg.predict (z_c) + mu_y                                      // predict z_c and add y's mean
    println (s"predict ($z) = $yp")

} // RidgeRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest2` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  Try non-default value for the 'lambda' hyper-parameter.
 *  > runMain scalation.analytics.RidgeRegressionTest2
 */
object RidgeRegressionTest2 extends App
{
    import RidgeRegression.hp

    println (s"hp = $hp")
    val hp2 = hp.updateReturn ("lambda", 1.0)                          // try different values
    println (s"hp2 = $hp2")

    // 5 data points:             x_0    x_1
    val x = new MatrixD ((5, 2), 36.0,  66.0,                          // 5-by-2 matrix
                                 37.0,  68.0,
                                 47.0,  64.0,
                                 32.0,  53.0,
                                  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    // Compute centered (zero mean) versions of x and y

    val mu_x = x.mean                                                  // columnwise mean of x
    val mu_y = y.mean                                                  // mean of y
    val x_c  = x - mu_x                                                // centered x (columnwise)
    val y_c  = y - mu_y                                                // centered y

    println (s"x_c = $x_c \ny_c = $y_c")

    banner ("RidgeRegression")
    val rrg = new RidgeRegression (x_c, y_c, hparam = hp2)
    rrg.train ().eval ()
    println (rrg.report)

    val z_c = z - mu_x                                                 // center z first
    val yp  = rrg.predict (z_c) + mu_y                                 // predict z_c and add y's mean
    println (s"predict ($z) = $yp")

    banner ("Optimize lambda")
    println (s"gcv = ${rrg.gcv ()}")

} // RidgeRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest3` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  Test regression, forward selection and backward elimination.
 *  > runMain scalation.analytics.RidgeRegressionTest3
 */
object RidgeRegressionTest3 extends App
{
    // 5 data points:             x_0    x_1
    val x = new MatrixD ((5, 2), 36.0,  66.0,                          // 5-by-2 matrix
                                 37.0,  68.0,
                                 47.0,  64.0,
                                 32.0,  53.0,
                                  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    // Compute centered (zero mean) versions of x and y

    val mu_x = x.mean                                                  // columnwise mean of x
    val mu_y = y.mean                                                  // mean of y
    val x_c  = x - mu_x                                                // centered x (columnwise)
    val y_c  = y - mu_y                                                // centered y

    println (s"x_c = $x_c \ny_c = $y_c")

    banner ("RidgeRegression")
    val rrg = new RidgeRegression (x_c, y_c)
    rrg.train ().eval ()
    println (rrg.report)

    banner ("Forward Selection Test")
    val fcols = Set [Int] ()                                           // start with no variable
    for (l <- 0 until x.dim2) {
        val (x_j, b_j, fit_j) = rrg.forwardSel (fcols)                 // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        fcols += x_j
    } // for

    banner ("Backward Elimination Test")
    val bcols = Set (0) ++ Array.range (1, x.dim2)                     // start with all x_j in model
    for (l <- 1 until x.dim2) {
        val (x_j, b_j, fit_j) = rrg.backwardElim (bcols)               // eliminate least predictive variable
        println (s"backward model: remove x_j = $x_j with b = $b_j \n fit = $fit_j")
        bcols -= x_j
    } // for

} // RidgeRegressionTest3 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest4` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  > runMain scalation.analytics.RidgeRegressionTest4
 */
object RidgeRegressionTest4 extends App
{
    // 4 data points:             x_1  x_2    y
    val xy = new MatrixD ((4, 3), 1.0, 1.0, 6.0,                       // 4-by-3 matrix
                                  1.0, 2.0, 8.0,
                                  2.0, 1.0, 7.0,
                                  2.0, 2.0, 9.0)
    val (x, y) = PredictorMat.pullResponse (xy)                        // divides into data matrix and response vector
    val z = VectorD (2.0, 3.0)

    println ("x = " + x)
    println ("y = " + y)

    val rrg = RidgeRegression (xy)                                     // factory function does centering
    rrg.train ().eval ()
    println (rrg.report)

    val z_c = z - x.mean                                               // first center z
    val yp = rrg.predict (z_c) + y.mean                                // predict z_c and add y's mean
    println (s"predict ($z) = $yp")

} // RidgeRegressionTest4 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest5` object tests the multi-collinearity method in the
 *  `RidgeRegression` class using the following regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 *  > runMain scalation.analytics.RidgeRegressionTest5
 */
object RidgeRegressionTest5 extends App
{
    import ExampleBPressure._

    var rrg = new RidgeRegression (x, y)                               // no intercept
    rrg.train ().eval ()
    println (rrg.report)
    println ("vif = " + rrg.vif)                                       // test multi-collinearity (VIF)

} // RidgeRegressionTest5 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest6` object tests the `RidgeRegression` class using the AutoMPG
 *  dataset.  It illustrates using the `Relation` class for reading the data
 *  from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.analytics.RidgeRegressionTest6
 */
object RidgeRegressionTest6 extends App
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
    val rrg = RidgeRegression (x, y, null, RidgeRegression.hp, Cholesky)
    rrg.train ().eval ()
    val n = x.dim2                                                     // number of variables
    println (rrg.report)
    println (rrg.summary)

    banner ("Forward Selection Test")
    val rSq = new MatrixD (x.dim2-1, 3)                                // R^2, R^2 Bar,  R^2 cv

    val fcols = Set (0)                                                // start with x_0 in model
    for (l <- 1 until n) {
        val (x_j, b_j, fit_j) = rrg.forwardSel (fcols)                 // add most predictive variable
        println (s"forward model: add x_j = $x_j with b = $b_j \n fit = $fit_j")
        if (x_j == -1) {
            println (s"the 'forwardSel' could not find a variable to add: x_j = $x_j")
        } else {
            fcols += x_j                                               // add variable x_j
            val x_cols = x.selectCols (fcols.toArray)                  // x projected onto cols_j columns
            rSq(l-1)   = Fit.qofVector (fit_j, rrg.crossVal (x_cols))    // use main model, 'rrg'
//          val rg_j   = RidgeRegression (x_cols, y, null, RidgeRegression.hp, Cholesky)   // regress with x_j added
//          rSq(l-1)   = Fit.qofVector (fit_j, rg_j.crossVal ())       // use new model, rg_j
        } // if
    } // for

    val k = fcols.size
    for (l <- k until n) rSq(l-1) = rSq(l-2)
    println (s"rSq = $rSq")
    val t = VectorD.range (1, k)                                       // instance index
    new PlotM (t, rSq.t, lines = true)

} // RidgeRegressionTest6 object

