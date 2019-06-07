
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Feb  2 16:43:07 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

// FIX - currently only works for Inverse

package scalation.analytics.par

import scala.collection.mutable.Set
import scala.math.pow

import scalation.linalgebra.{Fac_QR_H, MatriD, VectoD}
import scalation.linalgebra.par._
import scalation.plot.Plot
import scalation.stat.Statistic
import scalation.util.{Error, time}

import scalation.analytics.{PredictorMat, Strings}
import scalation.analytics.RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegression` class supports multiple linear regression.  In this
 *  case, 'x' is multi-dimensional [x_1, ... x_k].  Both the input matrix 'x' and 
 *  the response vector 'y' are centered (zero mean).  Fit the parameter vector
 *  'b' in the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to fit the parameter vector
 *  <p>
 *      b  =  x_pinv * y   [ alternative: b  =  solve (y) ]
 *  <p>
 *  where 'x_pinv' is the pseudo-inverse.  Three techniques are provided:
 *  <p>
 *      'Fac_QR'         // QR Factorization: slower, more stable (default)
 *      'Fac_Cholesky'   // Cholesky Factorization: faster, less stable (reasonable choice)
 *      'Inverse'        // Inverse/Gaussian Elimination, classical textbook technique (outdated)
 *  <p>
 *  This version uses parallel processing to speed up execution.
 *  @see statweb.stanford.edu/~tibs/ElemStatLearn/
 *  @param x          the centered input/design m-by-n matrix NOT augmented with a first column of ones
 *  @param y          the centered response vector
 *  @param fname_     the feature/variable names
 *  @param lambda     the shrinkage parameter (0 => OLS) in the penalty term 'lambda * b dot b'
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class RidgeRegression (x: MatrixD, y: VectorD, fname_ : Strings = null, lambda: Double = 0.1,
                       technique: RegTechnique = Inverse)
      extends PredictorMat (x, y, fname_)
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (x.dim1 <= x.dim2) flaw ("constructor", "not enough data rows in matrix to use regression")

    private val DEBUG      = true                               // debug flag
    private val r_df       = (m-1.0) / (m-k-1.0)                // ratio of degrees of freedom
    private var rSquared   = -1.0                               // coefficient of determination (quality of fit)
    private var rBarSq     = -1.0                               // Adjusted R-squared
    private var fStat      = -1.0                               // F statistic (quality of fit)

    type Fac_QR = Fac_QR_H [MatrixD]                            // change as needed

    private val fac = technique match {                         // select the factorization technique
        case QR       => new Fac_QR (x)                         // QR Factorization
        case Cholesky => new Fac_Cholesky (x.t * x)             // Cholesky Factorization
        case _        => null                                   // don't factor, use inverse
    } // match

    private val x_pinv = technique match {                      // pseudo-inverse of x
        case QR       => val (q, r) = fac.factor12 (); r.inverse * q.t
        case Cholesky => fac.factor (); null                    // don't compute it directly
        case _        => (xtx).inverse * x.t                    // classic textbook technique
    } // match

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute x.t * x and add lambda to the diagonal
     */
    def xtx: MatrixD = { val a = x.t * x; for (i <- 0 until a.dim1) a(i, i) += lambda; a }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      yy  =  b dot x + e  =  [b_1, ... b_k] dot [x_1, ... x_k] + e
     *  using the least squares method.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): RidgeRegression =
    {
        b        = if (x_pinv == null) fac.solve (yy)
                   else x_pinv * yy                             // x parameter vector [b_1, ... b_k]
        val e    = yy - x * b                                   // residual/error vector
        val sse  = e dot e                                      // residual/error sum of squares
        val sst  = (yy dot yy) - pow (yy.sum, 2) / m            // total sum of squares
        val ssr  = sst - sse                                    // regression sum of squares
        rSquared = ssr / sst                                    // coefficient of determination
        rBarSq   = 1.0 - (1.0-rSquared) * r_df                  // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k)                 // F statistic (msr / mse)
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula below.
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = b dot z

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
        val ir    =  if (adjusted) index_rSqBar else index_rSq      // qof = fit(ir) either rSqBar/rSq
        var j_max = -1                                              // index of variable to add
        var b_max =  b                                              // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)     // optimize on quality of fit

        for (j <- x.range2 if ! (cols contains j)) {
            val cols_j = cols + j                                   // try adding x_j
            val rg_j = new RidgeRegression (x.selectCols (cols_j.toArray), y)  // regress with x_j added
            rg_j.train ().eval ()                                   // train model, evaluate QoF
            val bb  = rg_j.parameter
            val ft  = rg_j.fit
            val qof = ft(ir)                                        // rSqBar/rSq
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
    def backwardElim (cols: Set [Int], adjusted: Boolean = true,
                      first: Int = 0): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq      // fit(ir) is rSqBar/rSq
        var j_max = -1                                              // index of variable to eliminate
        var b_max =  b                                              // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)     // optimize on quality of fit

        for (j <- first to k if cols contains j) {
            val cols_j = cols - j                                   // try removing x_j
            val rg_j = new RidgeRegression (x.selectCols (cols_j.toArray), y)  // regress with x_j removed
            rg_j.train ().eval ()                                   // train model, evaluate QoF
            val bb  = rg_j.parameter
            val ft  = rg_j.fit
            val qof = ft(ir)                                        // rSqBar/rSq
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
    def vif: VectorD =
    {
        val ir   = index_rSq                                            // fit(ir) is rSq
        val vifV = new VectorD (k)                                      // VIF vector
        val keep = m.toInt                                              // i-value large enough to not exclude any rows in slice

        for (j <- 1 to k) {
            val x_j  = x.col(j)                                         // x_j is jth column in x
            val rg_j = new RidgeRegression (x.sliceEx (keep, j), x_j)   // regress with x_j removed
            rg_j.train ().eval ()
            val rSq_j = rg_j.fit(ir)
            if (DEBUG) println (s"vif: for variable x_$j, rSq_$j = $rSq_j")
            vifV(j-1) =  1.0 / (1.0 - rSq_j)                            // store vif for x_1 in vifV(0)
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
        crossValidate ((x: MatriD, y: VectoD) => new RidgeRegression (x.asInstanceOf [MatrixD], y.asInstanceOf [VectorD]),
                                                 xx, k, rando)
    } // crossVal

} // RidgeRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegression` companion object is used to center the input matrix 'x'.
 *  This is done by subtracting the column means from each value.
 */
object RidgeRegression
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Center the input matrix 'x' to zero mean, column-wise, by subtracting the mean.
     *  @param x     the input matrix to center
     *  @param mu_x  the vector of column means of matrix x
     */
    def center (x: MatrixD, mu_x: VectoD): MatrixD =
    {
        val x_c = new MatrixD (x.dim1, x.dim2)
        for (j <- 0 until x.dim2) {
            x_c.setCol (j, x.col(j) - mu_x(j))       // subtract column means
        } // for
        x_c
    } // center

} // RidgeRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2.
 *  <p>
 *  Test regression and backward elimination.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 */
object RidgeRegressionTest extends App
{
    import RidgeRegression.center

    // 5 data points: x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 2), 36.0,  66.0,               // 5-by-3 matrix
                                 37.0,  68.0,
                                 47.0,  64.0,
                                 32.0,  53.0,
                                  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    // Compute centered (zero mean) versions of x, y and z

    val mu_x = x.mean                 // columnwise mean of x
    val mu_y = y.mean                 // mean of y
    val mu_z = z.mean                 // mean of z
    val x_c  = center (x, mu_x)       // centered x (columnwise)
    val y_c  = y - mu_y               // centered y
    val z_c  = z - mu_z               // centered y

    println ("x_c = " + x_c + "\ny_c = " + y_c + "\nz_c = " + z_c)

    val rrg = new RidgeRegression (x_c, y_c)
    rrg.train ().eval ()
    println ("fit = " + rrg.fit)
    val yp = rrg.predict (z_c) + mu_y                     // predict y for one point
    println ("predict (" + z + ") = " + yp)

//  val yyp = rrg.predict (x_c) + mu_y                    // predict y for several points
//  println ("predict (" + x + ") = " + yyp)
//
//  new Plot (x.col(0), y, yyp)
//  new Plot (x.col(1), y, yyp)

    val cols = Set (0) ++ Array.range (1, x.dim2)
    println ("reduced model: fit = " + rrg.backwardElim (cols))   // eliminate least predictive variable

} // RidgeRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest2` object tests `RidgeRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  <p>
 *  Test regression using QR Decomposition and Gaussian Elimination for computing
 *  the pseudo-inverse.
 */
object RidgeRegressionTest2 extends App
{
    // 4 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((4, 2), 1.0, 1.0,                  // 4-by-3 matrix
                                 1.0, 2.0,
                                 2.0, 1.0,
                                 2.0, 2.0)
    val y = VectorD (6.0, 8.0, 7.0, 9.0)
    val z = VectorD (2.0, 3.0)
    var rrg: RidgeRegression = null

    println ("x = " + x)
    println ("y = " + y)

    println ("-------------------------------------------------")
    println ("Fit the parameter vector b using QR Factorization")
    rrg = new RidgeRegression (x, y)
    rrg.train ().eval ()
    println ("fit = " + rrg.fit)
    val yp = rrg.predict (z)                         // predict y for on3 point
    println ("predict (" + z + ") = " + yp)

//  val yyp = rrg.predict (x)                        // predict y for several points
//  println ("predict (" + x + ") = " + yyp)
//
//  new Plot (x.col(1), y, yyp)
//  new Plot (x.col(2), y, yyp)

} // RidgeRegressionTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RidgeRegressionTest3` object tests the multi-collinearity method in the
 *  `RidgeRegression` class using the following regression equation.
 *  <p>
 *      y  =  b dot x  =  b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4 * x_4
 *  <p>
 *  @see online.stat.psu.edu/online/development/stat501/12multicollinearity/05multico_vif.html
 *  @see online.stat.psu.edu/online/development/stat501/data/bloodpress.txt
 */
object RidgeRegressionTest3 extends App
{
    // 20 data points:             x_1     x_2    x_3      x_4
    //                             Age  Weight    Dur   Stress
    val x = new MatrixD ((20, 4), 47.0,   85.4,   5.1,    33.0,
                                  49.0,   94.2,   3.8,    14.0,
                                  49.0,   95.3,   8.2,    10.0,
                                  50.0,   94.7,   5.8,    99.0,
                                  51.0,   89.4,   7.0,    95.0,
                                  48.0,   99.5,   9.3,    10.0,
                                  49.0,   99.8,   2.5,    42.0,
                                  47.0,   90.9,   6.2,     8.0,
                                  49.0,   89.2,   7.1,    62.0,
                                  48.0,   92.7,   5.6,    35.0,
                                  47.0,   94.4,   5.3,    90.0,
                                  49.0,   94.1,   5.6,    21.0,
                                  50.0,   91.6,  10.2,    47.0,
                                  45.0,   87.1,   5.6,    80.0,
                                  52.0,  101.3,  10.0,    98.0,
                                  46.0,   94.5,   7.4,    95.0,
                                  46.0,   87.0,   3.6,    18.0,
                                  46.0,   94.5,   4.3,    12.0,
                                  48.0,   90.5,   9.0,    99.0,
                                  56.0,   95.7,   7.0,    99.0)
    //  response BP
    val y = VectorD (105.0, 115.0, 116.0, 117.0, 112.0, 121.0, 121.0, 110.0, 110.0, 114.0,
                     114.0, 115.0, 114.0, 106.0, 125.0, 114.0, 106.0, 113.0, 110.0, 122.0)

    val rrg = new RidgeRegression (x, y)
    time { rrg.train ().eval () }

    println ("fit      = " + rrg.fit)       // fit model y = b_1*x_1 + b_2*x_2 + b_3*x_3 + b_4*x_4
    println ("vif      = " + rrg.vif)       // test multi-colinearity (VIF)

} // RidgeRegressionTest3 object

