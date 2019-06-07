
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael E. Cotterell
 *  @version 1.6
 *  @date    Sun Jan 11 19:05:20 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

// FIX: needs improved optimization: try IRWLS

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.{exp, log, sqrt}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD}
import scalation.minima.QuasiNewton
import scalation.stat.Statistic
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpRegression` class supports exponential regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in the
 *  exponential regression equation
 *  <p>
 *      log (mu (x))  =  b dot x  =  b_0 + b_1 * x_1 + ... b_k * x_k
 *  <p>
 *  @see www.stat.uni-muenchen.de/~leiten/Lehre/Material/GLM_0708/chapterGLM.pdf 
 *  @param x       the data/input matrix
 *  @param y       the response/output vector
 *  @param fname_  the feature/variable names
 *  @param hparam  the hyper-parameters (currently none)
 *  @param nonneg  whether to check that responses are nonnegative
 */
class ExpRegression (x: MatriD, y: VectoD,
                     fname_ : Strings = null, hparam: HyperParameter = null,
                     nonneg: Boolean = true)
      extends PredictorMat (x, y, fname_, hparam)
{
    if (nonneg && ! y.isNonnegative) flaw ("constructor", "response vector y must be nonnegative")

    private val DEBUG      = false                                  // debug flag
    private var n_dev      = -1.0                                   // null dev: -LL, for null model (intercept only)
    private var r_dev      = -1.0                                   // residual dev: -LL, for full model
    private var pseudo_rSq = -1.0                                   // McFaffen's pseudo R-squared

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector b, compute '-2 * Log-Likelihood' (-2LL).
     *  '-2LL' is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @see www.statisticalhorizons.com/wp-content/uploads/Allison.StatComp.pdf
     *  @param b  the parameters to fit
     */
    def ll (b: VectoD): Double = 
    {
        var sum = 0.0
        for (i <- y.range) {
            val bx = b dot x(i)
            sum += -bx - y(i) / exp (bx)
        } // for
        -2.0 * sum
    } // ll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector b, compute '-2 * Log-Likelihood' (-2LL) for
     *  the null model (the one that does not consider the effects of x(i)).
     *  @param b  the parameters to fit
     */
    def ll_null (b: VectoD): Double =
    {
        var sum = 0.0
        for (i <- y.range) {
            val bx = b(0)                                           // only use intercept
            sum += -bx - y(i) / exp (bx)
        } // for
        -2.0 * sum
    } // ll_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  exponential regression equation.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): ExpRegression =
    {
        // FIX - currently works for yy = y
        train_null ()
        val b0   = new VectorD (x.dim2)                             // use b_0 = 0 for starting guess for parameters
        val bfgs = new QuasiNewton (ll)                             // minimizer for -2LL
        b        = bfgs.solve (b0)                                  // find optimal solution for parameters

//      e = y / (x * b)                                             // residual/error vector e
        e = y - (x * b).map (exp _)                                 // residual/error vector e
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the null model, train the classifier by fitting the parameter vector
     *  (b-vector) in the logistic regression equation using maximum likelihood.
     *  Do this by minimizing -2l.
     */
    def train_null ()
    {
         val b0   = new VectorD (x.dim2)                            // use b0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll_null)                       // minimizer for -2l
         val b_n = bfgs.solve (b0)                                  // find optimal solution for parameters

         n_dev   = ll_null (b_n)                                    // measure of fitness for null model
    } // train_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of 'y = f(z)' by evaluating the formula 'y = exp (b dot z)',
     *  e.g., 'exp (b_0, b_1, b_2) dot (1, z_1, z_2)'.
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = exp (b dot z)

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
            val x_cols = x.selectCols (cols_j.toArray)              // x projected onto cols
            val rg_j   = new ExpRegression (x_cols, y, null, null, nonneg)  // regress with x_j added
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
     *                       (default (1) assume intercept x_0 will be in any model)
     */
    def backwardElim (cols: Set [Int], adjusted: Boolean = true,
                    first: Int = 1): (Int, VectoD, VectoD) =
    {
        val ir    =  if (adjusted) index_rSqBar else index_rSq      // fit(ir) is rSqBar/rSq
        var j_max = -1                                              // index of variable to eliminate
        var b_max =  b                                              // parameter values for best solution
        var ft_max: VectoD = VectorD.fill (fitLabel.size)(-1.0)     // optimize on quality of fit

        for (j <- first until x.dim2 if cols contains j) {
            val cols_j = cols - j                                   // try removing x_j
            val x_cols = x.selectCols (cols_j.toArray)              // x projected onto cols
            val rg_j   = new ExpRegression (x_cols, y, null, null, nonneg)  // regress with x_j removed
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
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new ExpRegression (x, y, fname, hparam, nonneg),
                                                 xx, k, rando)
    } // crossVal

} // ExpRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpRegressionTest` object tests `ExpRegression` class using the following
 *  exponential regression problem.
 *  > runMain scalation.analytics.ExpRegressionTest
 */
object ExpRegressionTest extends App
{
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x)

    val erg = new ExpRegression (x, y)
    erg.train ().eval ()
    println ("parameter = " + erg.parameter)
    println ("fitMap    = " + erg.fitMap)

    val yp = erg.predict (x)
    println ("y  = " + y)
    println ("yp = " + yp)

    val t = VectorD.range (0, y.dim)
    new Plot (t, y, yp, "ExpRegressionTest")

    val yp2 = erg.predict (z)
    println (s"predict ($z) = $yp2")

} // ExpRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpRegressionTest2` object has a basic test for the `ExpRegression` class.
 *  > runMain scalation.analytics.ExpRegressionTest
 */
object ExpRegressionTest2 extends App
{
    import scalation.random.{Uniform, Exponential, Random}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test `ExpRegression` by simulating 'n'-many observations.
     *  @param n  number of observations
     *  @param k  number of variables
     *  @return   (actual, estimated, r^2)
     */
    def test (n: Int = 10000, k: Int = 5): (Int, Int, VectorD, VectoD, Double) =
    {
        val u = new Uniform (0, 1)     // uniform random
        val e = new Exponential (1)    // exponential error
        val r = new Random ()

        val x = new MatrixD (n, k)     // data matrix
        val y = new VectorD (x.dim1)   // response vector
        val b = new VectorD (k)        // known coefficients

        for (i <- b.range) b(i) = 1 + r.gen * 6

        for (i <- x.range1; j <- x.range2) x(i, j) = u.gen

        for (i <- y.range) y(i) = exp (x(i) dot b) * e.gen

        val erg = new ExpRegression (x, y)
        erg.train ().eval ()

        (n, k, b, erg.parameter, erg.fit(0))
    } // test

    val tests = Array.ofDim [(Int, Int, VectorD, VectoD, Double)] (10)

    for (k <- tests.indices) tests(k) = test (1000, k + 1)

    tests.foreach {
        case (n, k, actual, fit, rSquared) => {
            actual.setFormat ("%.4f, ")
            fit.setFormat ("%.4f, ")
            println ("nobs = %d, regressors = %d, R^2 = %f\nactual = %s\nfir    = %s\n".format(n, k, rSquared, actual, fit))
        } // case
    } // foreach

} // ExpRegressionTest2

