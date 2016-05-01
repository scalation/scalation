
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael E. Cotterell
 *  @version 1.2
 *  @date    Sun Jan 11 19:05:20 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T
// FIX: needs improved optimization

package scalation.analytics

import math.{exp, pow}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.util.Error

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpRegression` class supports exponential regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in the
 *  exponential regression equation
 *  <p>
 *      log (mu (x))  =  b dot x  =  b_0 + b_1 * x_1 + ... b_k * x_k
 *  <p>
 *  @see www.stat.uni-muenchen.de/~leiten/Lehre/Material/GLM_0708/chapterGLM.pdf 
 *  @param x       the data/design matrix
 *  @param nonneg  whether to check that responses are nonnegative
 *  @param y       the response vector
 */
class ExpRegression (x: MatrixD, nonneg: Boolean, y: VectorD)
      extends Predictor with Error
{
    if (x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")
    if (nonneg && ! y.isNonnegative) flaw ("constructor", "response vector y must be nonnegative")

    private val k          = x.dim2 - 1          // number of variables (k = n-1)
    private val m          = x.dim1.toDouble     // number of data points (rows)
    private val r_df       = (m-1.0) / (m-k-1.0) // ratio of degrees of freedom

    private var rSquared   = -1.0                // coefficient of determination (quality of fit)
    private var rBarSq     = -1.0                // Adjusted R-squared
    private var fStat      = -1.0                // F statistic (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector b, compute '-2 * Log-Likelihood' (-2LL).
     *  '-2LL' is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @see www.statisticalhorizons.com/wp-content/uploads/Allison.StatComp.pdf
     *  @param b  the parameters to fit
     */
    def ll (b: VectorD): Double = 
    {
        var sum = 0.0
        for (i <- 0 until y.dim) {
            val bx = b dot x(i)
            sum += -bx - y(i) / exp { bx }
        } // for
        -2.0 * sum
    } // ll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector b, compute '-2 * Log-Likelihood' (-2LL) for
     *  the null model (the one that does not consider the effects of x(i)).
     *  @param b  the parameters to fit
     */
    def ll_null (b: VectorD): Double =
    {
        var sum = 0.0
        for (i <- 0 until y.dim) {
            val bx = b(0)                                  // only use intercept
            sum += -bx - y(i) / exp { bx }
        } // for
        -2.0 * sum
    } // ll_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  exponential regression equation.
     */
    def train ()
    {
        val b0   = new VectorD (x.dim2)             // use b_0 = 0 for starting guess for parameters
        val bfgs = new QuasiNewton (ll)             // minimizer for -2LL
        b        = bfgs.solve (b0)                  // find optimal solution for parameters
        val e    = y / (x * b)                      // residual/error vector
        val sse  = e dot e                          // residual/error sum of squares
        val sst  = (y dot y) - pow (y.sum, 2) / m   // total sum of squares
        val ssr  = sst - sse                        // regression sum of squares
        rSquared = ssr / sst                        // coefficient of determination (R-squared)
        rBarSq   = 1.0 - (1.0-rSquared) * r_df      // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (m-k-1.0)  / (sse * k)     // F statistic (msr / mse)
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.
     */
    def fit: VectorD = VectorD (rSquared, rBarSq, fStat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = b dot z

} // ExpRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpRegressionTest` object tests `ExpRegression` class using the following
 *  exponential regression problem.
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
    println ("y = " + y)

    val erg = new ExpRegression (x, true, y)
    erg.train ()
    println ("fit = " + erg.fit)

    val yp = erg.predict (z)
    println ("predict (" + z + ") = " + yp)

} // ExpRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExpRegressionTest2` object has a basic test for the `ExpRegression` class.
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
    def test (n: Int = 10000, k: Int = 5): Tuple5 [Int, Int, VectorD, VectoD, Double] =
    {
        val u = new Uniform (0, 1)     // uniform random
        val e = new Exponential (1)    // exponential error
        val r = new Random ()

        val x = new MatrixD (n, k)     // data matrix
        val y = new VectorD (x.dim1)   // response vector
        val b = new VectorD (k)        // known coefficients

        for (i <- 0 until b.dim) b(i) = 1 + r.gen * 6

        for (i <- 0 until x.dim1; j <- 0 until x.dim2) x(i, j) = u.gen

        for (i <- 0 until y.dim) y(i) = exp (x(i) dot b) * e.gen

        val erg = new ExpRegression (x, true, y)
        erg.train ()

        (n, k, b, erg.coefficient, erg.fit(0))
    } // test

    val tests = Array.ofDim [Tuple5 [Int, Int, VectorD, VectoD, Double]] (10)

    for (k <- 0 until tests.size) tests(k) = test(1000, k + 1)

    tests.foreach {
        case (n, k, actual, fit, rSquared) => {
            actual.setFormat ("%.4f, ")
            fit.setFormat ("%.4f, ")
            println ("nobs = %d, regressors = %d, R^2 = %f\nactual = %s\nfir    = %s\n".format(n, k, rSquared, actual, fit))
        } // case
    } // foreach

} // ExpRegressionTest2

