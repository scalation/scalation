
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Fri Apr  5 17:40:21 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

// U N D E R   D E V E L O P M E N T 

package scalation.analytics

import math.{exp, log}

import scalation.linalgebra.{MatrixD, QRDecomp, VectorD}
import scalation.linalgebra_gen.VectorN
import scalation.linalgebra_gen.Vectors.VectorI
import scalation.math.DoubleWithExp._
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The LogitRegression class supports logit regression.  In this case, x
 *  is multi-dimensional (1, x1, ... xk).  Fit the parameter vector b in the
 *  logit regression equation.
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x  the input/design matrix augmented with a first column of ones
 *  @param y  the binary response vector, y_i in {0, 1}
 */
class LogitRegression (x: MatrixD, y: VectorI)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                   // debug flag
    private val k          = x.dim2 - 1              // number of variables 
    private val n          = x.dim1.toDouble         // number of data points (rows)
    private val r_df       = (n-1.0) / (n-k-1.0)       // ratio of degrees of freedom
    private var b: VectorD = null                    // parameter vector (b0, b1, ... bk)
    private var rSquared   = -1.0                     // coefficient of determination (quality of fit)
    private var rBarSq     = -1.0                     // Adjusted R-squared
    private var fStat      = -1.0                     // F statistic (quality of fit)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the odds of an event ocurring (e.g., success, 1).
     *  @param p  the probability, a number between 0 and 1.
     */
    def logit (p: Double): Double = log (p / (1-p))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the inverse of the logit function.
     *  @param a  the logit value
     */
    def logitInv (a: Double): Double = 1.0 / (1.0 + exp (-a))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector b, compute -2 * Log-Likelihood (-2LL).
     *  -2LL is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @param b  the parameters to fit
     */
    def ll (b: VectorD): Double =
    {
        var sum = 0.0
        for (i <- 0 until x.dim1) {
            val bx = b dot x(i)
            sum += y(i) * bx - log (1.0 + exp (bx))
        } // for
        -2.0 * sum                               // set up for minimization
    } // ll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  logit regression equation using maximum likelihood.  Do this by
     *  minimizing -2LL.
     */
    def train ()
    {
         val b0   = new VectorD (x.dim2)        // use b0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll)        // minimizer for -2LL

         b = bfgs.solve (b0)                    // optimal solution for parameters
         println ("b = " + b + ", ll = %10.4f".format (ll (b)))
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation using the least squares method.
     *  FIX
     *
    def train ()
    {
        b        = x_pinv * y                        // parameter vector (b0, b1, ... bk)
        val e    = y - x * b                         // residual/error vector
        val sse  = e dot e                           // residual/error sum of squares
        val sst  = (y dot y) - y.sum~^2.0 / n         // total sum of squares
        val ssr  = sst - sse                         // regression sum of squares
        rSquared = ssr / sst                         // coefficient of determination (R-squared)
        rBarSq   = 1.0 - (1.0-rSquared) * r_df         // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (n-k-1.0)  / (sse * k)       // F statistic (msr / mse)
    } // train
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrain the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation  using the least squares method.
     *  FIX
     *  @param yy  the new response vector
     * 
    def train (yy: VectorD)
    {
        b        = x_pinv * yy                       // parameter vector (b0, b1, ... bk)
        val e    = yy - x * b                        // residual/error vector
        val sse  = e dot e                           // residual/error sum of squares
        val sst  = (yy dot yy) - yy.sum~^2.0 / n      // total sum of squares
        val ssr  = sst - sse                         // regression sum of squares
        rSquared = ssr / sst                         // coefficient of determination
        rBarSq   = 1.0 - (1.0-rSquared) * r_df         // R-bar-squared (adjusted R-squared)
        fStat    = ssr * (n-k-1.0)  / (sse * k)       // F statistic (msr / mse)
    } // train
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit rSquared)
     */
    def fit: Tuple4 [VectorD, Double, Double, Double] = (b, rSquared, rBarSq, fStat)

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  i.e., (b0, b1) dot (1.0, z1).
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z for
     *  each row of matrix z.
     *  @param z  the new matrix to predict
     */
    def predict (z: MatrixD): VectorD = z * b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     * 
    def backElim (): Tuple4 [Int, VectorD, Double, Double] =
    {
        var j_max   = -1                     // index of variable to eliminate
        var b_max: VectorD = null            // parameter values for best solution
        var rSq_max = -1.0                    // currently maximizing R squared
        var fS_max  = -1.0                    // could optimize on F statistic

        for (j <- 1 to k) {
            val keep = n.toInt               // i-value large enough to not exclude any rows in slice
            val rg_j = new LogitRegression (x.sliceExclude (keep, j), y)       // regress with x_j removed
            rg_j.train ()
            val (b, rSq, fS, rBar) =  rg_j.fit
            if (rSq > rSq_max) { j_max = j; b_max = b; rSq_max = rSq; fS_max = fS}
        } // for
        (j_max, b_max, rSq_max, fS_max)
    } // backElim
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-colinearity by regressing xj against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the varaince of xj can be predicted
     *  from the other variables, so xj is a candidate for removal from the model.
     *
    def vif: VectorD =
    {
        val vifV = new VectorD (k)           // VIF vector
        for (j <- 1 to k) {
            val keep = n.toInt               // i-value large enough to not exclude any rows in slice
            val x_j  = x.col(j)                                           // x_j is jth column in x
            val rg_j = new LogitRegression (x.sliceExclude (keep, j), x_j)     // regress with x_j removed
            rg_j.train ()
            vifV(j-1) =  1.0 / (1.0 - rg_j.fit._2)                      // store vif for x_1 in vifV(0)
        } // for
        vifV
    } // vif
     */

} // LogitRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test LogitRegression class.
 *  @seestatmaster.sdu.dk/courses/st111/module03/index.html
 *  @see www.stat.wisc.edu/~mchung/teaching/.../GLM.logistic.Rpackage.pdf
 */
object LogitRegressionTest extends App
{
    // 40 data points:            One     Low  Medium    High
    val x = new MatrixD ((40, 4), 1.0,  102.0,   89.0,    0.0,
                                  1.0,    7.0,  233.0,    1.0,
                                  1.0,    0.0,    4.0,   41.0,
                                  1.0,    8.0,   37.0,   13.0,
                                  1.0,   40.0,   79.0,   26.0,
                                  1.0,    0.0,  625.0,  156.0,
                                  1.0,    0.0,   12.0,   79.0,
                                  1.0,    0.0,    3.0,  119.0,
                                  1.0,  115.0,  136.0,   65.0,
                                  1.0,  428.0,  416.0,  435.0,
                                  1.0,   34.0,  174.0,   56.0,
                                  1.0,    0.0,    0.0,   37.0,
                                  1.0,   97.0,  162.0,   89.0,
                                  1.0,   56.0,   47.0,  132.0,
                                  1.0, 1214.0, 1515.0,  324.0,
                                  1.0,   30.0,  103.0,  161.0,
                                  1.0,    8.0,   11.0,  158.0,
                                  1.0,   52.0,  155.0,  144.0,
                                  1.0,  142.0,  119.0,   24.0,
                                  1.0, 1370.0, 2968.0, 1083.0,
                                  1.0,  790.0,  161.0,  231.0,
                                  1.0, 1142.0,  157.0,  131.0,
                                  1.0,    0.0,    2.0,   49.0,
                                  1.0,    0.0,    0.0,   50.0,
                                  1.0,    5.0,   68.0,   49.0,
                                  1.0,    0.0,    0.0,   48.0,
                                  1.0,    0.0,    6.0,   40.0,
                                  1.0,    1.0,    8.0,   64.0,
                                  1.0,    0.0,  998.0,  551.0,
                                  1.0,  253.0,   99.0,   60.0,
                                  1.0, 1395.0,  799.0,  244.0,
                                  1.0,    0.0,    0.0,   50.0,
                                  1.0,    1.0,   68.0,  145.0,
                                  1.0, 1318.0, 1724.0,  331.0,
                                  1.0,    0.0,    0.0,   79.0,
                                  1.0,    3.0,   31.0,   37.0,
                                  1.0,  195.0,  108.0,  206.0,
                                  1.0,    0.0,   15.0,  121.0,
                                  1.0,    0.0,  278.0,  513.0,
                                  1.0,    0.0,    0.0,  253.0)

    val y = VectorN (0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1,
                     1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1)

    println ("x = " + x)
    println ("y = " + y)

    val rg = new LogitRegression (x(0 until x.dim1, 0 until 2), y)
    rg.train ()
    println ("fit = " + rg.fit)

    val z  = VectorD (1.0, 100.0, 100.0, 100.0)         // predict y for one point
    val yp = rg.predict (z)
    println ("predict (" + z + ") = " + yp)

//  val yyp = rg.predict (x)                        // predict y for several points
//  println ("predict (" + x + ") = " + yyp)

//  new Plot (x.col(1), y, yyp)
//  new Plot (x.col(2), y, yyp)

} // LogitRegressionTest object

