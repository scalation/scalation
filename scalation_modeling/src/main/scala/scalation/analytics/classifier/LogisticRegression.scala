
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Dec 28 21:52:38 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

// FIX: needs improved optimization

package scalation.analytics
package classifier

import scala.math.{exp, log}

import scalation.linalgebra.{MatrixD, VectorD, VectoD, VectorI}
import scalation.minima.QuasiNewton
import scalation.plot.Plot

import ActivationFunc.sigmoid

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LogisticRegression` class supports (binomial) logistic regression.  In this
 *  case, 'x' may be multi-dimensional '[1, x_1, ... x_k]'.  Fit the parameter
 *  vector 'b' in the logistic regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model)
 *  and 'y' is now binary.
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x   the input/design matrix augmented with a first column of ones
 *  @param y   the binary response vector, y_i in {0, 1}
 *  @param fn  the names for all factors
 *  @param cn  the names for both classes
 */
class LogisticRegression (x: MatrixD, y: VectorI, fn: Array [String], cn: Array [String] = Array ("no", "yes"))
      extends ClassifierReal (x, y, fn, 2, cn)
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                    // debug flag
    private val k          = x.dim2 - 1               // number of variables 
    private val r_df       = (n-1.0) / (n-k-1.0)      // ratio of degrees of freedom
    private var b: VectorD = null                     // parameter vector (b_0, b_1, ... b_k)
    private var n_dev      = -1.0                     // null dev: -2LL, for null model (intercept only)
    private var r_dev      = -1.0                     // residual dev: -2LL, for full model
    private var aic        = -1.0                     // Akaike’s Information Criterion
    private var pseudo_rSq = -1.0                     // McFaffen's pseudo R-squared

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector 'b', compute '-2 * Log-Likelihood (-2LL)'.
     *  '-2LL' is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @see www.statisticalhorizons.com/wp-content/uploads/Allison.StatComp.pdf
     *  @param b  the parameters to fit
     */
    def ll (b: VectorD): Double =
    {
        var sum = 0.0
        for (i <- 0 until x.dim1) {
            val bx = b dot x(i)
//          sum += y(i) * bx - log (1.0 + exp (bx))
            sum += y(i) * bx - bx - log (exp (-bx) + 1.0)   // less prone to overflow (infinity)
        } // for
        -2.0 * sum                                          // set up for minimization
    } // ll
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector 'b = [b(0)]', compute '-2 * Log-Likelihood (-2LL)'.
     *  '-2LL' is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @see www.statisticalhorizons.com/wp-content/uploads/Allison.StatComp.pdf
     *  @param b  the parameters to fit
     */
    def ll_null (b: VectorD): Double =
    {
        var sum = 0.0
        val bx = b(0)                                       // only use the intercept
        for (i <- 0 until x.dim1) {
//          sum += y(i) * bx - log (1.0 + exp (bx))
            sum += y(i) * bx - bx - log (exp (-bx) + 1.0)   // less prone to overflow (infinity)
        } // for
        -2.0 * sum                                          // set up for minimization
    } // ll_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the full model, train the classifier by fitting the parameter vector
     *  (b-vector) in the logistic regression equation using maximum likelihood.
     *  Do this by minimizing '-2LL'.
     *  FIX: Use improved BFGS implementation or IRWLS
     *  @see stats.stackexchange.com/questions/81000/calculate-coefficients-in-a-logistic-regression-with-r
     *  @see en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
     *  @param testStart  starting index of test region (inclusive) used in cross-validation.
     *  @param testEnd    ending index of test region (exclusive) used in cross-validation.
     */
    def train (testStart: Int, testEnd: Int)    // FIX - use these parameters
    {
         val b0   = new VectorD (x.dim2)        // use b_0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll)        // minimizer for -2LL

         b     = bfgs.solve (b0)                // find optimal solution for parameters
         r_dev = ll (b)                         // measure of fitness for full model
         aic   = r_dev + 2.0 * x.dim2
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the null model, train the classifier by fitting the parameter vector
     *  (b-vector) in the logistic regression equation using maximum likelihood.
     *  Do this by minimizing -2LL.
     */
    def train_null ()
    {
         val b0   = new VectorD (x.dim2)        // use b0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll_null)   // minimizer for -2LL

         val b_n = bfgs.solve (b0)              // find optimal solution for parameters
         n_dev   = ll_null (b_n)                // measure of fitness for null model
    } // train_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fit (parameter vector b, quality of fit). Assumes both
     *  train_null and train have already been called.
     */
    def fit: Tuple5 [VectorD, Double, Double, Double, Double] = 
    {
        pseudo_rSq = 1.0 - r_dev / n_dev
        (b, n_dev, r_dev, aic, pseudo_rSq)
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the value of y = f(z) by evaluating the formula y = sigmoid (b dot z).
     *  Return the best class, its name and FIX.
     *  @param z  the new vector to classify
     */
    def classify (z: VectoD): (Int, String, Double) =
    {
        val c = if (sigmoid (b dot z) > 0.5) 1 else 0
        (c, cn(c), -1.0)                                    // Fix - need metric
    } // classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the value of 'y = f(z)' by evaluating the formula 'y = sigmoid (b dot z)',
     *  for an integer vector.
     *  @param z  the new integer vector to classify
     */
//  def classify (z: VectorI): (Int, String, Double) = classify (z.toDouble)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset ()
    {
        // FIX: to be implemented
    } // reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     *  FIX or remove
     */
//  def backElim (): Tuple4 [Int, VectorD, Double, Double] =
//  {
//      var j_max   = -1                     // index of variable to eliminate
//      var b_max: VectorD = null            // parameter values for best solution
//      var rSq_max = -1.0                   // currently maximizing R squared
//      var fS_max  = -1.0                   // could optimize on F statistic
//
//      for (j <- 1 to k) {
//          val keep = m                     // i-value large enough to not exclude any rows in slice
//          val rg_j = new LogisticRegression (x.sliceExclude (keep, j), y)       // regress with x_j removed
//          rg_j.train ()
//          val (b, rSq, fS, rBar) =  rg_j.fit
//          if (rSq > rSq_max) { j_max = j; b_max = b; rSq_max = rSq; fS_max = fS}
//      } // for
//      (j_max, b_max, rSq_max, fS_max)
//  } // backElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     *  FIX or remove
     */
//  def vif: VectorD =
//  {
//      val vifV = new VectorD (k)           // VIF vector
//      for (j <- 1 to k) {
//          val keep = m                     // i-value large enough to not exclude any rows in slice
//          val x_j  = x.col(j)                                                 // x_j is jth column in x
//          val rg_j = new LogisticRegression (x.sliceExclude (keep, j), x_j)   // regress with x_j removed
//          rg_j.train ()
//          vifV(j-1) =  1.0 / (1.0 - rg_j.fit._2)                              // store vif for x_1 in vifV(0)
//      } // for
//      vifV
//  } // vif

} // LogisticRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LogisticRegressionTest` object tests the `LogisticRegression` class.
 *  @see www.cookbook-r.com/Statistical_analysis/Logistic_regression/
 *  Answer: b = (-8.8331, 0.4304),
 *          n_dev = 43.860, r_dev = 25.533, aci = 29.533, pseudo_rSq = 0.4178
 *  > run-main scalation.analytics.classifier.LogisticRegressionTest
 */
object LogisticRegressionTest extends App
{
    // 32 data points:            One    Mpg
    val x = new MatrixD ((32, 2), 1.0,  21.0,        //  1 - Mazda RX4 
                                  1.0,  21.0,        //  2 - Mazda RX4 Wa
                                  1.0,  22.8,        //  3 - Datsun 710
                                  1.0,  21.4,        //  4 - Hornet 4 Drive
                                  1.0,  18.7,        //  5 - Hornet Sportabout
                                  1.0,  18.1,        //  6 - Valiant
                                  1.0,  14.3,        //  7 - Duster 360
                                  1.0,  24.4,        //  8 - Merc 240D 
                                  1.0,  22.8,        //  9 - Merc 230
                                  1.0,  19.2,        // 10 - Merc 280
                                  1.0,  17.8,        // 11 - Merc 280C
                                  1.0,  16.4,        // 12 - Merc 450S
                                  1.0,  17.3,        // 13 - Merc 450SL
                                  1.0,  15.2,        // 14 - Merc 450SLC
                                  1.0,  10.4,        // 15 - Cadillac Fleetwood
                                  1.0,  10.4,        // 16 - Lincoln Continental
                                  1.0,  14.7,        // 17 - Chrysler Imperial
                                  1.0,  32.4,        // 18 - Fiat 128
                                  1.0,  30.4,        // 19 - Honda Civic
                                  1.0,  33.9,        // 20 - Toyota Corolla
                                  1.0,  21.5,        // 21 - Toyota Corona
                                  1.0,  15.5,        // 22 - Dodge Challenger
                                  1.0,  15.2,        // 23 - AMC Javelin
                                  1.0,  13.3,        // 24 - Camaro Z28
                                  1.0,  19.2,        // 25 - Pontiac Firebird
                                  1.0,  27.3,        // 26 - Fiat X1-9
                                  1.0,  26.0,        // 27 - Porsche 914-2
                                  1.0,  30.4,        // 28 - Lotus Europa
                                  1.0,  15.8,        // 29 - Ford Pantera L
                                  1.0,  19.7,        // 30 - Ferrari Dino
                                  1.0,  15.0,        // 31 - Maserati Bora
                                  1.0,  21.4)        // 32 - Volvo 142E

    val y = VectorI (0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
                     0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1)

    var z: VectorD = null

    println ("x = " + x)
    println ("y = " + y)

    val fn = Array ("One", "Mpg")

    val rg = new LogisticRegression (x, y, fn)
    rg.train_null ()                                    // train based on null model
    rg.train ()                                         // train based on full model
    val res = rg.fit                                    // obtain results

    println ("---------------------------------------------------------------")
    println ("Logistic Regression Results")
    println ("b          = " + res._1)
    println ("n_dev      = " + res._2)
    println ("r_dev      = " + res._3)
    println ("aic        = " + res._4)
    println ("pseudo_rSq = " + res._5)

    z = VectorD (1.0, 15.0)                             // classify point z
    println ("classify (" + z + ") = " + rg.classify (z))

    z = VectorD (1.0, 30.0)                             // classify point z
    println ("classify (" + z + ") = " + rg.classify (z))

} // LogisticRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LogisticRegressionTest` object tests the `LogisticRegression` class.
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  @see www.stat.wisc.edu/~mchung/teaching/.../GLM.logistic.Rpackage.pdf
 *  > run-main scalation.analytics.classifier.classifier.LogisticRegressionTest2
 */
object LogisticRegressionTest2 extends App
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

    val y = VectorI (0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1,
                     1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1)

    val fn = Array ("One", "Low", "Medium", "High")
    val cn = Array ("no", "yes")

    println ("x = " + x)
    println ("y = " + y)

//  val rg = new LogisticRegression (x(0 until x.dim1, 0 until 2), y, cn)
    val rg = new LogisticRegression (x, y, fn, cn)
    rg.train_null ()                                    // train based on null model
    rg.train ()                                         // train based on full model
    val res = rg.fit                                    // obtain results

    println ("---------------------------------------------------------------")
    println ("Logistic Regression Results")
    println ("b          = " + res._1)
    println ("n_dev      = " + res._2)
    println ("r_dev      = " + res._3)
    println ("aic        = " + res._4)
    println ("pseudo_rSq = " + res._5)

    val z  = VectorD (1.0, 100.0, 100.0, 100.0)         // classify point z
    println ("classify (" + z + ") = " + rg.classify (z))

//  new Plot (x.col(1), y, yyp)
//  new Plot (x.col(2), y, yyp)

} // LogisticRegressionTest2 object

