
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Jan 11 19:05:20 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

// FIX: needs improved optimization

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.{exp, log, round}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.math.Combinatorics.fac
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonRegression` class supports Poisson regression.  In this case, 
 *  x' may be multi-dimensional '[1, x_1, ... x_k]'.  Fit the parameter
 *  vector 'b' in the Poisson regression equation
 *  <p>
 *      log (mu(x))  =  b dot x  =  b_0 + b_1 * x_1 + ... b_k * x_k
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model)
 *  and 'y' is now integer valued.
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x   the input/design matrix augmented with a first column of ones
 *  @param y   the integer response vector, y_i in {0, 1, ... }
 *  @param fn  the names of the features/variable
 */
class PoissonRegression (x: MatriD, y: VectoI, fn: Array [String] = null)
      extends Predictor with Error
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                    // debug flag
    private val k          = x.dim2 - 1               // number of variables 
    private val n          = x.dim1.toDouble          // number of data points (rows)
    private val r_df       = (n-1.0) / (n-k-1.0)      // ratio of degrees of freedom
    private var n_dev      = -1.0                     // null dev: -LL, for null model (intercept only)
    private var r_dev      = -1.0                     // residual dev: -LL, for full model
    private var aic        = -1.0                     // Akaike’s Information Criterion
    private var pseudo_rSq = -1.0                     // McFaffen's pseudo R-squared

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector 'b', compute '-Log-Likelihood' (-LL).
     *  '-LL' is the standard measure.
     *  @see dept.stat.lsa.umich.edu/~kshedden/Courses/Stat600/Notes/glm.pdf
     *  @param b  the parameters to fit
     */
    def ll (b: VectoD): Double =
    {
        var sum = 0.0
        for (i <- 0 until x.dim1) {
            val bx = b dot x(i)
            sum += y(i) * bx - exp (bx)          // last term not needed [ - log (fac (y(i))) ]
        } // for
        -sum                                     // set up for minimization
    } // ll
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector 'b = [b(0)], compute -2 * Log-Likelihood' (-2LL).
     *  '-2LL' is the standard measure that follows a Chi-Square distribution. 
     *  @see dept.stat.lsa.umich.edu/~kshedden/Courses/Stat600/Notes/glm.pdf
     *  @param b  the parameters to fit
     */
    def ll_null (b: VectoD): Double =
    {
        var sum = 0.0
        for (i <- 0 until x.dim1) {
            val bx = b(0)                        // only use the intercept
            sum += y(i) * bx - exp (bx)          // last term not needed [ - log (fac (y(i))) ]
        } // for
        - sum                                    // set up for minimization
    } // ll_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the full model, train the predictor by fitting the parameter vector
     *  (b-vector) in the Poisson regression equation using maximum likelihood.
     *  @param yy  the response vector
     */
    def train (yy: VectoD): PoissonRegression =
    {
        throw new UnsupportedOperationException ("train (yy) not implemeted yet")
        null
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the full model, train the predictor by fitting the parameter vector
     *  (b-vector) in the Poisson regression equation using maximum likelihood.
     *  Do this by minimizing '-2LL'.
     */
    def train (): PoissonRegression = 
    {
         val b0   = new VectorD (x.dim2)         // use b_0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll)         // minimizer for -2LL

         b     = bfgs.solve (b0)                 // find optimal solution for parameters
         r_dev = ll (b)                          // measure of fitness for full model
         aic   = r_dev + 2.0 * x.dim2
         this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the null model, train the classifier by fitting the parameter vector
     *  (b-vector) in the Poisson regression equation using maximum likelihood.
     *  Do this by minimizing '-2LL'.
     */
    def train_null ()
    {
         val b0   = new VectorD (x.dim2)         // use b0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll_null)    // minimizer for -2LL

         val b_n = bfgs.solve (b0)               // find optimal solution for parameters
         n_dev   = ll_null (b_n)                 // measure of fitness for null nodel
    } // train_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  @param yy   the response vector
     */
    def eval (yy: VectoD = y.toDouble)
    {
        e = yy - x * b                                         // compute residual/error vector e
        diagnose (yy)                                          // compute diagnostics
    } // eval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit including 'rSquared'.  Assumes both train_null and
     *  train have already been called.
     */
    override def fit: VectoD = 
    {
        pseudo_rSq = 1.0 - r_dev / n_dev
        VectorD (n_dev, r_dev, aic, pseudo_rSq)
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.
     */
    override def fitLabels: Seq [String] = Seq ("n_dev", "r_dev", "aic", "pseudo_rSq")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the value of 'y = f(z)' by evaluating the formula 'y = exp (b dot z)'.
     *  @param z  the new vector to predict
     */
    def predict (z: VectoD): Double = (round (exp (b dot z))).toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the value of 'y = f(z)' by evaluating the formula 'y = exp (b dot z)',
     *  for an integer vector.
     *  FIX or remove.
     *  @param z  the new integer vector to predict
     */
//  def predict (z: VectorI): (Int, String) = predict (z.toDouble)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to remove the least predictive variable
     *  from the model, returning the variable to eliminate, the new parameter
     *  vector, the new R-squared value and the new F statistic.
     *  FIX or remove.
     */
//  def backwardElim (cols: Set [Int]): (Int, VectoD, VectoD) =
//  {
//      var j_max   = -1                     // index of variable to eliminate
//      var b_max: VectoD = null             // parameter values for best solution
//      var rSq_max = -1.0                   // currently maximizing R squared
//      var fS_max  = -1.0                   // could optimize on F statistic
//
//      for (j <- 1 to k) {
//          val keep = n.toInt               // i-value large enough to not exclude any rows in slice
//          val rg_j = new PoissonRegression (x.sliceExclude (keep, j), y)       // regress with x_j removed
//          rg_j.train ().eval ()
//          val (b, rSq, fS, rBar) =  rg_j.fit
//          if (rSq > rSq_max) { j_max = j; b_max = b; rSq_max = rSq; fS_max = fS}
//      } // for
//      (j_max, b_max, rSq_max, fS_max)
//  } // backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor 'VIF' for each variable to test
     *  for multi-collinearity by regressing 'xj' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'xj' can be predicted
     *  from the other variables, so 'xj' is a candidate for removal from the model.
     *  FIX or remove.
     */
//  def vif: VectoD =
//  {
//      val vifV = new VectorD (k)           // VIF vector
//      for (j <- 1 to k) {
//          val keep = n.toInt               // i-value large enough to not exclude any rows in slice
//          val x_j  = x.col(j)                                               // x_j is jth column in x
//          val rg_j = new PoissonRegression (x.sliceExclude (keep, j), x_j)    // regress with x_j removed
//          rg_j.train ().eval ()
//          vifV(j-1) =  1.0 / (1.0 - rg_j.fit._2)                            // store vif for x_1 in vifV(0)
//      } // for
//      vifV
//  } // vif

} // PoissonRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonRegression` object tests the `PoissonRegression` class.
 *  @see http://www.cookbook-r.com/Statistical_analysis/Logistic_regression/
 *  Answer: b = (-8.8331, 0.4304),
 *          n_dev = 43.860, r_dev = 25.533, aci = 29.533, pseudo_rSq = 0.4178
 */
object PoissonRegressionTest extends App
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

    var z: VectoD = null

    println ("x = " + x)
    println ("y = " + y)

    val rg = new PoissonRegression (x, y)
    rg.train_null ()                                    // train based on null model
    rg.train ().eval ()                                 // train based on full model
    val b  = rg.coefficient                             // obtain coefficients
    val ft = rg.fit                                     // obtain quality of fit

    println ("---------------------------------------------------------------")
    println ("Poisson Regression Regression Results")
    println ("b          = " + b)
    println ("n_dev      = " + ft(0))
    println ("r_dev      = " + ft(1))
    println ("aic        = " + ft(2))
    println ("pseudo_rSq = " + ft(3))

    z = VectorD (1.0, 15.0)                            // predict point z
    println ("predict (" + z + ") = " + rg.predict (z))

    z = VectorD (1.0, 30.0)                            // predict point z
    println ("predict (" + z + ") = " + rg.predict (z))

} // PoissonRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonRegressionTest2` object tests the `PoissonRegression` class.
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  @see www.stat.wisc.edu/~mchung/teaching/.../GLM.logistic.Rpackage.pdf
 */
object PoissonRegressionTest2 extends App
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

    println ("x = " + x)
    println ("y = " + y)

//  val rg = new PoissonRegression (x(0 until x.dim1, 0 until 2), y, fn)
    val rg = new PoissonRegression (x, y, fn)
    rg.train_null ()                                    // train based on null model
    rg.train ().eval ()                                 // train based on full model

    println ("---------------------------------------------------------------")
    println ("Poisson Regression Regression Results")
    println ("fit = " + rg.fit)

    val z  = VectorD (1.0, 100.0, 100.0, 100.0)        // predict point z
    println ("predict (" + z + ") = " + rg.predict (z))

//  new Plot (x.col(1), y, yyp)
//  new Plot (x.col(2), y, yyp)

} // PoissonRegressionTest2 object

