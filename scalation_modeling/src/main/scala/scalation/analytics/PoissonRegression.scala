
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Jan 11 19:05:20 EST 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  ncss-wpengine.netdna-ssl.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Poisson_Regression.pdf
 */

// U N D E R   D E V E L O P M E N T 

// FIX: needs improved optimization

package scalation.analytics

import scala.collection.mutable.Set
import scala.math.{exp, log, round}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.math.Combinatorics.fac
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.stat.Statistic

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
 *  @param x       the data/input matrix augmented with a first column of ones
 *  @param y       the integer response/output vector, y_i in {0, 1, ... }
 *  @param fname_  the names of the features/variables
 */
class PoissonRegression (x: MatriD, y: VectoD, fname_ : Strings = null)
      extends PredictorMat (x, y, fname_)
{
    private val DEBUG      = false                    // debug flag
/*
    private val k          = x.dim2 - 1               // number of variables 
    private val n          = x.dim1.toDouble          // number of data points (rows)
    private val r_df       = (n-1.0) / (n-k-1.0)      // ratio of degrees of freedom
*/
    private var aic        = -1.0                     // Akaike’s Information Criterion
    private var n_dev      = -1.0                     // null dev: -LL, for null model (intercept only)
    private var r_dev      = -1.0                     // residual dev: -LL, for full model
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
        for (i <- x.range1) {
            val bx = b dot x(i)
            sum += y(i) * bx - exp (bx)               // last term not needed [ - log (fac (y(i))) ]
        } // for
        -sum                                          // set up for minimization
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
        for (i <- x.range1) {
            val bx = b(0)                             // only use the intercept
            sum += y(i) * bx - exp (bx)               // last term not needed [ - log (fac (y(i))) ]
        } // for
        - sum                                         // set up for minimization
    } // ll_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the full model, train the predictor by fitting the parameter vector
     *  (b-vector) in the Poisson regression equation using maximum likelihood.
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y.toDouble): PoissonRegression =
    {
         // FIX - yy currently not used
         train_null ()
         val b0   = new VectorD (x.dim2)              // use b_0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll)              // minimizer for -2LL

         b     = bfgs.solve (b0)                      // find optimal solution for parameters
         r_dev = ll (b)                               // measure of fitness for full model
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
         val b0   = new VectorD (x.dim2)              // use b0 = 0 for starting guess for parameters
         val bfgs = new QuasiNewton (ll_null)         // minimizer for -2LL

         val b_n = bfgs.solve (b0)                    // find optimal solution for parameters
         n_dev   = ll_null (b_n)                      // measure of fitness for null nodel
    } // train_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error and useful diagnostics.
     *  FIX - not x * b
    override def eval (xx: MatriD, yy: VectoD)
    {
        e = yy - xx * b                               // compute residual/error vector e
        diagnose (e, yy)                              // compute diagnostics
    } // eval
     */

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
    override def fitLabel: Seq [String] = Seq ("n_dev", "r_dev", "aic", "pseudo_rSq")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the value of 'y = f(z)' by evaluating the formula 'y = exp (b dot z)'.
     *  @param z  the new vector to predict
     */
    override def predict (z: VectoD): Double = round (exp (b dot z))

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
            val x_cols = x.selectCols (cols_j.toArray)              // project x onto cols
            val rg_j   = new PoissonRegression (x_cols, y)          // regress with x_j added
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
            val x_cols = x.selectCols (cols_j.toArray)              // project x onto cols
            val rg_j   = new PoissonRegression (x_cols, y)          // regress with x_j removed
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
     *  for multi-collinearity by regressing 'x_j' against the rest of the variables.
     *  A VIF over 10 indicates that over 90% of the variance of 'x_j' can be predicted
     *  from the other variables, so 'x_j' is a candidate for removal from the model.
     *  FIX or remove.
     */
    def vif: VectoD =
    {
        val ir   = index_rSq                                         // fit(ir) is rSq
        val vifV = new VectorD (k)                                   // VIF vector

        for (j <- 1 until x.dim2) {
            val x_j   = x.col(j)                                     // x_j is jth column in x
            val x_noj = x.sliceEx (m, j)                             // x matrix without column j
            val rg_j  = new PoissonRegression (x_noj, x_j)           // regress with x_j removed
            rg_j.train ().eval ()                                    // train model, evaluate QoF
            val rSq_j = rg_j.fit(ir)
            if (DEBUG) println (s"vif: for variable x_$j, rSq_$j = $rSq_j")
            vifV(j-1) =  1.0 / (1.0 - rSq_j)                         // store vif for x_1 in vifV(0)
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
        crossValidate ((x: MatriD, y: VectoD) => new PoissonRegression (x, y, fname),
                                                 xx, k, rando)
    } // crossVal

} // PoissonRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonRegression` companion object provides factory functions.
 */
object PoissonRegression
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `PoissonRegression` object given an integer response vector.
     *  @param x      the data matrix
     *  @param y      the response vector as an integer vector
     *  @param fname  the feature/variable names
     */
    def apply (x: MatriD, y: VectoI, fname: Strings = null): PoissonRegression =
    {
        new PoissonRegression (x, y.toDouble, fname)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `PoissonRegression` object from a combined data matrix.
     *  @param xy     the combined data-response matrix
     *  @param fname  the feature/variable names
     */
    def apply (xy: MatriD, fname: Strings): PoissonRegression =
    {
        val (x, y) = PredictorMat.pullResponse (xy)
        new PoissonRegression (x, y, fname)
    } // apply

} // PoissonRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonRegressionTest` object tests the `PoissonRegression` class.
 *  @see http://www.cookbook-r.com/Statistical_analysis/Logistic_regression/
 *  Answer: b = (-8.8331, 0.4304),
 *          n_dev = 43.860, r_dev = 25.533, aci = 29.533, pseudo_rSq = 0.4178
 *  > runMain scalation.analytics.PoissonRegressionTest
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

    val rg = PoissonRegression (x, y)
    rg.train_null ()                                    // train based on null model
    rg.train ().eval ()                                 // train based on full model

    println ("parameter = " + rg.parameter)             // obtain coefficients
    println ("fitMap    = " + rg.fitMap)                // obtain quality of fit

    val yp = rg.predict (x)
    println ("y  = " + y)
    println ("yp = " + yp.toInt)

    z = VectorD (1.0, 15.0)                            // predict point z
    println ("predict (" + z + ") = " + rg.predict (z))

    z = VectorD (1.0, 30.0)                            // predict point z
    println ("predict (" + z + ") = " + rg.predict (z))

} // PoissonRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonRegressionTest2` object tests the `PoissonRegression` class.
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  @see www.stat.wisc.edu/~mchung/teaching/.../GLM.logistic.Rpackage.pdf
 *  > runMain scalation.analytics.PoissonRegressionTest2
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

//  val rg = PoissonRegression (x(0 until x.dim1, 0 until 2), y, fn)
    val rg = PoissonRegression (x, y, fn)
    rg.train_null ()                                    // train based on null model
    rg.train ().eval ()                                 // train based on full model

    println ("parameter = " + rg.parameter)             // obtain coefficients
    println ("fitMap    = " + rg.fitMap)                // obtain quality of fit


    val z = VectorD (1.0, 100.0, 100.0, 100.0)          // predict point z
    println ("predict (" + z + ") = " + rg.predict (z))

//  new Plot (x.col(1), y, yyp)
//  new Plot (x.col(2), y, yyp)

} // PoissonRegressionTest2 object

