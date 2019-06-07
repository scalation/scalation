
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Dec 28 21:52:38 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scala.math.{exp, log}

import scalation.analytics.ActivationFun.{sigmoid, sigmoidV}
import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.math.ExtremeD.MAX_VALUE
import scalation.minima.QuasiNewton
import scalation.plot.Plot
import scalation.util.banner

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegression` class supports (binomial) logistic regression.
 *  In this case, 'x' is two-dimensional '[1, x_1]'.  Fit the parameter vector
 *  'b' in the logistic regression equation
 *  <p>
 *      logit (p_y)  =  b dot x + e  =  b_0 + b_1 * x_1 + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model)
 *  and 'y' is now binary.
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x    the input/design matrix augmented with a first column of ones
 *  @param y    the binary response vector, y_i in {0, 1}
 *  @param fn_  the names for all features/variables
 *  @param cn_  the names for both classes
 */
class SimpleLogisticRegression (x: MatriD, y: VectoI, fn_ : Strings = Array ("one", "x1"),
                                cn_ : Strings = null)
      extends ClassifierReal (x, y, fn_, 2, cn_)
{
    if (y != null && x.dim1 != y.dim) flaw ("constructor", "dimensions of x and y are incompatible")

    private val DEBUG      = false                    // debug flag
    private val k          = x.dim2 - 1               // number of variables 
    private val r_df       = (n-1.0) / (n-k-1.0)      // ratio of degrees of freedom
    private var b: VectoD  = null                     // parameter vector (b_0, b_1, ... b_k)
    private var n_dev      = -1.0                     // null dev: -2l, for null model (intercept only)
    private var r_dev      = -1.0                     // residual dev: -2l, for full model
    private var aic        = -1.0                     // Akaike’s Information Criterion
    private var pseudo_rSq = -1.0                     // McFaffen's pseudo R-squared

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector 'b', compute '-2 * Log-Likelihood (-2l)'.
     *  '-2l' is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @see www.statisticalhorizons.com/wp-content/uploads/Allison.StatComp.pdf
     *  @param b  the parameters to fit
     */
    def ll (b: VectoD): Double =
    {
        var sum = 0.0
        var bx  = 0.0                                       // beta
        for (i <- y.range) {
            bx = b(0) + b(1) * x(i, 1)
//          sum += y(i) * bx - log (1.0 + exp (bx))
            sum += y(i) * bx - bx - log (exp (-bx) + 1.0)   // less prone to overflow (infinity)
        } // for
        -2.0 * sum                                          // set up for minimization
    } // ll
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For a given parameter vector 'b = [b(0)]', compute '-2 * Log-Likelihood (-2l)'.
     *  '-2l' is the standard measure that follows a Chi-Square distribution. 
     *  @see www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
     *  @see www.statisticalhorizons.com/wp-content/uploads/Allison.StatComp.pdf
     *  @param b  the parameters to fit
     */
    def ll_null (b: VectoD): Double =
    {
        var sum = 0.0
        val bx = b(0)                                       // only use the intercept
        for (i <- y.range) {
//          sum += y(i) * bx - log (1.0 + exp (bx))
            sum += y(i) * bx - bx - log (exp (-bx) + 1.0)   // less prone to overflow (infinity)
        } // for
        -2.0 * sum                                          // set up for minimization
    } // ll_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the full model, train the classifier by fitting the parameter vector
     *  (b-vector) in the logistic regression equation using maximum likelihood.
     *  Do this by minimizing '-2l'.
     *  FIX: Use improved BFGS implementation or IRWLS
     *  @see stats.stackexchange.com/questions/81000/calculate-coefficients-in-a-logistic-regression-with-r
     *  @see en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
     *  @param itest  the indices of test data
     */
    def train (itest: Ints): SimpleLogisticRegression =     // FIX - use these parameters
    {
        train_null ()
        val b0   = new VectorD (x.dim2)                     // use b_0 = 0 for starting guess for parameters
        val bfgs = new QuasiNewton (ll)                     // minimizer for -2l
        b     = bfgs.solve (b0)                             // find optimal solution for parameters

        r_dev = ll (b)                                      // measure of fitness for full model
        aic   = r_dev + 2.0 * x.dim2
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the null model, train the classifier by fitting the parameter vector
     *  (b-vector) in the logistic regression equation using maximum likelihood.
     *  Do this by minimizing -2l.
     */
    def train_null ()
    {
        val b0   = new VectorD (x.dim2)                     // use b0 = 0 for starting guess for parameters
        val bfgs = new QuasiNewton (ll_null)                // minimizer for -2l
        val b_n = bfgs.solve (b0)                           // find optimal solution for parameters

        n_dev   = ll_null (b_n)                             // measure of fitness for null model
    } // train_null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of coefficient/parameter values.
     */
    def parameter: VectoD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the quality of fit.  Assumes both 'train_null' and 'train' have
     *  already been called.
     *  @param y   the actual class labels
     *  @param yp  the predicted class labels
     *  @param k   the number of class labels
     */
    override def fit (y: VectoI, yp: VectoI, k: Int = 2): VectoD = 
    {
        pseudo_rSq = 1.0 - r_dev / n_dev
        super.fit (y, yp) ++ VectorD (n_dev, r_dev, aic, pseudo_rSq)
    } // fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the fit.  Override when necessary.
     */
    override def fitLabel: Seq [String] = super.fitLabel ++ Seq ("n_dev", "r_dev", "aic", "pseudo_rSq")

    val index_prSq = 7                                        // index of pseudo_rSq

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Classify the value of 'y = f(z)' by evaluating the formula 'y = sigmoid (b dot z)'.
     *  Return the best class, its name and quality metric
     *  @param z  the new vector to classify
     */
    override def classify (z: VectoD): (Int, String, Double) =
    {
        val c = if (sigmoid (b dot z) > 0.5) 1 else 0
        (c, cn(c), -1.0)                                      // FIX - need metric
    } // classify

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset or re-initialize the frequency tables and the probability tables.
     */
    def reset () { /* Not Applicable */ }

} // SimpleLogisticRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegression` companion object provides factory methods.
 */
object SimpleLogisticRegression
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SimpleLogisticRegression` object. 
     *  @param x1  the vector of values for the predictor
     *  @param y   the binary response vector, y_i in {0, 1}
     *  @param fn  the names for all factors
     *  @param cn  the names for both classes
     */
    def apply (x1: VectorD, y: VectorI, fn: Strings = Array ("one", "x1"),
               cn: Strings = null): SimpleLogisticRegression =
    {
        new SimpleLogisticRegression (MatrixD.++^ (VectorD.one (x1.dim), x1), y, fn, cn)
    } // apply

} // SimpleLogisticRegression object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegressionTest` object tests the `SimpleLogisticRegression`
 *  class on the mtcars dataset.  Use built-in optimizer.
 *  @see ExampleMtcars.scala
 *  @see www.cookbook-r.com/Statistical_analysis/Logistic_regression/
 *  Answer: b = (-8.8331, 0.4304),
 *          n_dev = 43.860, r_dev = 25.533, aic = 29.533, pseudo_rSq = 0.4178
 *  > runMain scalation.analytics.classifier.SimpleLogisticRegressionTest
 */
object SimpleLogisticRegressionTest extends App
{
    val x  = ExampleMtcars.xy.sliceCol (0, 2)
    val x1 = ExampleMtcars.xy.col (1)
    val y  = ExampleMtcars.xy.col (2).toInt

    println ("x = " + x)
    println ("y = " + y)

    val fn = Array ("One", "Mpg")                         // feature names

    val lrg = new SimpleLogisticRegression (x, y, fn)     // Simple Logistic Regression classifier
//  lrg.train_null ()                                     // train based on null model
    lrg.train ()                                          // train based on full model

    banner ("Simple Logistic Regression Results")
    println ("b = " + lrg.parameter)                      // estimated parameter values

    val yp = lrg.classify (x)                             // classify all instances: yp
    println ("y  = " + y)
    println ("yp = " + yp)
    println (lrg.fitMap (y, yp))                          // print quality of fit

    banner ("classify new instances")
    var z = VectorD (1.0, 15.0)                           // classify point z = [1, 15]
    println ("classify (" + z + ") = " + lrg.classify (z))
    z = VectorD (1.0, 30.0)                               // classify point z = [1, 30]
    println ("classify (" + z + ") = " + lrg.classify (z))

    banner ("perform cross-validation")
    println ("acc = " + lrg.crossValidate (10, true))

    new Plot (x1, y.toDouble, null, "y vs. x1")
    new Plot (x1, yp.toDouble, null, "yp vs. x1")
    new Plot (x1, y.toDouble, yp.toDouble, "y and yp (red) vs. x1")

} // SimpleLogisticRegressionTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegressionTest2` object tests the `SimpleLogisticRegression`
 *  class on the mtcars dataset.  Use grid search optimizer.
 *  To get a better solution, refine the grid.
 *  @see ExampleMtcars.scala
 *  @see www.cookbook-r.com/Statistical_analysis/Logistic_regression/
 *  Answer: b = (-8.8331, 0.4304),
 *          n_dev = 43.860, r_dev = 25.533, aic = 29.533, pseudo_rSq = 0.4178
 *  > runMain scalation.analytics.classifier.SimpleLogisticRegressionTest2
 */
object SimpleLogisticRegressionTest2 extends App
{
    val x  = ExampleMtcars.xy.sliceCol (0, 2)
    val x1 = ExampleMtcars.xy.col (1)
    val y  = ExampleMtcars.xy.col (2).toInt

    println ("x = " + x)
    println ("y = " + y)

    val fn = Array ("One", "Mpg")                         // feature names

    val lrg = new SimpleLogisticRegression (x, y, fn)     // Simple Logistic Regression classifier

    banner ("Grid Search: ll(b0, b1) = -2 log-likelihood")
    var ll_min = MAX_VALUE
    var b_min: VectorD = null

    for (i <- -90 to -80; j <- 0 to 10) {
        val b = VectorD (i * 0.1, j * 0.1)
        val ll_b = lrg.ll (b)
        println (s"ll ($b) = $ll_b")
        if (ll_b < ll_min) { b_min = b; ll_min = ll_b }
    } // for
    println (s"ll_min ($b_min) = ${lrg.ll (b_min)}")

} // SimpleLogisticRegressionTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegressionTest3` object tests the `SimpleLogisticRegression`
 *  class.  Compare `SimpleLogisticRegressionTest` with `SimpleRegression`.
 *  @see www.cookbook-r.com/Statistical_analysis/Logistic_regression/
 *  Answer: b = (-8.8331, 0.4304),
 *          n_dev = 43.860, r_dev = 25.533, aic = 29.533, pseudo_rSq = 0.4178
 *  > runMain scalation.analytics.classifier.SimpleLogisticRegressionTest3
 */
object SimpleLogisticRegressionTest3 extends App
{
    import scalation.analytics.SimpleRegression

    // Mpg
    val x1 = VectorD (21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19.2, 17.8, 16.4, 17.3, 15.2, 10.4, 10.4,
                      14.7, 32.4, 30.4, 33.9, 21.5, 15.5, 15.2, 13.3, 19.2, 27.3, 26.0, 30.4, 15.8, 19.7, 15.0, 21.4)

    // V/S (e.g., V-6 vs. I-4)
    val y = VectorI (0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0,
                     0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1)

    val xx = new MatrixD (x1.dim, 2)
    for (i <- x1.range) xx(i) = VectorD (1.0, x1(i))
    println ("xx = " + xx)

    val lrg = SimpleLogisticRegression (x1, y)            // Simple Logistic Regression classifier
//  lrg.train_null ()                                     // train based on null model
    lrg.train ()                                          // train based on full model

    banner ("Simple Logistic Regression Results")
    println ("b = " + lrg.parameter)                      // lrg estimated parameter values

    val yp = lrg.classify (xx)
    println ("y  = " + y)
    println ("yp = " + yp)
    println (lrg.fitMap (y, yp))

    banner ("Simple Regression Results")
    val srg = SimpleRegression (x1, y.toDouble)
    srg.train ().eval ()
    println ("b = " + srg.parameter)                      // srg estimated parameter values
    val yyp = srg.predict (xx)
    println ("yyp = " + yyp)
    println (srg.fitMap)

    new Plot (x1, y.toDouble, null, "y vs. x1")
    new Plot (x1, yp.toDouble, null, "yp vs. x1")
    new Plot (x1, y.toDouble, yp.toDouble, "y and yp (red) vs. x1")
    new Plot (x1, yyp.toDouble, null, "yyp vs. x1")
    new Plot (x1, y.toDouble, yyp.toDouble, "y and yyp (red) vs. x1")

} // SimpleLogisticRegressionTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegressionTest4` is tests the `SimpleLogisticRegression`
 *  class.
 *  @see people.revoledu.com/kardi/tutorial/LDA/Numerical%20Example.html
 *  > runMain scalation.analytics.classifier.SimpleLogisticRegressionTest4
 */
object SimpleLogisticRegressionTest4 extends App
{
    // features/variable:
    //                     x - curvature
    val x = VectorD (2.95, 2.53, 3.57, 3.16, 2.58, 2.16, 3.27)
    val y = VectorI (   0,    0,    0,    0,    1,    1,    1)
    val xm = new MatrixD (x.dim, 2)
    xm.setCol(0, VectorD.one (x.dim))
    xm.setCol(1, x)

    val k  = 2                                                 // number of classes
    val fn = Array ("one", "curvature")                        // feature names
    val cn = Array ("pass", "fail")                            // class names

    val lrg = new SimpleLogisticRegression (xm, y, fn, cn)     // SimpleLogisticRegression classifier
    lrg.train ()
    println ("b = " + lrg.parameter)

    banner ("classify")
    val z  = VectorD (1.0, 2.81)
    println (s"classify ($z) = ${lrg.classify (z)}")

    banner ("test")
    val yp = lrg.classify (xm)
    println (lrg.fitMap (y, yp))

    val t = VectorD.range (0, x.dim)
    new Plot (t, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. t")
    new Plot (x, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. x")

    banner ("log-likelihood")
    val b1 = VectorD (1, -1)
    val b2 = VectorD (5, -2)
    println (s"ll ($b1) = ${lrg.ll (b1)}") 
    println (s"ll ($b2) = ${lrg.ll (b2)}") 

} // SimpleLogisticRegressionTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegressionTest5` is used to test 
 *  > runMain scalation.analytics.classifier.SimpleLogisticRegressionTest5
 */
object SimpleLogisticRegressionTest5 extends App
{

    // features/variable:
    //                     x - curvature
    val x = VectorD (1, 2, 3, 4, 5, 6)
    val y = VectorI (0, 0, 1, 0, 1, 1)
    val xm = new MatrixD (x.dim, 2)
    xm.setCol(0, VectorD.one (x.dim))
    xm.setCol(1, x)

    val k  = 2                                                 // number of classes
    val fn = Array ("one", "x1")                               // feature names
    val cn = Array ("pass", "fail")                            // class names

    val lrg = new SimpleLogisticRegression (xm, y, fn, cn)     // SimpleLogisticRegression classifier
    lrg.train ()
    println ("b = " + lrg.parameter)

    banner ("classify")
    val z  = VectorD (1.0, 2.81)
    println (s"classify ($z) = ${lrg.classify (z)}")

    banner ("test")
    val yp = lrg.classify (xm)
    println ("fitMap  = " + lrg.fitMap (y, yp))
    println ("confuse = " + (new ConfusionMat (y, yp)).confusion)

    val t = VectorD.range (0, x.dim)
    new Plot (t, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. t")
    new Plot (x, y.toDouble, yp.toDouble, "y(black)/yp(red) vs. x")

    banner ("log-likelihood")
    val b1 = VectorD (1, -1)
    val b2 = VectorD (-4, 1)
    val b3 = VectorD(-4.24934,	1.21409)
    println (s"ll ($b1) = ${lrg.ll (b1)}")
    println (s"ll ($b2) = ${lrg.ll (b2)}")
    println (s"ll ($b3) = ${lrg.ll (b3)}")

} // SimpleLogisticRegressionTest5

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLogisticRegressionTest6` is used to test the logistic function.
 *  > runMain scalation.analytics.classifier.SimpleLogisticRegressionTest6
 */
object SimpleLogisticRegressionTest6 extends App
{
    import scalation.analytics.ActivationFun.sigmoidV

    val z  = VectorD.range (0, 160) / 10.0 - 8.0
    val fz = sigmoidV (z)
    new Plot (z, fz)

} // SimpleLogisticRegressionTest6 object

