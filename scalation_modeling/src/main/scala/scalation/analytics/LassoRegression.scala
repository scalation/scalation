
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 1.5
 *  @date    Tue Apr 18 14:24:14 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.math.{abs, log, pow, sqrt}

import scalation.linalgebra._
import scalation.math.double_exp
import scalation.minima._
import scalation.plot.Plot
import scalation.random.CDF.studentTCDF
import scalation.util.Unicode.sub

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in
 *  the regression equation
 *  <p>
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x   the input/design m-by-n matrix
 *  @param y   the response vector
 *  @param λ0  the initial value for the regularization weight
 */
class LassoRegression (x: MatriD, y: VectoD, λ0: Double = 0.01)
      extends PredictorMat (x, y)
{
    private val DEBUG = false                                   // debug flag
    private var λ     = λ0                                      // weight to put on regularization

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the sum of squares error + λ * sum of the magnitude  of coefficients.
     *  This is the objective function to be minimized.
     *  @param yy  the response vector
     *  @param b   the vector of coefficients/parameters
     */
    def f (yy: VectoD)(b: VectoD): Double =
    {
        e = yy - x * b                                          // calculate the residuals/error
        e dot e + λ * b.norm1 
    } // f

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *  <p>
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  <p>
     *  regularized by the sum of magnitudes of the coefficients.
     *  @see pdfs.semanticscholar.org/969f/077a3a56105a926a3b0c67077a57f3da3ddf.pdf
     *  @see `scalation.minima.LassoAdmm`
     *  @param yy  the response vector
     */
    def train (yy: VectoD = y): LassoRegression =
    {
//      val g       = f(yy) _
//      val optimer = new CoordinateDescent (g)                 // Coordinate Descent optimizer
//      val optimer = new GradientDescent (g)                   // Gradient Descent optimizer
//      val optimer = new ConjugateGradient (g)                 // Conjugate Gradient optimizer
//      val optimer = new QuasiNewton (g)                       // Quasi-Newton optimizer
//      val optimer = new NelderMeadSimplex (g, x.dim2)         // Nelder-Mead optimizer
//      val b0 = new VectorD (k+1)                              // initial guess for coefficient vector
//      b = optimer.solve (b0, 0.5)                             // find an optimal solution for coefficients
        b = LassoAdmm.solve (x.asInstanceOf [MatrixD], y, λ)
        this
    } // train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    def crossVal (k: Int = 10, rando: Boolean = true)
    {
        crossValidate ((x: MatriD, y: VectoD) => new LassoRegression (x, y), k, rando)
    } // crossVal

} // LassoRegression class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegressionTest` object tests `LassoRegression` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  <p>
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.analytics.LassoRegressionTest
 */
object LassoRegressionTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

//  println ("model: y = b_0 + b_1*x_1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    val rg = new LassoRegression (x, y)
    rg.train ().eval ()
    println ("b = " + rg.coefficient)
    rg.summary ()

    println ("fit = " + rg.fit)
    val yp = rg.predict (z)                              // predict y for one point
    println ("predict (" + z + ") = " + yp)

    val yyp = rg.predict (x)                             // predict y for several points
    println ("predict (" + x + ") = " + yyp)

    new Plot (x.col(1), y, yyp)
    new Plot (x.col(2), y, yyp)

} // LassoRegressionTest object

