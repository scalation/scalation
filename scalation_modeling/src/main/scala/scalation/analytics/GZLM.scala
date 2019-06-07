
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.6
 *  @date    Sun Jan 18 20:46:18 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.linalgebra.VectorD.one

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A Generalized Linear Model 'GZLM' can be developed using the `GZLM` object.
 *  It provides factory methods for General Linear Models 'GLM' via inheritance
 *  and for proper Generalized Linear Models:
 *  `LogisticRegression` - logistic regression, (@see `classifier` package)
 *  `PoissonRegression`  - Poisson regression,
 *  `ExpRegression`      - Exponential regression,
 */
object GZLM extends GLM
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Poisson Regression model.
     *  @param x        the data/input m-by-n matrix
     *  @param y        the integer response/output vector, y_i in {0, 1, ... }
     *  @param fname    the feature/variable name
     *  @param poisson  whether it is `PoissonRegression`
     */
    def apply (x: MatriD, y: VectoI, fname: Strings, poisson: Boolean): PoissonRegression =
    {
        if (add_1)
            PoissonRegression (one (x.dim1) +^: x, y, fname)
        else
            PoissonRegression (x, y, fname)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an Exponential Regression model.
     *  @param x       the data/input m-by-n matrix
     *  @param y       the response/output vector
     *  @param fname   the feature/variable name
     *  @param nonneg  whether to check that responses are nonnegative
     */
    def apply (x: MatriD, y: VectoD, fname: Strings, nonneg: Boolean): ExpRegression =
    {
        if (add_1)
            new ExpRegression (one (x.dim1) +^: x, y, fname, null, nonneg)
        else
            new ExpRegression (x, y, fname, null, nonneg)
    } // apply

} // GZLM object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GZLMTest` object tests the `GZLM` object using the following regression
 *  equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2 + b_3*d_1 + b_4*d_2
 *  <p>
 */
object GZLMTest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0,  1.0, 101.0)
    val t = VectorI (1, 1, 2, 2, 3)                              // treatments levels
    val y = VectorI (1, 1, 1, 0, 0)                              // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0)

    println ("x = " + x)
    println ("t = " + t)
    println ("y = " + y)

    val levels = 3
/*
    val glm    = GZLM ( /* TBD */ )
    glm.train ().eval ()
    println ("fit = " + glm.fit)

    val yp = glm.predict (z)
    println ("predict (" + z + ") = " + yp)
*/

} // GZLMTest object

