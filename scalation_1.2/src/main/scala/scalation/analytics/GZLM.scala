
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell
 *  @version 1.2
 *  @date    Sun Jan 18 20:46:18 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.analytics.classifier.LogisticRegression
import scalation.linalgebra.{MatrixD, VectorD, VectorI}
import scalation.linalgebra.VectorD.one

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** A Generalized Linear Model 'GZLM' can be developed using the `GZLM` object.
 *  It provides factory methods for General Linear Models 'GLM' via inheritance
 *  and for proper Generalized Linear Models:
 *  `LogisticRegression` - logistic regression,
 *  `PoissonRegression`  - Poisson regression,
 *  `ExpRegression`      - Exponential regression,
 */
object GZLM extends GLM
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Logistic Regression model.
     *  @param x   the input/design m-by-n matrix
     *  @param y   the categorical response vector, y_i in {0, 1}
     *  @param cn  the names for both categories/classes
     */
    def apply (x: MatrixD, y: VectorI, cn: Array [String]): LogisticRegression =
    {
        if (add_1)
            new LogisticRegression (one (x.dim1) +^: x, y, cn)
        else
            new LogisticRegression (x, y, cn)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a Poisson Regression model.
     *  @param x        the input/design m-by-n matrix
     *  @param y        the integer response vector, y_i in {0, 1, ... }
     *  @param fn       the names for all factors
     *  @param poisson  whether it is `PoissonRegression`
     */
    def apply (x: MatrixD, y: VectorI, fn: Array [String], poisson: Boolean): PoissonRegression =
    {
        if (add_1)
            new PoissonRegression (one (x.dim1) +^: x, y, fn)
        else
            new PoissonRegression (x, y, fn)
    } // apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an Exponential Regression model.
     *  @param x       the input/design m-by-n matrix
     *  @param nonneg  whether to check that responses are nonnegative
     *  @param y       the response vector
     */
    def apply (x: MatrixD, nonneg: Boolean, y: VectorD): ExpRegression =
    {
        if (add_1)
            new ExpRegression (one (x.dim1) +^: x, nonneg, y)
        else
            new ExpRegression (x, nonneg, y)
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
    glm.train ()
    println ("fit = " + glm.fit)

    val yp = glm.predict (z)
    println ("predict (" + z + ") = " + yp)
*/

} // GZLMTest object

