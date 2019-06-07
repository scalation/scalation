
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Jan  4 23:09:27 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.{Map, Set}

import scalation.linalgebra._
import scalation.stat.Statistic
import scalation.util.{banner, Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVA` class supports ANalysis of COVAriance 'ANCOVA'.  It allows
 *  the addition of a categorical treatment variable 't' into a multiple linear
 *  regression.  This is done by introducing dummy variables 'dj' to distinguish
 *  the treatment level.  The problem is again to fit the parameter vector 'b'
 *  in the augmented regression equation
 *  <p>
 *      y  =  b dot x + e  =  b0  +  b_1   * x_1  +  b_2   * x_2  +  ... b_k * x_k
                                  +  b_k+1 * d_1  +  b_k+2 * d_2  +  ... b_k+l * d_l + e
 *  <p>
 *  where 'e' represents the residuals (the part not explained by the model).
 *  Use Least-Squares (minimizing the residuals) to solve for the parameter vector 'b'
 *  using the Normal Equations:
 *  <p>
 *      x.t * x * b  =  x.t * y
 *      b  =  fac.solve (.)
 *  <p>
 *  't' has  categorical values/levels, e.g., treatment levels (0, ... 't.max ()')
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x_         the data/input matrix of continuous variables
 *  @param t          the treatment/categorical variable matrix 
 *  @param y          the response/output vector
 *  @param fname_     the feature/variable names
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ANCOVA (x_ : MatriD, t: MatriI, y: VectoD, fname_ : Strings = null, technique: RegTechnique = QR)
      extends Regression (x_ ++^ ANCOVA.dummyVars (t), y, fname_, null, technique)
{
    if (t.dim1 != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform 'k'-fold cross-validation.
     *  @param xx     the data matrix to use (full data matrix or selected columns)
     *  @param k      the number of folds
     *  @param rando  whether to use randomized cross-validation
     */
    override def crossVal (xx: MatriD = x, k: Int = 10, rando: Boolean = true): Array [Statistic] =
    {
        crossValidate ((x: MatriD, y: VectoD) => new ANCOVA (x_, t, y, fname, technique),
                                                 xx, k, rando)
    } // crossVal

} // ANCOVA class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVA` companion object provides helper functions.
 */
object ANCOVA extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the dummy variables based on the treatment vector 't'.
     *  @param t  the treatment level matrix
     */
    def dummyVars (t: MatriI): MatriD =
    {
        val tmax   = VectorI (for (j <- t.range2) yield t.max ())
        val xd     = new MatrixD (t.dim1, tmax.sum)
        var offset = 0
        for (j <- t.range2) {
            val tj  = t.col (j)
            for (i <- tj.range) {
                val ti = tj(i)                                      // treatment level for ith item
                if (ti < 0) flaw ("dummyVars", s"treatment level $ti may not be negative")
                if (ti > 0) xd(i, offset + ti - 1) = 1.0
            } // for
            offset += tmax(j)
        } // for
        xd
    } // dummyVars

} // ANCOVA object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVATest` object tests the `ANCOVA` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2 + b_3*d_1 + b_4*d_2
 *  <p>
 *  > runMain scalation.analytics.ANCOVATest
 */
object ANCOVATest extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((6, 3), 1.0, 36.0,  66.0,                 // 6-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0, 42.0,  83.0,
                                 1.0,  1.0, 101.0)
    val t = new MatrixI ((6, 1), 0, 0, 1, 1, 2, 2)                 // treatments levels
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 643.0, 1598.0)    // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0, 0.0)

    println (s"x = $x")
    println (s"t = $t")
    println (s"y = $y")

    val xt = x ++^ t.toDouble

    banner ("Regression")
    val rg = new Regression (xt, y)
    rg.train ().eval ()
    println (s"xt = $xt")
    println (rg.report)
    println (rg.summary)

    banner ("ANCOVA")
    val anc = new ANCOVA (x, t, y)
    anc.train ().eval ()
    println (s"full x = ${anc.getX}")
    println (anc.report)
    println (anc.summary)

    val yp = anc.predict (z)
    println ("predict (" + z + ") = " + yp)

} // ANCOVATest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVATest2` object tests the `ANCOVA` object related to related to
 *  encoding a column 'x1' of strings.
 *  > runMain scalation.analytics.ANCOVATest2
 */
object ANCOVATest2 extends App
{
    val x1 = VectorS ("English", "French", "German", "Spanish")
    val (xe, map) = Converter.map2Int (x1)                        // map strings to integers
    val xm = MatrixI (Seq (xe))                                   // form a matrix from vector
    val xd = ANCOVA.dummyVars (xm)                                // make dummy variable columns

    println (s"encoded        xe = $xe")                          // encoded
    println (s"matrix encoded xm = $xm")                          // matrix encoded column
    println (s"matrix dummy   xd = $xd")                          // matrix dummy columns

} // ANCOVATest2 object

