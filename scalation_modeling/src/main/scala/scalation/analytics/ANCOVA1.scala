
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sun Jan  4 23:09:27 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.{Map, Set}

import scalation.linalgebra.{MatriD, MatrixD, VectoD, VectorD, VectoI, VectorI}
import scalation.util.{Error, time}

import RegTechnique._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVA1` class supports ANalysis of COVAriance 'ANCOVA1'.  It allows
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
 *  @param t          the treatment/categorical variable vector
 *  @param y          the response/output vector
 *  @param fname_     the feature/variable names
 *  @param technique  the technique used to solve for b in x.t*x*b = x.t*y
 */
class ANCOVA1 (x_ : MatriD, t: VectoI, y: VectoD, fname_ : Strings = null, technique: RegTechnique = QR)
      extends Regression (x_ ++^ ANCOVA1.dummyVars (t), y, fname_, null, technique)
{
    if (t.dim != y.dim) flaw ("constructor", "dimensions of t and y are incompatible")

} // ANCOVA1 class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVA1` companion object provides helper functions.
 */
object ANCOVA1 extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Assign values for the dummy variables based on the treatment vector 't'.
     *  @param t  the treatment level vector
     */
    def dummyVars (t: VectoI): MatriD =
    {
        val tmax = t.max ()
        val xd = new MatrixD (t.dim, tmax)
        for (i <- t.range) {
            val ti = t(i)                                      // treatment level for ith item
            if (ti < 0) flaw ("dummyVars", s"treatment level $ti may not be negative")
            if (ti < tmax) xd(i, ti) = 1.0
        } // for
        xd
    } // dummyVars

} // ANCOVA1 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ANCOVA1Test` object tests the `ANCOVA1` class using the following
 *  regression equation.
 *  <p>
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2 + b_3*d_1 + b_4*d_2
 *  <p>
 *  > runMain scalation.analytics.ANCOVA1Test
 */
object ANCOVA1Test extends App
{
    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = new MatrixD ((6, 3), 1.0, 36.0,  66.0,                 // 6-by-3 matrix
                                 1.0, 37.0,  68.0,
                                 1.0, 47.0,  64.0,
                                 1.0, 32.0,  53.0,
                                 1.0, 42.0,  83.0,
                                 1.0,  1.0, 101.0)
    val t = VectorI (0, 0, 1, 1, 2, 2)                             // treatments levels
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 643.0, 1598.0)    // response vector
    val z = VectorD (1.0, 20.0, 80.0, 1.0, 0.0)

    println ("x = " + x)
    println ("t = " + t)
    println ("y = " + y)

    val anc = new ANCOVA1 (x, t, y)
    anc.train ().eval ()

    println ("parameter = " + anc.parameter)
    println ("fitMap    = " + anc.fitMap)

    val yp = anc.predict (z)
    println ("predict (" + z + ") = " + yp)

} // ANCOVA1Test object

