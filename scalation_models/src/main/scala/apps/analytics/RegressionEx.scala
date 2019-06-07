
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Sep 24 16:00:20 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package apps.analytics
 
import scalation.analytics.{Regression, SimpleRegression}
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra.MatrixD.apply
import scalation.math.double_exp
import scalation.util.banner


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionEx` object provides simple test cases for `SimpleRegression`
 *  and `Regression`.
 *  > runMain apps.analytics.RegressionEx
 */
object RegressionEx extends App
{
    val x = VectorD (1, 1, 2, 2, 3, 3, 4, 4)
    val y = VectorD (2, 3, 4, 5, 6, 7, 8, 9)

    banner ("Simple Regression")
    val srg = SimpleRegression (x, y)
    srg.train ().eval ()
    println ("parameter = " + srg.parameter)
    println ("fitMap    = " + srg.fitMap)

    val y2 = VectorD (3, 4, 8, 9, 15, 16, 24, 25)

    val xy = MatrixD (Seq (VectorD.one (x.dim), x, x~^2, y2))

    banner ("Regression")
    val rg = Regression (xy)
    rg.train ().eval ()
    println ("parameter = " + rg.parameter)
    println ("fitMap    = " + rg.fitMap)

} // RegressionEx object

