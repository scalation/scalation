
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Nov 29 16:05:58 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package apps.analytics

//  U N D E R   D E V E L O P M E N T 

import scala.math.{cos, sin}

import scalation.calculus.Differential.ⅮⅮ
import scalation.dynamics.DormandPrince.integrate
import scalation.linalgebra.VectorD
import scalation.plot.Plot

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Pendulum` object is a simple application that fits data governed by
 *  a differential equation (pendulum equations) using Principal Differential
 *  Analysis.
 *  FIX - make a Scala DSL for readble system of Ordinary Differential Equations (ODEs)
 *  FIX - extend to Differential Algebraic Equations (DAEs)
 *  @see www.math.ucdavis.edu/~tracy/courses/math22B/22BBook.pdf
 *  > run-main apps.analytics.Pendulum
 */
object Pendulum extends App
{
    // FIX - should be second order
    // FIX - need a Derivative with one parameter when time t is not needed
    def derv (g: Double, l: Double)(t: Double, θ: Double): Double = - (g/l) * sin (θ)

    val (gg, ll) = (9.8, 10.0)
    val derv2 = derv (ll, gg) _

    val t = VectorD.range (0, 101) / 100.0
    val θ = t.map (integrate (derv2, 0.0, _))
    val x = θ.map (ll * sin (_))
    val y = θ.map (ll - ll *  cos (_))

    new Plot (t, x, y)
    new Plot (x, y)

    // make the data noisy and use Principal Differential Analysis

} // Pendulum object

