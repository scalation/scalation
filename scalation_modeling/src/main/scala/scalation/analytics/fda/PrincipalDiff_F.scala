
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Wed Nov 16 16:33:38 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see Functional Data Analysis, Second Edition (Ramsay, Silverman), section 19.5.3
 */

package scalation.analytics.fda

import scala.math.{cos, Pi, sin}

import scalation.linalgebra.VectorD
import scalation.plot.Plot
import scalation.random.Normal

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrincipalDiff` class uses Principal Differential Analysis (PDA) to fit
 *  functional data from a process governed by an Ordinary Differential Equation (ODE).
 *  FIX - TBD
 *  @param y    the (raw) data points
 *  @param t    the time points
 *  @param ldo  the linear differential operator
 */
class PrincipalDiff (y: VectorD, t: VectorD, ldo: Any)
{
} // PrincipalDiff


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PrincipalDiffTest` object is used to test the `PrincipalDiff` class.
 *  > run-main scalation.analytics.fda.PrincipalDiffTest
 *  FIX - smoothing not working correctly
 */
object PrincipalDiffTest extends App
{
    val nrn1 = Normal ()
    val nrn4 = Normal (0, 4)

    val c = (nrn1.gen, nrn4.gen, nrn1.gen, nrn1.gen)

    def x (t: Double): Double =
    {
        c._1 + c._2 * t + c._3 * sin (6*Pi*t) + c._4 * cos (6*Pi*t) + nrn1.gen
    } // x
 
    val t = VectorD.range (0, 101) / 100.0

    val y = t.map (x(_))

    val moo = new Smoothing_F (t, y)
    moo.train ()
    val xs = moo.predict (t)

    new Plot (t, y, xs)

} // PrincipalDiffTest

