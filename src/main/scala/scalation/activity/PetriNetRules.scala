
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Oct 12 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.activity

import scalation.dynamics.Derivatives.Derivative
import scalation.dynamics.DormandPrince.integrateV
//import scalation.dynamics.RungeKutta.integrateV
import scalation.linalgebra.{VectorD, VectorI}
import scalation.random.{Sharp, Variate}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PetriNetRules` class is used to define firing rules for the `PetriNet` class.
 *  It supports both constant flow and linear flow models of token (integer valued)
 *  and fluid (real valued) flow.  Typically, in the constant flow model, a base
 *  flow vector is used  for the threshold (require at least this number of tokens/amount
 *  of fluid) and the flow (move this number this number of tokens/amount of fluid
 *  over the arc).  It is also possible to set the flow  below the threshold.
 *  In the the linear flow model, a base flow vector can be augmented by additional
 *  flow that is a function of the residual left after the base is taken and the
 *  time it takes to fire the transition.  The total flow may not exceed the the
 *  number/amount at the place.  Additional flow models are under development.
 */
trait PetriNetRules
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the vector inequality is true: t >= b.
     *  The firing threshold should be checked for every incoming arc.
     *  If all return true, the transition should fire.
     *  @param  t  the token vector (number of tokens per color)
     *  @param  b  the base constant vector
     */
    def thresholdI (t: VectorI, b: VectorI): Boolean = t >= b 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the vector inequality is true: f >= b.
     *  The firing threshold should be checked for every incoming arc.
     *  If all return true, the transition should fire.
     *  @param  f  The fluid vector (amount of fluid per color)
     *  @param  b  The base constant vector
     */
    def thresholdD (f: VectorD, b: VectorD): Boolean = f >= b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Function to compute the delay in firing a transition.
     *  The base time is given by a random variate.
     *  This is adjusted by weight vectors multiplying the number of
     *  aggregate tokens and the aggregate amount of fluids summed over
     *  all input places: delay = v + w_t * t + w_f * f.
     *  @param  v    the random variate used to compute base firing time
     *  @param  w_t  the weight for the token vector
     *  @param  t    the aggregate token vector (summed over all input places)
     *  @param  w_f  the weight for the fluid vector
     *  @param  f    the aggregate fluid level vector (summed over all input places)
     */
    def calcFiringDelay (v: Variate, w_t: VectorD, t: VectorI, w_f: VectorD, f: VectorD): Double =
    {
        var delay = v.gen
        if (w_t != null) delay += w_t dot t.toDouble
        if (w_f != null) delay += w_f dot f
        delay
    } // calcFiringTime

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the number of tokens to flow over an arc according to the
     *  vector expression: b + r * (t-b) * d.  If d is 0, returns b.
     *  Supports linear (w.r.t. time delay) and constant (d == 0) flow models.
     *  @param t  the token vector (number of tokens per color)
     *  @param b  the constant vector for base token flow
     *  @param r  the rate vector (number of tokens per unit time)
     *  @param d  the time delay
     */
    def tokenFlow (t: VectorI, b: VectorI, r: VectorI = null, d: Double = 0): VectorI =
    {
        var tt = new VectorI (b.dim)
        if (d == 0 || r == null) {
            tt = b 
        } else {
            for (i <- 0 until b.dim) tt(i) = b(i) + ((r(i) * (t(i) - b(i))).toDouble * d).toInt
        } // if
        t min tt
    } // tokenFlow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the amount of fluid to flow over an arc according to the
     *  vector expression: b + r * (f-b) * d.  If r is 0, returns b.
     *  Supports linear (w.r.t. time delay) and constant (d == 0) flow models.
     *  @param f  the fluid vector (amount of fluid per color)
     *  @param b  the constant vector for base fluid flow
     *  @param r  the rate vector (amounts of fluids per unit time)
     *  @param d  the time delay
     */
    def fluidFlow (f: VectorD, b: VectorD, r: VectorD = null, d: Double = 0): VectorD =
    {
        f min (if (d == 0 || r == null) b else b + r * (f - b) * d)
    } // fluidFlow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the amount of fluid to flow over an arc according to the
     *  system of first-order Ordinary Differential Equation 'ODE's:
     *  "integral 'derv' from t0 to t".  Supports ODE base flow models.
     *  @param f     the fluid vector (amount of fluid per color)
     *  @param derv  the array of derivative functions
     *  @param t0    the current time
     *  @param d     the time delay
     */
    def fluidFlow (f: VectorD, derv: Array [Derivative], t0: Double, d: Double): VectorD =
    {
        println ("fluidFlow: f = " + f + " t0 = " + t0 + " t = " + (t0 + d))
        val g = integrateV (derv, f, t0 + d, t0)
        println ("fluidFlow: f = " + f + " g = " + g + " t0 = " + t0 + " t = " + (t0 + d))
        f min (g - f)
    } // fluidFlow

} // PetriNetRules trait


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PetriNetRulesTest` object is used to test the `PetriNetRules` trait.
 */
object PetriNetRulesTest extends App
       with PetriNetRules
{
    //:: Set the initial time.

    val t0 = 1.0

    //:: Set sample values for tokens.

    val t   = VectorI (5, 4)         // tokens
    val r_t = VectorI (1, 1)         // rates
    val b_t = VectorI (1, 2)         // base requirement
    val w_t = VectorD (.01, .01)     // weight

    //:: Set sample values for fluid.

    val f   = VectorD (5.5, 4.5)     // fluid levels
    val r_f = VectorD (.5, 1.0)      // rates
    val b_f = VectorD (1.5, 2.5)     // base requirement
    val w_f = VectorD (.01, .01)     // weight

    //:: Test the firing rules.

    println ("\n *** Show initial conditions\n")

    println ("Token vector t      = " + t)
    println ("Rate vector r_t     = " + r_t)
    println ("Base token flow b_t = " + b_t)
    println ("Fluid vector f      = " + f)
    println ("Rate vector f_t     = " + r_t)
    println ("Base fluid flow b_f = " + b_f)

    println ("\n *** Test token and fluid firing thresholds (t >= b_t)\n")

    println ("Token threshold:  tokens required: " + thresholdI (t, b_t))
    println ("Fluid threshold:  fluids required: " + thresholdD (f, b_f))

    println ("\n *** Test firing firing delay\n")

    val d = calcFiringDelay (Sharp (1), w_t, t, w_f, f)
    println ("Firing delay: time for transition: " + d)

    println ("\n *** Test token and fluid flows: constant flow model (b)\n")

    println ("Token flow:  place to transition: " + tokenFlow (t, b_t))
    println ("Fluid flow:  place to transition: " + fluidFlow (f, b_f))
    println ("Token flow:  transition to place: " + tokenFlow (t, b_t))
    println ("Fluid flow:  transition to place: " + fluidFlow (f, b_f))

    println ("\n *** Test token and fluid flows: linear flow model (b + r * (t - b) * d\n")

    println ("Token flow:  place to transition: " + tokenFlow (t, b_t, r_t, d))
    println ("Fluid flow:  place to transition: " + fluidFlow (f, b_f, r_f, d))
    println ("Token flow:  transition to place: " + tokenFlow (t, b_t, r_t, d))
    println ("Fluid flow:  transition to place: " + fluidFlow (f, b_f, r_f, d))

    println ("\n *** Test fluid flows: differential flow model integral derv\n")

    def derv1 (t: Double, y: Double) = y
    def derv2 (t: Double, y: Double) = 2.0 * y
    val dervs = Array [Derivative] (derv1, derv2)
    println ("Fluid flow:  place to transition: " + fluidFlow (f, dervs, t0, d))

} // PetriNetRulesTest

