
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Fri Dec 30 18:23:13 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     http://users.bart.nl/users/termaten/Publications/Coached/JdS_Radau_1997.pdf
 *  @see     http://www.dm.uniba.it/~testset/solvers/radau5.php
 */

// U N D E R   D E V E L O P M E N T

package scalation.dynamics

import math.sqrt
import util.control.Breaks.{breakable, break}

import scalation.calculus.Calculus._
import scalation.linalgebra.{MatrixD, VectorD}
import scalation.linalgebra.MatrixD.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Radau` object implements Radau IIA, which is a simple Ordinary Differential
 *  Equation 'ODE' solver for moderately stiff systems.  Solve for 'y' given
 *  <p>
 *      d/dt  y = f(t, y).
 *  <p>
 */
object Radau
       extends Integrator
{
    import Derivatives._

    private val EPSILON  = 1E-7
    private val MAX_ITER = 100
    private val root6    = sqrt (6.0)
    private val _1_3     = 1.0 / 3.0
    private val _1_4     = 1.0 / 4.0
    private val _3_4     = 3.0 / 4.0
    private val _1_12    = 1.0 / 12.0
    private val _5_12    = 5.0 / 12.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0.0, step: Double = defaultStepSize): Double =
    {
        // TBD
        0.0
    } // integrate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /**
     */
    def integrateVV (f: Array [DerivativeV], y0: VectorD, t: Double,
                     t0: Double = 0.0, step: Double = defaultStepSize): VectorD =
    {
        // TBD
        null
    } // integrateVV

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Jacobian matrix for a vector-valued derivative function
     *  represented as an array of scalar-valued functions.  The i-th row in the
     *  matrix is the gradient of the i-th function.
     *  @param f  the array of functions whose Jacobian is sought
     *  @param y  the point (vector) at which to estimate the Jacobian
     */
    def jacobian (f: Array [DerivativeV], y: VectorD, t: Double): MatrixD =
    {
        val j = new MatrixD (f.length, y.dim)
        for (i <- 0 until f.length) j(i) = gradient (f(i)(t, _: VectorD), y)
        j
    } // jacobian

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** 
     */
    def solve (f: Array [DerivativeV], yn_1: VectorD, fn_1: VectorD, tn_1: Double, tn: Double, h: Double)
    {
        val h1_3  = _1_3 * h
        val h1_4  = _1_4 * h
        val h3_4  = _3_4 * h
        val h1_12 = _1_12 * h
        val h5_12 = _5_12 * h

        var gn    = yn_1 + fn_1 * h1_3
        var yn    = yn_1 + fn_1 * h

        val jacob = jacobian (f, yn_1, tn_1)
        val ident = eye (f.length)
        val lu    = ident - jacob * (root6 * h / 6.0)

        breakable { for (k <- 1 to MAX_ITER) {
            val fg = new VectorD (f.length)
            for (i <- 0 until f.length) fg(i) = f(i) (tn_1 + h1_3, gn)
            val fy = new VectorD (f.length)
            for (i <- 0 until f.length) fy(i) = f(i) (tn, yn)
            val dg = lu.inverse * (yn_1 - gn + fg * h5_12 - fy * h1_12)
            val dy = lu.inverse * (dg * (4.0 * root6 - 8.0) + yn_1 - yn + fg * h3_4 + fy * h1_4)
    
            if (dy.norm < EPSILON) break
    
            gn += dg
            yn += dg * (8.0 - 4.0 * root6) + dy
        }} // for

        println ("yn = " + yn)
    } // solve 

} // Radau object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Radau5 object.
 */
object RadauTest extends App
{
    println ("Radau is not implemented yet")
    // call solve

} // RadauTest object

