
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Thu Oct 13 12:03:21 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import math.max

import scalation.linalgebra_gen.Vectors.VectorI

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class performs local search to find minima of functions defined on
 *  integer vector domains (z^n).
 *
 *  minimize    f(x)
 *  subject to  g(x) <= 0, x in Z^n
 *
 *  @param f        the objective function to be minimize (f maps an integer vector to a double)
 *  @param g        the constraint function to be satisfied, if any
 *  @param maxStep  the maximum/starting step size (make larger for larger domains)
 */
class IntegerLocalSearch (f: VectorI => Double,
                          g: VectorI => Double = null,
                          maxStep: Int = 5)
{
    /** Pair consisting of an integer vector and its functional value (a double)
     */
    type Vec_Func = Tuple2 [VectorI, Double]

    /** Debug flag
     */
    private val DEBUG = false

    /** Weight on penalty for constraint violation
     */
    private val WEIGHT = 1000

    /** Maximum number of iterations allowed in total (4 maxStep^2)
     */
    private val maxIter = 4 * maxStep * maxStep     // maxStep = 5 => MaxIter = 100

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function f re-scaled by a weighted penalty, if constrained.
     *  @param x  the coordinate values of the currrent point
     */
    def fg (x: VectorI): Double =
    {
        if (g == null) {                  // unconstrained
            f(x)
        } else {                          // constrained, g(x) <= 0
            val penalty = max (g(x), 0.0)
            f(x) * (1.0 + WEIGHT * penalty * penalty)
        } // if
    } // fg

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find a minimal neighbor of the current point x that is a distance step away.
     *  Let x be the current point with y being a step down and x being a step up
     *  in dimension i.  Recurse to handle all of the dimensions.
     *  @param x_f0  the current pair (the point and its functional value)
     *  @param i     the i-th dimension or coordinate (facilitates recursion)
     *  @param step  examine points that are this far away
     */
    def minNeighbor (x_f0: Vec_Func, i: Int, step: Int = 1): Vec_Func =
    {
        val x = x_f0._1                      // current point
        val y = x - (step, i)                // step down in dimension i: x_i - step
        val z = x + (step, i)                // step up in dimension i:   x_i + step

        var x_f = x_f0                       // current pair (vector and its function)
        var y_f = (y, fg(y))                 // down pair   
        var z_f = (z, fg(z))                 // up pair
        if (DEBUG) println ("minNeighbor: candidates at i = " + i + ": " + x_f + ", " + y_f + ", " + z_f)

        if (i < x_f._1.dim - 1) {
            x_f = minNeighbor (x_f, i + 1, step)   // min in neighborhood of x_f
            y_f = minNeighbor (y_f, i + 1, step)   // min in neighborhood of y_f
            z_f = minNeighbor (z_f, i + 1, step)   // min in neighborhood of z_f
        } // if

        if (x_f._2 < y_f._2) {               // find smallest of 3 functional value
            if (x_f._2 < z_f._2) x_f else z_f
        } else { 
            if (y_f._2 < z_f._2) y_f else z_f
        } // if
    } // minNeighbor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the minimization problem by repeatedly moving to a minimal neighbor
     *  until there is no improvement.
     *  @param x  the starting point for the search
     */
    def solve (x: VectorI): Vec_Func =
    {
        var x_f  = (x, fg(x))        // starting pair: vector x and its functional value f(x)
        var step = maxStep           // start with larger steps
        for (k <- 1 to maxIter) {
            println ("+ k = " + k + ", step = " + step + ", x_f = " + x_f)
            val y_f = minNeighbor (x_f, 0, step)
            if (x_f._2 <= y_f._2) {              // no improvement
               if (step == 1) return x_f         // => return solution when step is 1
               else step -= 1                    // => decrease step size otherwise
            } // if
            x_f = y_f                            // move to improved point
        } // for
        println ("no local optima found yet: " + x_f)
        x_f                                      // return sub-optimal solution
    } // solve

} // IntegerLocalSearch


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the IntegerLocalSearch class (unconstrained).
 */
object IntegerLocalSearchTest extends App
{
    def f (x: VectorI): Double = (x(0) - 10) * (x(0) - 10) + (x(1) - 20) * (x(1) - 20) + 1
    val x0  = new VectorI (2)
    val ils = new IntegerLocalSearch (f)
    println ("optimal solution = " + ils.solve (x0))

} // IntegerLocalSearchTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the IntegerLocalSearch class (constrained).
 */
object IntegerLocalSearchTest2 extends App
{
    def f (x: VectorI): Double = (x(0) - 10) * (x(0) - 10) + (x(1) - 20) * (x(1) - 20) + 1
    def g (x: VectorI): Double = -max (x(0) - 1, x(1) - 1)    // require x(i) >= 1
    val x0  = new VectorI (2); x0.set (1)
    val ils = new IntegerLocalSearch (f, g)
    println ("optimal solution = " + ils.solve (x0))

} // IntegerLocalSearchTest2 object

