
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Oct 24 16:25:29 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import scalation.linalgebra.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Minimizer` trait sets the pattern for optimization algorithms for solving
 *  Non-Linear Programming (NLP) problems of the form:
 * 
 *  minimize    f(x)
 *  subject to  g(x) <= 0    [ optionally g(x) == 0 ]
 *
 *  where f is the objective function to be minimized
 *        g is the constraint function to be satisfied, if any
 *
 *  Classes mixing in this trait must implement a function 'fg' that rolls the
 *  constraints into the objective functions as penalties for constraint violation,
 *  a one-dimensional Line Search (LS) algorithm 'lineSearch' and an iterative
 *  method (solve) that searches for improved solutions 'x'-vectors with lower
 *  objective function values (f(x)).
 */
trait Minimizer
{
    protected val EPSILON  = 1E-8               // number close to zero - roughly root of machine epsilon
    protected val TOL      = 100.0 * EPSILON    // default tolerance level more relaxed
    protected val STEP     = 1.0                // default initial step size
    protected val MAX_ITER = 500                // maximum number of major steps/iterations 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The objective function 'f' plus a weighted penalty based on the constraint
     *  function 'g'.  Override for constrained optimization and ignore for
     *  unconstrained optimization.
     *  @param x  the coordinate values of the current point
     */
    def fg (x: VectorD): Double = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact, e.g., 'GoldenSectionLS' or inexact, e.g., 'WolfeLS' line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem by starting at 'x0' and
     *  iteratively moving down in the search space to a minimal point.
     *  @param x0     the starting point 
     *  @param step   the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD

} // Minimizer trait

