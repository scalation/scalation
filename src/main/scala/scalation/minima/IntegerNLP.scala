
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Sun Sep 11 22:43:04 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.minima

import collection.mutable.ArrayBuffer
import math.{abs, ceil, floor, max, round, sqrt}
import util.control.Breaks.{breakable, break}

import scalation.calculus.Calculus.FunctionV2S
import scalation.linalgebra.VectorD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This `IntegerNLP`solves Integer Non-Linear Programming (INLP) and Mixed Integer
 *  Linear Non-Programming (MINLP) problems recursively using the Simplex algorithm.
 *  First, an NLP problem is solved.  If the optimal solution vector 'x' is
 *  entirely integer valued, the INLP is solved.  If not, pick the first 'x_j'
 *  that is not integer valued.  Define two new NLP problems which bound 'x_j'
 *  to the integer below and above, respectively.  Branch by solving each of
 *  these NLP problems in turn.  Prune by not exploring branches less optimal
 *  than the currently best integer solution.  This technique is referred to
 *  as Branch and Bound.  An exclusion set may be optionally provided for
 *  MINLP problems.
 *
 *  Given an objective function 'f(x)' and a constraint function 'g(x)',
 *  find values for the solution/decision vector 'x' that minimize the
 *  objective function 'f(x)', while satisfying the constraint function, i.e.,
 *
 *  minimize    f(x) 
 *  subject to  g(x) <= 0, some x_i must integer-valued
 *
 *  Make b_i negative to indicate a ">=" constraint
 *
 *  @param f     the objective function to be minimized
 *  @param g     the constraint function to be satisfied, if any
 *  @param excl  the set of variables to be excluded from the integer requirement 
 */
class IntegerNLP (f: FunctionV2S, n: Int, var g: FunctionV2S = null, excl: Set [Int] = Set ())
{
    private val EPSILON = 1E-7                     // number close to zero
    private val SQRT_EPSILON = sqrt (EPSILON)      // square root of EPSILON

    // best integer solution so far
    private var best: Tuple2 [VectorD, Double] = (null, Double.PositiveInfinity)

    def g0 (x: VectorD): Double = 0.0

    if (g == null) g = g0

    val x_le = new VectorD (n); x_le.set (Double.NaN)      // constraint x_j <= value 
    val x_ge = new VectorD (n); x_ge.set (Double.NaN)      // constraint x_j >= value

    println (">>>>>>>>>>>>>> root: dp = 0")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a new constraint to the current set of bounding constraints: x_j - bound <= 0
     *  or x_j - bound >= 0 (e.g., x_1 - 2 <= 0 or x_0 - 4 >= 0).
     *  @param j      the index of variable x_j
     *  @param le     whether it is a "less than or equal to" 'le' constraint
     *  @param bound  the bounding value
     */
    def addConstraint (j: Int, le: Boolean, bound: Double): Boolean =
    { 
        println ("addConstraint: (" + j + ", " + le + ", " + bound + ")")
        val low = x_le(j)
        val hi  = x_ge(j)
        if (le) {
            if (low.isNaN && hi.isNaN) x_le(j) = bound                   // add "<=" constraint
            else if (bound >= hi)      x_le(j) = bound                   // add "<=" constraint
            else if (bound < hi)     { x_le(j) = bound; x_ge(j) = -1 }   // replace ">=" constraint
            else if (bound < low)      x_le(j) = bound                   // replace "<=" constraint
            else return false
        } else {
            if (low.isNaN && hi.isNaN) x_ge(j) = bound                   // add ">=" constraint
            else if (bound <= low)     x_ge(j) = bound                   // add ">=" constraint
            else if (bound > low)    { x_ge(j) = bound; x_le(j) = -1 }   // replace "<=" constraint
            else if (bound > hi)       x_ge(j) = bound                   // replace ">=" constraint
            else return false
        } // if
        true
    } // addConstraint

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add up all the violation of bounds constraints.
     *  @param x  the current point
     */
    def gBounds (x: VectorD): Double =
    {
        var sum = 0.0
        for (j <- 0 until x.dim) {                               // loop over the variables x_j
            if (! x_le(j).isNaN) {                               // check for x_j <= bound
                println ("x_" + j + " - " + x_le(j) + " <= 0.0")
                sum += max (0.0, x(j) - x_le(j))
            } // if
                
            if (! x_ge(j).isNaN) {                               // check for x_j >= bound
                println (x_ge(j) + " - x_" + j + " <= 0.0")
                sum += max (0.0, x_ge(j) - x(j))
            } // if
        } // for
        sum
    } // gBounds

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return j such that x_j has a fractional (non-integer) value, -1 otherwise.
     *  Make sure that j is not in the exclusion list.
     *  @param x  the vector to check
     */
    def fractionalVar (x: VectorD): Int =
    {
        for (j <- 0 until x.dim if ! (excl contains j) && abs (x(j) - round (x(j))) > SQRT_EPSILON) return j
        -1
    } // fractionalVar

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Mixed Integer Non-linear, Linear Programming (MINLP) problem by using
     *  Branch and Bound and an NLP Algorithm, e.g., `QuasiNewton`, recursively.
     *  @param dp  the current depth of recursion
     *  @param gb  the supplementary constraint function formed from bounds
     */
    def solve (x0: VectorD, dp: Int)
    {
        val MAX_DEPTH = 2                                // limit on depth of recursion  FIX ??
        def gg (x: VectorD): Double = g(x) + gBounds(x)  // given + bounds constraints
        val nlp   = new QuasiNewton (f, gg)              // set up a new NLP problem
        val x     = nlp.solve (x0)                       // solve the new NLP problem
        val j     = fractionalVar (x)                    // find j such that x_j is not an integer
        var bound = 0.0

        println ("IntegerNLP.solve: x = " + x + " f(x) = " + f(x) + ", j = " + j)

        if (j != -1 && f(x) < best._2 && dp < MAX_DEPTH) {  // x_j is not an integer => bound on both sides

            println ("solve: add upper and lower bounds")

            // add lower bound constraint: x_j <= floor (x(j))
            bound = floor (x(j))
            if (addConstraint (j, true, bound)) {
                println (">>>>>>>>>>>>>> left branch:  dp = " + (dp + 1))
                println (">>>>>>>>>>>>>> add constraint x_" + j + " <= " + bound)
                solve (x, dp + 1)
            } // if

            // add upper bound constraint: x_j >= -ceil (x(j)) where "-" => ">=" constraint
            bound = ceil (x(j))
            if (addConstraint (j, false, bound)) {
                println (">>>>>>>>>>>>>> right branch: dp = " + (dp + 1))
                println (">>>>>>>>>>>>>> add constraint x_" + j + " >= " + bound)
                solve (x, dp + 1)
            } // if
        } // if

        if (j == -1) {
            println ("####################################################################")
            println ("IntegerNLP.solve: found an INTEGER solution (x, f(x)) = " + (x, f(x)))
            println ("####################################################################")
            if (f(x) < best._2) best = (x, f(x))                // save the best result
        } // if
    } // solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the optimal (minimal) integer solution.
     */
    def solution = best

} // IntegerNLP class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `IntegerNLPTest` object is used to test the `IntegerNLP` class.
 *  real solution    x = (.8, 1.6), f = 8.8
 *  integer solution x = (2, 1),    f = 10
 *  @see Linear Programming and Network Flows, Example 6.14
 */
object IntegerNLPTest extends App
{
    val x0 = new VectorD (2)
    def f (x: VectorD): Double = (x(0) - 3.5) * (x(0) - 3.5) + (x(1) - 5) * (x(1) - 5) + 1.0

    val inlp = new IntegerNLP (f, x0.dim)
    inlp.solve (x0, 0)
    println ("###############################################################")
    println ("optimal solution = " + inlp.solution)
    println ("###############################################################")

} // IntegerNLPTest object

