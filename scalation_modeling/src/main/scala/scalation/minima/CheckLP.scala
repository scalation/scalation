
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sun Sep  4 21:57:30 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *-----------------------------------------------------------------------------
 *  @see Linear Programming and Network Flows, Bazaraa and Jarvis
 *  @see www.wiley.com/WileyCDA/WileyTitle/productCd-0470462728,subjectCd-BA04.html
 *  @see Algorithms, 4th Edition, Robert Sedgewick and Kevin Wayne
 *  @see www.cs.princeton.edu/algs4/63or/Simplex.java.html
 *  @see en.wikibooks.org/wiki/Operations_Research/The_Simplex_Method
 */

package scalation.minima

import scala.math.abs

import scalation.linalgebra.{MatriD, VectoD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CheckLP` class checks the solution to Linear Programming (LP) problems.
 *  Given a constraint matrix 'a', limit/RHS vector 'b' and cost vector 'c',
 *  determine if the values for the solution/decision vector 'x' minimizes the
 *  objective function 'f(x)', while satisfying all of the constraints, i.e.,
 *
 *  minimize    f(x) = c x
 *  subject to  a x <= b, x >= 0
 *
 *  Check the feasibility and optimality of the solution.
 *  @param a  the M-by-N constraint matrix
 *  @param b  the M-length limit/RHS vector (make b_i negative for ">=" constraint => surplus)
 *  @param c  the N-length cost vector
 */
class CheckLP (a: MatriD, b: VectoD, c: VectoD)
      extends Error
{
    private val DEBUG   = true           // debug flag
    private val EPSILON = 1E-9           // number close to zero
    private val M       = a.dim1         // the number of constraints (row in a matrix)
    private val N       = a.dim2         // the number of decision variables (columns in a matrix)

    if (b.dim != M) flaw ("constructor", "b.dim = " + b.dim + " != " + M)
    if (c.dim != N) flaw ("constructor", "c.dim = " + c.dim + " != " + N)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the solution primal feasible 'x >= 0 and a x [<= | >=] b'.
     *  @param x  the N-length primal solution vector
     */
    def isPrimalFeasible (x: VectoD): Boolean =
    {
        if (x.dim != N) flaw ("constructor", "x.dim = " + x.dim + " != " + N)

        // non-negativity constraints: check that x >= 0
        for (j <- 0 until N if x(j) < 0.0) {
            flaw ("isPrimalFeasible", "x(" + j + ") = " + x(j) + " is negative")
            return false
        } // for

        val ax = a * x
        // resource limit constraints: check that ax_i <= b_i
        for (i <- 0 until M) {
            val ax_i = ax(i)
            val b_i  = b(i)
            if (ax_i > b_i + EPSILON) {
                flaw ("isPrimalFeasible", "constraint ax_i <= b_i violated for row " + i + ": "
                                                    + ax_i + " > " + b_i)
                return false
            } // if
        } // for
        true
    } // isPrimalFeasible

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the solution dual feasible 'y <= 0 and y a <= c'.
     *  @param y  the M-length dual solution vector
     */
    def isDualFeasible (y: VectoD): Boolean =
    {
        if (y.dim != M) flaw ("constructor", "y.dim = " + y.dim + " != " + M)

        // non-positivity constraints: check that y <= 0
        for (i <- 0 until M if y(i) > 0.0) {
            flaw ("isDualFeasible", "y(" + i + ") = " + y(i) + " is positive")
            return false
        } // for

        val ya = y *: a
        // dual constraints: check that ya_j <= c_j
        for (j <- 0 until N) {
            val ya_j = ya(j)
            val c_j  = c(j)
            if (ya_j > c_j + EPSILON) {
                flaw ("isDualFeasible", "constraint ya_j <= c_j violated for column " + j + ": "
                                                  + ya_j + " > " + c_j)
                return false
            } // if
        } // for
        true
    } // isDualFeasible

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the optimum objective function value f == c x == y b.
     *  @param x  the N-length primal solution vector
     *  @param y  the M-length dual solution vector
     *  @param f  the optimum (minimum) value of the objective function
     */
    def isOptimal (x: VectoD, y: VectoD, f: Double): Boolean =
    {
       val cx = c dot x            // c x
       val yb = y dot b            // y b

       if (abs (f - cx) > EPSILON) {
           flaw ("isOptimal", "failed since f = " + f + " != c x = " + cx)
           return false
       } // if
       if (abs (f - yb) > EPSILON) {
           flaw ("isOptimal", "failed since f = " + f + " != y b = " + yb)
           return false
       } // if
       true
    } // isOptimal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the solution is correct, feasible and optimal.
     *  @param x  the N-length primal solution vector
     *  @param y  the M-length dual solution vector
     *  @param f  the optimum (minimum) value of the objective function
     */
    def isCorrect (x: VectoD, y: VectoD, f: Double): Boolean =
    {
        val pFeas = isPrimalFeasible (x)
        val dFeas = isDualFeasible (y)
        val optim = isOptimal (x, y, f)
        if (DEBUG) {
            println ("CheckLP.isCorrect: isPrimalFeasible = " + pFeas)
            println ("CheckLP.isCorrect: isDualFeasible   = " + dFeas)
            println ("CheckLP.isCorrect: isOptimal        = " + optim)
        } // if
        pFeas && dFeas & optim
    } // isCorrect

} // CheckLP class

