
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Mar 10 20:54:50 EDT 2013
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
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.random.Randi

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Simplex` class solves Linear Programming (LP) problems using a tableau based
 *  Simplex Algorithm.  Given a constraint matrix 'a', limit/RHS vector 'b' and
 *  cost vector 'c', find values for the solution/decision vector 'x' that minimize
 *  the objective function 'f(x)', while satisfying all of the constraints, i.e.,
 *
 *  minimize    f(x) = c x
 *  subject to  a x <= b, x >= 0
 *
 *  In case of 'a_i x >= b_i', use '-b_i' as an indicator of a '>=' constraint.
 *  The program will flip such negative b_i back to positive as well as use
 *  a surplus variable instead of the usual slack variable, i.e.,
 *  a_i x <= b_i  =>  a_i x + s_i = b_i    // use slack variable s_i with coefficient 1
 *  a_i x >= b_i  =>  a_i x + s_i = b_i    // use surplus variable s_i with coefficient -1
 *
 *  Creates an MM-by-NN simplex tableau with
 *  -- [0..M-1, 0..N-1]    = a (constraint matrix)
 *  -- [0..M-1, N..M+N-1]  = s (slack/surplus variable matrix)
 *  -- [0..M-1, NN-1]      = b (limit/RHS vector)
 *  -- [M, 0..NN-2]        = c (cost vector)
 *
 *  @param a     the M-by-N constraint matrix
 *  @param b     the M-length limit/RHS vector (input b_i negative for surplus)
 *  @param c     the N-length cost vector
 *  @param x_B   the indices of the initial basis (if not available use Simple2P)
 *  @param n_eq  the number of equality constraints (must come last)
 */
class Simplex (a: MatrixD, b: VectorD, c: VectorD, x_B: Array [Int], n_eq: Int = 0)
      extends MinimizerLP
{
    private val DANTIZ   = true                // use Dantiz's pivot rule, else Bland's
    private val DEBUG    = false               // DEBUG mode => show all pivot steps
    private val CHECK    = true                // CHECK mode => check feasibility for each pivot
    private val _0       = 0.0                 // zero, for Floating Point Error (FPE) try setting to EPSILON

    private val M        = a.dim1              // the number of constraints
    private val N        = a.dim2              // the number of decision variables
    private val MpN      = M + N - n_eq        // the number of variables (decision/slack/surplus)
    private val MM       = M + 1               // # rows in tableau

    private val NN       = MpN + 1             // # columns in tableau
    private val JJ       = NN - 1              // the last column (b)
    private val MAX_ITER = 200 * N             // maximum number of iterations
    private var flip     = 1.0                 // 1(slack) or -1(surplus) depending on b_i

    if (b.dim != M) flaw ("constructor", "b.dim = " + b.dim + " != " + M)
    if (c.dim != N) flaw ("constructor", "c.dim = " + c.dim + " != " + N)
    if (x_B.length != M) flaw ("constructor", "x_B.length = " + x_B.length + " != " + M)

    private val t = new MatrixD (MM, NN)             // the MM-by-NN simplex tableau
    for (i <- 0 until M) {
         flip = if (b(i) < _0) -1.0 else 1.0
         t.set (i, a(i))                             // col x: constraint matrix a
         t(i, N+i) = flip                            // col y: slack/surplus variable matrix s
         t(i, JJ)  = b(i) * flip                     // col b: limit/RHS vector b
    } // for
    t(M)(0 until N) = -c                             // set cost row (M) in the tableau to given cost

    val checker = new CheckLP (a, b, c)              // checker determines if the LP solution is correct

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** In case there are no surplus variables (only slacks), the slack variables
     *  can form an initial basis.
     *  @param a  the M-by-N constraint matrix
     *  @param b  the M-length limit/RHS vector (input b_i negative for surplus)
     *  @param c  the N-length cost vector
     */
    def this (a: MatrixD, b: VectorD, c: VectorD)
    {
        this (a, b, c, Array.range (a.dim2, a.dim1 + a.dim2))
    } // constructor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_l to enter the basis.  Determine the index of
     *  entering variable corresponding to COLUMN l (e.g., using Dantiz's Rule
     *  or Bland's Rule).  Return -1 to indicate no such column.
     *  t(M).argmaxPos (JJ)       // use Dantiz's rule (index of max positive, cycling possible)
     *  t(M).firstPos (JJ)        // use Bland's rule  (index of first positive, FPE possible)
     */
    def entering (): Int =
    {
        if (DANTIZ) t(M).argmaxPos (JJ) else t(M).firstPos (JJ)
    } // entering

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_k to leave the basis given that x_l is entering.
     *  Determine the index of the leaving variable corresponding to ROW k using
     *  the Min-Ratio Rule.  Return -1 to indicate no such row.
     *  @param l  the entering variable (column)
     */
    def leaving (l: Int): Int =
    {
        val b_ = t.col (JJ)                                      // updated b column (RHS)
        var k  = -1
        for (i <- 0 until M if t(i, l) > _0) {                   // find the pivot row
            if (k == -1) k = i
            else if (b_(i) / t(i, l) <= b_(k) / t(k, l)) k = i   // lower ratio => reset k
        } // for
        if (k == -1) flaw ("leaving", "the solution is UNBOUNDED")
        k
    } // leaving

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pivot on entry (k, l) using Gauss-Jordan elimination to replace variable
     *  x_k with x_l in the basis.
     *  @param k  the leaving variable (row)
     *  @param l  the entering variable (column)
     */
    def pivot (k: Int, l: Int)
    {
        println ("pivot: entering = " + l + " leaving = " + k)
        t(k) /= t(k, l)                                      // make pivot 1
        for (i <- 0 to M if i != k) t(i) -= t(k) * t(i, l)   // zero rest of column l
        x_B(k) = l                                           // update basis (l replaces k)
    } // pivot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the Simplex Algorithm starting from an initial BFS and iteratively
     *  find a non-basic variable to replace a variable in the current basis
     *  so long as the objective function improves.  Return the optimal vector x.
     */
    def solve (): VectorD =
    {
        if (DEBUG) showTableau (0)                   // for iter = 0
        var k    = -1                                // the leaving variable (row)
        var l    = -1                                // the entering variable (column)

        breakable { for (it <- 1 to MAX_ITER) {
            l = entering (); if (l == -1) break      // -1 => optimal solution found
            k = leaving (l); if (k == -1) break      // -1 => solution is unbounded
            pivot (k, l)                             // pivot: k leaves and l enters
            if (CHECK && infeasible) break           // quit if infeasible
            if (DEBUG) showTableau (it)
        }} // for
        
        primal                                       // return the optimal vector x
    } // solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the current solution (x = primal) is still primal feasible.
     */
    def infeasible: Boolean =
    {
        if ( ! checker.isPrimalFeasible (primal)) {
            flaw ("infeasible", "solution x is no longer PRIMAL FEASIBLE")
            true
        } else {
            false
        } // if
    } // infeasible

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the primal solution vector x (only the basic variables are non-zero).
     */
    def primal: VectorD =
    {
        val x = new VectorD (N)
        for (i <- 0 until M if x_B(i) < N) x(x_B(i)) = t(i, JJ)   // RHS value
        x
    } // primal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dual solution vector y (cost row (M) under the slack columns).
     */
    def dual: VectorD = t(M)(N until MpN)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the objective function f(x) = c x.
     */
    def objF (x: VectoD): Double = t(M, JJ)    // bottom, right cell in tableau

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the current tableau.
     *  @param iter  the number of iterations do far
     */
    def showTableau (iter: Int)
    {
        println ("showTableau: --------------------------------------------------------")
        println (this)
        println ("showTableau: after " + iter + " iterations, with limit of " + MAX_ITER)
     } // showTableau

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the current tableau and basis to a string suitable for display.
     */
    override def toString: String =
    {
        var s = new StringBuilder ()
        for (i <- 0 to M) {
            s ++= (if (i == 0) "tableau = | " else        "          | ")
            for (j <- 0 until JJ-1) s++= "%8.3f, ".format (t(i, j))
            s ++= "%8.3f | %8.3f |\n".format (t(i, JJ-1), t(i, JJ))
        } // for
        s ++= "basis = " + x_B.deep
        s.toString
    } // toString

} // Simplex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimplexTest` object is used to test the `Simplex` class.
 */
object SimplexTest extends App
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the Simplex Algorithm for solving Linear Programming problems.
     *  @param a    the constraint matrix
     *  @param b    the limit/RHS vector
     *  @param c    the cost vector
     *  @param x_B  the indices of the initial basis
     */
    def test (a: MatrixD, b: VectorD, c: VectorD, x_B: Array [Int] = null)
    {
//      val lp = new Simplex (a, b, c, x_B)    // test with user specified basis
        val lp = new Simplex (a, b, c)         // test with default basis
        val x  = lp.solve ()                   // the primal solution vector x
        val y  = lp.dual                       // the dual solution vector y
        val f  = lp.objF (x)                   // the minimum value of the objective function

        println ("primal x = " + x)
        println ("dual   y = " + y)
        println ("objF   f = " + f)
        println ("optimal? = " + lp.check (x, y, f))
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 1:  Initialize matrix 'a', vectors 'b' and 'c', and optionally
     *  the basis 'x_B'.  For Simplex, matrix 'a' and vector 'c' are not augmented.
     *-------------------------------------------------------------------------
     *  Minimize    z = -1x_0 - 2x_1 + 1x_2 - 1x_3 - 4x_4 + 2x_5
     *  Subject to       1x_0 + 1x_1 + 1x_2 + 1y_3 + 1y_4 + 1x_5 <= 6
     *                   2x_0 - 1x_1 - 2x_2 + 1y_3 + 0y_4 + 0x_5 <= 4
     *                   0x_0 + 0x_1 + 1x_2 + 1y_3 + 2y_4 + 1x_5 <= 4
     *  where z is the objective variable and x is the decision vector.
     *-------------------------------------------------------------------------
     *  Solution:  primal  x_1 =  4, x_7 = 8, x_4 =  2
     *             dual    y_1 = -2, y_2 = 0, y_3 = -1
     *             objF    f = -16
     *  i.e., x = (4, 8, 2), x_B = (1, 7, 4), y = (-2, 0, -1), f = -16
     *  @see Linear Programming and Network Flows, Example 5.1
     */
    def test1 ()
    {
        val a = new MatrixD ((3, 6), 1.0,  1.0,  1.0,  1.0,  1.0,  1.0,       // constraint matrix
                                     2.0, -1.0, -2.0,  1.0,  0.0,  0.0,
                                     0.0,  0.0,  1.0,  1.0,  2.0,  1.0)
        val c   = VectorD          (-1.0, -2.0,  1.0, -1.0, -4.0,  2.0)       // cost vector
        val b   = VectorD (6.0, 4.0, 4.0)                                     // constant vector
        val x_B = Array (6, 7, 8)                                             // starting basis
        test (a, b, c)                                                        // x_B is optional
    } // test1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 2:
     *  Solution:  x = (2/3, 10/3, 0), x_B = (0, 1, 5), f = -22/3
     *  @see Linear Programming and Network Flows, Example 5.2
     */
    def test2 ()
    {
        val a = new MatrixD ((3, 3), 1.0,  1.0,  1.0,       // constraint matrix
                                    -1.0,  2.0, -2.0,
                                     2.0,  1.0,  0.0)
        val c   = VectorD          (-1.0, -2.0,  1.0)       // cost vector
        val b   = VectorD (4.0, 6.0, 5.0)                   // constant vector
        val x_B = Array (3, 4, 5)                           // starting basis
        test (a, b, c, x_B)
    } // test2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 3:
     *  Solution:  x = (1/3, 0, 13/3), x_B = (0, 2, 4), f = -17
     *  @see Linear Programming and Network Flows, Example 3.9
     */
    def test3 ()
    {
        val a = new MatrixD ((3, 3), 1.0, 1.0,  2.0,       // constraint matrix
                                     1.0, 1.0, -1.0,
                                    -1.0, 1.0,  1.0)
        val c   = VectorD           (1.0, 1.0, -4.0)       // cost vector
        val b   = VectorD (9.0, 2.0, 4.0)                  // constant vector
        val x_B = Array (3, 4, 5)                          // starting basis
        test (a, b, c, x_B)
    } // test3

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 4:  randomly generated LP problem.
     */
    def test4 ()
    {
        val rn = Randi (0, 8)
        val (m, n) = (100, 100)
        val a = new MatrixD (m, n)
        val b = new VectorD (m)
        val c = new VectorD (n)
        for (i <- 0 until m; j <- 0 until n) a(i, j) = rn.igen
        for (i <- 0 until m) b(i) = 100.0 * (rn.igen + 1)
        for (j <- 0 until n) c(j) = -10.0 * (rn.igen + 1)
        test (a, b, c, null)
    } // test4

    println ("\ntest1 ========================================================")
    test1 ()
    println ("\ntest2 ========================================================")
    test2 ()
    println ("\ntest3 ========================================================")
    test3 ()
    println ("\ntest4 ========================================================")
    test4 ()

} // SimplexTest object

