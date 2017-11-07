
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Sun Mar 10 20:54:50 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  @see Linear Programming and Network Flows, Bazaraa and Jarvis
 *  @see www.wiley.com/WileyCDA/WileyTitle/productCd-0470462728,subjectCd-BA04.html
 *  @see Algorithms, 4th Edition, Robert Sedgewick and Kevin Wayne
 *  @see www.cs.princeton.edu/algs4/63or/Simplex.java.html
 *  @see en.wikibooks.org/wiki/Operations_Research/The_Simplex_Method
 *  @see dspace.vpmthane.org:8080/.../Dual%20Simplex%20Method%20-%2...
 */

package scalation.minima

import scala.math.abs
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimplex` class solves Linear Programming (LP) problems using a tableau
 *  based Dual Simplex Algorithm.  It is particularly useful when re-optimizing
 *  after a constraint has been added.  The algorithm starts with an infeasible
 *  super-optimal solution and moves toward (primal) feasibility and optimality.
 *
 *  Given a constraint matrix 'a', limit/RHS vector 'b' and
 *  cost vector 'c', find values for the solution/decision vector 'x' that minimize
 *  the objective function f(x), while satisfying all of the constraints, i.e.,
 *
 *  minimize    f(x) = c x
 *  subject to  a x <= b, x >= 0
 *
 *  Creates an 'MM-by-NN' simplex tableau with
 *  -- [0..M-1, 0..N-1]    = a (constraint matrix)
 *  -- [0..M-1, N..M+N-1]  = s (slack/surplus variable matrix)
 *  -- [0..M-1, NN-1]      = b (limit/RHS vector)
 *  -- [M, 0..NN-2]        = c (cost vector)
 *
 *  @param a    the M-by-N constraint matrix
 *  @param b    the M-length limit/RHS vector (input b_i negative for surplus)
 *  @param c    the N-length cost vector
 *  @param x_B  the indices of the initial basis (e.g., from primal Simplex)
 */
class DualSimplex (a: MatrixD, b: VectorD, c: VectorD, x_B: Array [Int])
      extends MinimizerLP
{
    private val DEBUG    = true                // if in DEBUG mode, show all pivot step
    private val M        = a.dim1              // the number of constraints (row)
    private val N        = a.dim2              // the number of decision variables
    private val MpN      = M + N               // the number of variables
    private val MM       = M + 1               // # rows in tableau

    private val NN       = MpN + 1             // # columns in tableau
    private val JJ       = NN - 1              // the last column (b)
    private val MAX_ITER = 200 * N             // maximum number of iterations
//  private var flip     = 1.0                 // 1(slack) or -1(surplus) depending on b_i

    if (b.dim != M) flaw ("constructor", "b.dim = " + b.dim + " != " + M)
    if (c.dim != N) flaw ("constructor", "c.dim = " + c.dim + " != " + N)
    if (x_B.length != M) flaw ("constructor", "x_B.length = " + x_B.length + " != " + M)

    private val t  = new MatrixD (MM, NN)          // the MM-by-NN simplex tableau
    for (i <- 0 until M) {
         t.set (i, a(i))                           // col x: constraint matrix a
         t(i, N+i) = 1.0                           // col y: slack/surplus variable matrix s
         t(i, JJ)  = b(i)                          // col b: limit/RHS vector b
    } // for
    t(M)(0 until N) = -c                           // set cost row (M) in the tableau to given cost

    val checker = new CheckLP (a, b, c)    // checker determines if the LP solution is correct

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_k to leave the basis.  Determine the index of
     *  the leaving variable corresponding to ROW k by selecting the most negative
     *  RHS value.  Return -1 to indicate no such row.
     */
    def leaving (): Int = t.col (JJ).argminNeg (M)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_l to enter the basis given that x_k is leaving
     *  the basis.   Determine the index of the entering variable corresponding to
     *  COLUMN l by selecting the minimum ratio.  Return -1 to indicate no such column.
     *  @param k  the variable that is leaving the basis
     */
    def entering (k: Int) =
    {
        val c_ = t(M)                                           // updated c row (cost)
        var l  = -1
        for (j <- 0 until MpN if t(k, j) < 0.0) {               // find the pivot column
            if (l == -1) l = j
            else if (c_(j) / t(k, j) < c_(l) / t(k, l)) l = j   // lower ratio => reset l
        } // for
        if (l == -1) flaw ("entering", "the dual solution is UNBOUNDED")
        l
    } // entering

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simplex algorithm starting from an initial BFS and iteratively
     *  find a non-basic variable to replace a variable in the current basis
     *  so long as the objective function improves.  Return the optimal vector x.
     */
    def solve (): VectorD =
    {
        if (DEBUG) showTableau (0)                    // for iter = 0
        var k    = -1                                 // the leaving variable (row)
        var l    = -1                                 // the entering variable (column)

        breakable { for (it <- 1 to MAX_ITER) {
            k = leaving ();   if (k == -1) break      // -1 => optimal solution found
            l = entering (k); if (l == -1) break      // -1 => dual solution is unbounded
            pivot (k, l)                              // pivot: k leaves and l enters
            if (DEBUG) showTableau (it)
        }} // for

        primal                                       // return the optimal vector x
    } // solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the primal solution vector x (only the basic variables are non-zero).
     */
    def primal: VectorD =
    {
        val x = new VectorD (N)
        for (i <- 0 until M if x_B(i) < N) x(x_B(i)) = t(i, JJ)   // RHS value
        x
    } // primal

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dual solution vector y (cost row (M) under the slack columns).
     */
    def dual: VectorD = t(M)(N until MpN)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the objective function f(x) = c x.
     */
    def objF (x: VectoD): Double = t(M, JJ)   // bottom, right cell in tableau

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the current tableau.
     *  @param iter  the number of iterations do far
     */
    def showTableau (iter: Int)
    {
        println ("showTableau: --------------------------------------------------------")
        println (this)
        println ("showTableau: after " + iter + " iterations, with limit of " + MAX_ITER)
     } // showTableau

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

} // DualSimplex class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimplexTest` object is used to test the `DualSimplex` class.
 */
object DualSimplexTest extends App
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the Dual Simplex Algorithm for solving Linear Programming problems.
     *  @param a the constraint matrix
     *  @param b the limit/RHS vector
     *  @param c the cost vector
     */
    def test (a: MatrixD, b: VectorD, c: VectorD, x_B: Array [Int])
    {
        val lp = new DualSimplex (a, b, c, x_B)    // test the Dual Simplex Algorithm
        val x  = lp.solve ()                       // the primal solution vector x
        val y  = lp.dual                           // the dual solution vector y
        val f  = lp.objF (x)                       // the minimum value of the objective function

        println ("primal x = " + x)
        println ("dual   y = " + y)
        println ("objF   f = " + f)
        println ("optimal? = " + lp.check (x, y, f))
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 1:  Initialize matrix 'a', vectors 'b' and 'c', and optionally
     *  the basis 'x_B'.  For Dual Simplex, matrix 'a' and vector 'c' are not augmented.
     *--------------------------------------------------------------------------
     *  Minimize    z =  2x_0 + 3x_1 + 4x_2
     *  Subject to       1x_0 + 2x_1 + 1x_2 >= 3
     *                   2x_0 - 1x_1 + 3x_2 >= 4
     *  where z is the objective variable and x is the decision vector.
     *  Since constraints are >=, multiply by rows by -1
     *--------------------------------------------------------------------------
     *  Dual Solution:  x = (11/5, 2/5), x_B = (0, 1), f = 28/5
     *  @see Linear Programming and Network Flows, Example 6.6
     */
    def test1 ()
    {
        val a = new MatrixD ((2, 3), -1.0, -2.0, -1.0,     // constraint matrix
                                     -2.0,  1.0, -3.0)
        val c   = VectorD            (2.0,  3.0,  4.0)     // cost vector
        val b   = VectorD (-3.0, -4.0)                     // constant vector
        val x_B = Array (3, 4)                             // starting basis
        test (a, b, c, x_B)
    } // test1

    println ("\ntest1 ===================================================")
    test1 ()

} // DualSimplexTest object

