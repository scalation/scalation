
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sun Sep  4 21:57:30 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *-----------------------------------------------------------------------------
 *  @see Linear Programming and Network Flows, Bazaraa and Jarvis
 *       www.wiley.com/WileyCDA/WileyTitle/productCd-0470462728,subjectCd-BA04.html
 *  @see Algorithms, 4th Edition, Robert Sedgewick and Kevin Wayne
 *       www.cs.princeton.edu/algs4/63or/Simplex.java.html
 *  @see en.wikibooks.org/wiki/Operations_Research/The_Simplex_Method
 */

package scalation.maxima

import math.abs
import util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectorD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class solves Linear Programming (LP) problems using a tableau based
 *  Simplex Algorithm.  Given a constraint matrix 'a', limit/RHS vector 'b' and
 *  cost vector 'c', find values for the solution/decision vector 'x' that maximize
 *  the objective function f(x), while satisfying all of the constraints, i.e.,
 *
 *  maximize    f(x) = c x
 *  subject to  a x <= b, x >= 0
 *
 *  In case of "a_i x >= b_i", use -b_i as an indicator of a ">=" constraint.
 *  The program will flip such negative b_i back to positive as well as use
 *  a surplus and artificial variable instead of the usual slack variable, ie.,
 *  a_i x <= b_i  =>  a_i x + s_i = b_i    // use slack variable s_i with coeff 1
 *  a_i x >= b_i  =>  a_i x + s_i = b_i    // use surplus variable s_i with coeff -1
 *  For each ">=" constraint, an artificial variable is introduced and put into
 *  the initial basis.  These artificial variables must be removed from the basis
 *  during Phase I of the Two-Phase Simplex Algorithm.  After this, or if there
 *  are no artificial variables, Phase II is used to find an optimal value for x
 *  and the optimum value for f. 
 *
 *  Creates an MM-by-nn simplex tableau with
 *  -- [0..M-1, 0..N-1]    = a (constraint matrix)
 *  -- [0..M-1, N..M+N-1]  = s (slack/surplus variable matrix)
 *  -- [0..M-1, M+N..nn-2] = r (artificial variable matrix)
 *  -- [0..M-1, nn-1]      = b (limit/RHS vector)
 *  -- [M, 0..nn-2]        = c (cost vector)
 *
 *  @param a  the M-by-N constraint matrix
 *  @param b  the M-length limit/RHS vector (input b_i negative for surplus)
 *  @param c  the N-length cost vector
 */
class Simplex2P (a: MatrixD, b: VectorD, c: VectorD)
      extends Error
{
    private val DEBUG    = false               // if in DEBUG mode, show all pivot step
    private val M        = a.dim1              // the number of constraints (row)
    private val N        = a.dim2              // the number of decision variables
    private val R        = b.countNeg          // the number of artificial variables
    private val MpN      = M + N               // the number of non-artificial variables
    private val MM       = M + 1               // # row in tableau

    private var nn       = MpN + R + 1         // # columns in tableau
    private var jj       = nn - 1              // the last column (b)
    private val MAX_ITER = 200 * N             // maximum number of iterations
    private var flip     = 1.0                  // 1(slack) or -1(surplus) depending on b_i

    if (b.dim != M) flaw ("constructor", "b.dim = " + b.dim + " != " + M)
    if (c.dim != N) flaw ("constructor", "c.dim = " + c.dim + " != " + N)

    private val t  = new MatrixD (MM, nn)                // the MM-by-nn simplex tableau
    private var jr = -1                                  // index counter for artificial variables
    for (i <- 0 until M) {
         flip = if (b(i) < 0.0) -1.0 else 1.0
         t.set (i, a(i))                                 // col x: constraint matrix a
         t(i, N + i) = flip                              // col y: slack/surplus variable matrix s
         if (flip < 0) { jr += 1; t(i, MpN + jr) = 1.0 }  // col r: artificial variable matrix r
         t(i, jj) = b(i) * flip                          // col b: limit/RHS vector b
    } // for

    private val x_B = Array.ofDim [Int] (M)              // the indices of the basis

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_l to enter the basis.  Determine the index of
     *  entering variable corresponding to column l (e.g., using Dantiz's Rule
     *  or Bland's Rule).  Return -1 to indicate no such column.
     *  t(M).firstPos (jj)        // use Bland's rule (lowest index, +ve)
     *  t(M).argmaxPos (jj)       // use Dantiz's rule (min index, +ve, cycling possible)
     */
    def enteringMin () = t(M).firstPos (jj)          // minimization mode
    def enteringMax () = t(M).firstNeg (jj)          // maximization mode
    private var entering: () => Int = enteringMin    // start the function in minimization mode

    private val checker = new CheckLP (a, b, c)      // checker determines if the LP solution is correct

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize the basis to the slack and artificial variables.  Perform
     *  row operations to make cost row (t(M)) zero in artificial var columns. 
     *  If b(i) is negative have a surplus and artificial variable, otherwise,
     *  just a slack variable.
     */
    def initBasis ()
    {
        jr = -1
        for (i <- 0 until M) {
            if (b(i) >= 0.0) {
                x_B(i) = N + i        // put slack variable in basis
            } else {
                jr += 1
                x_B(i) = MpN + jr     // put artificial variable in basis
                t(M) += t(i)          // row op to make t(M, MpN + j) zero
            } // if
        } // for
    } // initBasis

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable x_k to leave the basis given that x_l is entering.
     *  Determine the index of the leaving variable corresponding to row k using
     *  the Min-Ratio Rule.  Return -1 to indicate no such row.
     *  @param l  the entering variable (column)
     */
    def leaving (l: Int): Int =
    {
        val b_ = t.col (jj)                                      // updated b column (RHS)
        var k  = -1
        for (i <- 0 until M if t(i, l) > 0.0) {                   // find the pivot row
            if (k == -1) k = i
            else if (b_(i) / t(i, l) <= b_(k) / t(k, l)) k = i   // lower ratio => reset k
        } // for
        if (k == -1) flaw ("leaving", "the solution is UNBOUNDED")
        if (DEBUG) println ("pivot = (" + k + ", " + l + ")") 
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
        t(k) /= t(k, l)                                     // make pivot 1
        for (i <- 0 to M if i != k) t(i) -= t(k) * t(i, l)  // zero rest of column l
        x_B(k) = l                                          // update basis (l replaces k)
        if (DEBUG) println (this)
    } // pivot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simplex algorithm starting from an initial BFS and iteratively
     *  find a non-basic variable to replace a variable in the current basis
     *  so long as the objective function improves.
     */
    def solve ()
    {
        var k = -1                     // the leaving variable (row)
        var l = -1                     // the entering variable (column)
        if (DEBUG) println (this)

        breakable { for (it <- 1 to MAX_ITER) {
            l = entering (); if (l == -1) break    // -1 => optimal solution found
            k = leaving (l); if (k == -1) break    // -1 => solution is unbounded
            pivot (k, l)                           // pivot: k leaves and l enters
        }} // for

        println ("solve: Final Tableau -----------------------------------------")
        println (this)
    } // solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the artifical variables and reset the cost row (M) in the tableau.
     */
    def removeArtificials ()
    {
        nn -= R                        // reduce the effective width of the tableau
        jj -= R                        // reset the index of the last column (jj)
        t.setCol(jj, t.col(jj + R))    // move the b vector to the new jj column
        t(M)(0 until N) = -c           // set cost row (M) in the tableau to given cost
        if (DEBUG) println (this)
        for (j <- 0 until N if x_B contains j) { 
            val pivotRow = t.col (j).argmax (M)    // find the pivot row where element = 1
            t(M) -= t(pivotRow) * t(M, j)          // make cost row 0 in pivot column (j)
        } // for
    } // removeArtificials

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the LP maximization problem using two phases if neceassry.  Note:
     *  phase I is always minimization.  Two phases are necessary if the number
     *  of artificial variables R > 0.
     */
    def solve2P (): Boolean =
    {
        var x: VectorD = null             // the decision variables
        var y: VectorD = null             // the dual variables
        var f = Double.NegativeInfinity   // worst possible value for maximization

        if (R > 0) {
            t(M)(MpN until jj) = -1.0      // set cost row (M) in the tableau to remove artificials
        } else {
            t(M)(0 until N) = -c          // set cost row (M) in the tableau to given cost vector
        } // if
        if (DEBUG) println (this)

        initBasis ()                      // initialize the basis to the slack and artificial vars

        if (R > 0) {                      // there are artificial variables => phase I required
            println ("solve2P: Phase I ---------------------------------------------")
            println (this)
            println ("decision = " + N + ", slack = " + (M-R)  + ", surplus = " + R + ", artificial = " + R)
            solve ()                      // solve the phase I problem: optimal f = 0
            x = primal; f = objective
            println ("solve2P: Phase I solution x = " + x + ", f = " + f)
            removeArtificials ()          // remove the artificial variables and reset cost row
        } // if

        println ("solve2P: Phase II --------------------------------------------")
        entering = enteringMax            // change to maximization mode
        solve ()                          // solve the phase II problem for final solution
        x = primal; y = dual; f = objective
        println ("solve2P: Phase II solution x = " + x + ", f = " + f)
        val correct = checker.isCorrect (x, y, f)
        if (! correct) flaw ("solve2P", "the Phase II solution is NOT correct")
        correct
    } // solve2P

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the primal solution vector x (only the basic variables are non-zero).
     */
    def primal: VectorD =
    {
        val x = new VectorD (N)
        for (i <- 0 until M if x_B(i) < N) x(x_B(i)) = t(i, jj)   // RHS value
        x
    } // primal

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dual solution vector y (cost row (M) under the slack columns).
     */
    def dual: VectorD = t(M)(N until MpN)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the objective function f(x) = c x.
     */
    def objective: Double = t(M, jj)           // bottom, right cell in tableau

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the current tableau and basis to a string suitable for display.
     */
    override def toString: String =
    {
        var s = new StringBuilder ()
        for (i <- 0 to M) {
            s ++= (if (i == 0) "tableau = | " else        "          | ")
            for (j <- 0 until jj-1) s++= "%8.3f, ".format (t(i, j))
            s ++= "%8.3f | %8.3f |\n".format (t(i, jj-1), t(i, jj))
        } // for
        s ++= "basis = " + x_B
        s.toString
    } // toString

} // Simplex2P class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Simplex2P class.
 */
object Simplex2PTest extends App
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the Simplex2P Algorithm for solving Linear Programming problems.
     *  @param a the constraint matrix
     *  @param b the limit/RHS vector
     *  @param c the cost vector
     */
    def test (a: MatrixD, b: VectorD, c: VectorD)
    {
        val lp = new Simplex2P (a, b, c)
        lp.solve2P ()
        val x = lp.primal         // the primal solution vector x
        val y = lp.dual           // the dual solution vector y
        val f = lp.objective      // the maximum value of the objective function

        println ("primal    x = " + x)
        println ("dual      y = " + y)
        println ("objective f = " + f)
    } // test

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 1:
     *  Phase I  solution - not needed
     *  Phase II solution x = (9, 9, 4), x_B = (0, 1, 2, 3, 6), f = 22
     *  @see 
     */
    def test1 ()
    {
        val a = new MatrixD ((5, 3), -1.0,  1.0,  0.0,
                                      1.0,  4.0,  0.0,
                                      2.0,  1.0,  0.0,
                                      3.0, -4.0,  0.0,
                                      0.0,  0.0,  1.0)
        val c = VectorD              (1.0,  1.0,  1.0)
        val b = VectorD (5.0, 45.0, 27.0, 24.0, 4.0)
        test (a, b, c)
    } // test1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 2:
     *  Phase I  solution - not needed
     *  Phase II solution x = (12, 28), x_B = (0, 1, 4), f = 800
     *  @see 
     */
    def test2 ()
    {
        val a = new MatrixD ((3, 2),  5.0, 15.0,
                                      4.0,  4.0,
                                     35.0, 20.0)
        val c = VectorD            ( 13.0, 23.0)
        val b = VectorD (480.0, 160.0, 1190.0)
        test (a, b, c)
    } // test2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 3:
     *  Phase I  solution - not needed
     *  Phase II solution x = (1, 0, 1, 0), x_B = (0, 2, 4), f = 1
     *  @see 
     *  Cycles if you choose most positive objective function coefficient.
     */
    def test3 ()
    {
        val a = new MatrixD ((3, 4), .5,  -5.5, -2.5,   9.0,
                                     .5,  -1.5, -0.5,   1.0,
                                    1.0,   0.0,  0.0,   0.0)
        val c = VectorD           (10.0, -57.0, -9.0, -24.0)
        val b = VectorD (0.0, 0.0, 1.0)
        test (a, b, c)
    } // test3

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 4:
     *  Phase I  solution - not needed
     *  Phase II solution - no solution
     *  @see 
     *  There is no solution since the LP is UNBOUNDED.
     */
    def test4 ()
    {
        val a = new MatrixD ((2, 4), -2.0, -9.0,  1.0,   9.0,
                                      1.0,  1.0, -1.0,  -2.0)
        val c = VectorD              (2.0,  3.0, -1.0, -12.0)
        val b = VectorD (3.0, 2.0)
        test (a, b, c)
    } // test4

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 5:
     *  Phase I  solution x = (4, 0), x_B = (0, 2), f = 0
     *  Phase II solution x = (4, 4), x_B = (0, 1, 5), f = 12
     *  @see 
     *  Use -b_i to indicate a ">=" constraint (e.g., b_1 = -8).
     */
    def test5 ()
    {
        val a = new MatrixD ((2, 2),  2.0, 3.0,
                                      2.0, 0.0)
        val c = VectorD              (1.0, 2.0)
        val b = VectorD (16.0, -8.0)
        test (a, b, c)
    } // test5 ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test case 6:
     *  Phase I  solution x = (4, 0), x_B = (0, 2), f = 0
     *  Phase II solution x = (0, 5), x_B = (0, 1, 5), f = 40
     *  @see 
     *  Use -b_i to indicate a ">=" constraint (e.g., b_4 = -5).
     */
    def test6 ()
    {
        val a = new MatrixD ((4, 2),  1.0, 1.0,
                                      5.0, 9.0,
                                      1.0, 0.0,
                                      0.0, 1.0)
        val c = VectorD              (5.0, 8.0)
        val b = VectorD (6.0, 45.0, 1.0, -5)
        test (a, b, c)
    } // test6 ()

    println ("\ntest1 ===================================================")
    test1 ()
    println ("\ntest2 ===================================================")
    test2 ()
    println ("\ntest3 ===================================================")
    test3 ()
    println ("\ntest4 ===================================================")
    test4 ()
    println ("\ntest5 ===================================================")
    test5 ()
    println ("\ntest6 ===================================================")
    test6 ()
    println ("===========================================================")

} // Simplex2PTest object

