
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Sat Aug 20 23:20:26 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation.maxima

import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.{MatrixD, VectoD, VectorD}
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RevisedSimplex` class solves Linear Programming (LP) problems using the
 *  Revised Simplex Algorithm.  Given a constraint matrix 'a', constant vector 'b'
 *  and cost vector 'c', find values for the solution/decision vector 'x' that
 *  maximize the objective function 'f(x)', while satisfying all of the constraints, i.e.,
 *
 *  maximize    f(x) = c x
 *  subject to  a x <= b, x >= 0
 *
 *  The revised algorithm has benefits over the Simplex Algorithm (less memory
 *  and reduced chance of round off errors).
 *  @see math.uc.edu/~halpern/Linear.progr.folder/Handouts.lp.02/Revisedsimplex.pdf
 *  @param a      the constraint matrix
 *  @param b      the constant/limit vector
 *  @param c      the cost/revenue vector
 *  @param x_B    the initial basis (set of indices where x_i is in the basis)
 */
class RevisedSimplex (a: MatrixD, b: VectorD, c: VectorD, var x_B: Array [Int] = null)
      extends Error
{
    private val EPSILON  = 1E-9                         // number close to zero
    private val M        = a.dim1                       // number of constraints (rows in a)
    private val N        = a.dim2                       // number of original variables (columns in a)
    private val MAX_ITER = 200 * N                      // maximum number of iterations

    if (b.dim != M) flaw ("constructor", "b.dim = " + b.dim + " != " + M)
    if (c.dim != N) flaw ("constructor", "c.dim = " + c.dim + " != " + N)

    private val b_inv    = a.selectCols (x_B).inverse   // basis of matrix-a inverted

    private var c_       = c.select (x_B) *: b_inv      // adjusted cost
    private val b_       = b_inv * b                    // adjusted constants

    private var u: VectoD = null                        // ?? FIX
    private var z: VectoD = null                        // ?? FIX

    if (x_B == null) x_B = setBasis ()

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** There are 'M+N' variables, 'N' decision and 'M' slack variables, of which,
     *  for each iteration, M are chosen for a Basic Feasible Solution (BFS).
     *  The the variables not in the basis are set to zero.  Setting 'j' to 'N'
     *  will start with the slack variables in the basis (only works if 'b >= 0').
     *  @param j  the offset to start the basis
     *  @param l  the size of the basis
     */
    def setBasis (j: Int = N, l: Int = M): Array [Int] =
    {
        val ba = Array.ofDim [Int] (l)
        for (i <- 0 until l) ba(i) = i + j
        ba
    } // setBasis

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable 'x_l' to enter the basis.
     */
    def entering (): Int = 
    {
        z = c_ *: a - c
        z.argmaxPos ()
    } // entering 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the best variable 'x_k' to leave the basis given that 'x_l' is entering.
     *  @param l  the variable chosen to enter the basis
     */
    def leaving (l: Int): Int =
    {
        u = b_inv * a.col(l)
        if (unbounded (u)) return -1
        var k = 0
        var r_min = Double.PositiveInfinity
        for (i <- 0 until M if u(i) > 0) {
            val r = b_(i) / u(i)
            if (r < r_min) { r_min = r; k = i}
        } // for
        k
    } // leaving

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pivot by replacing 'x_k' with 'x_l' in the basis.  Update 'b_inv',' b_' and 'c_'.
     *  @param k  the leaving variable
     *  @param l  the entering variable
     */
    def pivot (k: Int, l: Int)
    {
        x_B(k) = l                // update basis (l replaces k)
        b_inv(k) /= u(k)
        b_(k)    /= u(k)
        for (i <- 0 until M if i != k) {
            b_inv(i) -= b_inv(k) * u(i)
            b_ (i)   -= b_(k) * u(i)
        } // for
        c_ -= b_inv(k) * z(l)
    } // pivot

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'u <= 0.0', i.e., the solution is unbounded.
     *  @param u  the ?? vector FIX
     */
    def unbounded (u: VectoD): Boolean =
    {
        for (i <- 0 until u.dim if u(i) > 0.0) return false
        println ("the solution is unbounded")
        true
    } // unbounded

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve a Linear Programming (LP) problem using the Revised Simplex Algorithm.
     *  Iteratively pivot until there an optimal solution is found or it is
     *  determined that the solution is unbounded.
     */
    def solve (): Tuple2 [VectorD, Double] =
    {
        var k = -1       // the leaving variable
        var l = -1       // the entering variable
        showTableau ()

        breakable { for (it <- 1 to MAX_ITER) {
            l = entering (); if (l == -1) break    // optimal solution found
            k = leaving (l); if (k == -1) break    // solution is unbounded
            pivot (k, l)                           // pivot: k leaves and l enters
            showTableau ()
        }} // for

        val x = primal
        (x, objValue (x))    // return the primal solution and the optimal value
    } // solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the primal solution vector 'x'.
     */
    def primal: VectorD = b_inv * b

   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the dual solution vector 'y'.  FIX
     */
    def dual: VectoD = null

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the optimal objective function value 'f(x) = c x'.
     *  @param x  the primal solution vector
     */
    def objValue (x: VectorD): Double = c.select(x_B) dot x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the current revised tableau displaying the basis, 'b_inv', 'b_', 'c_'.
     */
    def showTableau ()
    {
        println ("Revised Tableau")
        println ("---------------------------------------------------")
        for (i <- 0 until a.dim1) {
            println ("x" + x_B(i) + " | " + b_inv(i) + " | " + b_(i))
        } // for
        println ("c_ | " + c_)
        println ("---------------------------------------------------")
    } // showTableau

} // RevisedSimplex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RevisedSimplexTest` tests the Revised Simplex Algorithm class with the
 *  following maximization problem:
 *  Maximize    z = 2x_0 + 3x_1 + 4x_2
 *  Subject to      3x_0 + 2x_1 + 1x_2 + 1y_3 + 0y_4 = 10
 *                 2x_0 + 5x_1 + 3x_2 + 0y_3 + 1y_4 = 15
 *  where z is the objective variable, x are the decision variables and
 *  y are slack variables.
 */
object RevisedSimplexTest extends App
{
    // initialize matrix a, vectors b and c and the basis
    //
    val a = new MatrixD ((3, 7), 0.0,  3.0,  3.0,  1.0, -1.0, 1.0, -5.0,
                                 1.0,  3.0, -1.0,  0.0, -1.0, 1.0,  3.0, 
                                 1.0,  2.0,  0.0,  1.0,  2.0, 0.0, -2.0)
    val c   = VectorD          (-2.0, -3.0,  4.0, -3.0, -1.0, 4.0, -6.0)
    val b   = VectorD (3.0, 4.0, 5.0)
    val x_B = Array (3, 5, 0)

    val result = (new RevisedSimplex (a, b, c, x_B)).solve ()

    println ("---------------------------------------------------")
    println ("optimal x = " + result._1 + ", max cx = " + result._2)
    println ("---------------------------------------------------")

    // initialize matrix a and vectors b and c
    //
//  val a = new MatrixD ((2, 3), 3.0, 2.0, 1.0,
//                               2.0, 5.0, 3.0)
//  val c   = VectorD           (2.0, 3.0, 4.0)
//  val b   = VectorD (10.0, 15.0)
//  val x_B = Array (1, 2)
//
//  val result = (new RevisedSimplex (a, b, c, x_B)).solve ()
//
//  println ("---------------------------------------------------")
//  println ("optimal x = " + result._1 + ", max cx = " + result._2)
//  println ("---------------------------------------------------")

} // RevisedSimplexTest object

