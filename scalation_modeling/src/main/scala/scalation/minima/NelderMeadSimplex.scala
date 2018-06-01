
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Sun May  5 13:13:42 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://www.scholarpedia.org/article/Nelder-Mead_algorithm
 *  @see     http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2097904
 */

package scalation.minima

import scala.math.{abs, max, pow}
import scala.util.control.Breaks.{breakable, break}

import scalation.linalgebra.VectorD
import scalation.math.FunctionV2S
import scalation.util.Error

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NelderMeadSimplex` solves Non-Linear Programming (NLP) problems using
 *  the Nelder-Mead Simplex algorithm.  Given a function 'f' and its dimension
 *  'n',  the algorithm moves a simplex defined by n + 1 points in order to find
 *  an optimal solution.  The algorithm is derivative-free.
 *
 *  minimize    f(x)
 *
 *  @param f  the vector-to-scalar objective function
 *  @param n  the dimension of the search space
 */
class NelderMeadSimplex (f: FunctionV2S, n: Int)
      extends Minimizer with Error
{
    type Vertex = Tuple2 [VectorD, Double]            // point and its functional value

    private val DEBUG   = true                        // debug flag
    private val np1     = n + 1                       // number of vertices/points in simplex
    private val simplex = Array.ofDim [Vertex] (np1)  // simplex used for search

    private val alpha = 1.0                           // alpha (> 0)  parameter for reflection
    private val beta  = 0.5                           // beta  (0, 1) parameter for contraction
    private val gamma = 2.0                           // gamma (> 1)  parameter for expansion
    private val delta = 0.5                           // delta (0, 1) parameter for shrinkage

    private var (f_h, f_s, f_l) = (0.0, 0.0, 0.0)     // worst, second worst, best functional values

    if (n < 2) flaw ("constructor", "requires at least a 2-dimensional problem")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize the search simplex by setting n + 1 vertices and computing
     *  their functional values.
     *  @param x0  the given starting point
     *  @param step  the step size
     */
    def initSimplex (x0: VectorD, step: Double)
    {
        simplex(0) = (x0, f(x0))                  // given starting point and its functional value
        for (i <- 1 to n) {
            val x = x0 + x0.oneAt (i-1) * step
            simplex(i) = (x, f(x))
        } // for
        sort ()                                   // order vertices high to low
    } // initSimplex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Sort the vertices in non-increasing order (high to low).  Then the key
     *  indices are worst/highest (h=0), second worst (s=1), and best/lowest (l=n).
     */
    def sort ()
    {
        for (i <- 0 until n) {
            var im = i
            for (j <- i+1 to n if simplex(j)._2 > simplex(im)._2) im = j
            if (im != i) {
                val t = simplex(i); simplex(i) = simplex(im); simplex(im) = t
            } // if
        } // for
    } // sort

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the centroid of the best-side of the simplex (excluding h=0),
     *  returning it and its functional value.
     */
    def centroid (): Vertex =
    {
        val c = new VectorD (n)                   // the centroid of the simplex
        for (i <- 1 to n) c += simplex(i)._1      // add vertex points, except h=0
        val x_c = c / n.toDouble                  // divide by # vertices - 1
        (x_c, f(x_c))
    } // centroid

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reflect: compute the reflection point of the worst point (h=0) across
     *  the centroid.
     *  @param x_c  the best-side centroid of the simplex
     */
    def reflect (x_c: VectorD): Vertex =
    {
        val x_r = x_c + (x_c - simplex(0)._1) * alpha
        (x_r, f(x_r))
    } // reflect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand:  compute the expansion point beyond the reflection point.
     *  @param x_c  the best-side centroid of the simplex
     *  @param x_r  the reflection point
     */
    def expand (x_c: VectorD, x_r: VectorD): Vertex = 
    {
        val x_e = x_c + (x_r - x_c) * gamma
        (x_e, f(x_e))
    } // reflect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Contract: compute the outer contraction point between x_r and x_c.
     *  @param x_c  the best-side centroid of the simplex
     *  @param x_r  the reflection point
     */
    def contractOut (x_c: VectorD, x_r: VectorD): Vertex =
    {
        val x_co = x_c + (x_r - x_c) * beta
        (x_co, f(x_co))
    } // reflect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Contract: compute the inner contraction point between x_h and x_c.
     *  @param x_c  the best-side centroid of the simplex
     *  @param x_r  the reflection point
     */
    def contractIn (x_c: VectorD, x_r: VectorD): Vertex =
    {
        val x_ci = x_c + (simplex(0)._1 - x_c) * beta
        (x_ci, f(x_ci))
    } // contractIn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shrink: fixing the best/lowest point (l=n), move the rest of the points
     *  toward it.
     */
    def shrink ()
    {
        val x_l = simplex(n)._1                             // the best vertex point
        for (i <- 0 until n) {
            val x = x_l + (simplex(i)._1 - x_l) * delta
            simplex(i) = (x, f(x))                          // updated vertex
        } // for
    } // shrink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact (e.g., `GoldenSectionLS`) or inexact (e.g., `WolfeLS`) line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  Currently NOT USED, but may be used to find a better point to add to simplex.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double = 0.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Replace the worst vertex (h=0) in the simplex with the new point.
     *  @param  x_n  the new replacement point
     */
    def replace (x_n: VectorD)
    {
        simplex(0) = (x_n, f(x_n))                                    // put new vertex at index h=0
        if (DEBUG) println ("replace: " + 0 + " with " + simplex(0))
        sort ()                                                       // re-establish the vertex order
    } // replace

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Improve the simplex by replacing the worst/highest vertex (x_h) with a
     *  a better one found on the line containing x_h and the centroid (x_c).
     *  Try the reflection, expansion, outer contraction and inner contraction
     *  points, in that order.  If none succeeds, shrink the simplex and iterate.
     *  Return both distance and difference between x_h (worst) and x_l (best).
     *  @param toler  the tolerance used for termination
     */
    def improveSimplex (toler: Double = EPSILON): Tuple2 [Double, Double] =
    {
        var dist = (simplex(0)._1 - simplex(n)._1).norm          // distance between x_h and x_l
        var diff =  simplex(0)._2 - simplex(n)._2                // difference between f_h and f_l

        breakable { for (k <- 1 to MAX_ITER) {
            f_h = simplex(0)._2                                  // functional value for x_h (highest/worst)
            f_s = simplex(1)._2                                  // functional value for x_s (second worst)
            f_l = simplex(n)._2                                  // functional value for x_l (lowest/best)
            val (x_c, f_c) = centroid ()                         // compute best-side centroid of simplex
            val (x_r, f_r) = reflect (x_c)                       // compute reflection point
            val smaller = f_r <  f_l                             // f_r smaller than best
            val larger  = f_r >= f_s                             // f_r at least as large as second worst

            if (! smaller && ! larger) { replace (x_r); break }  // f_r in middle, replace x_h with x_r

            if (smaller) {
                val (x_e, f_e) = expand (x_c, x_r)               // expand beyond reflection point
                if (f_e < f_r) { replace (x_e); break }          // replace worst x_h with x_e
                else           { replace (x_r); break }          // replace worst x_h with x_r
            } // if

            if (larger) {                                        // contract back from reflection point
                if (f_r < f_h) {                                 // f_r between second worst and worst
                    val (x_co, f_co) = contractOut (x_c, x_r)
                    if (f_co <= f_r) { replace (x_co); break }   // replace worst x_h with x_co
                } else {                                         // f_r at least as large as worst
                    val (x_ci, f_ci) = contractIn (x_c, x_r)
                    if (f_ci <= f_h) { replace (x_ci); break }   // replace worst x_h with x_ci
                } // if
            } // if

            shrink ()                                            // shrink the size of the simplex
            sort ()                                              // re-establish vertex order
            dist = (simplex(0)._1 - simplex(n)._1).norm          // recompute the distance
            diff =  simplex(0)._2 - simplex(n)._2                // recompute the difference
            if (dist < toler && diff < toler) break              // check termination condition
        }} // for

        (dist, diff)                                             // return distance and difference
    } // improveSimplex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the Nelder-Mead
     *  Simplex algorithm.
     *  @param x0     the given starting point
     *  @param step   the initial step size
     *  @param toler  the tolerance used for termination
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): VectorD =
    {
        var dist = Double.PositiveInfinity                     // distance between worst and best vertices
        var diff = Double.PositiveInfinity                     // difference between their functional values
        initSimplex (x0, step)
        if (DEBUG) println ("solve: " + 0 + ":\tdist = " + dist + " diff = " + diff + "\n\tsimplex = " + simplex.deep)

        breakable { for (k <- 1 to MAX_ITER) {
            val (dist, diff) = improveSimplex ()
            if (DEBUG) println ("solve: " + k + ":\tdist = " + dist + " diff = " + diff + "\n\tsimplex = " + simplex.deep)
            if (dist < toler && diff < toler) break            // check termination condition
        }} // for

        val x_l = simplex(n)._1
        println ("solve: optimal vertex = " + x_l)
        x_l                                                    // return the best vertex point
    } // solve

} // NelderMeadSimplex class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NelderMeadSimplexTest` object is used to test the `NelderMeadSimplex` class.
 *  > runMain scalation.minima.NelderMeadSimplexTest
 */
object NelderMeadSimplexTest extends App
{
    var x0 = VectorD (1.0, 1.0)                           // starting point

    println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
    def f (x: VectorD): Double = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
    val solver = new NelderMeadSimplex (f, 2)
    val x = solver.solve (x0)                             // optimal point
    println ("optimal solution = " + x + " at " + f(x))

} // NelderMeadSimplexTest

