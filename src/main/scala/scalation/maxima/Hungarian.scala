
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Thu Jul 14 11:51:50 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *---------------------------------------------------------------------------
 *  Translated from C code from Assignment Problem and Hungarian Algorithm 
 *  @see www.topcoder.com/tc?module=Static&d1=tutorials&d2=hungarianAlgorithm
 */

package scalation.maxima

import java.util.ArrayDeque   // use Java since ArrayDeque is faster than Scala's Queue

import Double.PositiveInfinity
import math.{max, min}
import util.control.Breaks.{breakable, break}

import scalation.linalgebra.MatrixD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This is an O(n^3) implementation of the Hungarian algorithm (or Kuhn-Munkres
 *  algorithm).  Find the maximum cost set of pairings between m x-nodes (workers)
 *  and n y-nodes (jobs) such that each worker is assigned to one job and each
 *  job has at most one worker assigned.
 *  It solves the maximum-weighted bipartite graph matching problem.
 *
 *  maximize sum i = 0 .. m-1 { cost(x_i, y_i) }
 *
 *  Caveat: only works if m <= n (i.e., there is at least one job for every worker).
 *  @param cost  the cost matrix: cost(x, y) = cost of assigning worker to job
 */
class Hungarian (cost: MatrixD)
{
    private val NA = -1                    // Not Assigned
    private val NO = -2                    // None Possible
    private val m  = cost.dim1             // m workers (x-nodes)
    private val n  = cost.dim2             // n jobs (y-nodes)

    private val r_m      = 0 until m                 // range for workers
    private val r_n      = 0 until n                 // range for jobs
    private val lx       = Array [Double] (m)        // labels of x-nodes (workers)
    private val ly       = Array [Double] (n)        // labels of y-nodes (jobs)
    private val slack    = Array [Double] (n)        // slack(y) = lx(x) + lx(y) - cost(x, y)
    private val slackX   = Array [Int] (n)           // slackX(y) = x-node for computing slack(y)
    private val xy       = Array.fill(m) { NA }      // xy(x) = y-node matched with x
    private val yx       = Array.fill(n) { NA }      // yx(y) = x-node matched with y
    private val qu       = new ArrayDeque [Int] (m)  // queue for Breadth First Search (BFS)
    private val maxMatch = min (n, m)                // maximum number of matches needed
    private var nMatch   = 0                         // number of nodes in current matching

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize cost labels for x-nodes by setting them to the largest cost
     *  on any incident edge (largest value in row). If feasible, this is the
     *  optimal solution, otherwise it is an upper bound.
     */
    private def initLabels ()
    {
        for (x <- r_m; y <- r_n) lx(x) = max (lx(x), cost(x, y))
    } // initLabels

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the cost labels for both x-nodes and y-nodes.
     */
    private def updateLabels (xSet: Array [Boolean], ySet: Array [Boolean])
    {
        var delta: Double = PositiveInfinity
        for (y <- r_n if ! ySet(y)) delta = min (delta, slack(y))
        for (x <- r_m if xSet(x))   lx(x) -= delta
        for (y <- r_n if ySet(y))   ly(y) += delta
        for (y <- r_n if ! ySet(y)) slack(y) -= delta
    } // updateLabels

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add new edges to the tree and update slack.
     *  @param x      current x-node
     *  @param prevX  previous x-node before x in the alternating path,
     *                so we add edges (prevX, xy(x)), (xy(x), x)
     */
    private def addToTree (x: Int, prevX: Int, prev: Array [Int], xSet: Array [Boolean]) 
    {
        xSet(x) = true                      // add x to xSet
        prev(x) = prevX                     // we need this when augmenting
        for (y <- r_n if lx(x) + ly(y) - cost(x, y) < slack(y)) {
            slack(y)  = lx(x) + ly(y) - cost(x, y)
            slackX(y) = x
        } // for
    } // addToTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find a root (an unparied x-node) and compute slack values for y-nodes. 
     */
    private def findRootSetSlack (prev: Array [Int], xSet: Array [Boolean])
    {
        var root = NA
        breakable { for (x <- r_m if xy(x) == NA) {
            root = x
            qu.add (x)
            prev(x) = NO           // root is first => no previous x-node
            xSet(x) = true
            break
        }} // for

        for (y <- r_n) {       // initialize the slack array
            slack(y)  = lx(root) + ly(y) - cost(root, y)
            slackX(y) = root
            println ("findRootSetSlack: slack(" + y + ") = " + slack(y) + ", slackX(" + y + ") = " + slackX(y))
        } // for
    } // findRootSetSlack

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reverse the edges along the augmenting path starting with the given edge.
     * @param  edge  the given edge
     */
    private def reverseEdges (edge: Tuple2 [Int, Int], prev: Array [Int])
    {
        var e  = edge
        var ty = NA
        while (e._1 != NO) {
            ty       = xy(e._1)
            yx(e._2) = e._1
            xy(e._1) = e._2
            e        = (prev(e._1), ty)
        } // while
    } // reverseEdges

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A recursive procedure to find augmenting paths to improve the assignments.
     *  Terminate when nMatch == maxMatch.
     */
    private def augment ()
    {
        println ("augment: nMatch = " + nMatch + " need " + maxMatch)
        val xSet = Array.fill (m) { false }    // initialize xSet to empty
        val ySet = Array.fill (n) { false }    // initialize ySet to empty
        val prev = Array.fill (m) { NA }       // initialize prev to NA (for alternating tree)
        var edge = (NA, NA)                    // edge for augmenting path
        var x    = NA                          // current x-node

        findRootSetSlack (prev, xSet)
    
        breakable { while (true) {                     // main loop
            while (! qu.isEmpty ()) {                  // building tree with BFS cycle
                x = qu.poll ()                         // get current x-node from queue
                for (y <- r_n if cost(x, y) == lx(x) + ly(y) && ! ySet(y)) {
                    if (yx(y) == NA) { edge = (x, y); break }  // exposed x-node => augmenting path exists
                    ySet(y) = true                     // else just add y to ySet
                    qu.add (yx(y))                     // add x-node yx(y) matched with y to the queue
                    addToTree (yx(y), x, prev, xSet)   // add edges (x, y) and (y, yx(y)) to the tree
                } // for
            } // while

            updateLabels (xSet, ySet)                  // augmenting path not found, so improve labeling
            qu.clear ()                                // empty the queue

            for (y <- r_n  if ! ySet(y) && slack(y) == 0) {
                if (yx(y) == NA) {                     // exposed x-node => augmenting path exists
                    x = slackX(y)
                    edge = (x, y); break
                } else {
                    ySet(y) = true                     // else just add y to ySet,
                    if (! xSet(yx(y))) {
                        qu.add (yx(y))                 // add node yx(y) matched with y to the queue
                        addToTree (yx(y), slackX(y), prev, xSet)  // add edges (x, y) and (y, yx(y)) to the tree
                    } // if
                } // if
            } // for
        }} // while

        reverseEdges (edge, prev)              // reverse edges along augmenting path
        nMatch += 1                            // increment number of nodes in matching
        if (nMatch < maxMatch) augment ()      // try to find another augmenting path
    } // augment

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The main procedure to solve an assignment problem by finding initial pairings
     *  and then finding augmenting paths to improve the pairings/assignments.
     */
    private def solve (): Double =
    {
        if (m > n) { println ("Hungarian: error - m = " + m + " > n = " + n); return -1.0 }
        initLabels ()   // initial the cost labels for x-nodes
        augment ()      // recursive method the find augmenting paths

        println ("------------------------------------")
        var total = 0.0                 // cost/weight of the optimal matching
        for (x <- r_m) {                // form answer -
            total += cost(x, xy(x))     // using values from x-side
            println ("cost (" + x + ", " + xy(x) + ") = " + cost(x, xy(x)))
        } // for
        println ("------------------------------------")
        return total
    } // solve

} // Hungarian class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the Hungarian class.
 */
object Hungarian extends App
{
    // m = 3 workers, n = 3 jobs
    val cost1 = new MatrixD ((3, 3), 1, 4, 5,
                                     5, 7, 6,
                                     5, 8, 8)

    // m = 5 workers, n = 5 jobs
    val cost2 = new MatrixD ((5, 5), 10, 19,  8, 15, 19,
                                     10, 18,  7, 17, 19,
                                     13, 16,  9, 14, 19,
                                     12, 19,  8, 18, 19,
                                     14, 17, 10, 19, 19)

    // m = 3 workers, n = 4 jobs
    val cost3 = new MatrixD ((3, 4), 1, 4, 5, 2,
                                     5, 7, 6, 2,
                                     5, 8, 8, 9)

    // m = 4 workers, n = 3 jobs => error, not enough jobs for workers
    val cost4 = new MatrixD ((4, 3), 1, 4, 5,
                                     5, 7, 6,
                                     2, 2, 9,
                                     5, 8, 8)

    println ("optimal cost1 = " + (new Hungarian (cost1).solve ()))
    println ("optimal cost2 = " + (new Hungarian (cost2).solve ()))
    println ("optimal cost3 = " + (new Hungarian (cost3).solve ()))
    println ("optimal cost4 = " + (new Hungarian (cost4).solve ()))

} // Hungarian object

