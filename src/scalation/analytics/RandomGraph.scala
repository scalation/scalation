
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Wed Jun  6 14:30:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import math.abs

import scalation.linalgebra.MatrixD
import scalation.random.Bernoulli
import scalation.random.Randi0

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Random undirected graph generator with clusters (as an adjacency matrix).
 *  @param n  the number of nodes in the graph
 *  @param p  the probability that any two nodes are connected
 *  @param c  the number of clusters to generate
 */
class RandomGraph (n: Int, p: Double, c: Int)
{
    private val g    = new MatrixD (n, n)     // adjacency matrix representation of graph
    private val coin = Bernoulli (p)          // a baised coin (p = probability of head(1))
    private val pick = Randi0 (n-1)           // random integer generator: 0, ..., n-1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a cluster random graph returning its adjacency matrix.
     */
    def gen (): MatrixD =
    {
        for (i <- 0 until g.dim1; j <- 0 until g.dim2 if i > j) {
            g(i, j) = coin.gen; g(j, i) = g(i, j)
        } // for
        for (k <- 0 until c) {
            val ic = pick.igen
            var jc = 0
            do jc = pick.igen while (abs (jc - ic) < 3)
            println ("cluster " + k + " at (" + ic, ", " + jc + ")")
            g(ic, jc) = 1.
            if (ic > 0)   { g(ic-1, jc) = 1. - coin.gen; g(jc, ic-1) = g(ic-1, jc) }
            if (ic > 1)   { g(ic-2, jc) = 1. - coin.gen; g(jc, ic-2) = g(ic-2, jc) }
            if (ic < n-1) { g(ic+1, jc) = 1. - coin.gen; g(jc, ic+1) = g(ic+1, jc) }
            if (ic < n-2) { g(ic+2, jc) = 1. - coin.gen; g(jc, ic+2) = g(ic+2, jc) }
            if (jc > 0)   { g(ic, jc-1) = 1. - coin.gen; g(jc-1, ic) = g(ic, jc-1) }
            if (jc > 1)   { g(ic, jc-2) = 1. - coin.gen; g(jc-2, ic) = g(ic, jc-2) }
            if (jc < n-1) { g(ic, jc+1) = 1. - coin.gen; g(jc+1, ic) = g(ic, jc+1) }
            if (jc < n-2) { g(ic, jc+2) = 1. - coin.gen; g(jc+2, ic) = g(ic, jc+2) }
        } // for
        g
    } // gen

} // RandomGraph


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the RandomGraph class.
 */
object RandomGraphTest extends App
{
    val rg = new RandomGraph (20, .1, 5)
    println ("graph = " + rg.gen ())

} // RandomGraphTestTest

