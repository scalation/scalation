
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
 *  @version 1.3
 *  @date    Thu Nov 19 18:43:58 EST 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

import TrafficLight._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TopSort` object provides the 'topSort' method for creating a
 *  topological sort of the vertices in a directed graph.  It also perform
 *  cycle detection.
 */
object TopSort
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Topological sort that returns an edge compatible ordering of the vertices.
     *  Translated from pseudo-code and implements Tarjan's algorithm.
     *  The topological sort will contain negative values, if there is a cycle.
     *  @see en.wikipedia.org/wiki/Topological_sorting
     *  @param g  the graph
     */
    def topSort (g: Graph): Array [Int] =
    {
        val n       = g.size                                // the number of vertices in g
        val color   = Array.fill (n)(G_N)                   // traffic light: GreeN, YelloW or ReD
        val vList   = Array.fill (n)(-1)                    // ordered list of vertices
        var last    = n - 1                                 // last open position in vList
        var acyclic = true                                  // assume acyclic until cycle detected

        for (v <- color.indices if acyclic && color(v) == G_N) dfs (v)

        /*  Recursively visit vertices, adding vertices onto a list at the end
         *  @param u  the current vertex
         */
        def dfs (u: Int)
        {
            if (acyclic) {
                if (color(u) == Y_W) {
                    vList(last) = -2
                    acyclic = false                                         // detected cycle
                } else if (color(u) == G_N) {
                    color(u) = Y_W
                    for (v <- g.ch(u)) dfs (v)
                    color(u) = R_D
                    if (vList(last) != -2) { vList(last) = u; last -= 1 }   // prepend to front of list
                } // if
            } // if
        } // dfs

        vList
    } // topSort

} // TopSort object

import TopSort.topSort

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TopSortTest` object tests the `TopSort` object using a directed
 *  graph.  Graphs are created by passing in an array of adjacency sets (one for
 *  each vertex).
 *  > run-main scalation.graphalytics.TopSortTest
 */
object TopSortTest extends App
{
    /** Test graph 1
     */
    val pg1 = new Graph (Array (SET (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                SET (2),           // edges from 1:  1 -> 2
                                SET [Int] ()))     // edges from 2:  no such edges
    println ("Precedence Graph pg1: --------------------------------------------")
    pg1.printG ()
    println ("pg1 order = " + topSort (pg1).deep)
    
    /** Test graph 2
     */
    val pg2 = new Graph (Array (SET (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                SET (2),           // edges from 1:  1 -> 2
                                SET (0)))          // edges form 2:  2 -> 0
    println ("Precedence Digraph pg2: --------------------------------------------")
    pg2.printG ()
    println ("pg2 order = " + topSort (pg2).deep)
    
} // TopSortTest object

