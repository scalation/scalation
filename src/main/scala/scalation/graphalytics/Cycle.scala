
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Jul 31 13:54:40 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

import TrafficLight._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cycle` object provides a means for building a precedence/directed graph
 *  and checking it for cycles.  For cycle detection, vertices are marked with
 *  traffic-light colors:
 *    - GreeN means go/unexplored,
 *    - YelloW means caution/been there before,
 *    - ReD mean stop/already fully explored.
 */
object Cycle
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the graph contains a cycle.
     *  @param g  the graph in which to check for cycles
     */
    def hasCycle (g: Graph): Boolean = 
    {
        val color = Array.fill (g.size)(G_N)    // traffic light colors: GreeN, YelloW, ReD

        for (v <- color.indices if color(v) == G_N && loopback (v)) return true 

        /*  Search the descendants of vertex 'u' to see if there is a loopback.
         *  @param u  the vertex where the search starts
         */
        def loopback (u: Int): Boolean =
        {
            if (color(u) == Y_W) return true
            color(u) = Y_W
            for (v <- g.ch(u) if color(v) != R_D && loopback (v)) return true
            color(u) = R_D
            false
        } // loopback

       false
    } // hasCycle

} // Cycle object

import Cycle.hasCycle

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CycleTest` object tests the `Cycle` object using a label-free precedence
 *  graph.  Graphs are created by passing in an array of adjacency sets (one for
 *  each vertex).
 *  > run-main scalation.graphalytics.CycleTest
 */
object CycleTest extends App
{
    /** Test precedence graph 1 (does not have a cycle)
     */
    val pg1 = new Graph (Array (SET (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                SET (2),           // edges from 1:  1 -> 2
                                SET [Int] ()))     // edges from 2:  no such edges
    println ("Precedence Graph pg1: ----------------------------------------------")
    pg1.printG ()
    println ("pg1 has cycle? = " + hasCycle (pg1))
    
    /** Test precedence graph 2 (has a cycle)
     */
    val pg2 = new Graph (Array (SET (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                SET (2),           // edges from 1:  1 -> 2
                                SET (0)))          // edges form 2:  2 -> 0
    println ("Precedence Graph pg2: ----------------------------------------------")
    pg2.printG ()
    println ("pg2 has cycle? = " + hasCycle (pg2))
    
} // CycleTest object

