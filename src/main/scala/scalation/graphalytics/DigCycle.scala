
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Jul 31 13:54:40 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.mutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigCycle` class provides a means for building a precedence/directed graph
 *  and checking it for cycles.  For cycle detection, vertices are marked with
 *  traffic-light colors:
 *    - Green means go/unexplored,
 *    - Yellow means caution/been there before,
 *    - Red mean stop/already fully explored.
 *  @param g  the graph in which to check for cycles
 */
case class DigCycle (g: Digraph)
{
    /** vertices are marked with traffic-light colors ('G'reen, 'Y'ellow, 'R'ed)
     */
    private val color = Array.ofDim [Char] (g.size)

    for (i <- color.indices) color(i) = 'G'        // initialize colors to Green

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the graph contains a cycle.
     */
    def hasCycle: Boolean = 
    {
       for (i <- color.indices if color(i) == 'G' && loopback (i)) return true 
       false
    } // hasCycle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Search the decendents of vertex 'i' to see if there is a loopback.
     *  @param i  the vertex where the search starts
     */
    private def loopback (i: Int): Boolean =
    {
        if (color(i) == 'Y') return true
        color(i) = 'Y'
        for (j <- g.ch(i) if color(j) != 'R' && loopback (j)) return true
        color(i) = 'R'
        false
    } // loopback

} // DigCycle class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigCycleTest` object tests the `DigCycle` class using a label-free precedence
 *  graph.  Digraphs are created by passing in an array of adjacency sets (one for
 *  each vertex).
 *  > run-main scalation.graphalytics.DigCycleTest
 */
object DigCycleTest extends App
{
    /** Test precedence graph 1 (does not have a cycle)
     */
    val pg1 = new Digraph (Array (SET (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                  SET (2),           // edges from 1:  1 -> 2
                                  SET [Int] ()))     // edges from 2:  no such edges
    println ("Precedence Digraph pg1: --------------------------------------------")
    pg1.print ()
    println ("pg1 has cycle? = " + DigCycle (pg1).hasCycle)
    
    /** Test precedence graph 2 (has a cycle)
     */
    val pg2 = new Digraph (Array (SET (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                  SET (2),           // edges from 1:  1 -> 2
                                  SET (0)))          // edges form 2:  2 -> 0
    println ("Precedence Diraph pg2: --------------------------------------------")
    pg2.print ()
    println ("pg2 has cycle? = " + DigCycle (pg2).hasCycle)
    
} // DigCycleTest object

