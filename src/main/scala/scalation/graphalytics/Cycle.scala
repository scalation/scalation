
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed Jul 31 13:54:40 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cycle` class provides a means for building a precedence/directed graph
 *  and checking it for cycles.  For cycle detection, vertices are marked with
 *  traffic-light colors:
 *    - Green means go/unexplored,
 *    - Yellow means caution/been there before,
 *    - Red mean stop/already fully explored.
 *  @param g  the graph in which to check for cycles
 */
class Cycle (g: Graph)
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

} // Cycle class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CycleTest` object tests the `Cycle` class using a label-free precedence
 *  graph.  Graphs are created by passing in an array of adjacency sets (one for
 *  each vertex).
 */
object CycleTest extends App
{
    /** Test precedence graph 1 (does not have a cycle)
     */
    val pg1 = new Graph (Array (Set (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                Set (2),           // edges from 1:  1 -> 2
                                Set [Int] ()))     // edges from 2:  no such edges
    println ("Precedence Graph pg1: ----------------------------------------------")
    pg1.print ()
    println ("pg1 has cycle? = " + (new Cycle (pg1).hasCycle))
    
    /** Test precedence graph 2 (has a cycle)
     */
    val pg2 = new Graph (Array (Set (1, 2),        // edges from 0:  0 -> 1, 0 -> 2
                                Set (2),           // edges from 1:  1 -> 2
                                Set (0)))          // edges form 2:  2 -> 0
    println ("Precedence Graph pg2: ----------------------------------------------")
    pg2.print ()
    println ("pg2 has cycle? = " + (new Cycle (pg2).hasCycle))
    
} // CycleTest object

