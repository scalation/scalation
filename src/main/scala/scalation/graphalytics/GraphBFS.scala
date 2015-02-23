
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.1
 *  @date    Mon Nov 11 19:03:45 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.{ArrayBuffer, Queue}

import Graph2Types._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphBFS` performs Breadth First Search on a Directed Graph.
 *  @param g  the directed graph to search
 */
class GraphBFS (g: Graph2)
{
    private val qu = new Queue [Int] ()                 // vertex queue
    private val go = Array.ofDim [Boolean] (g.size)     // go flags

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Search for the label 'lab' in this graph.
     *  @param lab  the label to search for
     */
    def search (lab: TLabel): Int =
    {
        for (i <- 0 until g.size) go(i) = true
        for (i <- 0 until g.size if go(i)) {
            qu.enqueue (i)
            val res = visit (lab)
            if (res >= 0) return res
        } // for
        -1
    } // search

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit the next vertex and check its label.
     *  @param lab  the label to search for
     */
   private def visit (lab: TLabel): Int =
   {
       val j = qu.dequeue ()                          // take next vertex from queue
       go(j) = false                                  // mark as visited
       println ("label (" + j + ") = " + g.label (j))
       if (g.label(j) == lab) return j                // found label?
       for (c <- g.adj(j) if go(j)) qu.enqueue (c)    // put children in queue
       -1                                             // not found
   } // visit

} // GraphBFS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphBFSTest` is used to test the `GraphBFS` class.
 */
object GraphBFSTest extends App
{
    val g = Graph2 (Array (ArrayBuffer (4, 5),                // 0           // adj
                           ArrayBuffer (5),                   // 1
                           ArrayBuffer (6, 7),                // 2
                           ArrayBuffer (7, 8),                // 3
                           ArrayBuffer (0, 5, 9),             // 4
                           ArrayBuffer (0, 1, 4, 6, 10),      // 5
                           ArrayBuffer (2, 5, 7, 10, 11),     // 6
                           ArrayBuffer (2, 3, 6, 8),          // 7
                           ArrayBuffer (3, 7, 12),            // 8
                           ArrayBuffer (4),                   // 9
                           ArrayBuffer (5, 6),                // 10
                           ArrayBuffer (6),                   // 11
                           ArrayBuffer (8)),                  // 12
                    Array (1, 2, 3, 4 , 5, 6, 13, 12, 11, 10, 9, 8, 7))      // labels

    g.print

    val bfs = new GraphBFS (g)

    val lab = 12                                                                 // find label lab
    println ("search (" + lab + ") = " + bfs.search (lab))

} // GraphBFSTest object

