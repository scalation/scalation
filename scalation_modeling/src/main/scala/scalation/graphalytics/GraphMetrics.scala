
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.3
 *  @date    Mon Nov 11 19:03:45 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}
import scala.collection.mutable.{ArrayBuffer, Queue}

import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphMetrics` class provides methods for determining graph metrics that
 *  can be efficiently computed using Breadth-First Search (BFS).  This works for 
 *  undirected graphs.  If a directed graph is passed in, it will be converted to 
 *  a corresponding undirected graph.
 *  @param g            the graph whose metrics are sought
 *  @param isUndirected indicates whether the graph is undirected
 */
class GraphMetrics (val g: Graph, isUndirected: Boolean = true)
{
    private val DEBUG = false                        // debug flag
    private val n     = g.size                       // number of vertices in g
    private val qu    = new Queue [Int] ()           // a queue supporting BFS
    private val go    = Array.ofDim [Boolean] (n)    // vertex visitation flag
    private val len   = Array.ofDim [Int] (n)        // path-length from vertex i to j
    
    if (! isUndirected) {
        for (i <- 0 until n; j <- g.ch (i)) g.ch (j) += i    // converting directed to undirected
    } // if    

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the diameter of graph 'g' (i.e., maximum eccentricity).  This also
     *  equals the "longest shortest path" between any pair of vertices in graph 'g'.
     */
    def diam: Int =
    {
        var max = ecc (0)
        for (i <- 1 until n) { val ecc_i = ecc (i); if (ecc_i > max) max = ecc_i }
        max
    } // diam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the radius of graph 'g' (i.e., minimum eccentricity).
     */
    def rad: Int =
    {
        var min = ecc (0)
        for (i <- 1 until n) { val ecc_i = ecc (i); if (ecc_i < min) min = ecc_i }
        min
    } // rad

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the central vertices, those with eccentricities equal to the radius.
     */
    def central: Array [Int] =
    {
        val centr = ArrayBuffer [Int] ()
        val ecc_v = Array.ofDim [Int] (n)
        for (i <- 0 until n) ecc_v(i) = ecc (i)
        var rd = ecc_v(0)
        for (i <- 1 until n) if (ecc_v(i) < rd) rd = ecc_v(i)
        for (i <- 0 until n) if (ecc_v(i) == rd) centr += i
        centr.toArray
    } // central 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the eccentricity of vertex 'i' (the length of the longest path from
     *  vertex 'i' to any other vertex).
     *  @param i  the vertex whose eccentricity is sought
     */
    def ecc (i: Int): Int =
    {
        for (j <- 0 until n) { go(j) = true; len(j) = 0 }
        qu.enqueue (i)
        while (! qu.isEmpty) visit ()                        // visit vertices in BFS order
        var ecc_i = len(0)
        for (j <- 1 until n) if (len(j) > ecc_i) ecc_i = len(j)
        if (DEBUG) println ("ecc (" + i + ") = " + ecc_i)
        ecc_i                                                // return max path-length
    } // ecc

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit the next vertex (at the head of queue 'qu'), mark it, compute the
     *  path-length 'len' for each of its children and put them in the queue. 
     */
    private def visit ()
    {
        val j = qu.dequeue ()                            // take next vertex from queue
        go(j) = false                                    // mark as visited
        val len_c = len(j) + 1                           // path-length to child vertices
        if (DEBUG) println (" len (" + j + ") = " + len(j))
        for (c <- g.ch(j)) {                             // for each child of vertex j
            if (go(c) && len(c) == 0) {
                len(c) = len_c                           // distance from vertex i to c
                qu.enqueue (c)                           // put child c in queue
            } // if
        } // for
    } // visit

} // GraphMetrics


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphMetricsTest` object is used to test the `GraphMetrics` class.
 *  @see http://math.stackexchange.com/questions/240556/radius-diameter-and-center-of-graph
 */
object GraphMetricsTest extends App
{
//  val g = new Graph (Array (SET (4, 5),                // 0           // ch
//                            SET (5),                   // 1
//                            SET (6, 7),                // 2
//                            SET (7, 8),                // 3
//                            SET (0, 5, 9),             // 4
//                            SET (0, 1, 4, 6, 10),      // 5
//                            SET (2, 5, 7, 10, 11),     // 6
//                            SET (2, 3, 6, 8),          // 7
//                            SET (3, 7, 12),            // 8
//                            SET (4),                   // 9
//                            SET (5, 6),                // 10
//                            SET (6),                   // 11
//                            SET (8)))                  // 12   

    val g = new Graph (Array (SET (2, 1),                // 0           // ch
                              SET (3),                   // 1
                              SET (),                    // 2
                              SET ()),                   // 3
                       Array (1, 0, 0, 0))                

    g.printG ()
    val bfs = new GraphMetrics (g)

    // Compute the diameter of graph g
    var dia = 0
    for (k <- 0 until 10) {
        time { dia = bfs.diam }
        println ("diameter  = " + dia)
    } // for
    
    
    // Compute the radius of graph g
    var rd = 0
    for (k <- 0 until 10) {
        time { rd = bfs.rad }
        println ("radius  = " + rd)
    } // for

    // Return the central vertices of graph g
    var ctr: Array [Int] = null
    for (k <- 0 until 10) {
        time { ctr = bfs.central }
        println ("central  = " + ctr.deep)
    } // for

} // GraphMetricsTest object

