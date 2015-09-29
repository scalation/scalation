
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Thu Jul  9 14:47:27 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Depth First Search (DFS)
 *  Breadth First Search (BFS)
 */

package scalation.graphalytics

import collection.mutable.{ArrayStack, Queue}
import collection.mutable.{Set => SET}

import LabelType.{TLabel, TLabel_DEFAULT}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigDFS` performs Depth First Search (DFS) or Breadth First Search (BFS)
 *  on a Directed Graph.  The class currently supports three predicates:
 *      (1) to find a matching label, and
 *      (2) to see if a destination vertex is reachable.
 *  @param g    the directed graph to search
 *  @param bfs  switch from DFS to BFS, if bfs flag is true (defaults to false)
 */
class DigDFS (g: Digraph, bfs: Boolean = false)
{
    type STACK = ArrayStack [Int]
    type QUEUE = Queue [Int]

    private val DEBUG = false                                     // debug flag
    private val go    = Array.ofDim [Boolean] (g.size)            // go (unvisited) flags
    private var qu    = if (bfs) new STACK ()                     // vertex FIFO queue for BFS
                        else     new QUEUE ()                     // or stack (LIFO queue) for DFS
    private var lab   = TLabel_DEFAULT                            // label to find
    private var dest  = -1                                        // destination vertex to reach

    def pred1 (j: Int): Boolean = g.label(j) == lab               // predicate to find label
    def pred2 (j: Int): Boolean = j == dest                       // predicate to reach destination vertex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the label 'lab' in 'this' graph, returning the first vertex id with
     *  a matching label, or -1 if vertex is not found.
     *  @param _lab  the label to search for
     */
    def find (_lab: TLabel): Int =
    {
        lab = _lab                                                // assign field for pred1 closure
        for (i <- 0 until g.size) go(i) = true                    // set go flags to true

        for (i <- 0 until g.size if go(i)) {
            qu += i                                               // put unvisited vertex in queue
            val res = visit (pred1)                               // visit vertices in DFS/BFS order
            if (res >= 0) { qu.clear; return res }                // return vertex where found
        } // for
        qu.clear; -1                                              // not found
    } // find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether vertex '_dest' is reachable from 'src', i.e., there is
     *  a directed path from vertex 'src' to '_dest'.
     *  @param src    the source/starting vertex
     *  @param _dest  the destination/ending vertex
     */
    def reach (src: Int, _dest: Int): Boolean =
    {
        dest = _dest                                              // assign field for pred2 closure
        for (i <- 0 until g.size) go(i) = true                    // set go flags to true

        qu += src                                                 // put source vertex in queue
        val res = visit (pred2)                                   // visit vertices in DFS/BFS order
        if (res >= 0) { qu.clear; return true }                   // return true, if dest vertex found
        qu.clear; false                                           // not found
    } // reach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of weakly connected components in the graph.
     *  In a weakly connected component, the vertices in the underlying
     *  undirected graph are connected.
     */
    def weakComps: Int =
    {
        for (i <- 0 until g.size) go(i) = true                    // set go flags to true
        var count = 0

        for (i <- 0 until g.size if go(i)) {
            qu += i                                               // put unvisited vertex in queue
            visit ()                                              // visit vertices in DFS/BFS order
            count += 1                                            // increment component count
        } // for
        qu.clear; count                                           // return strongly connected component count
    } // weakComps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strongly connected components in the graph.
     *  In a strongly connected component, 'reach (u, v)' is always true.
     */
    def strongComps: Int =
    {
        0                          // FIX - to be implemented
    } // strongComps:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit vertices in DFS/BFS order, returning the id of the first vertex
     *  satisfying the predicate 'pred', or -1 if vertex is not found.
     *  @param pred  the predicate to satisfy
     */
    def visit (pred: Int => Boolean): Int =
    {
        var j = 0
        while (qu.nonEmpty) {
            if (bfs) {
                j = qu.asInstanceOf [STACK].pop ()                    // take next go vertex from LIFO queue
            } else {
                j = qu.asInstanceOf [QUEUE].dequeue ()                // take next go vertex from FIFO queue
            } // if

            if (go(j)) {
                if (DEBUG) println (s"visit: label($j) = ${pred(j)}")
                if (pred (j)) return j                                // return vertex, if predicate satisfied
                go(j) = false                                         // mark as visited
                for (c <- g.ch(j) if go(c)) qu += c                   // put unvisited children in queue
            } // if
        } // while
        -1                                                            // not found
    } // visit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit vertices in DFS/BFS order accessing children and parents (as
     *  if it is an undirected graph).
     *  @param pred  the predicate to satisfy
     */
    def visit ()
    {
        if (g.pa == null) g.addPar                                    // make sure parent references exist
        var j = 0
        while (qu.nonEmpty) {
            if (bfs) {
                j = qu.asInstanceOf [STACK].pop ()                    // take next go vertex from LIFO queue
            } else {
                j = qu.asInstanceOf [QUEUE].dequeue ()                // take next go vertex from FIFO queue
            } // if

            if (go(j)) {
                if (DEBUG) println (s"visit: label($j)")
                go(j) = false                                         // mark as visited
                for (c <- g.ch(j) if go(c)) qu += c                   // put unvisited children in queue
                for (p <- g.ch(j) if go(p)) qu += p                   // put unvisited parent in queue
            } // if
        } // while
    } // visit

} // DigDFS class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigDFSTest` is used to test the `DigDFS` class.
 *  > run-main scalation.graphalytics.DigDFSTest
 */
object DigDFSTest extends App
{
    val g = new Digraph (Array (SET (4, 5),                        // 0        // ch
                                SET (5),                           // 1
                                SET (6, 7),                        // 2
                                SET (7, 8),                        // 3
                                SET (0, 5, 9),                     // 4
                                SET (0, 1, 4, 6, 10),              // 5
                                SET (2, 5, 7, 10, 11),             // 6
                                SET (2, 3, 6, 8),                  // 7
                                SET (3, 7, 12),                    // 8
                                SET (4),                           // 9
                                SET (5, 6),                        // 10
                                SET (6),                           // 11
                                SET [Int] ()),                     // 12
//                              SET (8)),                          // 12
                         Array (1, 2, 3, 4 , 5, 6, 13, 12, 11, 10, 9, 8, 7))   // labels

    g.print ()
    println ("Test DFS -----------------------------------------------------")
    test (new DigDFS (g))                                          // test DFS
    println ("Test BFS -----------------------------------------------------")
    test (new DigDFS (g, true))                                    // test BFS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the 'find', 'reach' and 'strongComps' methods.
     *  @param gs  the graph search to test
     */
    def test (gs: DigDFS)
    {
        println ("Test find method")
        for (lab <- 0 to 14) {
            println (s"find ($lab)  = ${gs.find (lab)}")           // find (lab)
        } // for

        println ("Test reach method")
        for (i <- 0 until g.size; j <- 0 until g.size) {
            println (s"reach ($i, $j) = ${gs.reach (i, j)}")       // reach (i, j)
        } // for

        println ("Test weakComps method")
        println (s"weakComps = ${gs.weakComps}")                   // weakComps

        println ("Test strongComps method")
        println (s"strongComps = ${gs.strongComps}")               // strongComps
    } // test

} // DigDFSTest object

