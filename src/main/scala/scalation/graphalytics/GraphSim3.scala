
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.1
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.ArrayBuffer

import scalation.util.Timer.time

import Graph2Types._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim3` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from GraphSim in the looping order in the main for-loop
 *  and early termination when phi(u) is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim3 (g: Graph2, q: Graph2)
      extends PatternMatcher2 (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [AList] = saltzGraphSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Graph Simulation by itself can not produce bijections.
     */
    def bijections (): Set [Array [Int]] =
    {
        throw new UnsupportedOperationException ()
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def saltzGraphSim (phi: Array [AList]): Array [AList] =
    {
        var alter = true
        while (alter) {                                           // check for matching children
            alter = false

            // loop over query vertices u, u's children u_c, and data vertices v in phi(u)

            for (u <- qRange; u_c <- q.adj(u); v <- phi(u)) {
                if ((g.adj(v) intersect phi(u_c)).isEmpty) {      // v must have a child in phi(u_c)
                    phi(u) -= v                                   // remove vertex v from phi(u)
                    if (phi(u).isEmpty) return Array [AList] ()   // no match for vertex u => no overall match
                    alter = true
                } // if
            } // for

        } // while
        phi
    } // saltzGraphSim

} // GraphSim3 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim3Test` object is used to test the `GraphSim3` class.
 */
object GraphSim3Test extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGenerator2.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGenerator2.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new GraphSim3 (g, q)                     // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    for (i <- phi.indices) println ("u_" + i + ": " + phi(i))

} // GraphSim3Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim3Test2` object is used to test the `GraphSim3` class.
 */
object GraphSim3Test2 extends App
{
    val q = Graph2 (Array (ArrayBuffer (1),
                           ArrayBuffer (0)),
                    Array (0, 1))

    val g = Graph2 (Array (ArrayBuffer (1),
                           ArrayBuffer (0),
                           ArrayBuffer (0),
                           ArrayBuffer (2)),
                    Array (0, 1, 1, 0))

    q.print
    g.print

    val matcher = new GraphSim3 (g, q)                     // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    for (i <- phi.indices) println ("u_" + i + ": " + phi(i))

} // GraphSim3Test2 object

