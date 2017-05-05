
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.3
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from `GraphSim` in the looping order in the main for-loop
 *  and early termination when 'phi(u)' is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim2 (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    private val DEBUG = true                    // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                          // check for matching children
            if (DEBUG) showMappings (phi)
            alter = false

            // loop over query vertices u, u's children u_c, and data vertices v in phi(u)

            for (u <- qRange; u_c <- q.ch(u); v <- phi(u)) {
                if (overlaps (g.ch(v),  phi(u_c))) {             // v must have a child in phi(u_c)
                    phi(u) -= v                                  // remove vertex v from phi(u)
                    if (phi(u).isEmpty) return phi               // no match for vertex u => no overall match
                    alter = true
                } // if
            } // for

        } // while
        phi
    } // saltzGraphSim

} // GraphSim2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test` object is used to test the `GraphSim2` class.
 *  > run-main scalation.graphalytics.GraphSim2Test
 */
object GraphSim2Test extends App
{
    val g = Graph.g1
    val q = Graph.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test2` object is used to test the `GraphSim2` class.
 *  > run-main scalation.graphalytics.GraphSim2Test2
 */
object GraphSim2Test2 extends App
{
    val g = Graph.g2
    val q = Graph.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test3` object is used to test the `GraphSim2` class.
 *  > run-main scalation.graphalytics.GraphSim2Test3
 */
object GraphSim2Test3 extends App
{
    val gSize     = 1000           // size of the data graph
    val qSize     =   10           // size of the query graph
    val nLabels   =  100           // number of distinct labels
    val gAvDegree =    5           // average vertex out degree for data graph
    val qAvDegree =    2           // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g, false, "q")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test3 object

