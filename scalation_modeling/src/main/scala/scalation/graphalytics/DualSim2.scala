
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.3
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Dual Simulation Using Immutable Sets
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2` class provides a second implementation for Dual Graph Simulation.
 *  It differs from `DualSim` by not using inverse adjacency sets ('pa') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim2 (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    private val DEBUG = true                                      // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                           // check for matching children/parents
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {                   // for each u in q and its children u_c  
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                var newPhi = SET [Int] ()                         // subset of phi(u_c) having a parent in phi(u)

                for (v <- phi(u)) {                               // data vertex v matching u's label
                    val phiInt = g.ch(v) & phi(u_c)               // children of v contained in phi(u_c)
                    if (phiInt.isEmpty) {
                        phi(u) -= v                               // remove vertex v from phi(u)
                        if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                        alter = true
                    } // if
                    // build newPhi to contain only those vertices in phi(u_c) which also have a parent in phi(u)
                    newPhi ++= phiInt
                } // for

                if (newPhi.isEmpty) return phi                    // empty newPhi => no match
                if (newPhi.size < phi(u_c).size) alter = true     // since newPhi is smaller than phi(u_c)

                if (SELF_LOOPS && u_c == u) phi(u_c) &= newPhi else phi(u_c) = newPhi
            } // for

        } // while
        phi
    } // prune

} // DualSim2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test` object is used to test the `DualSim2` class.
 *  > run-main scalation.graphalytics.DualSim2Test
 */
object DualSim2Test extends App
{
    val g = Graph.g1
    val q = Graph.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test2` object is used to test the `DualSim2` class.
 *  > run-main scalation.graphalytics.DualSim2Test2
 */
object DualSim2Test2 extends App
{
    val g = Graph.g2
    val q = Graph.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test3` object is used to test the 'DualSim2' class.
 *  > run-main scalation.graphalytics.DualSim2Test3
 */
object DualSim2Test3 extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g, false, "q")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test3 object

