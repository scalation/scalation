
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.6
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Dual Simulation Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2` class provides an implementation for Dual Graph Simulation
 *  using `Graph` and is based on `DualSim22`, which differs from `DualSim2`
 *  by not using inverse adjacency sets ('pa') in order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim2 [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    private val DEBUG = false                                       // debug flag

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
                val newPhi = SET [Int] ()                         // subset of phi(u_c) having a parent in phi(u)
                val v_rem   = SET [Int] ()                        // vertices to be removed

                for (v <- phi(u)) {                               // for each v in g image of u
                    val phiInt = g.ch(v) & phi(u_c)               // children of v contained in phi(u_c)
                    if (phiInt.isEmpty) {
                        v_rem += v                                // add v to removal set
                        alter = true
                        if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                    } // if
                    // build newPhi to contain only those vertices in phi(u_c) which also have a parent in phi(u)
                    newPhi ++= phiInt
                } // for

                if (! v_rem.isEmpty) {
                    phi(u) --= v_rem                          // remove vertices in v_rem from phi(u)
                    if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                } // if

                if (newPhi.isEmpty) return phi                    // empty newPhi => no match
                if (newPhi.size < phi(u_c).size) alter = true     // since newPhi is smaller than phi(u_c)

                if (SELF_LOOPS && u_c == u) phi(u_c) &= newPhi else phi(u_c) = newPhi
            } // for

        } // while
        phi
    } // prune

} // DualSim2 class

import scalation.graph_db.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test` object is used to test the `DualSim2` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > runMain scalation.graph_db.pattern_matching.DualSim2Test
 */
object DualSim2Test extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test2` object is used to test the `DualSim2` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.DualSim2Test2
 */
object DualSim2Test2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test3` object is used to test the `DualSim2` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.DualSim2Test3
 */
object DualSim2Test3 extends App
{
    val (g, q) = GraphGen.genGraphs ("0.0")
    g.addPar()
    q.addPar()

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test4` object is used to test the `DualSim2` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.DualSim2Test4
 */
object DualSim2Test4 extends App
{
    val (g, q) = GraphGen.genPowerGraphs ("0.0")
    g.addPar()
    q.addPar()

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim2Test5` object is used to test the `DualSim2` class.
 *  This object tests graphs read from files.
 *  > runMain scalation.graph_db.pattern_matching.DualSim2Test5
 */
object DualSim2Test5 extends App
{
    val (g, q) = (GraphIO [Double] ("gfile"), GraphIO [Double] ("qfile"))
    g.addPar()
    q.addPar()

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new DualSim2 (g, q)).test ("DualSim2")    // Dual Graph Simulation Pattern Matcher

} // DualSim2Test5 object

