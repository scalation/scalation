
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz, Aravind Kalimurthy
 *  @version 1.4
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  `MGraph` Dual Simulation Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2` class provides a second implementation for Dual Graph Simulation.
 *  It differs from `DualSim` by not using inverse adjacency sets ('pa') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MDualSim2 [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    private val DEBUG   = false                                   // debug flag
    private val NO_EDGE = false                                   // ignore edge labels

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

            for (u <- qRange; u_c <- q.ch(u)) {                   // for each u in q and its children u_
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val newPhi     = SET [Int] ()                         // subset of phi(u_c) having a parent in phi(u)
                val elab_u2u_c = q.elabel ((u, u_c))              // edge label in q for (u, u_c)

                for (v <- phi(u)) {                               // for each v in g image of u
                    val v_c = if (NO_EDGE) g.ch(v)                                          // don't filter on edge labels
                              else         g.ch(v).filter (elab_u2u_c == g.elabel (v, _))   // filter on edge labels
                    if (DEBUG) println (s"v = $v, v_c = $v_c, phi_u_c = " + phi(u_c))

                    val phiInt = v_c & phi(u_c)                   // children of v contained in phi(u_c)
                    if (phiInt.isEmpty) {
                        phi(u) -= v                               // remove vertex v from phi(u)
                        if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                        alter = true
                    } // if
                    // build newPhi to contain only those vertices in phi(u_c) which also have a parent in phi(u)
                    newPhi ++= phiInt
                } // for

                if (newPhi.isEmpty) return phi                     // empty newPhi => no match
                if (newPhi.size < phi(u_c).size) alter = true      // since newPhi is smaller than phi(u_c)

                if (SELF_LOOPS && u_c == u) phi(u_c) &= newPhi else phi(u_c) = newPhi
            } // for

        } // while
        phi
    } // prune

} // MDualSim2 class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2Test` object is used to test the `MDualSim2` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.MDualSim2Test
 */
object MDualSim2Test extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSim2 (g, q)).test ("MDualSim2")    // Dual Graph Simulation Pattern Matcher

} // MDualSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2Test2` object is used to test the `MDualSim2` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.MDualSim2Test2
 */
object MDualSim2Test2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSim2 (g, q)).test ("MDualSim2")    // Dual Graph Simulation Pattern Matcher

} // MDualSim2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2Test3` object is used to test the `MDualSim2` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > run-main scalation.graph_db.pattern_matching.MDualSim2Test3
 */
object MDualSim2Test3 extends App
{
    val (g, q) = (EX_GRAPH.g3p, EX_GRAPH.q3p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSim2 (g, q)).test ("MDualSim2")    // Dual Graph Simulation Pattern Matcher

} // MDualSim2Test3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2Test4` object is used to test the `MDualSim2` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.MDualSim2Test4
 */
object MDualSim2Test4 extends App
{
    val (g, q) = MGraphGen.genGraphs ("0.0")
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

    (new MDualSim2 (g, q)).test ("MDualSim2")    // Dual Graph Simulation Pattern Matcher

} // MDualSim2Test4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2Test5` object is used to test the `MDualSim2` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.MDualSim2Test5
 */
object MDualSim2Test5 extends App
{
    val (g, q) = MGraphGen.genPowerGraphs ("0.0")
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

    (new MDualSim2 (g, q)).test ("MDualSim2")    // Dual Graph Simulation Pattern Matcher

} // MDualSim2Test5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim2Test6` object is used to test the `MDualSim2` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.MDualSim2Test6
 */
object MDualSim2Test6 extends App
{
    val (g, q) = (MGraphIO [Double] ("gfile"), MGraphIO [Double] ("qfile"))
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

    (new MDualSim2 (g, q)).test ("MDualSim2")    // Dual Graph Simulation Pattern Matcher

} // MDualSim2Test6 object

