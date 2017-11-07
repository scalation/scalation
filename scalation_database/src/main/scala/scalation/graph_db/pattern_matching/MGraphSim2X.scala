
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz, Aravind Kalimurthy
 *  @version 1.4
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  `MGraph` Graph Simulation Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.reflect.ClassTag

import scalation.util.banner

import LabelVer.version 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2X` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from `GraphSim` in the looping order in the main for-loop
 *  and early termination when 'phi(u)' is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MGraphSim2X [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    private val DEBUG = false                                      // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                           // check for matching children
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {                   // for each u in q and its children u_c
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val elab_u2u_c = q.elabel ((u, u_c)).asInstanceOf [String]  // edge label in q for (u, u_c)
                val phi_u_c = phi(u_c)
                val v_rem   = SET [Int] ()                        // vertices to be removed
                val (labVer, elab_u2u_cX) = version (elab_u2u_c)

                for (v <- phi(u)) {                               // for each v in g image of u
                    val v_c = labVer match {
                        case LabelVer.Normal => g.ch(v).filter (elab_u2u_c   == g.elabel (v, _))
                        case _               => g.ch(v).filter (elab_u2u_cX =~ g.elabel (v, _).asInstanceOf [String])
                    } // match

                    if (DEBUG) println (s"v = $v, v_c = $v_c, phi_u_c = $phi_u_c")

                    if (disjoint (v_c, phi_u_c)) {                // v must have a child in phi(u_c)
                        v_rem += v                                // add to removal set
                        alter = true
                    } // if
                } // for

                if (! v_rem.isEmpty) {
                    if (DEBUG) println (s"v_rem = $v_rem from phi($u)")
                    phi(u) --= v_rem                              // remove vertices in v_rem from phi(u)
                    v_rem.clear ()
                    if (phi(u).isEmpty) return phi                // no match for vertex u => no overall match
                } // if
            } // for

        } // while
        phi
    } // prune

} // MGraphSim2X class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2XTest` object is used to test the `MGraphSim2X` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.MGraphSim2XTest
 */
object MGraphSim2XTest extends App
{
    val (g, q) = (EX_GRAPH.g1, EX_GRAPH.q1)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSim2X (g, q)).test ("MGraphSim2X")    // Graph Simulation Pattern Matcher

} // MGraphSim2XTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2XTest2` object is used to test the `MGraphSim2X` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.MGraphSim2XTest2
 */
object MGraphSim2XTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q2)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSim2X (g, q)).test ("MGraphSim2X")    // Graph Simulation Pattern Matcher

} // MGraphSim2XTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2XTest3` object is used to test the `MGraphSim2X` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > run-main scalation.graph_db.pattern_matching.MGraphSim2XTest3
 */
object MGraphSim2XTest3 extends App
{
    val (g, q) = (EX_GRAPH.g3, EX_GRAPH.q3)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSim2X (g, q)).test ("MGraphSim2X")    // Graph Simulation Pattern Matcher

} // MGraphSim2XTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2XTest4` object is used to test the `MGraphSim2X` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.MGraphSim2XTest4
 */
object MGraphSim2XTest4 extends App
{
    val (g, q) = MGraphGen.genGraphs ("0.0")

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MGraphSim2X (g, q)).test ("MGraphSim2X")    // Graph Simulation Pattern Matcher

} // MGraphSim2XTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2XTest5` object is used to test the `MGraphSim2X` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.MGraphSim2XTest5
 */
object MGraphSim2XTest5 extends App
{
    val (g, q) = MGraphGen.genPowerGraphs ("0.0")

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MGraphSim2X (g, q)).test ("MGraphSim2X")    // Graph Simulation Pattern Matcher

} // MGraphSim2XTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim2XTest6` object is used to test the `MGraphSim2X` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.MGraphSim2XTest6
 */
object MGraphSim2XTest6 extends App
{
    val (g, q) = (MGraphIO [Double] ("gfile"), MGraphIO [Double] ("qfile"))

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MGraphSim2X (g, q)).test ("MGraphSim2X")    // Graph Simulation Pattern Matcher

} // MGraphSim2XTest6 object


