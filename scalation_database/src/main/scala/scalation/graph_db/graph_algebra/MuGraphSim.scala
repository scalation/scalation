
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.6
 *  @date    Tue Nov  1 19:12:16 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Graph 'MuGraph' Simulation Using Mutable Sets
 */

package scalation.graph_db.graph_algebra

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graph_db.pattern_matching.GraphMatcher

import MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphSim` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from `GraphSim` in the looping order in the main for-loop
 *  and early termination when 'phi(u)' is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MuGraphSim (g: MuGraph [Double], q: MuGraph [Double])
      extends GraphMatcher [Double] (g, q)
{
    private val DEBUG = true                                      // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                           // check for matching children
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {                   // for each u in q and its children u_c
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val elab_u2u_c = q.elabel ((u, u_c))              // edge label in q for (u, u_c)
                val phi_u_c = phi(u_c)
                val v_rem   = SET [Int] ()                        // vertices to be removed

                for (v <- phi(u)) {                               // for each v in g image of u
//                  val v_c = g.ch(v)                                                // don't filter on edge labels
//                  val v_c = g.ch(v).filter (elab_u2u_c == g.elabel (v, _))         // filter on edge labels with ==
                    val v_c = g.ch(v).filter (elab_u2u_c subsetOf g.elabel (v, _))   // filter on edge labels with subset
                    if (DEBUG) println (s"v = $v, v_c = $v_c, phi_u_c = $phi_u_c")

                    if ((v_c & phi_u_c).isEmpty) {                // v must have a child in phi(u_c)
//                      phi(u) -= v                               // if not, remove vertex v from phi(u)
//                      if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                        v_rem += v
                        alter = true
                    } // if
                    if (! v_rem.isEmpty) {
                        if (DEBUG) println (s"v_rem = $v_rem from phi($u)")
                        phi(u) --= v_rem                          // remove vertices in v_rem from phi(u)
                        v_rem.clear ()
                        if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                    } // if
                } // for

            } // for

        } // while
        phi
    } // prune

} // MuGraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphSimTest` object is used to test the `MuGraphSim` class.
 *  > runMain scalation.graphalytics.multi.MuGraphSimTest
 */
object MuGraphSimTest extends App
{
    val g = new MuGraph (Array (SET (1, 3),                      // ch(0)
                                SET (2),                         // ch(1)
                                SET (),                          // ch(2)
                                SET (),                          // ch(3)
                                SET (2),                         // ch(4)
                                SET (4)),                        // ch(5)
                         Array (10.0, 11.0, 11.0, 11.0, 11.0, 10.0),
                         Map ((0, 1) -> ν(-1.0, -2.0, -3.0),
                              (0, 3) -> ν(-1.0),
                              (1, 2) -> ν(-1.0),
                              (4, 2) -> ν(-1.0),
                              (5, 4) -> ν(-1.0, -2.0)),          // change from -1 to -2 filter out vertices
                         false, "g")

    val q = new MuGraph (Array (SET (1),                         // ch(0)
                                SET (2),                         // ch(1)
                                SET ()),                         // ch(2)
                         Array (10.0, 11.0, 11.0),
                         Map ((0, 1) -> ν(-1.0, -2.0),
                              (1, 2) -> ν(-1.0)),
                         false, "g")

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("MuGraphSim")

} // MuGraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphSimTest2` object is used to test the `MuGraphSim` class.
 *  > runMain scalation.graphalytics.multi.MuGraphSimTest2
 */
object MuGraphSimTest2 extends App
{
    import scalation.graph_db.graph_algebra.{ExampleMuGraphD => EX_GRAPH}
    import MatchAnswers._

    val g = EX_GRAPH.g2p
    val q = EX_GRAPH.q2p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test("MuGraphSim", shift (graphSim))

} // MuGraphSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphSimTest3` object is used to test the `MuGraphSim` class.
 *  > runMain scalation.graphalytics.multi.MuGraphSimTest3
 */
object MuGraphSimTest3 extends App
{
    import scalation.graph_db.graph_algebra.{ExampleMuGraphD => EX_GRAPH}

    val g = EX_GRAPH.g1p
    val q = EX_GRAPH.q1p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("MuGraphSim")

} // MuGraphSimTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphSimTest4` object is used to test the `MuGraphSim` class.
 *  > runMain scalation.graphalytics.multi.MuGraphSimTest4
 */
object MuGraphSimTest4 extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = null.asInstanceOf [MuGraph [Double]] // GraphGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")        // FIX
    val q = null.asInstanceOf [MuGraph [Double]] // GraphGen.genBFSQuery (qSize, qAvDegree, g, false, "q")                 // FIX

    println (s"q.checkEdges   = " + q.checkEdges)
    println (s"q.checkElabels = " + q.checkElabels)
    q.printG ()

    val matcher = new MuGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("MuGraphSim")

} // MuGraphSimTest4 object

