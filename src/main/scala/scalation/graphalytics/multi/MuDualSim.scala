
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz, Supriya Ramireddy
 *  @version 1.2
 *  @date    Sun Nov  6 16:04:08 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Multi-Graph 'MuGraph' Dual Simulation Using Mutable Sets
 */

package scalation.graphalytics.multi

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graphalytics.mutable.GraphMatcher

import MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualSim` class provides an implementation for Dual Graph Simulation
 *  for multi-graphs.
 *  It differs from `DualSim` by not using inverse adjacency sets ('pa') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MuDualSim [TLabel] (g: MuGraph [TLabel], q: MuGraph [TLabel])
      extends GraphMatcher (g, q)
{
    private val DEBUG = true                                     // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = saltzDualSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def saltzDualSim (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                            // check for matching children/parents
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {                    // for each u in q and its children u_
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val newPhi = SET [Int] ()                          // subset of phi(u_c) having a parent in phi(u)
                val elab_u2u_c = q.elabel ((u, u_c))               // edge label in q for (u, u_c)

                for (v <- phi(u)) {                                // for each v in g image of u
//                  val v_c = g.ch(v)                                                // don't filter on edge labels
//                  val v_c = g.ch(v).filter (elab_u2u_c == g.elabel (v, _))         // filter on edge labels, using ==
                    val v_c = g.ch(v).filter (elab_u2u_c subsetOf g.elabel (v, _))   // filter on edge labels, using subset
                    if (DEBUG) println (s"v = $v, v_c = $v_c, phi_u_c = " + phi(u_c))

                    val phiInt = v_c & phi(u_c)                    // children of v contained in phi(u_c)
                    if (phiInt.isEmpty) {
                        phi(u) -= v                                // remove vertex v from phi(u)
                        if (phi(u).isEmpty) return phi             // no match for vertex u => no overall match
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
    } // saltzDualSim

} // MuDualSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualSimTest` object is used to test the `MuDualSim` class.
 *  > run-main scalation.graphalytics.multi.MuDualSimTest
 */
object MuDualSimTest extends App
{
    val g = new MuGraph (Array (SET (1,3),                       // ch(0)
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
                              (5, 4) -> ν(-1.0)),                // change from -1 to -2 filter out vertices
                              false, "g")

    val q = new MuGraph (Array (SET (1),                         // ch(0)
                                SET (2),                         // ch(1)
                                SET ()),                         // ch(2)
                         Array (10.0, 11.0, 11.0),
                         Map ((0, 1) -> ν(-1.0,-2.0),
                              (1, 2) -> ν(-1.0)),
                              false, "q")

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuDualSim (g, q)                           // Dual Graph Simulation Pattern Matcher
    matcher.test ("MuDualSim")

} // MuDualSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualSimTest2` object is used to test the `MuDualSim` class.
 *  > run-main scalation.graphalytics.multi.MuDualSimTest2
 */
object MuDualSimTest2 extends App
{
    import scalation.graphalytics.multi.{ExampleMuGraphD => EX_GRAPH}
    import MatchAnswers._

    val g = EX_GRAPH.g2p
    val q = EX_GRAPH.q2p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuDualSim (g, q)                           // Dual Graph Simulation Pattern Matcher
    matcher.test ("MuDualSim", shift (dualSim))

} // MuDualSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualSimTest3` object is used to test the `MuDualSim` class.
 *  > run-main scalation.graphalytics.multi.MuDualSimTest3
 */
object MuDualSimTest3 extends App
{
    import scalation.graphalytics.multi.{ExampleMuGraphD => EX_GRAPH}

    val g = EX_GRAPH.g1p
    val q = EX_GRAPH.q1p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuDualSim (g, q)                           // Dual Graph Simulation Pattern Matcher
    matcher.test ("MuDualSim")

} // MuDualSimTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualSimTest4` object is used to test the `MuDualSim` class.
 *  > run-main scalation.graphalytics.multi.MuDualSimTest4
 */
//object MuDualSimTest4 extends App
//{
//    val gSize     = 1000         // size of the data graph
//    val qSize     =   10         // size of the query graph
//    val nLabels   =  100         // number of distinct labels
//    val gAvDegree =    5         // average vertex out degree for data graph
//    val qAvDegree =    2         // average vertex out degree for query graph
//
//    q.printG ()
//
//    val g = genRandomGraph (gSize, nLabels, gAvDegree, false, "g")
//    val q = genBFSQuery (qSize, qAvDegree, g, false, "q")
//
//    val matcher = new MuDualSim (g, q)                     // Dual Graph Simulation Pattern Matcher
//    val phi     = time { matcher.mappings () }             // time the matcher
//    matcher.showMappings (phi)                             // display results
//
//} // MuDualSimTest4 object

