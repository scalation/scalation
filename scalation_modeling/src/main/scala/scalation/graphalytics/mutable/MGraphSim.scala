
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.3
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  `MGraph` Graph Simulation Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}

import scalation.graphalytics.mutable.{ExampleMGraphD => EX_GRAPH}
import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSim` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from `GraphSim` in the looping order in the main for-loop
 *  and early termination when 'phi(u)' is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MGraphSim (g: MGraph [Double], q: MGraph [Double])
      extends GraphMatcher [Double] (g, q)
{
    private val DEBUG = true                                      // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = saltzGraphSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def saltzGraphSim (phi: Array [SET [Int]]): Array [SET [Int]] =
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
//                  val v_c = g.ch(v)                                          // don't filter on edge labels
                    val v_c = g.ch(v).filter (g.elabel (v, _) == elab_u2u_c)   // filter on edge labels
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
    } // saltzGraphSim

} // MGraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimTest` object is used to test the `MGraphSim` class.
 *  > run-main scalation.graphalytics.mutable.MGraphSimTest
 */
object MGraphSimTest extends App
{
    val g = new MGraph (Array (SET (),                       // ch(0)
                               SET (0, 2, 3, 4),             // ch(1)
                               SET (0),                      // ch(2)
                               SET (4),                      // ch(3)
                               SET ()),                      // ch(4)
                        Array (11.0, 10.0, 11.0, 11.0, 11.0),
                        Map ((1, 0) -> -1.0,
                             (1, 2) -> -1.0,
                             (1, 3) -> -1.0,
                             (1, 4) -> -1.0,
                             (2, 0) -> -1.0,
                             (3, 4) -> -2.0),                // change from -1 to -2 filter out vertices
                             false, "g")

    val q = new MGraph (Array (SET (1, 2),                   // ch(0)
                               SET (),                       // ch(1)
                               SET (1)),                     // ch(2)
                        Array (10.0, 11.0, 11.0),
                        Map ((0, 1) -> -1.0,
                             (0, 2) -> -1.0,
                             (2, 1) -> -1.0),
                             false, "g")

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MGraphSim (g, q)                       // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }             // time the matcher
    matcher.showMappings (phi)                             // display results

} // MGraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimTest2` object is used to test the `MGraphSim` class.
 *  > run-main scalation.graphalytics.mutable.MGraphSimTest2
 */
//object MGraphSimTest2 extends App
//{
//    val gSize     = 1000         // size of the data graph
//    val qSize     =   10         // size of the query graph
//    val nLabels   =  100         // number of distinct labels
//    val gAvDegree =    5         // average vertex out degree for data graph
//    val qAvDegree =    2         // average vertex out degree for query graph
//
//    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")
//    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g, false, "q")
//
//    println (s"q.checkEdges   = " + q.checkEdges)
//    println (s"q.checkElabels = " + q.checkElabels)
//    q.printG ()
//
//    val matcher = new MGraphSim (g, q)                     // Graph Simulation Pattern Matcher
//    val phi     = time { matcher.mappings () }           // time the matcher
//    matcher.showMappings (phi)                           // show results
//
//} // MGraphSimTest2 object

