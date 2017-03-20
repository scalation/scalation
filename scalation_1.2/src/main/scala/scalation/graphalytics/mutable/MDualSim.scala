
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.2
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  `MGraph` Dual Simulation Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graphalytics.mutable.{ExampleMGraphD => EX_GRAPH}
import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim` class provides a second implementation for Dual Graph Simulation.
 *  It differs from `DualSim` by not using inverse adjacency sets ('pa') in
 *  order to save space.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MDualSim [TLabel] (g: MGraph [TLabel], q: MGraph [TLabel])
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
        while (alter) {                                           // check for matching children/parents
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {                   // for each u in q and its children u_
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val newPhi = SET [Int] ()                         // subset of phi(u_c) having a parent in phi(u)
                val elab_u2u_c = q.elabel ((u, u_c))              // edge label in q for (u, u_c)

                for (v <- phi(u)) {                               // for each v in g image of u
//                  val v_c = g.ch(v)                                          // don't filter on edge labels
                    val v_c = g.ch(v).filter (elab_u2u_c == g.elabel (v, _))   // filter on edge labels
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
    } // saltzDualSim

} // MDualSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest` object is used to test the `MDualSim` class.
 *  run-main scalation.graphalytics.mutable.MDualSimTest
 */
object MDualSimTest extends App
{
    val g = new MGraph (Array (SET (),                     // ch(0)
                               SET (0, 2, 3, 4),           // ch(1)
                               SET (0),                    // ch(2)
                               SET (4),                    // ch(3)
                               SET ()),                    // ch(4)
                        Array (11.0, 10.0, 11.0, 11.0, 11.0),
                        Map ((1, 0) -> -1.0,
                             (1, 2) -> -1.0,
                             (1, 3) -> -1.0,
                             (1, 4) -> -1.0,
                             (2, 0) -> -1.0,
                             (3, 4) -> -2.0))               // change from -1 to -2 filter out vertices

    val q = new MGraph (Array (SET (1, 2),                  // ch(0)
                               SET (),                      // ch(1)
                               SET (1)),                    // ch(2)
                        Array (10.0, 11.0, 11.0),
                        Map ((0, 1) -> -1.0,
                             (0, 2) -> -1.0,
                             (2, 1) -> -1.0))
    g.printG ()
    q.printG ()

    val matcher = new MDualSim (g, q)                       // Graph Simulation Pattern Matcher
    val phi     = time { matcher.mappings () }              // time the matcher
    matcher.showMappings (phi)                              // display results

} // MDualSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest2` object is used to test the `MDualSim` class.
 *  run-main scalation.graphalytics.mutable.MDualSimTest2
 */
//object MDualSimTest2 extends App
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
//    val matcher = new MDualSim (g, q)                   // Dual Graph Simulation Pattern Matcher
//    val phi     = time { matcher.mappings () }             // time the matcher
//    matcher.showMappings (phi)                             // display results
//
//} // MDualSimTest2 object

