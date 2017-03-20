
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.3
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Simulation Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.mutable.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from `GraphSim` in the parent directory in the looping
 *  order in the main for-loop and early termination when 'phi(u)' is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher (g, q)
{
    private val DEBUG = true                                  // debug flag

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
        while (alter) {                                       // check for matching children
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {               // for each u in q and its children u_c
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val phi_u_c = phi(u_c)
                val v_rem   = SET [Int] ()                    // vertices to be removed

                for (v <- phi(u)) {                           // for each v in g image of u
                    if ((g.ch(v) & phi_u_c).isEmpty) {        // v must have a child in phi(u_c)
                        // phi(u) -= v                        // if not, remove vertex v from phi(u)
                        // if (phi(u).isEmpty) return phi     // no match for vertex u => no overall match
                        v_rem += v
                        alter = true
                    } // if
                    if (! v_rem.isEmpty) {
                        phi(u) --= v_rem                      // remove vertices in v_rem from phi(u)
                        if (phi(u).isEmpty) return phi        // no match for vertex u => no overall match
                    } // if
                } // for

            } // for

        } // while
        phi
    } // saltzGraphSim

} // GraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.mutable.GraphSimTest
 */
object GraphSimTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest2` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.mutable.GraphSimTest2
 */
object GraphSimTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest3` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.mutable.GraphSimTest3
 */
object GraphSimTest3 extends App
{
    val gGen = new GraphGen [Double] (0.0)

    val gSize     = 1000           // size of the data graph
    val qSize     =   10           // size of the query graph
    val nLabels   =  100           // number of distinct labels
    val gAvDegree =    5           // average vertex out degree for data graph
    val qAvDegree =    2           // average vertex out degree for query graph

    val g = gGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")
    val q = gGen.genBFSQuery (qSize, qAvDegree, g, false, "q")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest3 object

