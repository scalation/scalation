
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz, Aravind Kalimurthy
 *  @version 1.4
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Simulation Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}
import scala.reflect.ClassTag

import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2` class provides a second implementation for Simple Graph
 *  Simulation.  It differ from `GraphSim2` in the parent directory in the looping
 *  order in the main for-loop and early termination when 'phi(u)' is empty.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim2 [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    private val DEBUG = false                                  // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                       // check for matching children
            alter = false

            for (u <- qRange; u_c <- q.ch(u)) {               // for each u in q and its children u_c
                if (DEBUG) { println (s"for u = $u, u_c = $u_c"); showMappings (phi) }
                val phi_u_c = phi(u_c)
                val v_rem   = SET [Int] ()                    // vertices to be removed

                for (v <- phi(u)) {                           // for each v in g image of u
                    if (disjoint (g.ch(v), phi_u_c)) {        // v must have a child in phi(u_c)
                        v_rem += v                            // add v to removal set
                        alter = true
                    } // if
                } // for

                if (! v_rem.isEmpty) {
                    phi(u) --= v_rem                          // remove vertices in v_rem from phi(u)
                    if (phi(u).isEmpty) return phi            // no match for vertex u => no overall match
                } // if
            } // for

        } // while
        phi
    } // prune

} // GraphSim2 class

import scalation.graph_db.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test` object is used to test the `GraphSim2` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.GraphSim2Test
 */
object GraphSim2Test extends App
{
    val (g, q) = (EX_GRAPH.g1, EX_GRAPH.q1)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test2` object is used to test the `GraphSim2` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.GraphSim2Test2
 */
object GraphSim2Test2 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q2)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test3` object is used to test the `GraphSim2` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.GraphSim2Test3
 */
object GraphSim2Test3 extends App
{
    val (g, q) = GraphGen.genGraphs ("0.0")

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test4` object is used to test the `GraphSim2` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.GraphSim2Test4
 */
object GraphSim2Test4 extends App
{
    val (g, q) = GraphGen.genPowerGraphs ("0.0")

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim2Test5` object is used to test the `GraphSim2` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.GraphSim2Test5
 */
object GraphSim2Test5 extends App
{
    val (g, q) = (GraphIO [Double] ("gfile"), GraphIO [Double] ("qfile"))

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new GraphSim2 (g, q)).test ("GraphSim2")    // Graph Simulation Pattern Matcher

} // GraphSim2Test5 object

