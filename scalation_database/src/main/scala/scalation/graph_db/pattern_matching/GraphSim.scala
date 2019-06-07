
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.6
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Graph Simulation CAR Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.{banner, MultiSet}

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    /** The DEBUG flag 
     */
    private val DEBUG = false 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                     // check for matching children
            alter = false

            // loop over query vertices u and data vertices v in phi(u) - child match

            for (u <- qRange; v <- phi(u); u_c <- q.ch(u)) {
                if (disjoint (g.ch(v), phi(u_c)))  {        // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match
                    alter  = true
                } // if
            } //for
        } // while
        phi
    } // prune

} // GraphSim class

import scalation.graph_db.{ExampleGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest` object is used to test the `GraphSim` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > runMain scalation.graph_db.pattern_matching.GraphSimTest
 */
object GraphSimTest extends App
{
    val (g, q) = (EX_GRAPH.g1, EX_GRAPH.q1)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest2` object is used to test the `GraphSim` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.GraphSimTest2
 */
object GraphSimTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q2)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest3` object is used to test the `GraphSim` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.GraphSimTest3
 */
object GraphSimTest3 extends App
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

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest4` object is used to test the `GraphSim` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.GraphSimTest4
 */
object GraphSimTest4 extends App
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

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest5` object is used to test the `GraphSim` class.
 *  This object tests graphs read from files.
 *  > runMain scalation.graph_db.pattern_matching.GraphSimTest5
 */
object GraphSimTest5 extends App
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

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest8` object is used to test the `GraphSim` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.GraphSimTest8
 *
object GraphSimTest8 extends App
{
    val (g, q) = (EX_GRAPH.g5, EX_GRAPH.q5)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    banner ("Graph Sim Test Aravind Graph")
    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest8 object
 */

