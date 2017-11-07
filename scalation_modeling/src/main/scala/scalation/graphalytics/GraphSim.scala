
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.3
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}
import scala.reflect.ClassTag

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
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

import scalation.graphalytics.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.GraphSimTest
 */
object GraphSimTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest2` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.GraphSimTest2
 */
object GraphSimTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest3` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.GraphSimTest3
 *
object GraphSimTest3 extends App
{
    val g = EX_GRAPH.g3
    val q = EX_GRAPH.q3

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest3 object
 */

