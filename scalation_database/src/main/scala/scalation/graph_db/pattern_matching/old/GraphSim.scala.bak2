
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller
 *  @version 1.3
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = nisarGraphSim (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    private def nisarGraphSim (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                     // check for matching children
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.ch(u)) {
                if ((g.ch(v) & phi(u_c)).isEmpty) {         // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match 
                    alter   = true
                } // if
            } //for           

        } // while
        phi
    } // nisarGraphSim

} // GraphSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimTest` object is used to test the `GraphSim` class.
 *  > run-main scalation.graphalytics.GraphSimTest
 */
object GraphSimTest extends App
{
    val g = Graph.g1p
    val q = Graph.q1p

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
    val g = Graph.g2p
    val q = Graph.q2p

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphSimTest3' object is used to test the 'GraphSim' class.  Read the
 *  query graph 'q' and data graph 'g' from files.
 *  > run-main scalation.graphalytics.GraphSimTest3
 */
object GraphSimTest3 extends App
{
    val g = GraphIO ("gfile")
    val q = GraphIO ("qfile")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSim (g, q)).test ("GraphSim")    // Graph Simulation Pattern Matcher

} // GraphSimTest3 object

