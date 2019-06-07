
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller
 *  @version 1.6
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Graph Dual Simulation Using Immutable Sets
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSim` class provides an implementation for Dual Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSim (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                     // check for matching children and parents
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.ch(u)) {
                if (disjoint (g.ch(v), phi(u_c)))  {        // v must have a child in phi(u_c)
                    phi(u) -= v                             // remove v due to lack of child match 
                    alter  = true
                } // if
            } //for

            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p

            for (u <- qRange; v <- phi(u); u_p <- q.pa(u)) {
                if (disjoint (g.pa(v), phi(u_p))) {         // v must have a parent in phi(u_p)
                    phi(u) -= v                             // remove v due to lack of parent match
                    alter   = true
                } // if
            } //for

        } // while
        phi
    } // prune

} // DualSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimTest` object is used to test the `DualSim` class.
 *  > runMain scalation.graphalytics.DualSimTest
 */
object DualSimTest extends App
{
    val g = Graph.g1p
    val q = Graph.q1p

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim (g, q)).test ("DualSim")    // Dual Graph Simulation Pattern Matcher

} // DualSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimTest2` object is used to test the `DualSim` class.
 *  > runMain scalation.graphalytics.DualSimTest2
 */
object DualSimTest2 extends App
{
    val g = Graph.g2p
    val q = Graph.q2p

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim (g, q)).test ("DualSim")    // Dual Graph Simulation Pattern Matcher

} // DualSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimTest3` object is used to test the `DualSim` class.  Read the
 *  query graph 'q' and data graph 'g' from files.
 *  > runMain scalation.graphalytics.DualSimTest3
 */
object DualSimTest3 extends App
{
    val g = GraphIO ("gfile")
    val q = GraphIO ("qfile")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSim (g, q)).test ("DualSim")    // Dual Graph Simulation Pattern Matcher

} // DualSimTest3 object

