
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller, Aravind Kalimurthy
 *  @version 1.6
 *  @date    Mon May  6 10:50:37 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Graph Dual Simulation Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSim` class provides an implementation for Dual Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MDualSim [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
{
    /** The DEBUG flag
     */
    private val DEBUG = false 
    if (!(q.inverse && g.inverse)) println("The vertices should have inverse adjacent set (parent reference)")
    assert (q.inverse && g.inverse) 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                                    // check for matching children and parents
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c

            for (u <- qRange; v <- phi(u); u_c <- q.ch(u)) {
                val elab_u2u_c = q.elabel ((u, u_c))                       // edge label in q for (u, u_c)
                // val v_c = g.ch(v)                                       // don't filter on edge labels
                val v_c = g.ch(v).filter (g.elabel (v, _) == elab_u2u_c)   // filter on edge labels

                if (disjoint (v_c, phi(u_c)))  {                           // v must have a child in phi(u_c)
                    phi(u) -= v                                            // remove v due to lack of child match 
                    alter  = true
                } // if
            } //for

            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p

            for (u <- qRange; v <- phi(u); u_p <- q.pa(u)) {
                val elab_u_p2u = q.elabel ((u_p, u))                       // edge label in q for (u, u_c)
                // val v_p = g.pa(v)                                       // don't filter on edge labels
                val v_p = g.pa(v).filter (g.elabel (_, v) == elab_u_p2u)   // filter on edge labels

                if (disjoint (v_p, phi(u_p))) {                            // v must have a parent in phi(u_p)
                    phi(u) -= v                                            // remove v due to lack of parent match
                    alter   = true
                } // if
            } //for

        } // while
        phi
    } // prune

} // MDualSim class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest` object is used to test the `MDualSim` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimTest
 */
object MDualSimTest extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSim (g, q)).test ("MDualSim")    // Dual Graph Simulation Pattern Matcher

} // MDualSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest2` object is used to test the `MDualSim` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimTest2
 */
object MDualSimTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSim (g, q)).test ("MDualSim")    // Dual Graph Simulation Pattern Matcher

} // MDualSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest3` object is used to test the `MDualSim` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimTest3
 */
object MDualSimTest3 extends App
{
    val (g, q) = (EX_GRAPH.g3p, EX_GRAPH.q3p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSim (g, q)).test ("MDualSim")    // Dual Graph Simulation Pattern Matcher

} // MDualSimTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest4` object is used to test the `MDualSim` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimTest4
 */
object MDualSimTest4 extends App
{
    val (g, q) = MGraphGen.genGraphs ("0.0")
    g.addPar()
    q.addPar()

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MDualSim (g, q)).test ("MDualSim")    // Dual Graph Simulation Pattern Matcher

} // MDualSimTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest5` object is used to test the `MDualSim` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimTest5
 */
object MDualSimTest5 extends App
{
    val (g, q) = MGraphGen.genPowerGraphs ("0.0")
    g.addPar()
    q.addPar()

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MDualSim (g, q)).test ("MDualSim")    // Dual Graph Simulation Pattern Matcher

} // MDualSimTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimTest6` object is used to test the `MDualSim` class.
 *  This object tests graphs read from files.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimTest6
 */
object MDualSimTest6 extends App
{
    val (g, q) = (MGraphIO [Double] ("gfile"), MGraphIO [Double] ("qfile"))
    g.addPar()
    q.addPar()

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MDualSim (g, q)).test ("MDualSim")    // Dual Graph Simulation Pattern Matcher

} // MDualSimTest6 object


