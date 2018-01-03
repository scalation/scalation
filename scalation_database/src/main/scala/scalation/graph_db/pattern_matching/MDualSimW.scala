
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, John Miller, Aravind Kalimurthy
 *  @version 1.4
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

import scalation.util.{banner, Matchable, time}

import LabelVer.version 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimW` class provides an implementation for Dual Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MDualSimW [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel])
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

            for (u <- qRange; v <- phi(u); u_c <- q.ch(u)) {               // for each u in q and its children u_
                val elab_u2u_c = q.elabel ((u, u_c)).asInstanceOf [String] // edge label in q for (u, u_c)
                val (labVer, elab_u2u_cW) = version (elab_u2u_c) 

                val v_c = labVer match {
                    case LabelVer.Normal => g.ch(v).filter (elab_u2u_c   == g.elabel (v, _))
                    case _               => g.ch(v).filter (elab_u2u_cW =~ g.elabel (v, _).asInstanceOf [String])
                } // match

                if (disjoint (v_c, phi(u_c)))  {                           // v must have a child in phi(u_c)
                    phi(u) -= v                                            // remove v due to lack of child match 
                    alter  = true
                } // if
            } //for

            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p

            for (u <- qRange; v <- phi(u); u_p <- q.pa(u)) {
                val elab_u_p2u = q.elabel ((u_p, u)).asInstanceOf [String] // edge label in q for (u, u_c)
                val (labVer, elab_u_p2uW) = version (elab_u_p2u) 
                
                val v_p = labVer match {
                    case LabelVer.Normal => g.pa(v).filter (g.elabel (_, v) == elab_u_p2u)
                    case _               => g.pa(v).filter (elab_u_p2uW =~ g.elabel (_, v).asInstanceOf [String])
                    } // match

                if (disjoint (v_p, phi(u_p))) {                            // v must have a parent in phi(u_p)
                    phi(u) -= v                                            // remove v due to lack of parent match
                    alter   = true
                } // if
            } //for

        } // while
        phi
    } // prune

} // MDualSimW class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimWTest` object is used to test the `MDualSimW` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimWTest
 */
object MDualSimWTest extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimW (g, q)).test ("MDualSimW")    // Dual Graph Simulation Pattern Matcher

} // MDualSimWTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimWTest2` object is used to test the `MDualSimW` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimWTest2
 */
object MDualSimWTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimW (g, q)).test ("MDualSimW")    // Dual Graph Simulation Pattern Matcher

} // MDualSimWTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimWTest3` object is used to test the `MDualSimW` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimWTest3
 */
object MDualSimWTest3 extends App
{
    val (g, q) = (EX_GRAPH.g3p, EX_GRAPH.g3p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimW (g, q)).test ("MDualSimW")    // Dual Graph Simulation Pattern Matcher

} // MDualSimWTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimWTest4` object is used to test the `MDualSimW` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimWTest4
 */
object MDualSimWTest4 extends App
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

    (new MDualSimW (g, q)).test ("MDualSimW")    // Dual Graph Simulation Pattern Matcher

} // MDualSimWTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimWTest5` object is used to test the `MDualSimW` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimWTest5
 */
object MDualSimWTest5 extends App
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

    (new MDualSimW (g, q)).test ("MDualSimW")    // Dual Graph Simulation Pattern Matcher

} // MDualSimWTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimWTest6` object is used to test the `MDualSimW` class.
 *  This object tests graphs read from files.
 *  > runMain scalation.graph_db.pattern_matching.MDualSimWTest6
 */
object MDualSimWTest6 extends App
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

    (new MDualSimW (g, q)).test ("MDualSimW")    // Dual Graph Simulation Pattern Matcher

} // MDualSimWTest6 object


