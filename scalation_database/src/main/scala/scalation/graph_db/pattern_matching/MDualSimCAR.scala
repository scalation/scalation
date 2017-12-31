
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.4
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  `MGraph` Dual Simulation CAR Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.{banner, MultiSet, Wildcard}
import scalation.util.Wildcard.hasWildcards

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'MDualSimCAR' classs provides an implementation for Dual Graph Simulation.
 *  with CArdinality Restrictions.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MDualSimCAR [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel]) 
      extends GraphMatcher (g, q)
{
    /** The DEBUG flag
     */
    private val DEBUG = false
    if (! (q.inverse && g.inverse)) throw new Exception ("Must use graph with Parents") 

    /** The Child labels for the query graph
     */
    private val cLabel = Array.ofDim [MultiSet [TLabel]] (q.size)
    for (u <- cLabel.indices) cLabel(u) = qChildLabels (q, u)

    /** The Parent labels for the query graph
     */
    private val pLabel = Array.ofDim [MultiSet [TLabel]] (q.size)
    for (u <- pLabel.indices) pLabel(u) = qParentLabels (q, u)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where (1) v's children fail to match u's
     *  or (2) v's parents fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                                // check for matching children and parents
            alter = false

            // loop over query vertices u and data vertices v in phi(u) - child match

            for (u <- qRange; v <- phi(u)) {
                val chu    = cLabel(u)                                 // child label of u
                val chv    = gChildLabels (g, v,  u, q.ch(u), phi)     // labels of children of phi(u)
                val cMatch = chu ⊆ chv                                 // child match for query graph

                for (u_c <- q.ch(u); v_c <- g.ch(v)) {
                    val elab_u2u_c = q.elabel (u, u_c)                 // edge label in q for (u, u_c)
                    //val v_c = g.ch(v)                                        // don't filter on edge labels
                    val v_c = g.ch(v).filter (g.elabel (v, _) == elab_u2u_c)   // filter on edge labels

                    if (disjoint (v_c, phi(u_c)))  {                           // v must have a child in phi(u_c)
                        phi(u) -= v                                            // remove v due to lack of child match
                        alter  = true
                    } // if

                } // for

                if (DEBUG) println (s"u = $u, v = $v, chu = $chu, chv = $chv, cMatch = $cMatch")

                if (!cMatch)  {
                    phi(u) -= v                                        // remove v due to lack of child match
                    alter   = true
                } // if
            } // for

            // loop over query vertices u and data vertices v in phi(u) - parent match

            for (u <- qRange; v <- phi(u)) {
                val pau    = pLabel(u)                                 // parent label of u
                val pav    = gParentLabels (g, v, u, q.pa(u), phi)     // labels of parents of phi(u)
                val pMatch = pau ⊆ pav                                 // parent match for query graph

                for (u_p <- q.pa(u); v_p <- g.pa(v)) {
                    val elab_u2u_p = q.elabel ((u_p, u))               // edge label in q for (u_p, u)
                    //val v_p = g.pa(v)                                        // don't filter on edge labels
                    val v_p = g.pa(v).filter (g.elabel (_, v) == elab_u2u_p)   // filter on edge labels

                    if (disjoint (v_p, phi(u_p))) {                            // v must have a parent in phi(u_p)
                        phi(u) -= v                                            // remove v due to lack of parent match
                        alter   = true
                    } // if
                } // for

                if (DEBUG) println (s"u = $u, v = $v, pau = $pau, pav = $pav, pMatch = $pMatch")

                if (!pMatch)  {
                    phi(u) -= v                                        // remove v due to lack of parent match
                    alter   = true
                } // if
            } // for
        } // while
        phi
    } // prune

} // MDualSimCAR class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest` object is used to test the `MDualSimCAR` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.MDualSimCARTest
 */
object MDualSimCARTest extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // MDualSimCARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest2` object is used to test the `MDualSimCAR` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.MDualSimCARTest2
 */
object MDualSimCARTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // MDualSimCARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest3` object is used to test the `MDualSimCAR` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > run-main scalation.graph_db.pattern_matching.MDualSimCARTest3
 */
object MDualSimCARTest3 extends App
{
    val (g, q) = (EX_GRAPH.g3p, EX_GRAPH.q3p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // MDualSimCARTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest4` object is used to test the `MDualSimCAR` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.MDualSimCARTest4
 */
object MDualSimCARTest4 extends App
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

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // MDualSimCARTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest5` object is used to test the `MDualSimCAR` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.MDualSimCARTest5
 */
object MDualSimCARTest5 extends App
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

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // MDualSimCARTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest6` object is used to test the `MDualSimCAR` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.MDualSimCARTest6
 */
object MDualSimCARTest6 extends App
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

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // MDualSimCARTest6 object


