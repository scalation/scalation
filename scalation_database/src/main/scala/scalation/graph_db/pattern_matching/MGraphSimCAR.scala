
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.6
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  `MGraph` Graph Simulation CAR Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.{banner, MultiSet, Wildcard}

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCAR` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MGraphSimCAR [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel]) 
      extends GraphMatcher [TLabel] (g, q)
{
    /** The DEBUG flag 
     */
    private val DEBUG = false 

    /** The Child labels for the query graph 
     */
    private val cLabel = Array.ofDim [MultiSet [TLabel]] (q.size)
    for (u <- cLabel.indices) cLabel(u) = qChildLabels (q, u)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                                    // check for matching children
            alter = false

            // loop over query vertices u and data vertices v in phi(u) - child match

            for (u <- qRange; v <- phi(u)) {
                val chu     = cLabel(u)                                 // child label of u
                val chv     = gChildLabels (g, v,  u, q.ch(u), phi)     // labels of children of phi(u)
                val cMatch  = chu ⊆ chv                                 // child match for query graph
                val v_rem   = SET [Int] ()                              // vertices to be removed

                for (u_c <- q.ch(u); v_c <- g.ch(v)) {

                    val elab_u2u_c = q.elabel (u, u_c)                 // edge label in q for (u, u_c)
                    val phi_u_c = phi(u_c)
//                  val v_c = g.ch(v)                                          // don't filter on edge labels
                    val v_c = g.ch(v).filter (g.elabel (v, _) == elab_u2u_c)   // filter on edge labels

                    if (disjoint (v_c, phi_u_c)) {                // v must have a child in phi(u_c)
                        v_rem += v                                // add to removal set
                        alter = true
                    } // if

                } // for

                if (DEBUG) println (s"u = $u, v = $v, chu = $chu, chv = $chv, cMatch = $cMatch")

                if (!cMatch)  {
                    phi(u) -= v                                        // remove v due to lack of child match
                    alter   = true
                } // if

                if (! v_rem.isEmpty) {
                    if (DEBUG) println (s"v_rem = $v_rem from phi($u)")
                    phi(u) --= v_rem                              // remove vertices in v_rem from phi(u)
                    v_rem.clear ()
                    if (phi(u).isEmpty) return phi                // no match for vertex u => no overall match
                } // if

            } // for
        } // while
        phi
    } // prune

} // MGraphSimCAR class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest` object is used to test the `MGraphSimCAR` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimCARTest
 */
object MGraphSimCARTest extends App
{
    val (g, q) = (EX_GRAPH.g1, EX_GRAPH.q1)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // Graph Simulation Pattern Matcher

} // MGraphSimCARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest2` object is used to test the `MGraphSimCAR` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimCARTest2
 */
object MGraphSimCARTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q2)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // Graph Simulation Pattern Matcher

} // MGraphSimCARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest3` object is used to test the `MGraphSimCAR` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimCARTest3
 */
object MGraphSimCARTest3 extends App
{
    val (g, q) = (EX_GRAPH.g3, EX_GRAPH.q3)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // Graph Simulation Pattern Matcher

} // MGraphSimCARTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest4` object is used to test the `MGraphSimCAR` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimCARTest4
 */
object MGraphSimCARTest4 extends App
{
    val (g, q) = MGraphGen.genGraphs ("0.0")

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // Graph Simulation Pattern Matcher

} // MGraphSimCARTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest5` object is used to test the `MGraphSimCAR` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimCARTest5
 */
object MGraphSimCARTest5 extends App
{
    val (g, q) = MGraphGen.genPowerGraphs ("0.0")

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // Graph Simulation Pattern Matcher

} // MGraphSimCARTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest6` object is used to test the `MGraphSimCAR` class.
 *  This object tests graphs read from files.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimCARTest6
 */
object MGraphSimCARTest6 extends App
{
    val (g, q) = (MGraphIO [Double] ("gfile"), MGraphIO [Double] ("qfile"))

    banner ("data graph")
    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (GraphMetrics.stats (g))

    banner ("query graph")
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()
    println (GraphMetrics.stats (q))

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // Graph Simulation Pattern Matcher

} // MGraphSimCARTest6 object


