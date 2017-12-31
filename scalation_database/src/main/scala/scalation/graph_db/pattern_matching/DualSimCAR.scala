
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.4
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Dual Simulation CAR Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.{banner, MultiSet}

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSimCAR' classs provides an implementation for Dual Graph Simulation
 *  with CArdinality Restrictions.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSimCAR [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher [TLabel] (g, q)
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
        while (alter) {                                     // check for matching children and parents
            alter = false

            // loop over query vertices u and data vertices v in phi(u) - child match

            for (u <- qRange; v <- phi(u)) {
                val chu    = cLabel(u)                                 // child label of u
                val chv    = gChildLabels (g, v,  u, q.ch(u), phi)     // labels of children of phi(u)
                val cMatch = chu ⊆ chv                                 // child match for query graph
                if (DEBUG) println (s"u = $u, v = $v, chu = $chu, chv = $chv, cMatch = $cMatch")
                if (!cMatch)   {
                    phi(u) -= v                                        // remove v due to lack of child match
                    alter   = true
                } // if
            } // for

            // loop over query vertices u and data vertices v in phi(u) - parent match

            for (u <- qRange; v <- phi(u)) {
                val pau    = pLabel(u)                                 // parent label of u
                val pav    = gParentLabels (g, v, u, q.pa(u), phi)     // labels of parents of phi(u)
                val pMatch = pau ⊆ pav                                 // parent match for query graph
                if (DEBUG) println (s"u = $u, v = $v, pau = $pau, pav = $pav, pMatch = $pMatch")
                if (!pMatch)  {
                    phi(u) -= v                                        // remove v due to lack of parent match
                    alter   = true
                } // if
            } // for

        } // while
        phi
    } // prune

} // DualSimCAR class

import scalation.graph_db.{ExampleGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest` object is used to test the `DualSimCAR` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.DualSimCARTest
 */
object DualSimCARTest extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest2` object is used to test the `DualSimCAR` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.DualSimCARTest2
 */
object DualSimCARTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest3` object is used to test the `DualSimCAR` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.DualSimCARTest3
 */
object DualSimCARTest3 extends App
{
    val (g, q) = GraphGen.genGraphs ("0.0")
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

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest4` object is used to test the `DualSimCAR` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.DualSimCARTest4
 */
object DualSimCARTest4 extends App
{
    val (g, q) = GraphGen.genPowerGraphs ("0.0")
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

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest5` object is used to test the `DualSimCAR` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.DualSimCARTest5
 */
object DualSimCARTest5 extends App
{
    val (g, q) = (GraphIO [Double] ("gfile"), GraphIO [Double] ("qfile"))
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

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest8` object is used to test the `DualSimCAR` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.DualSimCARTest8
 *
object DualSimCARTest8 extends App
{
    val (g, q) = (EX_GRAPH.g5p, EX_GRAPH.q5p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    banner ("Dual SimCAR Test Aravind Dual")
    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual SimCARulation Pattern Matcher

} // DualSimCARTest8 object
 */

