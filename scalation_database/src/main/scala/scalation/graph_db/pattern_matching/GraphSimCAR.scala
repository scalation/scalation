
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.4
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
/** The `GraphSimCAR` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSimCAR [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
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
        while (alter) {                                     // check for matching children
            alter = false

            // loop over query vertices u and data vertices v in phi(u) - child match

            for (u <- qRange; v <- phi(u)) {
                val chu    = cLabel(u)                                 // child label of u
                val chv    = gChildLabels (g, v,  u, q.ch(u), phi)     // labels of children of phi(u)
                val cMatch = chu ⊆ chv                                 // child match for query graph
                if (DEBUG) println (s"u = $u, v = $v, chu = $chu, chv = $chv, cMatch = $cMatch")
                if (!cMatch) {
                    phi(u) -= v                                        // remove v due to lack of child match
                    alter   = true
                } // if
            } // for
        } // while
        phi
    } // prune

} // GraphSimCAR class

import scalation.graph_db.{ExampleGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest` object is used to test the `GraphSimCAR` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.GraphSimCARTest
 */
object GraphSimCARTest extends App
{
    val (g, q) = (EX_GRAPH.g1, EX_GRAPH.q1)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest2` object is used to test the `GraphSimCAR` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.GraphSimCARTest2
 */
object GraphSimCARTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q2)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest3` object is used to test the `GraphSimCAR` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.GraphSimCARTest3
 */
object GraphSimCARTest3 extends App
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

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest4` object is used to test the `GraphSimCAR` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.GraphSimCARTest4
 */
object GraphSimCARTest4 extends App
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

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest5` object is used to test the `GraphSimCAR` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.GraphSimCARTest5
 */
object GraphSimCARTest5 extends App
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

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest8` object is used to test the `GraphSimCAR` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.GraphSimCARTest8
 *
object GraphSimCARTest8 extends App
{
    val (g, q) = (EX_GRAPH.g5, EX_GRAPH.q5)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    banner ("Graph SimCAR Test Aravind Graph")
    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph SimCARulation Pattern Matcher

} // GraphSimCARTest8 object
 */

