
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.3
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Graph Simulation CAR Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.mutable.{ExampleGraphS => EX_GRAPH}
import scalation.util.MultiSet

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCAR` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSimCAR [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher (g, q)
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

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c
            for (u <- qRange; v <- phi(u)) {
                val chu = cLabel(u)
                val chv = gChildLabels(g, v,  u, q.ch(u), phi)
                val res = ! (chu ⊆  chv)
		if (DEBUG) println("u : " + u + " v : " + v + " chu : " + chu + " chv : " + chv + " res : " + res)
		if (res) {
                    phi(u) -= v                             // remove v due to lack of child match
                    alter   = true
                } // if
            } // for           
        } // while
        phi
    } // prune

} // GraphSimCAR class

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest` object is used to test the `GraphSimCAR` class.
 *  > run-main scalation.graphalytics.GraphSimCARTest
 */
object GraphSimCARTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest2` object is used to test the `GraphSimCAR` class.
 *  > run-main scalation.graphalytics.GraphSimCARTest2
 */
object GraphSimCARTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest2 object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimCARTest3` object is used to test the `GraphSimCAR` class.
 *  > run-main scalation.graphalytics.GraphSimCARTest3
 *
object GraphSimCARTest3 extends App
{
    val g = EX_GRAPH.g3
    val q = EX_GRAPH.q3

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new GraphSimCAR (g, q)).test ("GraphSimCAR")    // Graph Simulation Pattern Matcher

} // GraphSimCARTest3 object
 */

