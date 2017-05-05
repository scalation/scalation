
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.3
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  Dual Simulation CAR Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.mutable.{ExampleGraphS => EX_GRAPH}
import scalation.util.MultiSet

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'DualSimCAR' classs provides an implementation for Dual Graph Simulation
 *  with CArdinality Restrictions.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class DualSimCAR [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel])
      extends GraphMatcher (g, q)
{
    /** The DEBUG flag
     */
    private val DEBUG = false

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

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c
            for (u <- qRange; v <- phi(u)) {
                val chu = cLabel(u)
                val chv = gChildLabels(g, v,  u, q.ch(u), phi)
                val res = ! (chu ⊆  chv)
                if (DEBUG) println("u : " + u + " v : " + v + " chu : " + chu + " chv : " + chv + " res : " + res)
                if (res)  {  
                    phi(u) -= v                             // remove v due to lack of child match
                    alter  = true
                } // if
            } //for
            
            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p
            for (u <- qRange; v <- phi(u)) {
                val pau = pLabel(u)
                val pav = gParentLabels(g, v, u, q.pa(u), phi)
                val res = ! (pau ⊆  pav)
                if (DEBUG) println("u : " + u + " v : " + v + " pau : " + pau + " pav : " + pav + " res : " + res)
                if (res)  {
                    phi(u) -= v                             // remove v due to lack of child match
                    alter  = true
                } // if
            } //for

        } // while
        phi
    } // prune

} // DualSimCAR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest` object is used to test the `DualSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.DualSimCARTest
 */
object DualSimCARTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest2` object is used to test the `DualSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.DualSimCARTest2
 */
object DualSimCARTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualSimCARTest3` object is used to test the `DualSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.DualSimCARTest3
 *
object DualSimCARTest3 extends App
{
    val g = EX_GRAPH.g3
    val q = EX_GRAPH.q3

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new DualSimCAR (g, q)).test ("DualSimCAR")    // Dual Graph Simulation Pattern Matcher

} // DualSimCARTest3 object
 */
