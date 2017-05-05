
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.3
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  `MGraph` Dual Simulation CAR Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.mutable.{ExampleMGraphS => EX_GRAPH}
import scalation.util.{MultiSet, Wildcard}
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
            for (u <- qRange; v <- phi(u); u_c <- q.ch(u); v_c <- g.ch(v)) {
                val elab_u2u_c = q.elabel (u, u_c)        // edge label in q for (u, u_c)
                val elab_v2v_c = g.elabel (v, v_c)        // edge label in g for (v, v_c)
                val chu = cLabel(u)
                val chv = gChildLabels(g, v,  u, q.ch(u), phi)
                val res = ! (chu ⊆ chv)
                val res1 = (elab_u2u_c != elab_v2v_c)
                //val hasWild     = hasWildcards (elab_u2u_c)
                //val welab_u2u_c = if (hasWild) new Wildcard (elab_u2u_c.asInstanceOf [String]) else null.asInstanceOf [Wildcard]
                //val result = (welab_u2u_c =~ (elab_v2v_c).asInstanceOf [String])
                val result = !(elab_v2v_c == elab_u2u_c)
                println("Child loop")
                println(" u = " + u + " , v =  " + v + "  " + res + "  " + res1 + "  " + elab_v2v_c + "  " + elab_u2u_c + "  " + result)

                if (DEBUG) println("u : " + u + " v : " + v + " chu : " + chu + " chv : " + chv + " res : " + res)
                //if (res || res1)   {  
                if (res || res1 && result)  {  
                    phi(u) -= v                             // remove v due to lack of child match
                    println("Removed")
                    alter  = true
                } // if
            } //for
            
            // loop over query vertices u, data vertices v in phi(u), and u's parents u_p
            for (u <- qRange; v <- phi(u); u_c <- q.ch(u); v_c <- g.ch(v)) {

                val elab_u2u_p = q.elabel ((u, u_c))                       // edge label in q for (u, u_c)
                val elab_v2v_p = g.elabel ((v, v_c))                       // edge label in g for (v, v_c)
                val pau = pLabel(u)
                val pav = gParentLabels(g, v, u, q.pa(u), phi)
                val res = ! (pau ⊆ pav)
                val res1 =  (elab_u2u_p != elab_v2v_p )
                //val hasWild     = hasWildcards (elab_u2u_p)
                //val welab_u2u_p = if (hasWild) new Wildcard (elab_u2u_p.asInstanceOf [String]) else null.asInstanceOf [Wildcard]
                //val result = (welab_u2u_p =~ (elab_v2v_p).asInstanceOf [String])
                val result = !(elab_u2u_p == elab_v2v_p)
                println("Parent loop")
                println(" u = " + u + " , v =  " + v + "  " + res + "  " + res1 + "  " + elab_u2u_p + "  " + elab_v2v_p + "  " + result)

                if (DEBUG) println("u : " + u + " v : " + v + " pau : " + pau + " pav : " + pav + " res : " + res)
                //if (res || res1)   {
                if (res || res1 && result)  {
                    phi(u) -= v                             // remove v due to lack of child match
                    println("Removed")
                    alter  = true
                } // if
            } //for

        } // while
        phi
    } // prune

} // MDualSimCAR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest` object is used to test the `MDualSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.MDualSimCARTest
 */
object MDualSimCARTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // MDual Simulation Pattern Matcher

} // MDualSimCARTest object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest2` object is used to test the `MDualSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.MDualSimCARTest2
 */
object MDualSimCARTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // MDual Simulation Pattern Matcher

} // MDualSimCARTest2 object

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MDualSimCARTest3` object is used to test the `MDualSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.MDualSimCARTest3
 *
object MDualSimCARTest3 extends App
{
    val g = EX_GRAPH.g3
    val q = EX_GRAPH.q3

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MDualSimCAR (g, q)).test ("MDualSimCAR")    // MDual Simulation Pattern Matcher

} // MDualSimCARTest3 object
 */


