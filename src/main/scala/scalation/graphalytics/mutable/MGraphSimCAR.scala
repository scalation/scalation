
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.2
 *  @date    Tue Dec 20 15:10:55 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     www2012.wwwconference.org/proceedings/proceedings/p949.pdf
 *
 *  `MGraph` Graph Simulation CAR Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.mutable.{ExampleMGraphS => EX_GRAPH}
import scalation.util.{MultiSet, Wildcard}

import LabelFunctions._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCAR` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MGraphSimCAR [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel]) 
      extends GraphMatcher (g, q)
{
    /** The DEBUG flag 
     */
    private val DEBUG = true 

    /** The Child labels for the query graph 
     */
    private val cLabel = Array.ofDim [MultiSet [TLabel]] (q.size)
    for (u <- cLabel.indices) cLabel(u) = qChildLabels (q, u)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = nisarMGraphSimCAR (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  eliminate mappings 'u -> v' when v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    private def nisarMGraphSimCAR (phi: Array [SET [Int]]): Array [SET [Int]] =
    {
        var alter = true
        while (alter) {                                                    // check for matching children
            alter = false

            // loop over query vertices u, data vertices v in phi(u), and u's children u_c
            for (u <- qRange; v <- phi(u)) {
                val chu = cLabel(u)
                val chv = gChildLabels (g, v,  u, q.ch(u), phi)
                val res = ! (chu ⊆  chv)

                for (u_c <- q.ch(u); v_c <- g.ch(v)) { 
                    val elab_u2u_c = q.elabel ((u, u_c))                       // edge label in q for (u, u_c)
                    val elab_v2v_c = g.elabel ((v, v_c))                       // edge label in g for (v, v_c)
                    //val hasWild    = elab_u2u_c.isInstanceOf [String] && hasWildcards (elab_u2u_c.asInstanceOf [String])
                    //val welab_u2u_c = if (hasWild) new Wildcard (elab_u2u_c.asInstanceOf [String]) else null.asInstanceOf [Wildcard]
                    //val result = (welab_u2u_c =~ (elab_v2v_c).asInstanceOf [String]) 
                    val result = (elab_v2v_c == elab_u2u_c)

		    if (DEBUG) println ("u = " + u + " v = " + v + " chu = " + chu + " chv = " + chv 
                                      + " qLabel = " + elab_u2u_c + " gLabel = " + elab_v2v_c )
		    if ((res || (elab_u2u_c != elab_v2v_c )) && (!result) ) {
                        phi(u) -= v                                            // remove v due to lack of child match
                        alter   = true
                    } // if
                } // for
            } // for           
        } // while
        phi
    } // nisarMGraphSimCAR

} // MGraphSimCAR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest` object is used to test the `MGraphSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.MGraphSimCARTest
 */
object MGraphSimCARTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // MGraph Simulation Pattern Matcher

} // MGraphSimCARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest2` object is used to test the `MGraphSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.MGraphSimCARTest2
 */
object MGraphSimCARTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    q.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    g.printG ()

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // MGraph Simulation Pattern Matcher

} // MGraphSimCARTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimCARTest3` object is used to test the `MGraphSimCAR` class.
 *  > run-main scalation.graphalytics.mutable.MGraphSimCARTest3
 *
object MGraphSimCARTest3 extends App
{
    val g = EX_GRAPH.g3
    val q = EX_GRAPH.q3

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimCAR (g, q)).test ("MGraphSimCAR")    // MGraph Simulation Pattern Matcher

} // MGraphSimCARTest3 object
 */

