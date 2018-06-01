
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Usman Nisar, Aravind Kalimurthy, John Miller
 *  @version 1.5
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

import scalation.util.{banner, MultiSet, PatMatcher, Wildcard}

import LabelFunctions._
import LabelVer.version 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimW` class provides an implementation for Simple Graph Simulation.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MGraphSimW [TLabel: ClassTag] (g: MGraph [TLabel], q: MGraph [TLabel]) 
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

            for (u <- qRange; v <- phi(u); u_c <- q.ch(u)) {
                val elab_u2u_c = q.elabel ((u, u_c)).asInstanceOf [String] // edge label in q for (u, u_c)
                val (labVer, elab_u2u_cW) = version (elab_u2u_c)

                val v_c = labVer match {
                    case LabelVer.Normal  => g.ch(v).filter (elab_u2u_c   == g.elabel (v, _))
                    case _                => g.ch(v).filter (elab_u2u_cW =~ g.elabel (v, _).asInstanceOf [String])
                    } // match

                if (disjoint (v_c, phi(u_c)))  {                           // v must have a child in phi(u_c)
                    phi(u) -= v                                            // remove v due to lack of child match
                    alter  = true
                } // if
            } // for
        } // while
        phi
    } // prune

} // MGraphSimW class

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest` object is used to test the `MGraphSimW` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest
 */
object MGraphSimWTest extends App
{
    val (g, q) = (EX_GRAPH.g1, EX_GRAPH.q1)
    g.addPar()
    q.addPar()

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimW (g, q)).test ("MGraphSimW")    // Graph Simulation Pattern Matcher

} // MGraphSimWTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest2` object is used to test the `MGraphSimW` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest2
 */
object MGraphSimWTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q2)
    g.addPar()
    q.addPar()

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimW (g, q)).test ("MGraphSimW")    // Graph Simulation Pattern Matcher

} // MGraphSimWTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest3` object is used to test the `MGraphSimW` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest3
 */
object MGraphSimWTest3 extends App
{
    val (g, q) = (EX_GRAPH.g3, EX_GRAPH.q3)
    g.addPar()
    q.addPar()

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimW (g, q)).test ("MGraphSimW")    // Graph Simulation Pattern Matcher

} // MGraphSimWTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest4` object is used to test the `MGraphSimW` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest4
 */
object MGraphSimWTest4 extends App
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

    (new MGraphSimW (g, q)).test ("MGraphSimW")    // Graph Simulation Pattern Matcher

} // MGraphSimWTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest5` object is used to test the `MGraphSimW` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest5
 */
object MGraphSimWTest5 extends App
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

    (new MGraphSimW (g, q)).test ("MGraphSimW")    // Graph Simulation Pattern Matcher

} // MGraphSimWTest5 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest6` object is used to test the `MGraphSimW` class.
 *  This object tests graphs read from files.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest6
 */
object MGraphSimWTest6 extends App
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

    (new MGraphSimW (g, q)).test ("MGraphSimW")    // Graph Simulation Pattern Matcher

} // MGraphSimWTest6 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphSimWTest7` object is used to test the `MGraphSimW` class.
 *  This object tests the data graph g3 and query graph q3.
 *  > runMain scalation.graph_db.pattern_matching.MGraphSimWTest7
 */
object MGraphSimWTest7 extends App
{
    val (g, q) = (EX_GRAPH.g2, EX_GRAPH.q4w)
    g.addPar()
    q.addPar()

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MGraphSimW (g, q)).test ("MGraphSimW", Answers_g2.phi1)      // MGraph Simulation W

} // MGraphSimWTest7 object

