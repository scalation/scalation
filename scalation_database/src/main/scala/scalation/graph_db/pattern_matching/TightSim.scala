
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain, John Miller
 *  @version 1.4
 *  @date    Thu Nov 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  Tight Simulation Using Mutable Sets
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{ArrayStack, HashMap, ListBuffer, Map, MutableList}
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag
import scala.util.control.Breaks.{break, breakable}

import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'TightSim' class provides an implementation for tight simulation
 *  graph pattern matching.  This version uses `DualSim`.
 *  @see hipore.com/ijbd/2014/IJBD%20Vol%201%20No%201%202014.pdf
 *  @param q  the query graph Q(U, D, k)
 *  @param g  the data graph  G(V, E, l)
 */
class TightSim [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel], duals: GraphMatcher [TLabel]) 
      extends GraphMatcher [TLabel] (g, q)
{
    private val DEBUG     = false
    private val qmet      = new GraphMetrics (q.clone, false)                 // creating graph metrics object of query graph
    private val dataSize  = g.size                                            // size of the data graph
    private val querySize = q.size                                            // size of the query graph

    private val phi0 = duals.mappings ()
    if (DEBUG) println (s"phi0 = ${phi0.deep}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Tight Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    override def mappings (): Array [SET [Int]] = Ball.merge (mappings2 (), querySize)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return mapping results per ball.
     */
    def mappings2 (): Map [Int, Array [SET [Int]]] = refine (phi0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Refine 'phi' using tight simulation to find mappings with balls.
     *  @param phi  the initial mapping after applying Dual to the whole graph
     */
    def refine (phi: Array [SET [Int]]): Map [Int, Array [SET [Int]]] =
    {
        if (phi.size == 0) { println ("No dual match."); return null }  // exit if no match after dual simulation
        if (DEBUG) println (s"phi = ${phi.deep}")
        
        val newGraph   = filterGraph (phi)                              // if doing tight sim more than once, must clone g
        val qDiameter  = qmet.rad                                       // get the query diameter
        val balls      = HashMap [Int, Ball [TLabel]] ()                // map of balls: center -> ball               
        val matches    = HashMap [Int, Array [SET [Int]]] ()            // map of matches in balls: center -> match
        val gCenters  = (0 until q.size).flatMap (phi(_)).distinct      // set of mapped data graph centers
        val bCenters   = SET [Int] ()                                   // set of centers for all balls
        var ballSum    = 0

        for (center <- gCenters) {                                      // for each mapped data graph center
            val ball = new Ball (newGraph, center, qDiameter)           // create a new ball for that center vertex
            ballSum += ball.nodesInBall.size                            // calculate ball size
            val mat  = dualFilter (phi.clone, ball)                     // perform dual filter on the ball
            if (DEBUG) println (s"center = $center, mat = ${mat.deep}")
            balls += center -> ball
            if (mat.size != 0) { bCenters += center; matches += center -> mat }
            else if (DEBUG)  println ("No match for ball centered at " + center + "\n")
        } // for

        if (DEBUG) println (s"""SUMMARY:
        bCenters                  = $bCenters
        Query Graph Diameter      = $qDiameter
        Number of Balls           = ${gCenters.size}
        Average Ball Size         = ${ballSum / gCenters.size.toDouble}
        Number of Matched Balls   = ${bCenters.size}
        Average Matched Ball Size = ${ballSum / bCenters.size.toDouble}""")

        matches
    } // refine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prune the data graph by consider only those vertices and edges which
     *  are part of feasible matches after performing initial dual simulation.
     *  @param phi  mappings from a query vertex u_q to { graph vertices v_g }
     * 
    def filterGraph (phi: Array [SET [Int]]): Graph [TLabel] = 
    {
        val nodesInSimset = phi.flatten.toSet                     // get all the vertices of feasible matches
        for (i <- 0 until dataSize) g.ch(i) &= nodesInSimset      // prune via intersection            

        val newCh = Array.ofDim [SET [Int]] (dataSize)
        for (i <- 0 until dataSize) newCh(i) = SET [Int] ()

        for (u <- 0 until q.size; w <- phi(u)) {             // new ch and pa set for data graph based upon feasible vertices
            for (v <- q.ch(u)) newCh(w) |= (g.ch(w) & phi(v))
        } // for
        new Graph (newCh, g.label, g.inverse, g.name + "2")     // create a new data graph
    } // filterGraph
     */ 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform dual simulation onto the ball.
     *  @param phi  mappings from a query vertex u_q to { graph vertices v_g }
     *  @param ball the Ball B(Graph, Center, Radius)
     */ 
    def dualFilter (phi: Array [SET [Int]], ball: Ball [TLabel]): Array [SET [Int]] = 
    {
        for (v <- phi.indices) phi(v) &= ball.nodesInBall         // project simset onto ball
        val filterSet = new ArrayStack [(Int, Int)] ()
        var filtered  = false
        for (u <- phi.indices; v <- phi(u) if ball.borderNodes contains v) {
            filtered = false                                      // filtering ball based on child relationship
            breakable { for (u1 <- q.ch(u)) { 
                if ((ball.post (v) & phi (u1)).isEmpty) {
                    filterSet.push ((u, v))
                    filtered = true
                    break
                } // if
            }} // breakable for
            if (! filtered) {                                     // filtering ball based on parent relationship,
                breakable { for (u2 <- q.pa(u)) {                 // if no child has been filtered out
                    if ((ball.pre (v) & phi(u2)).isEmpty) {
                        filterSet.push ((u, v))
                        break
                    } // if
                }} // breakable for
            } // if
        } // for

        while (! filterSet.isEmpty) {                             // refine ch and pa relationship for the vertex v,  
            val (u, v) = filterSet.pop ()                         // which is now not a feasible match  
            phi(u) -= v
            for (u2 <- q.pa(u); v2 <- (ball.pre (v) & phi(u2)) if (ball.post (v2) & phi(u)).isEmpty) 
                filterSet.push ((u2, v2))
            for (u1 <- q.ch(u); v1 <- (ball.post (v) & phi(u1)) if (ball.pre (v1) & phi(u)).isEmpty)
                filterSet.push ((u1, v1))
        } // while

        val chSet = HashMap [Int, SET [Int]] ()
        val paSet = HashMap [Int, SET [Int]] ()
        // create new ch and pa set for the ball after above pruning
        for (u <- phi.indices; v <- phi(u); uc <- q.ch(u); vc <- (ball.post (v) & phi(uc))) {                                 
            chSet.getOrElseUpdate (v, SET [Int] ())  += vc
            paSet.getOrElseUpdate (vc, SET [Int] ()) += v
        } // for

        // Finding max perfect subgraph
        val stack = new ArrayStack [Int] ()
        val visited = SET (ball.center)
        stack.push (ball.center)
        while (! stack.isEmpty) {
            val v = stack.pop ()
            for (child <- (chSet.getOrElse (v, SET ()) | paSet.getOrElse (v, SET ()))) {
                if (! visited.contains (child)) {
                    stack.push (child)
                    visited += child
                } // if
            } // for
        } // while
        for ( v <- phi.indices) phi(v) = phi(v) & visited

        //fixes the edges in the ball
        //(note that it does not change the parent set; this is only used for printing)
        //uncomment if you want to see the ball after finding maximum perfect subgraph

        ball.chMap = Map [Int, SET [Int]] ()
        val matchNodes = phi.flatten.toSet
        for ((n, nset) <- chSet; nc <- nset) {
            if ((matchNodes contains n) && (matchNodes contains nc)) ball.chMap.getOrElseUpdate (n, SET () ) += nc
        } // for
  
        for (v <- phi.indices if phi(v).isEmpty) return Array [SET [Int]] ()
        phi
    } //dualFilter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vertex from an array of central vertices, those which have 
     *  highest 'ch' set size and lowest frequency of label in the query graph, i.e.,
     *  highest ratio.
     *  @param centr the array of vertices whose eccentricity is equal to the radius
     */
    def selectivityCriteria (qmet: GraphMetrics [TLabel]): Int =
    {
        var index = 0
        var max   = 0.0
        for (ctr <- qmet.central) {
            val ratio = qmet.g.ch(ctr).size.toDouble / qmet.g.labelMap (qmet.g.label(ctr)).size.toDouble
            if (max < ratio) { max = ratio; index = ctr }
        } // for
        index
    } // selectivityCriteria 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'prune' is not needed, pruning is delegated to incorporated dual graph
     *  simulation algorithm.
     *  @param phi  array of mappings from a query vertex u_q to { graph vertices v_g }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] = throw new UnsupportedOperationException ()

} // TightSim class

import scalation.graph_db.{ExampleGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest` object is used to test the `TightSim` class.
 *  This object tests the data graph g1 and query graph q1.
 *  > run-main scalation.graph_db.pattern_matching.TightSimTest
 */
object TightSimTest extends App
{
    val (g, q) = (EX_GRAPH.g1p, EX_GRAPH.q1p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new TightSim (g, q, new DualSim (g, q))).test ("TightSim")        // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSim2 (g, q))).test ("TightSim2")      // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSimCAR (g, q))).test ("TightSimCAR")  // Tight Graph Simulation Pattern Matcher

} // TightSimTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest2` object is used to test the `TightSim` class.
 *  This object tests the data graph g2 and query graph q2.
 *  > run-main scalation.graph_db.pattern_matching.TightSimTest2
 */
object TightSimTest2 extends App
{
    val (g, q) = (EX_GRAPH.g2p, EX_GRAPH.q2p)

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new TightSim (g, q, new DualSim (g, q))).test ("TightSim")        // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSim2 (g, q))).test ("TightSim2")      // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSimCAR (g, q))).test ("TightSimCAR")  // Tight Graph Simulation Pattern Matcher

} // TightSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest3` object is used to test the `TightSim` class.
 *  This object tests randomly (uniform) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.TightSimTest3
 */
object TightSimTest3 extends App
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

    (new TightSim (g, q, new DualSim (g, q))).test ("TightSim")        // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSim2 (g, q))).test ("TightSim2")      // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSimCAR (g, q))).test ("TightSimCAR")  // Tight Graph Simulation Pattern Matcher

} // TightSimTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest4` object is used to test the `TightSim` class.
 *  This object tests randomly (PowerLaw) generated graphs.
 *  > run-main scalation.graph_db.pattern_matching.TightSimTest4
 */
object TightSimTest4 extends App
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

    (new TightSim (g, q, new DualSim (g, q))).test ("TightSim")        // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSim2 (g, q))).test ("TightSim2")      // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSimCAR (g, q))).test ("TightSimCAR")  // Tight Graph Simulation Pattern Matcher

} // TightSimTest4 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest5` object is used to test the `TightSim` class.
 *  This object tests graphs read from files.
 *  > run-main scalation.graph_db.pattern_matching.TightSimTest5
 */
object TightSimTest5 extends App
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

    (new TightSim (g, q, new DualSim (g, q))).test ("TightSim")        // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSim2 (g, q))).test ("TightSim2")      // Tight Graph Simulation Pattern Matcher
    (new TightSim (g, q, new DualSimCAR (g, q))).test ("TightSimCAR")  // Tight Graph Simulation Pattern Matcher

} // TightSimTest5 object

