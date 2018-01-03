
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain, John Miller
 *  @version 1.4
 *  @date    Thu Nov 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection._
import scala.collection.immutable.{Set => SET}
import scala.collection.mutable.{ArrayStack, HashMap, ListBuffer, Map, MutableList, Set}
import scala.math.pow
import scala.util.control.Breaks.{break, breakable}
import scala.util.Random

import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'TightSim' class provides an implementation for tight simulation
 *  graph pattern matching.  This version uses `DualSim`.
 *  @see hipore.com/ijbd/2014/IJBD%20Vol%201%20No%201%202014.pdf
 *  @param q  the query graph Q(U, D, k)
 *  @param g  the data graph  G(V, E, l)
 */
class TightSim (g: Graph, q: Graph) 
      extends GraphMatcher (g, q)
{
    private val listOfDistinctReducedSet = new ListBuffer [SET [String]] ()   // contains total number of matches 
                                                                              // after post processing
    private val mapOfBallWithSize = Map [Int, Long] ()                        // contains balls left after
                                                                              // post processing with diameter.
    private val listOfMatchedBallVertices = MutableList [Int] ()              // contains list of center vertices

    private val qmet = new GraphMetrics (q.clone, false)                      // creating graph metrics object of query graph

    private val dataSize  = g.size                                            // size of the data graph
    private val querySize = q.size                                            // size of the query graph

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
    def mappings2 (): Map [Int, Array [SET [Int]]] = refine (new DualSim (g, q).mappings ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Refine 'phi' using tight simulation to find mappings with balls.
     *  @param phi  the initial mapping after applying Dual to the whole graph
     */
    private def refine (phi: Array [SET [Int]]): Map [Int, Array [SET [Int]]] =
    {
        if (phi.size == 0) { println ("No dual match."); return null }  // exit if no match after dual simulation

        val newGraph   = filterGraph (phi)                              // if doing strong sim more than once, must clone g
        val prunedSize = phi.flatten.toSet.size                         // size of feasible matches after strict simulation
        val qDiameter  = qmet.rad                                       // get the query diameter
        val balls      = HashMap [Int, Ball] ()                         // map of balls: center -> ball               
        val matches    = HashMap [Int, Array [SET [Int]]] ()            // map of matches in balls: center -> match
        val qCenter    = selectivityCriteria (qmet)                     // selected center for query graph
        val gCenters   = phi(qCenter)                                   // set of mapped data graph centers 
        val bCenters   = Set [Int] ()                                   // set of centers for all balls
        var ballSum    = 0

        println (s"qCenter  = $qCenter")
        println (s"gCenters = $gCenters")

        for (center <- gCenters) {                                      // for each mapped data graph center
            val ball = new Ball (newGraph, center, qDiameter)           // create a new ball for that center vertex
            ballSum += ball.nodesInBall.size                            // calculate ball size
            val mat  = dualFilter (phi.clone, ball)                     // perform dual filter on the ball
            println (s"center = $center, mat = ${mat.deep}")
            balls.put (center, ball)
            if (mat.size != 0) { bCenters += center; matches += center -> mat }
            else println ("No match for ball centered at " + center + "\n")
        } // for

        println ("SEQUENTIAL:    Data Graph Name:  " + g.name +
                 "\n  Number of Data Graph Nodes:  " + dataSize +
                 "\n            Query Graph Name:  " + q.name +
                 "\n Number of Query Graph Nodes:  " + querySize +
                 "\n     Number of Tight Matches:  " + bCenters.size +
                 "\n    Graph Size after Pruning:  " + prunedSize + " nodes" +
                 "\n              Query Diameter:  " + qDiameter +
                 "\n           Average Ball Size:  " + (ballSum / prunedSize.toDouble) +
                 "\n        Total Distinct Edges:  " + calculateTotalEdges (g, balls, bCenters) +
                 "\n     Total Distinct Vertices:  " + calculateTotalVertices ())
        println ("Ball Diameter Metrics(Min, Max, Mean, StdDev): " + calculateBallDiameterMetrics (balls) )
        matches
    } // tightSim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prune the data graph by consider only those vertices and edges which
     *  are part of feasible matches after performing initial dual simulation.
     *  @param phi  mappings from a query vertex u_q to { graph vertices v_g }
     */ 
    def filterGraph (phi: Array [SET [Int]]): Graph = 
    {
        val nodesInSimset = phi.flatten.toSet                     // get all the vertices of feasible matches
        for (i <- 0 until dataSize) g.ch(i) &= nodesInSimset      // prune via intersection            

        val newCh = Array.ofDim [SET [Int]] (dataSize)
        for (i <- 0 until dataSize) newCh(i) = SET [Int] ()

        for (u <- 0 until q.size; w <- phi(u)) {             // new ch and pa set for data graph based upon feasible vertices
            for (v <- q.ch(u)) newCh(w) |= (g.ch(w) & phi(v))
        } // for
        new Graph (newCh, g.label, g.inverse, g.name + "2")       // create a new data graph
    } // filterGraph
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform dual simulation onto the ball.
     *  @param phi  mappings from a query vertex u_q to { graph vertices v_g }
     *  @param ball the Ball B(Graph, Center, Radius)
     */ 
    def dualFilter (phi: Array [SET [Int]], ball: Ball): Array [SET [Int]] = 
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

        val chSet = HashMap [Int, Set [Int]] ()
        val paSet = HashMap [Int, Set [Int]] ()
        // create new ch and pa set for the ball after above pruning
        for (u <- phi.indices; v <- phi(u); uc <- q.ch(u); vc <- (ball.post (v) & phi(uc))) {                                 
            chSet.getOrElseUpdate (v, Set [Int] ())  += vc
            paSet.getOrElseUpdate (vc, Set [Int] ()) += v
        } // for

        // Finding max perfect subgraph
        val stack = new ArrayStack [Int] ()
        val visited = Set (ball.center)
        stack.push (ball.center)
        while (! stack.isEmpty) {
            val v = stack.pop ()
            for (child <- (chSet.getOrElse (v, Set ()) | paSet.getOrElse (v, Set ()))) {
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

        ball.chMap = Map [Int, Set [Int]] ()
        val matchNodes = phi.flatten.toSet
        for ((n, nset) <- chSet; nc <- nset) {
            if ((matchNodes contains n) && (matchNodes contains nc)) ball.chMap.getOrElseUpdate (n, Set () ) += nc
        } // for
  
        for (v <- phi.indices if phi(v).isEmpty) return Array [SET [Int]] ()
        phi
    } //dualFilter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count distinct vertices left after post processing.
     */
    def calculateTotalVertices (): Int = 
    {
        val totalSet = Set [String] ()
        for (i <- 0 until listOfDistinctReducedSet.length) totalSet ++= listOfDistinctReducedSet(i)
        totalSet.size
    } // calculateTotalVertices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count distinct edges left after post processing.
     *  @param g               the data graph  G(V, E, l)
     *  @param balls           mappings from a center vertex to the Ball B(Graph, Center, Radius)
     *  @param matchCenters    set of all vertices which are considered as center
     */
    def calculateTotalEdges (g: Graph, balls: Map [Int, Ball], matchCenters: Set [Int]): Int = 
    {
        val distinctEdges = Set [String] ()
        for (vert_id <- 0 until g.ch.length; if balls.keySet.contains (vert_id)) { 
            balls.get (vert_id).get.chMap.foreach (i => i._2.foreach (j => distinctEdges += (i._1.toString + "_" + j.toString)))
        } // for
        distinctEdges.size
    } // calculateTotalEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate statistics (e.g., min, max, average diameter and standard deviation)
     *  on the  balls left after post-processing.
     *  @param balls  mappings from a center vertex to the Ball B(Graph, Center, Radius)
     */
    def calculateBallDiameterMetrics (balls: Map [Int, Ball]): Statistic =
    {
        val ballStats = new Statistic ()
        for (vert_id <- listOfMatchedBallVertices) ballStats.tally (balls.get (vert_id).get.getBallDiameter)
        ballStats
    } // calculateBallDiameterMetrics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vertex from an array of central vertices, those which have 
     *  highest 'ch' set size and lowest frequency of label in the query graph, i.e.,
     *  highest ratio.
     *  @param centr the array of vertices whose eccentricity is equal to the radius
     */
    def selectivityCriteria (qmet: GraphMetrics): Int =
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


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `TightSimTest` object is used to test the `TightSim` class.
 *  > runMain scalation.graphalytics.TightSimTest
 */
object TightSimTest extends App 
{
    val g = Graph.g1p
    val q = Graph.q1p

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new TightSim (g, q)).test ("TightSim")   // Tight Graph Simulation Pattern Matcher

} // TightSimTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `TightSimTest2` object is used to test the `TightSim` class.
 *  > runMain scalation.graphalytics.TightSimTest2
 */
object TightSimTest2 extends App 
{
    val g = Graph.g2p
    val q = Graph.q2p

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new TightSim (g, q)).test ("TightSim")   // Tight Graph Simulation Pattern Matcher

} // TightSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightSimTest3` object test the `TightSim` class by passing data graph
 *  and query graph relative file paths.
 *  > runMain scalation.graphalytics.TightSimTest3
 */
object TightSimTest3 extends App 
{
    val g = GraphIO ("gfile")
    val q = GraphIO ("qfile")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new TightSim (g, q)).test ("TightSim")   // Tight Graph Simulation Pattern Matcher

} // TightSimTest3 object

