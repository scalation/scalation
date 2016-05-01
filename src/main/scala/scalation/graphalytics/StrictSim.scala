
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain, Aravind Kalimurthy, John Miller
 *  @version 1.2
 *  @date    Thu Nov 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection._
import scala.collection.immutable.{Set => SET}
import scala.collection.mutable.{ListBuffer, Map, HashMap, MutableList, Set, Stack}
import scala.math.pow
import scala.util.control.Breaks.{break, breakable}
import scala.util.Random

import scalation.stat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'StrictSim' class provides an implementation for strict simulation
 *  graph pattern matching.  This version uses `DualSim`.
 *  @see hipore.com/ijbd/2014/IJBD%20Vol%201%20No%201%202014.pdf
 *  @param q  the query graph Q(U, D, k)
 *  @param g  the data graph  G(V, E, l)
 */
class StrictSim (g: Graph, q: Graph) 
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
    /** Apply the Strict Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = merge (mappings2 ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Mapping results per ball.
     */
    def mappings2 (): HashMap [Int, Array [SET [Int]]] = strictSim (new DualSim (g, q).mappings ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merged mapping results, the union over all balls. 
     */
    def merge (matches: HashMap [Int, Array [SET [Int]]]): Array [SET [Int]] =
    { 
         val phi_all = Array.ofDim [SET [Int]] (querySize)
         for (i <- 0 until querySize) phi_all (i) = SET [Int] ()
         for ((c, phi_c) <- matches) {
             println (s"(c, phi_c) = ($c, ${phi_c.deep})")
             for (i <- 0 until querySize) phi_all(i) ++= phi_c(i)
         } // for
         phi_all
    } // merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs strict simulation to find mappings with balls.
     *  @param phi  the initial mapping after applying Dual to the whole graph
     */
    def strictSim (phi: Array [SET [Int]]): HashMap [Int, Array [SET [Int]]] =
    {
        if (phi.size == 0) { println ("No dual match."); return null }  // exit if no match after dual simulation

        val newGraph   = filterGraph (phi)                              // if doing strong sim more than once, must clone g
        val prunedSize = phi.flatten.toSet.size                         // size of feasible matches after strict simulation
        val qDiameter  = qmet.diam                                      // get the query diameter
        val balls      = HashMap [Int, Ball] ()                         // map of balls: center -> ball               
        val matches    = HashMap [Int, Array [SET [Int]]] ()            // map of matches in balls: center -> match
        val gCenters   = (0 until q.size).flatMap(phi(_))               // set of mapped data graph centers
        val bCenters   = Set [Int] ()                                   // set of centers for all balls
        var ballSum    = 0

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
                 "\n     Number of Strict Matches:  " + bCenters.size +
                 "\n    Graph Size after Pruning:  " + prunedSize + " nodes" +
                 "\n              Query Diameter:  " + qDiameter +
                 "\n           Average Ball Size:  " + (ballSum / prunedSize.toDouble) +
                 "\n        Total Distinct Edges:  " + calculateTotalEdges (g, balls, bCenters) +
                 "\n     Total Distinct Vertices:  " + calculateTotalVertices ())
        println ("Ball Diameter Metrics(Min, Max, Mean, StdDev): " + calculateBallDiameterMetrics (balls) )
        matches
    } // strictSim

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
        val filterSet = new Stack [(Int, Int)] ()
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
        val stack = new Stack [Int] ()
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
    def calculateTotalEdges (g: Graph, balls: HashMap [Int, Ball], matchCenters: Set [Int]): Int = 
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
    def calculateBallDiameterMetrics (balls: HashMap [Int, Ball]): Statistic =
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

} // StrictSim class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `StrictSimTest` object is used to test the `StrictSim` class.
 *  > run-main scalation.graphalytics.StrictSimTest
 */
object StrictSimTest extends App 
{
    val g = Graph.g1p
    val q = Graph.q1p

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new StrictSim (g, q)).test ("StrictSim")   // Strict Graph Simulation Pattern Matcher

} // StrictSimTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `StrictSimTest2` object is used to test the `StrictSim` class.
 *  > run-main scalation.graphalytics.StrictSimTest2
 */
object StrictSimTest2 extends App 
{
    val g = Graph.g2p
    val q = Graph.q2p

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new StrictSim (g, q)).test ("StrictSim")   // Strict Graph Simulation Pattern Matcher

} // StrictSimTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrictSimTest3` object test the `StrictSim` class by passing data graph
 *  and query graph relative file paths.
 *  > run-main scalation.graphalytics.StrictSimTest3
 */
object StrictSimTest3 extends App 
{
    val g = GraphIO ("gfile")
    val q = GraphIO ("qfile")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new StrictSim (g, q)).test ("StrictSim")   // Strict Graph Simulation Pattern Matcher

} // StrictSimTest3 object

