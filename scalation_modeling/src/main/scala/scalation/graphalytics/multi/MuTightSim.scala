
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Arash Fard, Usman Nisar, Ayushi Jain, John Miller
 *  @version 1.3
 *  @date    Wed Nov  9 14:58:54 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.multi

import scala.collection.mutable.{ArrayStack, ListBuffer, Map, HashMap, MutableList}
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag
import scala.util.control.Breaks.{break, breakable}

import scalation.graphalytics.mutable.{Ball, GraphMatcher, GraphMetrics}
import scalation.stat.Statistic

import MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'MuTightSim' class provides an implementation for tight simulation
 *  graph pattern matching.  This version uses `DualSim`.
 *  @see hipore.com/ijbd/2014/IJBD%20Vol%201%20No%201%202014.pdf
 *  @param q  the query graph Q(U, D, k)
 *  @param g  the data graph  G(V, E, l)
 */
class MuTightSim [TLabel: ClassTag] (g: MuGraph [TLabel], q: MuGraph [TLabel]) 
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

    private val phi0 = new MuDualSim (g, q).mappings ()
    println (s"phi0 = ${phi0.deep}")

    def prune (phi: Array [SET [Int]]): Array [SET [Int]] = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Tight Graph Simulation pattern matching algorithm to find the mappings
     *  from the query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    override def mappings (): Array [SET [Int]] = merge (mappings2 ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Mapping results per ball.
     */
    def mappings2 (): HashMap [Int, Array [SET [Int]]] = tightSim (phi0)

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
    /** Performs tight simulation to find mappings with balls.
     *  @param phi  the initial mapping after applying Dual to the whole graph
     */
    def tightSim (phi: Array [SET [Int]]): HashMap [Int, Array [SET [Int]]] =
    {
        if (phi.size == 0) { println ("No dual match."); return null }  // exit if no match after dual simulation

        println (s"phi = ${phi.deep}")
        val newGraph   = filterGraph (phi)                              // if doing strong sim more than once, must clone g
        val prunedSize = phi.clone.flatten.toSet.size                   // size of feasible matches after strict simulation
        val qDiameter  = qmet.rad                                       // get the query diameter
        val balls      = HashMap [Int, Ball [TLabel]] ()                // map of balls: center -> ball               
        val matches    = HashMap [Int, Array [SET [Int]]] ()            // map of matches in balls: center -> match
        val qCenter    = selectivityCriteria (qmet)                     // selected center for query graph
        val gCenters   = phi(qCenter)                                   // set of mapped data graph centers 
        val bCenters   = SET [Int] ()                                   // set of centers for all balls
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
    def filterGraph (phi: Array [SET [Int]]): MuGraph [TLabel] = 
    {
        val nodesInSimset = phi.flatten.toSet                     // get all the vertices of feasible matches
        for (i <- 0 until dataSize) g.ch(i) &= nodesInSimset      // prune via intersection            

        val newCh = Array.ofDim [SET [Int]] (dataSize)
        for (i <- 0 until dataSize) newCh(i) = SET [Int] ()

        for (u <- 0 until q.size; w <- phi(u)) {             // new ch and pa set for data graph based upon feasible vertices
            for (v <- q.ch(u)) newCh(w) |= (g.ch(w) & phi(v))
        } // for
        new MuGraph (newCh, g.label, g.elabel, g.inverse, g.name + "2")    // create a new data graph
    } // filterGraph
 
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count distinct vertices left after post processing.
     */
    def calculateTotalVertices (): Int = 
    {
        val totalSet = SET [String] ()
        for (i <- 0 until listOfDistinctReducedSet.length) totalSet ++= listOfDistinctReducedSet(i)
        totalSet.size
    } // calculateTotalVertices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count distinct edges left after post processing.
     *  @param g               the data graph  G(V, E, l)
     *  @param balls           mappings from a center vertex to the Ball B(Graph, Center, Radius)
     *  @param matchCenters    set of all vertices which are considered as center
     */
    def calculateTotalEdges (g: MuGraph [TLabel], balls: HashMap [Int, Ball [TLabel]], matchCenters: SET [Int]): Int = 
    {
        val distinctEdges = SET [String] ()
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
    def calculateBallDiameterMetrics (balls: HashMap [Int, Ball [TLabel]]): Statistic =
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

} // MuTightSim class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuTightSimTest` object is used to test the `MuTightSim` class.
 *  > run-main scalation.graphalytics.multi.MuTightSimTest
 */
object MuTightSimTest extends App
{
    val g = new MuGraph (Array (SET (1,3),                       // ch(0)
                                SET (2),                         // ch(1)
                                SET (),                          // ch(2)
                                SET (),                          // ch(3)
                                SET (2),                         // ch(4)
                                SET (4)),                        // ch(5)
                         Array (10.0, 11.0, 11.0, 11.0, 11.0, 10.0),
                         Map ((0, 1) -> ν(-1.0, -2.0, -3.0),
                              (0, 3) -> ν(-1.0),
                              (1, 2) -> ν(-1.0),
                              (4, 2) -> ν(-1.0),
                              (5, 4) -> ν(-1.0)),                // change from -1 to -2 filter out vertices
                              false, "g")

    val q = new MuGraph (Array (SET (1),                         // ch(0)
                                SET (2),                         // ch(1)
                                SET ()),                         // ch(2)
                         Array (10.0, 11.0, 11.0),
                         Map ((0, 1) -> ν(-1.0,-2.0),
                              (1, 2) -> ν(-1.0)),
                              false, "q")

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuTightSim (g, q)                          // Tight Graph Simulation Pattern Matcher
    matcher.test ("MuTightSim")

} // MuTightSimTest object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `MuTightSimTest2` object is used to test the `MuTightSim` class.
 *  > run-main scalation.graphalytics.multi.MuTightSimTest2
 */
object MuTightSimTest2 extends App 
{
    import scalation.graphalytics.multi.{ExampleMuGraphD => EX_GRAPH}
    import MatchAnswers._

    val g = EX_GRAPH.g2p
    val q = EX_GRAPH.q2p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuTightSim (g, q)                          // Tight Graph Simulation Pattern Matcher
    matcher.test ("MuTightSim", shift (tightSim))

} // MuTightSimTest2 object


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ::::::::::::
/** The `MuTightSimTest3` object is used to test the `MuTightSim` class.
 *  > run-main scalation.graphalytics.multi.MuTightSimTest3
 */
object MuTightSimTest3 extends App 
{
    import scalation.graphalytics.multi.{ExampleMuGraphD => EX_GRAPH}

    val g = EX_GRAPH.g1p
    val q = EX_GRAPH.q1p

    println (s"g.checkEdges = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuTightSim (g, q)                          // Tight Graph Simulation Pattern Matcher
    matcher.test ("MuTightSim")

} // MuTightSimTest3 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuTightSimTest4` object test the `MuTightSim` class by passing data graph
 *  and query graph relative file paths.
 *  > run-main scalation.graphalytics.multi.MuTightSimTest4
 */
/*object MuTightSimTest4 extends App 
{
    val g = MuGraphIO [Double] ("gfile")
    val q = MuGraphIO [Double] ("qfile")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    (new MuTightSim (g, q)).test ("MuTightSim")                  // Tight Graph Simulation Pattern Matcher

} // MuTightSimTest4 object */

