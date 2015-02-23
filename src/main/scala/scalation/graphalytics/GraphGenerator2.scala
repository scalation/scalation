
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz
 *  @version 1.1
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.{ArrayBuffer, Queue}
import math.pow
import scala.util.Random

import Graph2Types._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenerator2` object is used to build random graph with various
 *  characteristics.
 */
object GraphGenerator2
{
    /** Random number generator
     */
    private val rand = new Random

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex is uniformly distributed.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with the specified size (number of vertices), 
     *  average degree and labels evenly distributed across vertices from 0 to
     *  nLabels - 1.  Not necessarily a connected graph.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     */
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int): Graph2 =
    {
        val adj = Array.ofDim [AList] (size)
        for (i <- adj.indices) {                                          // each vertex i
            val degree = rand.nextInt (avDegree * 2 + 1)                  // out degree
            for (j <- 0 until degree if ! (adj contains j)) adj(i) += j   // add the edge i -> j
        } // for

        val label = randDistLabels (size, nLabels)
        Graph2 (adj, label)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random connected graph by using genRandomGraph and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int): Graph2 =
    {
        var g = genRandomGraph (size, nLabels, avDegree)
        while (! g.isConnected) g = genRandomGraph (size, nLabels, avDegree)
        g
    } // genRandomConnectedGraph
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with labels distributed based on a power law
     *  distribution (currently with the magic number 2.1 for the power law exponent).
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed according to power law)
     *  @param avDegree  the average degree
     */
    def genRandomGraph_PowLabels (size: Int, nLabels: Int, avDegree: Int): Graph2 =
    {
        val adj = Array.ofDim [AList] (size)
        for (i <- adj.indices) {                                          // each vertex i
            val degree = rand.nextInt (avDegree * 2 + 1)                  // out degree
            for (j <- 0 until degree if ! (adj contains j)) adj(i) += j   // add the edge i -> j
        } // for

        // 2.1 is used in WWW graph pg 72 of m&m graph data
        val label = powDistLabels (size, nLabels, 2.1)
        Graph2 (adj, label)
    } // genRandomGraph_PowLabels

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex follows a power law distribution.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a graph with power law degree distribution with exponent 'distPow'
     *  and uniformly distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed uniformly)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     */
    def genPowerLawGraph (size: Int, nLabels: Int, maxDegree: Int, distPow: Double): Graph2 =
    {
        val adj = Array.ofDim [AList] (size)
        for (i <- adj.indices) {                                          // each vertex i
            val degree = powInt (0, maxDegree, distPow)                   // out degree
            for (j <- 0 until degree if ! (adj contains j)) adj(i) += j   // add the edge i -> j
        } // for

        val label = randDistLabels (size, nLabels)
        Graph2 (adj, label)
    } // genPowerLawGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a graph with power law degree distribution with exponent 'distPow'
     *  and power law distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed according to power law)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     */
    def genPowerLawGraph_PowLabels (size: Int, nLabels: Int, maxDegree: Int, distPow: Double): Graph2 =
    {
        val adj = Array.ofDim [AList] (size)
        for (i <- adj.indices) {                                          // each vertex i
            val degree = powInt (0, maxDegree, distPow)                   // out degree
            for (j <- 0 until degree if ! (adj contains j)) adj(i) += j   // add the edge i -> j
        } // for

        val label = powDistLabels (size, nLabels, distPow)
        Graph2 (adj, label)
    } // genPowerLawGraph_PowLabels

    //------------------------------------------------------------------------
    // Methods for generating/extracting query graphs from data graphs.
    // Ensures that matches will exist. 
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a graph 'g', performs a breadth first search starting at a random vertex
     *  until the breadth first tree contains 'size' vertices.  At each junction,
     *  it chooses a random number of children to traverse, with that random
     *  number averaging to 'avDegree'.
     *  @param size      the number of vertices to extract
     *  @param avDegree  the average out degree
     *  @param g         the data graph to extract from
     */
    def genBFSQuery (size: Int, avDegree: Int, g: Graph2): Graph2 =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var cycle       = false
        var nodes       = ArrayBuffer [Int] ()
        var adjMap: Map [Int, AList] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            adjMap     = Map [Int, AList] ()
            nodes      = ArrayBuffer [Int] ()
            val q      = Queue [Int] ()
            val start  = rand.nextInt (g.size)     // randomly pick a start node in adj
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var adjs = ArrayBuffer [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.adj (newNode)
                if (! newNodeChildren.isEmpty) {
                    val nncArr = newNodeChildren.toArray
                    for (i <- 0 until rand.nextInt (avDegree * 2 + 1) if nodes.size < size) {
                        val newChild = nncArr (rand.nextInt (newNodeChildren.size)) 
                        if (!nodes.contains(newChild)) { nodes += newChild; q.enqueue (newChild) }
                        else cycle = true
                        if (newChild != newNode) adjs += newChild 
                    } // for
                    adjMap += (newNode -> (adjMap.getOrElse (newNode, ArrayBuffer [Int] ()) ++ adjs))
                } // if
            } // while

            if(nodes.size < size) nRestarts += 1
        } // while
    
        if (nRestarts == maxRestarts) { println ("genBFSQuery: could not find a good query"); return null }
    
        // gives the nodes new ids (FIX: refactor to renumber)
        var newLabelMap = Map [Int, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val adj = Array.ofDim [AList] (size).map (x => ArrayBuffer [Int] ())
        for ((node, children) <- adjMap) adj (newLabelMap (node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        if (cycle) println ("genBFSQuery: query has a cycle")
        Graph2 (adj, label)
    } // genBFSQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extracts a subgraph of 'size' vertices from graph 'g' by performing a
     *  breadth-first search from a random vertex.
     *  @param size  the number of vertices to extract
     *  @param g     the data graph to extract from
     */
    def extractSubgraph (size: Int, g: Graph2): Graph2 =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var adjMap: Map [Int, AList] = null
        var nodes:  AList = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            adjMap    = Map [Int, AList] ()
            nodes     = ArrayBuffer [Int] ()
            val q     = Queue [Int] ()
            val start = rand.nextInt (g.size)         // randomly pick a start node in adj
            println ("extractSubgraph: start node: " + start)
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var adjs = ArrayBuffer [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.adj (newNode)
                if (! newNodeChildren.isEmpty) {
                    for (newChild <- newNodeChildren if nodes.size < size) {
                        if (! nodes.contains (newChild)) { nodes += newChild; q.enqueue (newChild) }
                    } // for
                } // if
            } // while

            for (n <- nodes) { val adjs = g.adj(n) intersect nodes; adjMap += (n -> adjs ) }
            if (nodes.size < size) {
                nRestarts += 1
                println ("nodes.size only " + nodes.size)
            } // if
        } // while

        if (nRestarts == maxRestarts) { println ("extractSubgraph: could not find a good query"); return null }

        // gives the nodes new ids (FIX: refactor to renumber)
        var newLabelMap = Map [Int, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val adj = Array.ofDim [AList] (size).map (x => ArrayBuffer [Int] ())
        for ((node, children) <- adjMap) adj (newLabelMap(node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        Graph2 (adj, label)
    } // extractSubgraph

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Renumber the selected nodes (give them new consecutive ids).
     *  @param node
     *  @param adjMap
     *
    private def renumber (node: AList, adjMap: Map [Int, AList]): Array [AList] =
    {
        var oldId2newId = Map [Int, Int] ()
        var i = 0
        for (v <- node) { oldId2newId += (v -> i); i += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        for (
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        Array.ofDim [AList] (size).map (x => ArrayBuffer [Int] ())
    } // renumber
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => rand.nextInt (nLabels).asInstanceOf [Double])
    } // randDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a power law distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     *  @param pow      the power/exponent
     */
    private def powDistLabels (size: Int, nLabels: Int, pow: Double): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => powInt (0, nLabels, pow).asInstanceOf [TLabel])
    } // powDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a Gaussian/Normal distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def gaussianDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => gaussInt (nLabels / 2.0).asInstanceOf [Double])
    } // gaussianDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a random integer between min and max with a frequency determined
     *  by a power law distribution.
     *  @param min      the minimum value
     *  @param max      the maximum value
     *  @param distPow  the power distribution
     */
    private def powInt (min: Int, max: Int, distPow: Double): Int =
    {
        val exp = distPow + 1.0
        max - 1 - pow (( (pow (max, exp) - pow (min, exp)) * rand.nextDouble + pow (min, exp) ),
                       (1.0 / exp)).toInt
    } // powInt

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an integer with a probability based on a gaussian distribution
     *  centered at d
     *  FIX: may need to truncate with math.min(math.max((rand.nextGaussian()*d+d).toInt, 0), d*2).toInt
     *  @param d  the WHAT??
     */
    private def gaussInt (d: Double) = (rand.nextGaussian () * 2.0 * d).toInt

} // GraphGenerator2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphGenerator2Test' object is used to test the 'GraphGenerator2' class
 *  for building random graphs where a vertex's degree is uniformly distributed.
 */
object GraphGenerator2Test extends App
{
    import GraphGenerator2._

    println ("GraphGenerator2Test: test genRandomGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomGraph (4, 100, 1)
        g.print
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("GraphGenerator2Test: test genRandomConnectedGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomConnectedGraph (4, 100, 1)
        g.print
    } // foreach

    println ("GraphGenerator2Test: test geneRandomGraph_PowLabels")
    val g1 = genRandomGraph_PowLabels (200, 50, 2)
    g1.print
    g1.labelMap.toSeq.sortBy (_._1).foreach { println(_) }
 
} // GraphGenerator2Test


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphGenerator2Test2' object is used to test the 'GraphGenerator2' class
 *  for building power law graphs.
 */
object GraphGenerator2Test2 extends App
{
    import GraphGenerator2._

    println ("GraphGenerator2Test2: test genPowerLawGraph")
    val g2 = genPowerLawGraph (50, 10, 10, 2.1)
    g2.print
    g2.adj.sortBy (_.size).foreach { println(_) }

    println ("GraphGenerator2Test2: test genPowerLawGraph_PowLabels")
    val g3 = genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.print
    g3.adj.sortBy (_.size).foreach { println(_) }
    g3.labelMap.toSeq.sortBy (_._1).foreach { println(_) }

} // GraphGenerator2Test2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenerator2Test3` object is used to test the `GraphGenerator2` class
 *  for extracting query graphs from data graphs.
 */
object GraphGenerator2Test3 extends App
{
    import GraphGenerator2._

    var g = genRandomGraph (1000000, 10, 16)
    println ("done generating data graph")
    println ("g.size: " + g.size)
    println ("g.nEdges: " + g.nEdges)
    println ("GraphGenerator2Test3: test genBFSQuery")
    (2 until 10).foreach { i =>
        var q = genBFSQuery (25, 3, g)
        q.print
        println (q.size)
        println (q.nEdges)
        println (q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // GraphGenerator2Test3

