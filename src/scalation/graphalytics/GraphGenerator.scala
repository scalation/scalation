
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz
 *  @version 1.0
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.Queue
import math.pow
import scala.util.Random

import GraphTypes._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphGenerator' object is used to build random graph with various
 *  characteristics.
 */
object GraphGenerator
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
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int): Graph =
    {
        val adj = (0 until size).map { node => 
            val degree = rand.nextInt (avDegree * 2 + 1)
            (0 until degree).map ( _ => rand.nextInt (size) ).toSet.filter (_ != node)
        }.toArray
        val label = randDistLabels (size, nLabels)
        new Graph (adj, label)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random connected graph by using genRandomGraph and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int): Graph =
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
    def genRandomGraph_PowLabels (size: Int, nLabels: Int, avDegree: Int): Graph =
    {
        val adj = Array.ofDim [ISet] (size).map ( node => {
            val degree = rand.nextInt (avDegree * 2 + 1)
            (0 until degree).map ( _ => rand.nextInt (size) ).toSet
        })
        // 2.1 is used in WWW graph pg 72 of m&m graph data
        val label = powDistLabels (size, nLabels, 2.1)
        new Graph (adj, label)
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
    def genPowerLawGraph (size: Int, nLabels: Int, maxDegree: Int, distPow: Double): Graph =
    {
        val adj = (0 until size).map { node => 
            val degree = powInt (0, maxDegree, distPow)
            (0 until degree).map ( _ => rand.nextInt (size)).toSet.filter (_ != node)
        }.toArray
        val label = randDistLabels (size, nLabels)
        new Graph (adj, label)
    } // genPowerLawGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a graph with power law degree distribution with exponent 'distPow'
     *  and power law distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed according to power law)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     */
    def genPowerLawGraph_PowLabels (size: Int, nLabels: Int, maxDegree: Int, distPow: Double): Graph =
    {
        val adj = Array.ofDim [ISet] (size).map ( node => {
            val degree = powInt (0, maxDegree, distPow)
            (0 until degree).map ( _ => rand.nextInt(size) ).toSet
          })
        val label = powDistLabels (size, nLabels, distPow)
        new Graph (adj, label)
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
    def genBFSQuery (size: Int, avDegree: Int, g: Graph): Graph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var cycle       = false
        var adjMap: Map [TLabel, ISet] = null
        var nodes:  ISet = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            adjMap     = Map [TLabel, ISet] ()
            nodes      = Set[Int]()
            val q      = Queue[Int]()
            val start  = rand.nextInt(g.size)     // randomly pick a start node in adj
            q.enqueue(start)
            nodes += start
            while (! q.isEmpty && nodes.size < size) {
                var adjs = Set [Int] ()
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
                    adjMap += (newNode -> (adjMap.getOrElse (newNode, Set [Int] ()) ++ adjs))
                } // if
            } // while
            if(nodes.size < size) nRestarts += 1
        } // while
    
        if (nRestarts == maxRestarts) { println ("genBFSQuery: could not find a good query"); return null }
    
        // gives the nodes new ids
        var newLabelMap = Map [TLabel, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val adj = Array.ofDim [ISet] (size).map (x => Set [Int] ())
        for ((node, children) <- adjMap) adj (newLabelMap (node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        if (cycle) println ("query has a cycle")
        new Graph (adj, label)
    } // genBFSQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extracts a subgraph of 'size' vertices from graph 'g' by performing a
     *  breadth-first search from a random vertex.
     *  @param size  the number of vertices to extract
     *  @param g     the data graph to extract from
     */
    def extractSubgraph (size: Int, g: Graph): Graph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var adjMap: Map [TLabel, ISet] = null
        var nodes:  ISet = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            adjMap    = Map [TLabel, ISet] ()
            nodes     = Set [Int] ()
            val q     = Queue [Int] ()
            val start = rand.nextInt (g.size)         // randomly pick a start node in adj
            println ("start node: " + start)
            q.enqueue (start)
            nodes += start
            while (! q.isEmpty && nodes.size < size) {
                var adjs = Set [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.adj (newNode)
                if (! newNodeChildren.isEmpty) {
                    for (newChild <- newNodeChildren if nodes.size < size) {
                        if (! nodes.contains (newChild)) { nodes += newChild; q.enqueue (newChild) }
                    } // for
                } // if
            } // while
            for (n <- nodes) { val adjs = g.adj(n) & nodes; adjMap += (n -> adjs ) }
            if (nodes.size < size) {
                nRestarts += 1
                println ("nodes.size only " + nodes.size)
            } // if
        } // while

        if (nRestarts == maxRestarts) { println ("extractSubgraph: could not find a good query"); return null }

        // gives the nodes new ids
        var newLabelMap = Map[Int, Int]() 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val adj = Array.ofDim [ISet] (size).map (x => Set [Int] ())
        for ((node, children) <- adjMap) adj (newLabelMap(node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        new Graph (adj, label)
    } // extractSubgraph

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [Int] =
    {
        Array.ofDim [Int] (size).map ( x => rand.nextInt (nLabels) )
    } // randDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a power law distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     *  @param pow      the power/exponent
     */
    private def powDistLabels (size: Int, nLabels: Int, pow: Double): Array [Int] =
    {
        Array.ofDim [Int] (size).map ( x => powInt (0, nLabels, pow) )
    } // powDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a Gaussian/Normal distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def gaussianDistLabels (size: Int, nLabels: Int): Array [Int] =
    {
        Array.ofDim [Int] (size).map ( x => gaussInt (nLabels / 2.0) )
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

} // GraphGenerator class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphGeneratorTest' object is used to test the 'GraphGenerator' class
 *  for building random graphs where a vertex's degree is uniformly distributed.
 */
object GraphGeneratorTest extends App
{
    import GraphGenerator._

    println ("GraphGeneratorTest: test genRandomGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomGraph (4, 100, 1)
        g.print
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("GraphGeneratorTest: test genRandomConnectedGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomConnectedGraph (4, 100, 1)
        g.print
    } // foreach

    println ("GraphGeneratorTest: test geneRandomGraph_PowLabels")
    val g1 = genRandomGraph_PowLabels (200, 50, 2)
    g1.print
    g1.labelMap.toSeq.sortBy (_._1).foreach { println(_) }
 
} // GraphGeneratorTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphGeneratorTest' object is used to test the 'GraphGenerator' class
 *  for building power law graphs.
 */
object GraphGeneratorTest2 extends App
{
    import GraphGenerator._

    println ("GraphGeneratorTest2: test genPowerLawGraph")
    val g2 = genPowerLawGraph (50, 10, 10, 2.1)
    g2.print
    g2.adj.sortBy (_.size).foreach { println(_) }

    println ("GraphGeneratorTest2: test genPowerLawGraph_PowLabels")
    val g3 = genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.print
    g3.adj.sortBy (_.size).foreach { println(_) }
    g3.labelMap.toSeq.sortBy (_._1).foreach { println(_) }

} // GraphGeneratorTest2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'GraphGeneratorTest' object is used to test the 'GraphGenerator' class
 *  for extracting query graphs from data graphs.
 */
object GraphGeneratorTest3 extends App
{
    import GraphGenerator._

    var g = GraphGenerator.genRandomGraph (1000000, 10, 16)
    println ("done generating data graph")
    println ("g.size: " + g.size)
    println ("g.nEdges: " + g.nEdges)
    println ("GraphGeneratorTest3: test genBFSQuery")
    (2 until 10).foreach { i =>
        var q = genBFSQuery (25, 3, g)
        q.print
        println (q.size)
        println (q.nEdges)
        println (q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // GraphGeneratorTest3

