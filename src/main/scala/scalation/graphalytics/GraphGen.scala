
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz
 *  @version 1.2
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}
import scala.collection.mutable.Queue
import scala.math.pow
import scala.util.Random

import LabelType.{TLabel, toTLabel}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGen` object is used to build random graph with various
 *  characteristics.
 */
object GraphGen
{
    /** Random number generator
     */
    private val rand = new Random

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex is uniformly distributed.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random graph with the specified size (number of vertices), 
     *  average degree and labels evenly distributed across vertices from 0 to
     *  'nLabels - 1'.  Not necessarily a connected graph.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                        name: String = "g"): Graph =
    {
        val ch = (0 until size).map { node => 
            val degree = rand.nextInt (avDegree * 2 + 1)
            (0 until degree).map ( _ => rand.nextInt (size) ).toSet.filter (_ != node)
        }.toArray
        val label = randDistLabels (size, nLabels)
        new Graph (ch, label, inverse, name)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random connected graph by using 'genRandomGraph' and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                                 name: String = "g"): Graph =
    {
        var g: Graph = null
        do g = genRandomGraph (size, nLabels, avDegree, inverse, name) while (! g.isConnected)
        g
    } // genRandomConnectedGraph
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random graph with labels distributed based on a power law
     *  distribution (currently with the magic number 2.1 for the power law exponent).
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed according to power law)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph_PowLabels (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                                  name: String = "g"): Graph =
    {
        val ch = Array.ofDim [SET [Int]] (size).map ( node => {
            val degree = rand.nextInt (avDegree * 2 + 1)
            (0 until degree).map ( _ => rand.nextInt (size) ).toSet
        })
        // 2.1 is used in WWW graph pg 72 of m&m graph data
        val label = powDistLabels (size, nLabels, 2.1)
        new Graph (ch, label, inverse, name)
    } // genRandomGraph_PowLabels

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex follows a power law distribution.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a graph with power law degree distribution with exponent 'distPow'
     *  and uniformly distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed uniformly)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     *  @param inverse    whether to create inverse adjacency (parents)
     *  @param name       the name of the graph
     */
    def genPowerLawGraph (size: Int, nLabels: Int, maxDegree: Int, distPow: Double,
                          inverse: Boolean = false, name: String = "g"): Graph =
    {
        val ch = (0 until size).map { node => 
            val degree = powInt (0, maxDegree, distPow)
            (0 until degree).map ( _ => rand.nextInt (size)).toSet.filter (_ != node)
        }.toArray
        val label = randDistLabels (size, nLabels)
        new Graph (ch, label, inverse, name)
    } // genPowerLawGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a graph with power law degree distribution with exponent 'distPow'
     *  and power law distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed according to power law)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     *  @param inverse    whether to create inverse adjacency (parents)
     *  @param name       the name of the graph
     */
    def genPowerLawGraph_PowLabels (size: Int, nLabels: Int, maxDegree: Int, distPow: Double,
                                    inverse: Boolean = false, name: String = "g"): Graph =
    {
        val ch = Array.ofDim [SET [Int]] (size).map ( node => {
            val degree = powInt (0, maxDegree, distPow)
            (0 until degree).map ( _ => rand.nextInt(size) ).toSet
          })
        val label = powDistLabels (size, nLabels, distPow)
        new Graph (ch, label, inverse, name)
    } // genPowerLawGraph_PowLabels

    //------------------------------------------------------------------------
    // Methods for generating/extracting query graphs from data graphs.
    // Ensures that matches will exist. 
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a graph 'g', perform a breadth first search starting at a random vertex
     *  until the breadth first tree contains 'size' vertices.  At each junction,
     *  it chooses a random number of children to traverse, with that random
     *  number averaging to 'avDegree'.
     *  @param size      the number of vertices to extract
     *  @param avDegree  the average out degree
     *  @param g         the data graph to extract from
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genBFSQuery (size: Int, avDegree: Int, g: Graph, inverse: Boolean = false,
                     name: String = "g"): Graph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var cycle       = false
        var nodes       = Set [Int] ()
        var adjMap: Map [Int, SET [Int]] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            adjMap     = Map [Int, SET [Int]] ()
            nodes      = Set [Int] ()
            val q      = Queue [Int] ()
            val start  = rand.nextInt (g.size)     // randomly pick a start node in ch
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var adjs = Set [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.ch (newNode)
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
        var newLabelMap = Map [Int, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val ch = Array.ofDim [SET [Int]] (size).map (x => Set [Int] ())
        for ((node, children) <- adjMap) ch (newLabelMap (node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        if (cycle) println ("query has a cycle")
        new Graph (ch, label, inverse, name)
    } // genBFSQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract a subgraph of 'size' vertices from graph 'g' by performing a
     *  breadth-first search from a random vertex.
     *  @param size     the number of vertices to extract
     *  @param g        the data graph to extract from
     *  @param inverse  whether to create inverse adjacency (parents)
     *  @param name     the name of the graph
     */
    def extractSubgraph (size: Int, g: Graph, inverse: Boolean = false, name: String = "g"): Graph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var adjMap: Map [Int, SET [Int]] = null
        var nodes:  SET [Int] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            adjMap    = Map [Int, SET [Int]] ()
            nodes     = Set [Int] ()
            val q     = Queue [Int] ()
            val start = rand.nextInt (g.size)         // randomly pick a start node in ch
            println ("start node: " + start)
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var adjs = Set [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.ch (newNode)
                if (! newNodeChildren.isEmpty) {
                    for (newChild <- newNodeChildren if nodes.size < size) {
                        if (! nodes.contains (newChild)) { nodes += newChild; q.enqueue (newChild) }
                    } // for
                } // if
            } // while

            for (n <- nodes) { val adjs = g.ch(n) & nodes; adjMap += (n -> adjs ) }
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
        val ch = Array.ofDim [SET [Int]] (size).map (x => Set [Int] ())
        for ((node, children) <- adjMap) ch (newLabelMap(node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        new Graph (ch, label, inverse, name)
    } // extractSubgraph

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with labels distributed between 0 and 'nLabels - 1'
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => toTLabel (rand.nextInt (nLabels).toString))
    } // randDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with labels distributed between 0 and 'nLabels - 1'
     *  based on a power law distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     *  @param pow      the power/exponent
     */
    private def powDistLabels (size: Int, nLabels: Int, pow: Double): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => toTLabel (powInt (0, nLabels, pow).toString))
    } // powDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with labels distributed between 0 and 'nLabels - 1'
     *  based on a Gaussian/Normal distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def gaussianDistLabels (size: Int, nLabels: Int): Array [Int] =
    {
        Array.ofDim [Int] (size).map ( x => gaussInt (nLabels / 2.0) )
    } // gaussianDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a random integer between min and max with a frequency determined
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
    /** Return an integer with a probability based on a Gaussian distribution
     *  FIX: may need to truncate with
     *       'math.min(math.max((rand.nextGaussian()*d+d).toInt, 0), d*2).toInt
     *  @param d  the distance/rescaling parameter
     */
    private def gaussInt (d: Double) = (rand.nextGaussian () * 2.0 * d).toInt

} // GraphGen class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest` object is used to test the `GraphGen` class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 */
object GraphGenTest extends App
{
    import GraphGen._

    println ("GraphGenTest: test genRandomGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomGraph (4, 100, 1)
        g.printG ()
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("GraphGenTest: test genRandomConnectedGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomConnectedGraph (4, 100, 1)
        g.printG ()
    } // foreach

    println ("GraphGenTest: test genRandomGraph_PowLabels")
    val g1 = genRandomGraph_PowLabels (200, 50, 2)
    g1.printG ()
    println (s"g1.labelMap = ${g1.labelMap}")
 
} // GraphGenTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest2` object is used to test the `GraphGen` class for building
 *  power law graphs.
 */
object GraphGenTest2 extends App
{
    import GraphGen._

    println ("GraphGenTest2: test genPowerLawGraph")
    val g2 = genPowerLawGraph (50, 10, 10, 2.1)
    g2.printG ()
    g2.ch.sortBy (_.size).foreach { println(_) }

    println ("GraphGenTest2: test genPowerLawGraph_PowLabels")
    val g3 = genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.printG ()
    g3.ch.sortBy (_.size).foreach { println(_) }
    println (s"g3.labelMap = ${g3.labelMap}")

} // GraphGenTest2

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest3` object is used to test the `GraphGen` class for
 *  extracting query graphs from data graphs.
 */
object GraphGenTest3 extends App
{
    import GraphGen._

    var g = genRandomGraph (1000000, 10, 16)
    println ("done generating data graph")
    println ("g.size: " + g.size)
    println ("g.nEdges: " + g.nEdges)
    println ("GraphGenTest3: test genBFSQuery")
    (2 until 10).foreach { i =>
        var q = genBFSQuery (25, 3, g)
        q.printG ()
        println (q.size)
        println (q.nEdges)
        println (q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // GraphGenTest3

