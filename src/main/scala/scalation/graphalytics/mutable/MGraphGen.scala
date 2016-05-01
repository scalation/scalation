
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller, Aravind Kalimurthy
 *  @version 1.2
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.{Map, Queue}
import scala.collection.mutable.{Set => SET}
import scala.math.pow
import scala.util.Random

import scalation.linalgebra.VectorI
import scalation.random.RandomVecI

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphGen` object is used to build random graph with various characteristics.
 */
object MGraphGen
{
    /** Random number generator
     */
    private val rand = new Random

    /** Map of edge labels: (u, v) -> label
     */
    private val elabel:  Map [Tuple2 [Int, Int], TLabel] = Map ()

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex is uniformly distributed.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with the specified size (number of vertices), 
     *  average degree and labels evenly distributed across vertices from 0 to
     *  'nLabels - 1'.  Not necessarily a connected graph.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of vertex labels (distributed uniformly)
     *  @param eLabels   the number of edge labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph (size: Int, nLabels: Int, eLabels: Int, avDegree: Int, inverse: Boolean = false,
                        name: String = "g"): MGraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                    // for each vertex i
            val degree = rand.nextInt (avDegree * 2 + 1)           // out degree for vertex i
            val rvec   = RandomVecI (degree, size-1, i)            // random vector of integers
            ch(i)      = SET ((rvec.igen)(): _*)                   // children of vertex i
        } // for
        val label  = randDistLabels (size, nLabels)                // randomly assigned vertex labels
        val elabel = randDisteLabels (ch, eLabels)                 // randomly assigned edge labels
        new MGraph (ch, label, elabel, inverse, name)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random connected graph by using 'genRandomGraph' and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of vertex labels (distributed uniformly)
     *  @param eLabels   the number of edge labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, eLabels: Int, avDegree: Int, inverse: Boolean = false,
                                 name: String = "g"): MGraph =
    {
        var g: MGraph = null
        do g = genRandomGraph (size, nLabels, eLabels, avDegree, inverse, name) while (! g.isConnected)
        g
    } // genRandomConnectedGraph
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with labels distributed based on a power law
     *  distribution (currently with the magic number 2.1 for the power law exponent).
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed according to power law)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph_PowLabels (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                                  name: String = "g"): MGraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = rand.nextInt (avDegree * 2 + 1)                  // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        // 2.1 is used in WWW graph pg 72 of m&m graph data
        val label = powDistLabels (size, nLabels, 2.1)
        new MGraph (ch, label, elabel, inverse, name)
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
     *  @param inverse    whether to create inverse adjacency (parents)
     *  @param name       the name of the graph
     */
    def genPowerLawGraph (size: Int, nLabels: Int, maxDegree: Int, distPow: Double,
                          inverse: Boolean = false, name: String = "g"): MGraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = powInt (0, maxDegree, distPow)                   // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        val label = randDistLabels (size, nLabels)
        new MGraph (ch, label, elabel, inverse, name)
    } // genPowerLawGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a graph with power law degree distribution with exponent 'distPow'
     *  and power law distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed according to power law)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent
     *  @param inverse    whether to create inverse adjacency (parents)
     *  @param name       the name of the graph
     */
    def genPowerLawGraph_PowLabels (size: Int, nLabels: Int, maxDegree: Int, distPow: Double,
                                    inverse: Boolean = false, name: String = "g"): MGraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {                                          // each vertex i
            val degree = powInt (0, maxDegree, distPow)                   // out degree
            for (j <- 0 until degree if ! (ch contains j)) ch(i) += j   // add the edge i -> j
        } // for

        val label = powDistLabels (size, nLabels, distPow)
        new MGraph (ch, label, elabel, inverse, name)
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
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genBFSQuery (size: Int, avDegree: Int, g: MGraph, inverse: Boolean = false,
                     name: String = "g"): MGraph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var cycle       = false
        var nodes       = SET [Int] ()
        var chMap: Map [Int, SET [Int]] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            chMap     = Map [Int, SET [Int]] ()
            nodes      = SET [Int] ()
            val q      = Queue [Int] ()
            val start  = rand.nextInt (g.size)     // randomly pick a start node in ch 
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                val chs = SET [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.ch (newNode)
                if (! newNodeChildren.isEmpty) {
                    val nncArr = newNodeChildren.toArray
                    for (i <- 0 until rand.nextInt (avDegree * 2 + 1) if nodes.size < size) {
                        val newChild = nncArr (rand.nextInt (newNodeChildren.size)) 
                        if (!nodes.contains(newChild)) { nodes += newChild; q.enqueue (newChild) }
                        else cycle = true
                        if (newChild != newNode) chs += newChild 
                    } // for
                    chMap += (newNode -> (chMap.getOrElse (newNode, SET [Int] ()) ++ chs))
                } // if
            } // while

            if (nodes.size < size) nRestarts += 1
        } // while
    
        if (nRestarts == maxRestarts) { println ("genBFSQuery: could not find a good query"); return null }
    
        // create a vertex map from old to new ids, e.g., 7 -> 0, 11 -> 1, 15 -> 2
        val vertexMap = Map [Int, Int] ()
        var c = 0
        for (v <- nodes) { vertexMap += (v -> c); c += 1 }

        // for each new id, record the old id
        val new2OldIds = Array.ofDim [Int] (size)
        vertexMap.foreach { case (oldId, newId) => new2OldIds(newId) = oldId }

        // for each mapped vertex, assign its mapped children
        val ch = Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
        for ((v, v_ch) <- chMap) ch(vertexMap (v)) = v_ch.map (vertexMap (_))

        // map the vertex, edge labels
        val qlabel  = new2OldIds.map (g.label(_)).toArray
        val qelabel = Map [Tuple2 [Int, Int], TLabel] ()
        for (i <- ch.indices; j <- ch(i)) qelabel += (i, j) -> g.elabel((new2OldIds(i), new2OldIds(j)))

        if (cycle) println ("genBFSQuery: query has a cycle")
        new MGraph (ch, qlabel, qelabel, inverse, name)
    } // genBFSQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extracts a subgraph of 'size' vertices from graph 'g' by performing a
     *  breadth-first search from a random vertex.
     *  @param size     the number of vertices to extract
     *  @param g        the data graph to extract from
     *  @param inverse  whether to create inverse adjacency (parents)
     *  @param name     the name of the graph
     */
    def extractSubgraph (size: Int, g: MGraph, inverse: Boolean = false, name: String = "g"): MGraph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var chMap: Map [Int, SET [Int]] = null
        var nodes:  SET [Int] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            chMap    = Map [Int, SET [Int]] ()
            nodes     = SET [Int] ()
            val q     = Queue [Int] ()
            val start = rand.nextInt (g.size)         // randomly pick a start node in ch
            println ("extractSubgraph: start node: " + start)
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                val chs = SET [Int] ()
                val newNode = q.dequeue
                val newNodeChildren = g.ch (newNode)
                if (! newNodeChildren.isEmpty) {
                    for (newChild <- newNodeChildren if nodes.size < size) {
                        if (! nodes.contains (newChild)) { nodes += newChild; q.enqueue (newChild) }
                    } // for
                } // if
            } // while

            for (n <- nodes) { val chs = g.ch(n) intersect nodes; chMap += (n -> chs ) }
            if (nodes.size < size) {
                nRestarts += 1
                println ("nodes.size only " + nodes.size)
            } // if
        } // while

        if (nRestarts == maxRestarts) { println ("extractSubgraph: could not find a good query"); return null }

        // gives the nodes new ids (FIX: refactor to renumber)
        val newLabelMap = Map [Int, Int] () 
        var c = 0 
        for (x <- nodes) { newLabelMap += (x -> c); c += 1 }
        val newToOldLabels = Array.ofDim [Int] (size)
        newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
        val ch = Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
        for ((node, children) <- chMap) ch (newLabelMap(node)) = children.map (x => newLabelMap (x)) 
        val label = newToOldLabels.map (x => g.label(x)).toArray
        new MGraph (ch, label, elabel, inverse, name)
    } // extractSubgraph

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Renumber the selected nodes (give them new consecutive ids).
     *  @param node
     *  @param chMap
     */
//  private def renumber (node: SET [Int], chMap: Map [Int, SET [Int]]): Array [SET [Int]] =
//  {
//      val oldId2newId = Map [Int, Int] ()
//      var i = 0
//      for (v <- node) { oldId2newId += (v -> i); i += 1 }
//      val newToOldLabels = Array.ofDim [Int] (size)
//      for (
//      newLabelMap.foreach { case (oldL, newL) => newToOldLabels (newL) = oldL }
//      Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
//  } // renumber

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with labels distributed between 0 and 'nLabels - 1'.
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => rand.nextInt (nLabels).asInstanceOf [TLabel])
    } // randDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with edge labels distributed between 0 and 'eLabels - 1'.
     *  based on a uniform distribution.
     *  @param ch       the adjacency (child array)
     *  @param nLabels  the number of edge labels
     */
    private def randDisteLabels (ch: Array [SET [Int]], eLabels: Int): Map [Tuple2 [Int, Int], TLabel] =
    {
        val elab = Map [Tuple2 [Int, Int], TLabel] ()
        for (i <- ch.indices; c <- ch(i)) elab += (i, c) -> rand.nextInt (eLabels).asInstanceOf [TLabel]
        elab
    } // randDisteLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with labels distributed between 0 and 'nLabels - 1'.
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
    /** Return an array with labels distributed between 0 and 'nLabels - 1'.
     *  based on a Gaussian/Normal distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def gaussianDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => gaussInt (nLabels / 2.0).asInstanceOf [TLabel])
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
    /** Return an integer with a probability based on a gaussian distribution
     *  centered at d
     *  FIX: may need to truncate with
     *      'math.min(math.max((rand.nextGaussian()*d+d).toInt, 0), d*2).toInt'
     *  @param d  the distance indicator
     */
    private def gaussInt (d: Double) = (rand.nextGaussian () * 2.0 * d).toInt

} // MGraphGen class

import MGraphGen._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'MGraphGenTest' object is used to test the 'MGraphGen' class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 *  > run-main scalation.graphalytics.mutable.MGraphGenTest
 */
object MGraphGenTest extends App
{
    println ("MGraphGenTest: test genRandomGraph")
    (1 to 5).foreach { _ =>
        val g = genRandomGraph (4, 100, 5, 1)
        g.printG ()
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("MGraphGenTest: test genRandomConnectedGraph")
    (1 to 5).foreach { _ => genRandomConnectedGraph (4, 100, 5, 1).printG () }

    println ("MGraphGenTest: test geneRandomGraph_PowLabels")
    val g1 = genRandomGraph_PowLabels (200, 50, 2)
    g1.printG ()
    println (s"g1.labelMap = ${g1.labelMap}")
 
} // MGraphGenTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'MGraphGenTest2' object is used to test the 'MGraphGen' class for building
 *  power law graphs.
 *  > run-main scalation.graphalytics.mutable.MGraphGenTest2
 */
object MGraphGenTest2 extends App
{
    println ("MGraphGenTest2: test genPowerLawGraph")
    val g2 = genPowerLawGraph (50, 10, 10, 2.1)
    g2.printG ()
    g2.ch.sortBy (_.size).foreach { println(_) }

    println ("MGraphGenTest2: test genPowerLawGraph_PowLabels")
    val g3 = genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.printG ()
    g3.ch.sortBy (_.size).foreach { println (_) }
    println (s"g3.labelMap = ${g3.labelMap}")

} // MGraphGenTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGraphGenTest3` object is used to test the `MGraphGen` class for extracting
 *  query graphs from data graphs.
 *  > run-main scalation.graphalytics.mutable.MGraphGenTest3
 */
object MGraphGenTest3 extends App
{
    val nVertices = 10000
    val nLabels   =    10
    val eLabels   =     5
    val avDegree  =    16
    val g = genRandomGraph (nVertices, nLabels, eLabels, avDegree)
    println ("done generating data graph")
    println ("g.size    = " + g.size)
    println ("g.nEdges  = " + g.nEdges)
    println ("av degree = " + g.nEdges / g.size.toDouble)

    println ("MGraphGenTest3: test genBFSQuery")
    (1 to 5).foreach { _ =>
        val q = genBFSQuery (25, 3, g)
        q.printG ()
        println ("q.size    = " + q.size)
        println ("q.nEdges  = " + q.nEdges)
        println ("av degree = " + q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // MGraphGenTest3 object

