
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.2
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.Queue
import collection.mutable.{Set => SET}
import math.pow

import scalation.linalgebra.VectorI
import scalation.random.{Randi0, Random, RandomSet}

import LabelType.TLabel

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigGen` object is used to build random graph with various characteristics.
 */
object DigGen
{
    /** Random number generator in interval (0, 1)
     */
    private val ran = Random ()

    /** Random number/integer generator: 0, 1, ...
     */
    private val rng = Randi0 ()

    /** Random set generator
     */
    private val rsg = RandomSet ()

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
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                        name: String = "g"): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = rng.iigen (avDegree * 2 + 1)              // out degree for vertex i
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = randDistLabels (size, nLabels)                 // vertex labels
        new Digraph (ch, label, inverse, name)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random connected graph by using genRandomGraph and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                                 name: String = "g"): Digraph =
    {
        var g: Digraph = null
        do g = genRandomGraph (size, nLabels, avDegree, inverse, name) while (! g.isConnected)
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
                                  name: String = "g"): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = rng.iigen (avDegree * 2 + 1)              // out degree for vertex i
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = powDistLabels (size, nLabels, 2.1)             // 2.1 is used in WWW graph pg 72 of m&m graph data
        new Digraph (ch, label, inverse, name)
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
                          inverse: Boolean = false, name: String = "g"): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = powInt (0, maxDegree, distPow)            // out degree
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = randDistLabels (size, nLabels)
        new Digraph (ch, label, inverse, name)
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
                                    inverse: Boolean = false, name: String = "g"): Digraph =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = powInt (0, maxDegree, distPow)            // out degree
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = powDistLabels (size, nLabels, distPow)
        new Digraph (ch, label, inverse, name)
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
    def genBFSQuery (size: Int, avDegree: Int, g: Digraph, inverse: Boolean = false,
                     name: String = "g"): Digraph =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var cycle       = false
        var nodes       = SET [Int] ()
        var chMap: Map [Int, SET [Int]] = null

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            chMap      = Map [Int, SET [Int]] ()
            nodes      = SET [Int] ()
            val q      = Queue [Int] ()
            val start  = rng.iigen (g.size - 1)     // randomly pick a start node in ch 
            q.enqueue (start)
            nodes += start

            while (! q.isEmpty && nodes.size < size) {
                var chs  = SET [Int] ()                                 // set of children to be formed
                val v    = q.dequeue                                    // candidate vertex
                val v_ch = g.ch (v)                                     // its children
                if (v_ch.nonEmpty) {
                    val v_chArr = v_ch.toArray                          // its children in an array
                    val degree = rng.iigen (avDegree * 2 + 1)           // desired out degree
                    for (i <- 0 until degree if nodes.size < size) {
                        val newChild = v_chArr (rng.iigen (v_ch.size - 1)) 
                        if (! (nodes contains newChild)) { nodes += newChild; q.enqueue (newChild) }
                        else cycle = true
                        if (newChild != v) chs += newChild               // add newChild
                    } // for
                    chMap += (v -> (chMap.getOrElse (v, SET [Int] ()) ++ chs))
                } // if
            } // while

            if(nodes.size < size) nRestarts += 1
        } // while
    
        if (nRestarts == maxRestarts) { println ("genBFSQuery: could not find a good query"); return null }
    
        // create a vertex map from old to new ids, e.g., 7 -> 0, 11 -> 1, 15 -> 2
        var vertexMap = Map [Int, Int] () 
        var c = 0 
        for (v <- nodes) { vertexMap += (v -> c); c += 1 }

        // for each new id, record the old id
        val new2OldIds = Array.ofDim [Int] (size)
        vertexMap.foreach { case (oldId, newId) => new2OldIds(newId) = oldId }

        // for each mapped vertex, assign its mapped children
        val ch = Array.ofDim [SET [Int]] (size).map (x => SET [Int] ())
        for ((v, v_ch) <- chMap) ch(vertexMap (v)) = v_ch.map (vertexMap (_)) 

        // map the vertex labels
        val label = new2OldIds.map (g.label(_)).toArray
        if (cycle) println ("genBFSQuery: query has a cycle")
        new Digraph (ch, label, inverse, name)
    } // genBFSQuery

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 0 and nLabels - 1
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        Array.ofDim [TLabel] (size).map ( x => rng.iigen (nLabels - 1).asInstanceOf [TLabel])
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
    /** Returns a random integer between min and max with a frequency determined
     *  by a power law distribution.
     *  @param min      the minimum value
     *  @param max      the maximum value
     *  @param distPow  the power distribution
     */
    private def powInt (min: Int, max: Int, distPow: Double): Int =
    {
        val exp = distPow + 1.0
        max - 1 - pow (( (pow (max, exp) - pow (min, exp)) * ran.gen + pow (min, exp) ),
                       (1.0 / exp)).toInt
    } // powInt

} // DigGen class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigGenTest` object is used to test the `DigGen` class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 *  > run-main scalation.graphalytics.DigGenTest
 */
object DigGenTest extends App
{
    import DigGen._

    println ("DigGenTest: test genRandomGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomGraph (4, 100, 1)
        g.print ()
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("DigGenTest: test genRandomConnectedGraph")
    (0 until 10).foreach { _ =>
        val g = genRandomConnectedGraph (4, 100, 1)
        g.print ()
    } // foreach

    println ("DigGenTest: test geneRandomGraph_PowLabels")
    val g1 = genRandomGraph_PowLabels (200, 50, 2)
    g1.print ()
    println (s"g1.labelMap = ${g1.labelMap}")
 
} // DigGenTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigGenTest2` object is used to test the `DigGen` class for building
 *  power law graphs.
 *  > run-main scalation.graphalytics.DigGenTest2
 */
object DigGenTest2 extends App
{
    import DigGen._

    println ("DigGenTest2: test genPowerLawGraph")
    val g2 = genPowerLawGraph (50, 10, 10, 2.1)
    g2.print ()
    g2.ch.sortBy (_.size).foreach { println(_) }

    println ("DigGenTest2: test genPowerLawGraph_PowLabels")
    val g3 = genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.print ()
    g3.ch.sortBy (_.size).foreach { println(_) }
    println (s"g3.labelMap = ${g3.labelMap}")

} // DigGenTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DigGenTest3` object is used to test the `DigGen` class for extracting
 *  query graphs from data graphs.
 *  > run-main scalation.graphalytics.DigGenTest3
 */
object DigGenTest3 extends App
{
    import DigGen._

    println ("DigGenTest3: test genRandomGraph")
    val nVertices = 1000
    val avDegree  =   10     
    val nLabels   =   16
    var g = genRandomGraph (nVertices, avDegree, nLabels)
    println ("done generating data graph")
    println ("g.size   = " + g.size)
    println ("g.nEdges = " + g.nEdges)

    println ("DigGenTest3: test genBFSQuery")
    (1 until 5).foreach { i =>
        var q = genBFSQuery (20, 3, g)
        q.print ()
        println (q.size)
        println (q.nEdges)
        println (q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // DigGenTest3

