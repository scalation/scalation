
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.3
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Queue
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.random._
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGen` object is used to build random graphs with various characteristics.
 *  Needs to generate vertex labels of various types including `Int`, `Double`,
 *  `String`, `VectorD` based on the `TLabel` type.
 *  @param typeSelector  the variable for type matches (work around for generic erasure)
 */
class GraphGen [TLabel: ClassTag] (typeSelector: TLabel)
       extends Error
{
    /** Random number stream to use (0 to 999)
     */
    private val stream = 0

    /** Random number generator in interval (0, 1)
     */
    private val ran = Random ()

    /** Random number/integer generator: 0, 1, ...
     */
    private val rng = Randi0 ()

    /** Random set generator
     */
    private val rsg = RandomSet ()

    /** Random label generator for the `TLabel` type parameter
     */
    private var rlg: Variate = _

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the Random Variate Generator, using the one based on the `TLabel` type.
     *  @param nLabs  the number of labels to generate
     */
    def setVariate (nLabs: Int)
    { 
        typeSelector match {
        case _: Int     => rlg = Randi (1, nLabs, stream)
        case _: Double  => rlg = Randi (1, nLabs, stream)
        case _: String  => rlg = RandomWord (nLabs, stream = stream)
//      case _: VectorD => rlg = RandomVecD (nLabs, stream)
        case _          => flaw ("setVariate", "label type not supported")
        } // match
    } // setVariate

    //------------------------------------------------------------------------
    // Methods generating random graphs where the number of outgoing edges (the degree)
    // for a vertex is uniformly distributed.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random graph with the specified size (number of vertices), 
     *  average degree and labels evenly distributed across vertices from 0 to
     *  'nLabels - 1'.  Not necessarily a connected graph.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                        name: String = "g"): Graph [TLabel] =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = rng.iigen (avDegree * 2 + 1)              // out degree for vertex i
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = randDistLabels (size, nLabels)                 // vertex labels
        new Graph (ch, label, inverse, name)
    } // genRandomGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generates a random connected graph by using 'genRandomGraph' and
     *  checking whether it is connected.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                                 name: String = "g"): Graph [TLabel] =
    {
        var g: Graph [TLabel] = null
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
                                  name: String = "g"): Graph [TLabel] =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = rng.iigen (avDegree * 2 + 1)              // out degree for vertex i
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = powDistLabels (size, nLabels, 2.1)             // 2.1 is used in WWW graph pg 72 of m&m graph data
        new Graph (ch, label, inverse, name)
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
                          inverse: Boolean = false, name: String = "g"): Graph [TLabel] =
    {
        val powLaw = PowerLaw (0, maxDegree, distPow)
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = powLaw.igen                               // out degree
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = randDistLabels (size, nLabels)
        new Graph (ch, label, inverse, name)
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
                                    inverse: Boolean = false, name: String = "g"): Graph [TLabel] =
    {
        val powLaw = PowerLaw (0, maxDegree, distPow)
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = powLaw.igen                               // out degree
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for
        val label = powDistLabels (size, nLabels, distPow)
        new Graph (ch, label, inverse, name)
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
    def genBFSQuery (size: Int, avDegree: Int, g: Graph [TLabel], inverse: Boolean = false,
                     name: String = "g"): Graph [TLabel] =
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
        new Graph (ch, label, inverse, name)
    } // genBFSQuery

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 1 and 'nLabels'.
     *  based on a uniform distribution.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     */
    private def randDistLabels (size: Int, nLabels: Int): Array [TLabel] =
    {
        setVariate (nLabels)                                                                // set rlg
        typeSelector match {
        case _: Int     => Array.ofDim (size).map (_ => rlg.igen.asInstanceOf [TLabel])     // for `Int`
        case _: Double  => Array.ofDim (size).map (_ => rlg.gen.asInstanceOf [TLabel])      // for `Double`
        case _: String  => Array.ofDim (size).map (_ => rlg.sgen.asInstanceOf [TLabel])     // for `String`
//      case _: VectorD => // implement                                                     // for `VectorD`
        case _          => { flaw ("randDistLabels", "label type not supported"); null.asInstanceOf [Array [TLabel]] }
        } // match
    } // randDistLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns an array with labels distributed between 1 and 'nLabels'.
     *  based on a power law distribution.
     *  FIX: 'nLabels' not valid to `Double` and `String` and check correctness of `PowerLaw`.
     *  FIX: for `String` would like something like `RandomWord`.
     *  @param size     the number of vertices
     *  @param nLabels  the number of labels
     *  @param pow      the power/exponent
     */
    private def powDistLabels (size: Int, nLabels: Int, pow: Double): Array [TLabel] =
    {
        val powLaw = PowerLaw (1.0, nLabels + 1.0, pow, stream)
        typeSelector match {
        case _: Int     => Array.ofDim (size).map (_ => powLaw.igen.asInstanceOf [TLabel])
        case _: Double  => Array.ofDim (size).map (_ => powLaw.gen.asInstanceOf [TLabel])
        case _: String  => Array.ofDim (size).map (_ => powLaw.sgen.asInstanceOf [TLabel])
//      case _: VectorD => // implement
        case _          => { flaw ("powDistLabels", "label type not supported"); null.asInstanceOf [Array [TLabel]] }
        } // match
    } // powDistLabels

} // GraphGen class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest` object is used to test the `GraphGen` class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 *  This work build graphs with `Int` vertex labels.
 *  > run-main scalation.graphalytics.mutable.GraphGenTest
 */
object GraphGenTest extends App
{
    val gGen = new GraphGen [Int] (0)

    println ("GraphGenTest: test genRandomGraph")
    (1 to 5).foreach { _ =>
        val g = gGen.genRandomGraph (4, 100, 1)
        g.printG ()
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("GraphGenTest: test genRandomConnectedGraph")
    (1 to 5).foreach { _ => gGen.genRandomConnectedGraph (4, 100, 1).printG () }

    println ("GraphGenTest: test genRandomGraph_PowLabels")
    val g1 = gGen.genRandomGraph_PowLabels (200, 50, 2)
    g1.printG ()
    println (s"g1.labelMap = ${g1.labelMap}")
 
} // GraphGenTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest2` object is used to test the `GraphGen` class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 *  This work build graphs with `Double` vertex labels.
 *  > run-main scalation.graphalytics.mutable.GraphGenTest2
 */
object GraphGenTest2 extends App
{
    val gGen = new GraphGen [Double] (0.0)

    println ("GraphGenTest: test genRandomGraph")
    (1 to 5).foreach { _ =>
        val g = gGen.genRandomGraph (4, 100, 1)
        g.printG ()
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("GraphGenTest2: test genRandomConnectedGraph")
    (1 to 5).foreach { _ => gGen.genRandomConnectedGraph (4, 100, 1).printG () }

    println ("GraphGenTest2: test genRandomGraph_PowLabels")
    val g1 = gGen.genRandomGraph_PowLabels (200, 50, 2)
    g1.printG ()
    println (s"g1.labelMap = ${g1.labelMap}")

} // GraphGenTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest3` object is used to test the `GraphGen` class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 *  This work build graphs with `String` vertex labels.
 *  > run-main scalation.graphalytics.mutable.GraphGenTest3
 */
object GraphGenTest3 extends App
{
    val gGen = new GraphGen [String] ("0")

    println ("GraphGenTest: test genRandomGraph")
    (1 to 5).foreach { _ =>
        val g = gGen.genRandomGraph (4, 100, 1)
        g.printG ()
        println ("CONNECTED?  " + g.isConnected)
    } // foreach

    println ("GraphGenTest3: test genRandomConnectedGraph")
    (1 to 5).foreach { _ => gGen.genRandomConnectedGraph (4, 100, 1).printG () }

    println ("GraphGenTest3: test genRandomGraph_PowLabels")
    val g1 = gGen.genRandomGraph_PowLabels (200, 50, 2)
    g1.printG ()
    println (s"g1.labelMap = ${g1.labelMap}")

} // GraphGenTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest4` object is used to test the `GraphGen` class for building
 *  power law graphs.
 *  > run-main scalation.graphalytics.mutable.GraphGenTest4
 */
object GraphGenTest4 extends App
{
    val gGen = new GraphGen [Int] (0)

    println ("GraphGenTest4: test genPowerLawGraph")
    val g2 = gGen.genPowerLawGraph (50, 10, 10, 2.1)
    g2.printG ()
    g2.ch.sortBy (_.size).foreach { println(_) }

    println ("GraphGenTest4: test genPowerLawGraph_PowLabels")
    val g3 = gGen.genPowerLawGraph_PowLabels (50, 10, 10, 2.1)
    g3.printG ()
    g3.ch.sortBy (_.size).foreach { println(_) }
    println (s"g3.labelMap = ${g3.labelMap}")

} // GraphGenTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest5` object is used to test the `GraphGen` class for extracting
 *  query graphs from data graphs.
 *  > run-main scalation.graphalytics.mutable.GraphGenTest5
 */
object GraphGenTest5 extends App
{
    val gGen = new GraphGen [Int] (0)

    println ("GraphGenTest5: test genRandomGraph")
    val nVertices = 10000
    val nLabels   =    10
    val avDegree  =    16     
    var g = gGen.genRandomGraph (nVertices, nLabels, avDegree)
    println ("done generating data graph")
    println ("g.size    = " + g.size)
    println ("g.nEdges  = " + g.nEdges)
    println ("av degree = " + g.nEdges / g.size.toDouble)

    println ("GraphGenTest5: test genBFSQuery")
    (1 to 5).foreach { _ =>
        var q = gGen.genBFSQuery (20, 3, g)
        q.printG ()
        println ("q.size    = " + q.size)
        println ("q.nEdges  = " + q.nEdges)
        println ("av degree = " + q.nEdges / q.size.toDouble)
    } // foreach
    println ("done")

} // GraphGenTest5

