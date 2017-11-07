
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.4
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db

import scala.collection.mutable.{Map, Queue}
import scala.collection.mutable.{Set => SET}
import scala.math.{abs, pow}
import scala.reflect.ClassTag

import scalation.random._
import scalation.util.{banner, Error}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGen` object is used to build random graphs with various characteristics.
 *  Needs to generate vertex labels of various types including `Int`, `Double`,
 *  `String`, `VectorD` based on the `TLabel` type.
 *  @param typeSelector  the variable for type matches (work around for generic erasure)
 *  @param stream        the random number stream to use (0 to 999)
 */
class GraphGen [TLabel: ClassTag] (typeSelector: TLabel, stream: Int = 0)
       extends Error
{
    /** Debug flag
     */
    private val DEBUG = false 

    /** Random number generator with interval (0, 1)
     */
    private val ran = Random (stream)

    /** Random number/integer generator: 0, 1, ...
     */
    private val rng = Randi0 (stream = stream)

    /** Random set generator
     */
    private val rsg = RandomSet (stream = stream)

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
    /** Generate a random graph with the specified size (number of vertices), 
     *  average degree and labels evenly distributed across vertices from 0 to
     *  'nLabels - 1'.  Not necessarily a connected graph.
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed uniformly)
     *  @param avDegree  the average degree
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     *  @param locality  whether to select children from anywhere in graph or locally
     */
    def genRandomGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = false,
                        name: String = "g", locality: Boolean = true): Graph [TLabel] =
    {
        val hwidth = avDegree + 4                                   // half width for generation window
        val width  = 2 * hwidth                                     // full width
        val ch     = Array.fill [SET [Int]] (size)(SET [Int] ())    // holder for children

        for (i <- ch.indices) {
            val degree = rng.igen1 (avDegree * 2 + 1)               // desired out-degree for vertex i
            if (locality) {
                val delta  = rsg.igen (degree, width, i)            // offsets for children
                for (j <- delta) {
                    var ch_i = abs (i + j - hwidth)
                    if (ch_i >= size) ch_i -= ch_i - size + 1
                    if (ch_i == i) ch_i = abs (i - 1)
                    ch(i) += ch_i                                   // generate children randomly with locality
                } // for
            } else {
                ch(i) = rsg.igen (degree, size-1, i)                // generate children uniformly across graph
            } // if
        } // for

        val label = randDistLabels (size, nLabels)                  // randomly assign vertex labels
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
    def genRandomConnectedGraph (size: Int, nLabels: Int, avDegree: Int, inverse: Boolean = true,
                                 name: String = "g"): Graph [TLabel] =
    {
        var g: Graph [TLabel] = null
        var it = 0
        do {
            g = genRandomGraph (size, nLabels, avDegree, inverse, name)
            it += 1
        } while (! g.isConnected)
        println (s"genRandomConnectedGraph: $it iterations")
        g
    } // genRandomConnectedGraph
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a random graph with labels distributed based on a power law
     *  distribution (currently with the magic number 2.1 for the power law exponent).
     *  @param size      the number of vertices to generate
     *  @param nLabels   the number of labels (distributed according to power law)
     *  @param avDegree  the average out-degree
     *  @param distPow   the power/exponent (2.1 is used in WWW graph pg 72 of m&m graph data)
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genRandomGraph_PowLabels (size: Int, nLabels: Int, avDegree: Int, distPow: Double = 2.1,
                                  inverse: Boolean = true, name: String = "g"): Graph [TLabel] =
    {
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = rng.igen1 (avDegree * 2 + 1)                   // out-degree for vertex i
            ch(i)      = rsg.igen (degree, size - 1, i)                 // children of vertex i
        } // for

        val label = powDistLabels (size, nLabels, distPow)              // power law for vertex lables
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
     *  @param distPow    the power/exponent (2.1 is used in WWW graph pg 72 of m&m graph data)
     *  @param inverse    whether to create inverse adjacency (parents)
     *  @param name       the name of the graph
     */
    def genPowerLawGraph (size: Int, nLabels: Int, maxDegree: Int, distPow: Double = 2.1,
                          inverse: Boolean = true, name: String = "g"): Graph [TLabel] =
    {
        val powLaw = PowerLaw (1, maxDegree, distPow)
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = powLaw.igen                               // out-degree
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for

        val label = randDistLabels (size, nLabels)                 // randomly assign vertex labels
        new Graph (ch, label, inverse, name)
    } // genPowerLawGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a graph with power law degree distribution with exponent 'distPow'
     *  and power law distributed labels.
     *  @param size       the number of vertices 
     *  @param nLabels    the number of labels (distributed according to power law)
     *  @param maxDegree  the maximum allowed degree for any vertex
     *  @param distPow    the power/exponent (2.1 is used in WWW graph pg 72 of m&m graph data)
     *  @param inverse    whether to create inverse adjacency (parents)
     *  @param name       the name of the graph
     */
    def genPowerLawGraph_PowLabels (size: Int, nLabels: Int, maxDegree: Int, distPow: Double = 2.1,
                                    inverse: Boolean = false, name: String = "g"): Graph [TLabel] =
    {
        val powLaw = PowerLaw (1, maxDegree, distPow)
        val ch = Array.ofDim [SET [Int]] (size)
        for (i <- ch.indices) {
            val degree = powLaw.igen                               // out-degree
            ch(i)      = rsg.igen (degree, size - 1, i)            // children of vertex i
        } // for

        val label = powDistLabels (size, nLabels, distPow)         // power law for vertex lables
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
     *  @param avDegree  the average out-degree
     *  @param g         the data graph to extract from
     *  @param inverse   whether to create inverse adjacency (parents)
     *  @param name      the name of the graph
     */
    def genBFSQuery (size: Int, avDegree: Int, g: Graph [TLabel], inverse: Boolean = false,
                     name: String = "g"): Graph [TLabel] =
    {
        val maxRestarts = 5000                                            // limit on retries
        val nedges      = avDegree * size                                 // desired number of edges

        var nRestarts   = 0                                               // restarts so far
        var nodes       = SET [Int] ()                                    // current set of extracted vertices
        var edges       = 0                                               // number of edges so far
        var maxEdges    = 0                                               // number of edges in best try
        var maxNodes: SET [Int] = null                                    // nodes in best try
        var chMap:    Map [Int, SET [Int]] = null                         // child map
        var maxChMap: Map [Int, SET [Int]] = null                         // child map in best try

        while ((nodes.size < size || edges < nedges) && nRestarts < maxRestarts) {
            nodes   = SET [Int] ()
            chMap   = Map [Int, SET [Int]] ()
            val q   = Queue [Int] ()                                      // queue for BFS visitation
            var v_s = -1                                                  // starting vertex

            var degree = 1 + rng.igen1 (avDegree * 2)                     // desired out-degree
            do {
                v_s = rng.igen1 (g.size - 1)                              // randomly pick a start vertex
            } while (g.ch(v_s).size < degree + 1)

            nodes += v_s                                                  // add starting vertex to nodes
            q.enqueue (v_s)                                               // place it in the BFS queue
            edges = 0                                                     // no edges yet

            while (! q.isEmpty && nodes.size < size) {                    // need size number of vertices

                val v = q.dequeue                                         // candidate vertex v
                val v_chs = g.ch(v)                                       // all its children
                degree = 1 + rng.igen1 (avDegree * 2)                     // desired out-degree
                val chs   = pickChildren (v, v_chs.toArray, degree)       // pick random subset of v's children
                val chs2  = SET [Int] ()                                  // those making the cut

                for (v_ch <- chs) {                                       // chs2: appropriate vertices from chs
                    degree = 1 + rng.igen1 (avDegree * 2)
                    if (g.ch(v_ch).size >= degree + 1) {
                        if (nodes.size < size) {                          // still need vertices => take edges
                            if (! (nodes contains v_ch)) {
                                nodes += v_ch                             // add child vertex to nodes
                                q.enqueue (v_ch)                          // put it on the BFS queue
                            } // if
                            chs2 += v_ch
                            edges += 1
                        } else if (nodes contains v_ch) {                 // can only take edge if child in nodes
                            chs2 += v_ch
                            edges += 1
                        } else {
                           println ("genBFSquery: can't find enough child vertices for $v")
                        } // if
                    } // if
                } // for

                println (s"nodes = $nodes")
                chMap += (v -> (chMap.getOrElse (v, SET [Int] ()) ++ chs2))
            } // while

            println (s"chMap = $chMap")

            if (edges > maxEdges) {                                       // if most edges so far, save as best
                maxEdges = edges
                maxNodes = nodes.clone ()
                maxChMap = chMap.clone ()
            } // if

            if (nodes.size < size || edges < nedges) {                    // not enough vertices/edges, try again
                nRestarts += 1
                println ("=" * 80)
                println (s"RESTART: #restarts = $nRestarts, #nodes = ${nodes.size}, #edges = $edges")
                println ("=" * 80)
            } // if
        } // while

        println (s"maxEdges = $maxEdges")

        if (nRestarts == maxRestarts) println ("genBFSQuery: could not find a query graph with enough edges")
    
        GraphGen.buildQGraph (maxNodes, maxChMap, g.label, inverse, name)
    } // genBFSQuery

    //------------------------------------------------------------------------
    // Private helper methods.
    //------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly pick 'degree' children of vertex 'v'.  Requires 'v' to have at
     *  least 'degree children.
     *  @param v       the given vertex
     *  @param v_ch    vertex v's children as an array
     *  @param degree  the number of children to pick (out-degree)
     */
    private def pickChildren (v: Int, v_ch: Array [Int], degree: Int): SET [Int] =
    {
        println (s"pickChildren: v = $v, v_ch = ${v_ch.deep}, degree = $degree")
        val chs = SET [Int] ()                                       // set of children to be formed
        var it  = 0                                                  // iteration counter
        do {
            val v_c = v_ch(rng.igen1 (v_ch.size - 1))
            if (v_c != v) chs += v_c
            it += 1
            if (it > 100 * degree) { println ("pickChildren: can't find enough children"); return SET [Int] () }
        } while (chs.size < degree)
        println (s"pickChildren: chs = $chs")
        chs
    } // pickChildren


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an array with labels distributed between 1 and 'nLabels'.
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
    /** Return an array with labels distributed between 1 and 'nLabels'.
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
/** The `GraphGen` companion object provides simple methods for creating data
 *  and query graphs.
 */
object GraphGen
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly generate a data graph 'g'.
     *  @param gSize      the size of the data graph
     *  @param nLabels    the number of distinct labels
     *  @param gAvDegree  the average vertex out-degree for data graph
     *  @param addPa      whether to add direct references to parents
     *  @param typeSel    the type selector
     */
    def genGraph [TLabel: ClassTag] (typeSel: TLabel, stream: Int = 0, gSize: Int = 100,
                  nLabels: Int = 100, gAvDegree: Int = 8, addPa: Boolean = false):
        Graph [TLabel] =
    {
        val gGen = new GraphGen [TLabel] (typeSel, stream)
        gGen.genRandomConnectedGraph (gSize, nLabels, gAvDegree, addPa, "g")   // data graph
    } // genGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly generate both a data graph 'g' and a query graph 'q'.
     *  @param gSize      the size of the data graph
     *  @param qSize      the size of the query graph
     *  @param nLabels    the number of distinct labels
     *  @param gAvDegree  the average vertex out-degree for data graph
     *  @param qAvDegree  the average vertex out-degree for query graph
     *  @param addPa      whether to add direct references to parents
     *  @param typeSel    the type selector
     */
    def genGraphs [TLabel: ClassTag] (typeSel: TLabel, stream: Int = 0,
                   gSize: Int = 100, qSize: Int = 10, nLabels: Int = 100,
                   gAvDegree: Int = 8, qAvDegree: Int = 2, addPa: Boolean = false):
        (Graph [TLabel], Graph [TLabel]) =
    {
        val gGen = new GraphGen [TLabel] (typeSel, stream)
        val g = gGen.genRandomConnectedGraph (gSize, nLabels, gAvDegree, addPa, "g")   // data graph
        val q = gGen.genBFSQuery (qSize, qAvDegree, g, addPa, "q")                     // query graph
        (g, q)
    } // genGraphs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly generate both a data graph 'g' and a query graph 'q' 
     *  using Power Law
     *  @param gSize      the size of the data graph
     *  @param qSize      the size of the query graph
     *  @param nLabels    the number of distinct labels
     *  @param gAvDegree  the average vertex out-degree for data graph
     *  @param qAvDegree  the average vertex out-degree for query graph
     *  @param addPa      whether to add direct references to parents
     *  @param typeSel    the type selector
     */
    def genPowerGraphs [TLabel: ClassTag] (typeSel: TLabel, stream: Int = 0,
                   gSize: Int = 100, qSize: Int = 10, nLabels: Int = 100,
                   gAvDegree: Int = 8, qAvDegree: Int = 2, addPa: Boolean = false):
        (Graph [TLabel], Graph [TLabel]) =
    {
        val gGen = new GraphGen [TLabel] (typeSel, stream)
        val g = gGen.genPowerLawGraph (gSize, nLabels, gAvDegree)                      // data graph
        val q = gGen.genBFSQuery (qSize, qAvDegree, g, addPa, "q")                     // query graph
        (g, q)
    } // genPowerGraphs

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Extract a subgraph of 'size' vertices from graph 'g' by performing a
     *  breadth-first search from a random vertex.
     *  @param size     the number of vertices to extract
     *  @param g        the data graph to extract from
     *  @param inverse  whether to create inverse adjacency (parents)
     *  @param name     the name of the graph
     */
    def extractSubgraph [TLabel: ClassTag] (size: Int, g: Graph [TLabel], inverse: Boolean = false, 
                                            name: String = "g", stream: Int = 0): Graph [TLabel] =
    {
        val maxRestarts = 5000
        var nRestarts   = 0
        var chMap: Map [Int, SET [Int]] = null
        var nodes = SET [Int] ()                                    // current set of extracted vertices
        val rng = Randi0 (stream = stream)

        while (nodes.size < size && nRestarts < maxRestarts) {
            if (nRestarts % 100 == 0) println ("restarting " + nRestarts)
            chMap     = Map [Int, SET [Int]] ()
            nodes     = SET [Int] ()
            val q     = Queue [Int] ()
            val start =  rng.igen1 (g.size - 1)                           // randomly pick a start node in ch
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

        buildQGraph (nodes, chMap, g.label, inverse, name)

    } // extractSubgraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a query graph from the subgraph extracted from the data graph.
     *  Includes the renumbering of vertex ids.
     *  @param nodes    the nodes extracted from the data graph
     *  @param chMap    the child map: vertex -> children
     *  @param gLabel   the labels from the data graph
     *  @param inverse  whether to create inverse adjacency (parents)
     *  @param name     the name for the new query graph
     */
    def buildQGraph [TLabel: ClassTag] (nodes: SET [Int], chMap: Map [Int, SET [Int]], gLabel: Array [TLabel],
                             inverse: Boolean, name: String): Graph [TLabel] =
    {
        // create a vertex map from old to new ids, e.g., 7 -> 0, 11 -> 1, 15 -> 2
        var vertexMap = Map [Int, Int] ()
        var c = 0
        for (v <- nodes) { vertexMap += (v -> c); c += 1 }

        // for each new id, record the old id
        val new2OldIds = Array.ofDim [Int] (nodes.size)
        vertexMap.foreach { case (oldId, newId) => new2OldIds(newId) = oldId }

        // for each mapped vertex, assign its mapped children
        val ch = Array.ofDim [SET [Int]] (nodes.size).map (x => SET [Int] ())
        for ((v, v_ch) <- chMap) ch(vertexMap (v)) = v_ch.map (vertexMap (_))

        // map the vertex labels
        val label = new2OldIds.map (gLabel(_)).toArray
        new Graph (ch, label, inverse, name)
    } // buildQGraph

} // GraphGen


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest` object is used to test the `GraphGen` class for building
 *  random graphs where a vertex's degree is uniformly distributed.
 *  This work build graphs with `Int` vertex labels.
 *  > run-main scalation.graph_db.GraphGenTest
 */
object GraphGenTest extends App
{
    val gGen = new GraphGen (0)

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
 *  > run-main scalation.graph_db.GraphGenTest2
 */
object GraphGenTest2 extends App
{
    val gGen = new GraphGen (0.0)

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
 *  > run-main scalation.graph_db.GraphGenTest3
 */
object GraphGenTest3 extends App
{
    val gGen = new GraphGen ("0")

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
 *  > run-main scalation.graph_db.GraphGenTest4
 */
object GraphGenTest4 extends App
{
    val gGen = new GraphGen (0)

    println ("GraphGenTest4: test genPowerLawGraph")
    val g2 = gGen.genPowerLawGraph (50, 10, 10)
    g2.printG ()
    g2.ch.sortBy (_.size).foreach { println(_) }

    println ("GraphGenTest4: test genPowerLawGraph_PowLabels")
    val g3 = gGen.genPowerLawGraph_PowLabels (50, 10, 10)
    g3.printG ()
    g3.ch.sortBy (_.size).foreach { println(_) }
    println (s"g3.labelMap = ${g3.labelMap}")

} // GraphGenTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest5` object is used to test the `GraphGen` class for extracting
 *  query graphs from data graphs (note: data graph should be connected).
 *  > run-main scalation.graph_db.GraphGenTest5
 */
object GraphGenTest5 extends App
{
    val gGen = new GraphGen (0.0)

    banner ("GraphGenTest5: test genRandomConnectedGraph")
    val nVertices = 100
    val nLabels   =    10
    val avDegree  =    5     
    var g = gGen.genRandomConnectedGraph (nVertices, nLabels, avDegree)
    println ("done generating data graph")
    println (GraphMetrics.stats (g))

    banner ("GraphGenTest5: test genBFSQuery")
    (1 to 5).foreach { _ =>
        var q = gGen.genBFSQuery (5, 2, g)
        q.printG ()
        println (GraphMetrics.stats (q))
    } // foreach
    println ("done")

} // GraphGenTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest6` object is used to test the `GraphGen` companion object
 *  for generating both data and query graphs.
 *  > run-main scalation.graph_db.GraphGenTest6
 */
object GraphGenTest6 extends App
{
    for (stream <- 0 until 3) {
        val (g, q) = GraphGen.genGraphs (0.0, stream)
        banner ("data graph")
        g.printG ()
        println (GraphMetrics.stats (g))
        banner ("query graph")
        q.printG ()
        println (GraphMetrics.stats (q))
    } // for

} // GraphGenTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest7` object is used to test the `GraphGen` companion object
 *  for generating data graphs.
 *  > run-main scalation.graph_db.GraphGenTest7
 */
object GraphGenTest7 extends App
{
    for (stream <- 0 until 3) {
        val g = GraphGen.genGraph (0.0, stream)
        banner ("data graph")
        g.printG ()
        println (GraphMetrics.stats (g))
    } // for

} //GraphGenTest7


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphGenTest8` object is used to test the `MGraphGen` companion object
 *  for generating both data and query graphs.
 *  > run-main scalation.graph_db.GraphGenTest8
 */
object GraphGenTest8 extends App
{
     val g = GraphGen.genGraph ("0.0")
     banner ("data graph")
     g.printG ()
     println (GraphMetrics.stats (g))
     banner ("query graph")
     val q = GraphGen.extractSubgraph(5, g)
     q.printG ()
     println (GraphMetrics.stats (q))

} // GraphGenTest8

