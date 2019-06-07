
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.3
 *  @date    Thu Nov 10 14:14:46 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.multi

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.graphalytics.mutable.GraphMatcher
import scalation.util.time

import MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualIso` class provides an implementation for Subgraph Isomorphism
 *  that uses Dual Graph Simulation for pruning.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class MuDualIso [TLabel: ClassTag] (g: MuGraph [TLabel], q: MuGraph [TLabel])
      extends GraphMatcher (g, q)
{
    private val duals        = new MuDualSim (g, q)        // object for Dual Simulation algorithm
    private var t0           = 0.0                         // start time for timer
    private var matches      = SET [Array [SET [Int]]] ()  // initialize matches to empty
    private var noBijections = true                        // no results yet
    private var limit        = 1000000                     // limit on number of matches

    def prune (phi: Array [SET [Int]]): Array [SET [Int]] = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set an upper bound on the number matches to allow before quitting.
     *  @param _limit  the number of matches before quitting
     */
    def setLimit (_limit: Int) { limit = _limit }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Subgraph Isomorphism algorithm to find subgraphs of data
     *  graph 'g' that isomorphically match query graph 'q'.  These are represented
     *  by a set of single-valued bijections {'psi'} where each 'psi' function
     *  maps each query graph vertex 'u' to a data graph vertices 'v'.
     */
    override def bijections (): SET [Array [Int]] =
    {
        matches = SET [Array [SET [Int]]] ()               // initialize matches to empty
        val phi = duals.feasibleMates ()                   // initial mappings from label match
        saltzDualIso (duals.saltzDualSim (phi), 0)         // recursively find all bijections
        val psi = simplify (matches)                       // pull bijections out matches
        noBijections = false                               // results now available
        psi                                                // return the set of bijections
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Subgraph Isomorphism pattern matching algorithm to find
     *  the mappings from the query graph 'q' to the data graph 'g'.  These are
     *  represented by a  multi-valued function 'phi' that maps each query graph
     *  vertex 'u' to a set of data graph vertices '{v}'.
     */
    override def mappings (): Array [SET [Int]] = 
    {
        var psi: SET [Array [Int]] = null              // mappings from Dual Simulation
        if (noBijections) psi = bijections ()          // if no results, create them
        merge (psi)                                    // merge bijections to create mappings
    } // mappings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the count of the number of matches.
     */
    def numMatches (): Int = matches.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Refine the mappings 'phi' using the Dual Subgraph Isomorphism algorithm.
     *  Enumerate bijections by using an Ullmann-like recursion that uses Dual
     *  Graph Simulation for pruning.
     *  @param phi    array of mappings from a query vertex u_q to { graph vertices v_g }
     *  @param depth  the depth of recursion
     */
    private def saltzDualIso (phi: Array [SET [Int]], depth: Int)
    {
        if (depth == q.size) {
            if (! phi.isEmpty) {
                matches += phi
                if (matches.size % CHECK == 0) println ("dualIso: matches so far = " + matches.size)
            } // if
        } else if (! phi.isEmpty) {
            for (i <- phi (depth) if (! contains (phi, depth, i))) {
                val phiCopy = phi.map (x => x)                           // make a copy of phi
                phiCopy (depth) = SET [Int] (i)                          // isolate vertex i
                if (matches.size >= limit) return                        // quit if at LIMIT
                saltzDualIso (duals.saltzDualSim (phiCopy), depth + 1)   // solve recursively for the next depth
            } // for
        } // if
    } // saltzDualIso

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether vertex 'j' is contained in any 'phi(i)' for the previous depths.
     *  @param phi    array of mappings from a query vertex u_q to { graph vertices v_g }
     *  @param depth  the current depth of recursion
     *  @param j      the vertex j to check
     */
    private def contains (phi: Array [SET [Int]], depth: Int, j: Int): Boolean =
    {
        for (i <- 0 until depth if phi(i) contains j) return true
        false
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an array to hold matches for each vertex 'u' in the query graph
     *  'q' and initialize it to contain all empty sets.  Then for each bijection,
     *  add each element of the bijection to its corresponding match set.
     *  @param psi  the set of bijections
     */
    private def merge (psi: SET [Array [Int]]): Array [SET [Int]] =
    {
        val matches = Array.ofDim [SET [Int]] (q.size).map (_ => SET [Int] ())
        for (b <- bijections; i <- b.indices) matches(i) += b(i)
        matches
    } // merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pull the bijections out of the complete match set.
     *  @param matches  the complete match set embedding all bijections
     */
    private def simplify (matches: SET [Array [SET [Int]]]): SET [Array [Int]] =
    {
        matches.map (m => m.map (set => set.iterator.next))
    } // simplify

} // MuDualIso class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualIsoTest` object is used to test the `MuDualIso` class.
 *  > run-main scalation.graphalytics.multi.MuDualIsoTest
 */
object MuDualIsoTest extends App
{
    val g = new MuGraph (Array (SET (1,3),                       // ch(0)
                                SET (2),                         // ch(1)
                                SET (),                          // ch(2)
                                SET (),                          // ch(3)
                                SET (2),                         // ch(4)
                                SET (4)),                        // ch(5)
                         Array (10.0, 11.0, 11.0, 11.0, 11.0, 10.0),
                         Map ((0, 1) -> ν(-1.0,-2.0,-3.0),
                              (0, 3) -> ν(-1.0),
                              (1, 2) -> ν(-1.0),
                              (4, 2) -> ν(-1.0),
                              (5, 4) -> ν(-1.0)),                // change from -1 to -2 filter out vertices
                              false, "g")

    val q = new MuGraph (Array (SET (1,3),                       // ch(0)
                                SET (2),                         // ch(1)
                                SET (),                          // ch(2)
                                SET (2)),                        // ch(3)
			 Array (10.0, 11.0, 11.0,11.0),          // vertex labels
                         Map ((0, 1) -> ν(-1.0,-2.0),            // edge labels
                              (0, 3) -> ν(-1.0),
			      (1, 2) -> ν(-1.0),
			      (3, 1) -> ν(-1.0)),
                              false, "g")

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuDualIso (g, q)                           // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                     // time the matcher
    println ("Number of Matches: " + matcher.numMatches)
    for (p <- psi) println (s"psi = ${p.deep}")

    matcher.test ("MuDualIso")

} // MuDualIsoTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualIsoTest2` object is used to test the `MuDualIso` class.
 *  > run-main scalation.graphalytics.multi.MuDualIsoTest2
 */
object MuDualIsoTest2 extends App
{
    import scalation.graphalytics.multi.{ExampleMuGraphD => EX_GRAPH}
    import MatchAnswers._

    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuDualIso (g, q)                           // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                     // time the matcher
    println ("Number of Matches: " + matcher.numMatches)
    for (p <- psi) println (s"psi = ${p.deep}")
    
    matcher.test ("MuDualIso", shift (dualIso))

} // MuDualIsoTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualIsoTest3` object is used to test the `MuDualIso` class.
 *  > run-main scalation.graphalytics.multi.MuDualIsoTest3
 */
object MuDualIsoTest3 extends App
{
    import scalation.graphalytics.multi.{ExampleMuGraphD => EX_GRAPH}

    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkeLabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new MuDualIso (g, q)                           // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                     // time the matcher
    println ("Number of Matches: " + matcher.numMatches)
    for (p <- psi) println (s"psi = ${p.deep}")
   
    matcher.test ("MuDualIso")

} // MuDualIsoTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuDualIsoTest4` object is used to test the `MuDualIso` class.
 *  > run-main scalation.graphalytics.multi.DualIsoTest4
 */
/*object MuDualIsoTest4 extends App
{
    val mgGen = new MuGraphGen [Double]

    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct vertex labels
    val eLabels   =   10         // number of distinct edge labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = mgGen.genRandomGraph (gSize, nLabels, eLabels, gAvDegree, false, "g")
    val q = mgGen.genBFSQuery (qSize, qAvDegree, g, false, "q")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    val matcher = new MuDualIso (g, q)                           // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                     // time the matcher
    println ("Number of Matches: " + matcher.numMatches)
    for (p <- psi) println (s"psi = ${p.deep}")

} // MuDualIsoTest4 */

