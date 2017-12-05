
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller, Ayushi Jain
 *  @version 1.4
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.reflect.ClassTag

import scalation.util.{banner, time}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIso` class provides an implementation for Subgraph Isomorphism
 *  that uses Dual Graph Simulation for pruning.
 *  @param g      the data graph  G(V, E, l)
 *  @param q      the query graph Q(U, D, k)
 *  @param duals  the base dual simulation pattern matcher
 */
class DualIso [TLabel: ClassTag] (g: Graph [TLabel], q: Graph [TLabel], duals: GraphMatcher [TLabel])
      extends GraphMatcher (g, q)
{
    private var t0           = 0.0                         // start time for timer
    private var matches      = SET [Array [SET [Int]]] ()  // initialize matches to empty
    private var noBijections = true                        // no results yet
    private var limit        = 1000000                     // limit on number of matches

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set an upper bound on the number matches to allow before quitting.
     *  @param _limit  the number of matches before quitting
     */
    def setLimit (_limit: Int) { limit = _limit }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Subgraph Isomorphism algorithm to find subgraphs of data
     *  graph 'g' that isomorphically match query graph 'q'.  These are represented
     *  by a set of single-valued bijections {'psi'} where each 'psi' unction
     *  maps each query graph vertex 'u' to a data graph vertices 'v'.
     */
    override def bijections (): SET [Array [Int]] =
    {
        matches = SET [Array [SET [Int]]] ()               // initialize matches to empty
        val phi = duals.feasibleMates ()                   // initial mappings from label match
        refine (duals.prune (phi), 0)                      // recursively find all bijections
        val psi = simplify (matches)                       // pull bijections out of matches
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
    private def refine (phi: Array [SET [Int]], depth: Int)
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
                refine (duals.prune (phiCopy), depth + 1)                // solve recursively for the next depth
            } // for
        } // if
    } // refine

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
        // FIX - may fail on .next method - provide a more robust implementation
        matches.map (m => m.map (set => set.iterator.next))
    } // simplify

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'prune' is not needed, pruning is delegated to incorporated graph
     *  simulation algorithm.
     *  @param phi  array of mappings from a query vertex u_q to { graph vertices v_g }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] = throw new UnsupportedOperationException ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of mappings between query graph vertices 'u_i' and their sets
     *  of data graph vertices {v}, giving the number of distinct vertices and edges.
     *  @param psi  the set of bijection
     */
    def countMappings (psi: SET [Array [Int]]): Pair =
    {
        val distVertices = SET [Int] ()
        val distEdges    = SET [Pair] ()
        for (pi <- psi) distVertices ++= pi
        for (pi <- psi) {
            for (v <- pi; v_c <- g.ch(v) if distVertices contains v_c) distEdges += ((v, v_c))
        } // for
        (distVertices.size, distEdges.size)
    } // countMappings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the Graph Iso Pattern Matcher.
     *  @param mName  the name of graph pattern matcher
     *  @param ans    the correct answer
     */
    def test (name: String, ans: SET [Array [Int]]): SET [Array [Int]] =
    {
        val psi = time { bijections () }                            // time the matcher
        banner (s"query ${q.name} (${q.size}) via $name on data ${g.name} (${g.size})")
//      for (pi <- psi) println (s"psi = ${pi.deep}")
        println (s"number of matches = $numMatches")
        println (s"(#distVertices, #distEdges) = ${countMappings (psi)}")

        if (ans != null) {
            // FIX - compare result with answer
        } // if 
        psi
    } // test

} // DualIso class

import scalation.graph_db.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoTest` object is used to test the `DualIso` class.
 *  > runMain scalation.graph_db.pattern_matching.DualIsoTest
 */
object DualIsoTest extends App
{
    val g = EX_GRAPH.g1
    val q = EX_GRAPH.q1

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    val matcher = new DualIso (g, q, new DualSim2 (g, q))             // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                          // time the matcher
    println (s"number of matches = ${matcher.numMatches}")
    println (s"(vertices, edges) = ${matcher.countMappings (psi)}")
    for (p <- psi) println (s"psi = ${p.deep}")

} // DualIsoTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoTest2` object is used to test the `DualIso` class.
 *  > runMain scalation.graph_db.pattern_matching.DualIsoTest2
 */
object DualIsoTest2 extends App
{
    val g = EX_GRAPH.g2
    val q = EX_GRAPH.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    val matcher = new DualIso (g, q, new DualSim2 (g, q))             // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                          // time the matcher
    println (s"number of matches = ${matcher.numMatches}")
    println (s"(vertices, edges) = ${matcher.countMappings (psi)}")
    for (p <- psi) println (s"psi = ${p.deep}")

} // DualIsoTest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoTest3` object is used to test the `DualIso` class.
 *  > runMain scalation.graph_db.pattern_matching.DualIsoTest3
 */
object DualIsoTest3 extends App
{
    val gGen = new GraphGen [Double] (0.0)

    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = gGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")
    val q = gGen.genBFSQuery (qSize, qAvDegree, g, false, "q")

    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    val matcher = new DualIso (g, q, new DualSim2 (g, q))             // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                          // time the matcher
    println (s"number of matches = ${matcher.numMatches}")
    println (s"(vertices, edges) = ${matcher.countMappings (psi)}")
    for (p <- psi) println (s"psi = ${p.deep}")

} // DualIsoTest3 object

