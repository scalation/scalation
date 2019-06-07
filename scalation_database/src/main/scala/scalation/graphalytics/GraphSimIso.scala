
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.6
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimIso` object provides an implementation for Subgraph Isomorphism
 *  that uses an adjacency set version of Ullmann's Algorithm.
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSimIso (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    private val sims         = new GraphSim2 (g, q)    // object for Dual Simulation algorithm
    private var matches      = SET [Array [SET [Int]]] ()   // initialize matches to empty
    private var noBijections = true                    // no results yet

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Ullmann's Subgraph Isomorphism pattern matching algorithm to
     *  find the mappings from the query graph 'q' to the data graph 'g'.
     *  These are represented by a multi-valued function 'phi' that maps each
     *  query graph vertex 'u' to a set of data graph vertices '{v}'.
     *
    def mappings (): Array [SET [Int]] = 
    {
        var psi: SET [Array [Int]] = null              // mappings from Dual Simulation
        if (noBijections) psi = bijections ()          // if no results, create them
        merge (psi)                                    // merge bijections to create mappings
    } // mappings
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Ullmann's Subgraph Isomorphism algorithm to find subgraphs of data
     *  graph 'g' that isomorphically match query graph 'q'.  These are represented
     *  by a set of single-valued bijections {'psi'} where each 'psi' function
     *  maps each query graph vertex 'u' to a data graph vertices 'v'.
     */
    override def bijections (): SET [Array [Int]] =
    {
        val phi = sims.feasibleMates ()                          // initial mappings from label match
        refine (sims.prune (phi), makeOrder (), 0)               // recursively find all bijections
        val psi = simplify (matches)                             // pull bijections out matches
        noBijections = false                                     // results now available
        psi                                                      // return the set of bijections
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Refine the mappings 'phi' using the Ullmann Subgraph Isomorphism algorithm.
     *  @param phi    array of mappings from a query vertex u_q to { graph vertices v_g }
     *  @param depth  the depth of recursion
     */
    private def refine (phi: Array [SET [Int]], ordering: Array [Int], depth: Int)
    {
        if (depth == q.size) {
            if (! phi.isEmpty) {
                matches += phi
                if (matches.size % CHECK == 0) println ("GraphSimIso: matches so far = " + matches.size)
            } // if
        } else if (! phi.isEmpty) {
            for (i <- phi (ordering (depth)) if (! contains (phi, i, ordering, depth))) {
                val phiCopy = phi.map (x => x)                           // make a copy of phi
                phiCopy (ordering (depth)) = SET [Int] (i)               // isolate vertex i
                if (matches.size >= LIMIT) return                        // quit if at LIMIT
                // solve recursively for the next depth 
                refine (sims.prune (phiCopy), ordering, depth + 1)
            } // for
        } // if
    } // refine

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Establish a vertex order from largest to smallest adjacency set ('ch') size.
     */
    def makeOrder (): Array [Int] = q.ch.zipWithIndex.sortBy (-1 * _._1.size).map (_._2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether vertex 'j' is contained in any 'phi(i)' for the previous depths.
     *  @param phi       array of mappings from a query vertex u_q to { graph vertices v_g }
     *  @param j         the vertex j to check
     *  @param ordering  the 'ch' size based ordering of the vertices
     *  @param depth     the current depth of recursion
     */
    private def contains (phi: Array [SET [Int]], j: Int, ordering: Array [Int], depth: Int): Boolean =
    {
        for (i <- 0 until depth) if (phi (ordering(i)) contains j) return true
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the count of the number of matches.
     */
    def getMatches (): Int = matches.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'prune' is not needed, pruning is delegated to incorporated graph
     *  simulation algorithm.
     *  @param phi  array of mappings from a query vertex u_q to { graph vertices v_g }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] = throw new UnsupportedOperationException ()

} // GraphSimIso class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSimIsoTest` object is used to test the `GraphSimIso` class.
 */
object GraphSimIsoTest extends App
{
    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = GraphGen.genRandomGraph (gSize, nLabels, gAvDegree)
    val q = GraphGen.genBFSQuery (qSize, qAvDegree, g)

    val matcher = new GraphSimIso (g, q)                    // GraphSimIso Subgraph Isomorphism Pattern Matcher
    for (i <- 0 until 3) {
        val psi = time { matcher.bijections () }            // time the matcher
	println ("Number of Matches: " + matcher.getMatches)
	for (p <- psi) println (p.deep)
    } // for

} // GraphSimIsoTest

