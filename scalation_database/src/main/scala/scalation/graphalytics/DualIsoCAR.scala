
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Luis A. Ibarra, Matthew Saltz, John Miller
 *  @version 1.6
 *  @date    Mon May 1 11:28:31 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  Dual Cardinality Subgraph Isomorphism Using Immutable Sets
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoCAR` classes used `DualSim2CAR` for pruning, otherwise it works
 *  like `DualIso`, which uses ``DualSim2` for pruning.
 *  @param g  the data graph  G
 *  @param q  the query graph Q
 */
class DualIsoCAR (g: Graph, q: Graph)
      extends GraphMatcher (g, q)
{
    private val DEBUG        = true                         // debug flag
    private var limit        = 1000000                      // limit on number of matches before quitting
    private val duals        = new DualSim2CAR (g, q)       // object for Dual Simulation algorithm
    private var matches      = SET [Array [SET [Int]]] ()   // initialize matches to empty
    private var noBijections = true                         // no results yet

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
        matches = SET [Array [SET [Int]]] ()            // initialize matches to empty
        val phi = duals.feasibleMates ()                // initial mappings from label match
        refine (duals.prune (phi), 0)                   // recursively find all bijections
        val psi = simplify (matches)                    // pull bijections out matches
        noBijections = false                            // results now available
        psi                                             // return the set of bijections
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find bijections using the more restricted 'feasibleMatesWithSets' method.
     */
    def bijectionsWithSets (): SET [Array [Int]] =
    {
        matches = SET [Array [SET [Int]]] ()            // initialize matches to empty
        val phi = duals.feasibleMatesWithSets ()        // initial mappings from label match
        refine (duals.prune (phi), 0)                   // recursively find all bijections
        val psi = simplify (matches)                    // pull bijections out matches
        noBijections = false                            // results now available
        psi                                             // return the set of bijections
    } // bijectionsWithSets

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Subgraph Isomorphism pattern matching algorithm to find
     *  the mappings from the query graph 'q' to the data graph 'g'.  These are
     *  represented by a  multi-valued function 'phi' that maps each query graph
     *  vertex 'u' to a set of data graph vertices '{v}'.
     */
    override def mappings (): Array [SET [Int]] =
    {
        var psi: SET [Array [Int]] = null               // mappings from Dual Simulation
        if (noBijections) psi = bijections ()           // if no results, create them
        merge (psi)                                     // merge bijections to create mappings
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
        if (DEBUG)  println (s"depth = $depth phi(0) == ${phi(0)} phi(1) == ${phi(1)} phi(2) == ${phi(2)}")
        if (depth == q.size) {
            if (!phi.isEmpty) {
                matches += phi
                if (matches.size % CHECK == 0) println ("dualIsoCAR: matches so far = " + matches.size)
            } // if

        } else if (! phi.isEmpty) {
            for (i <- phi(depth)) {
                if (! contains (phi, depth, i)) {
                    val phiCopy = phi.map {x => x}               // make a copy of phi
                    phiCopy(depth) = SET [Int](i)                // isolate vertex i
                    if (matches.size >= limit) return            // quit if at LIMIT

//                  println (s"before depth = $depth phi(0) == ${phi(0)} phi(1) == ${phi(1)} phi(2) == ${phi(2)}")
//                  phi = phiCopy.map (x => x)
                    val fi = duals.prune (phiCopy)
//                  showMappings (fi)
                    refine (fi, depth + 1) // solve recursively for the next depth
//                  println (s"after depth = $depth phi(0) == ${phi(0)} phi(1) == ${phi(1)} phi(2) == ${phi(2)}")
                } // if
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
    /** The method from dual cardinality simulation is used for pruning.
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]] = throw new UnsupportedOperationException ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an array to hold matches for each vertex 'u' in the query graph
      * 'q' and initialize it to contain all empty sets.  Then for each bijection,
      * add each element of the bijection to its corresponding match set.
      * @param psi  the set of bijections
      */
    private def merge (psi: SET [Array [Int]]): Array [SET [Int]] =
    {
        val matches = Array.fill [SET [Int]] (q.size)(SET [Int]())
        for (b <- bijections; i <- b.indices) matches(i) += b(i)
        matches
    } // merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pull the bijections out of the complete match set.
     *  @param matches  the complete match set embedding all bijections
     */
    private def simplify (matches: SET [Array [SET [Int]]]): SET [Array [Int]] =
    {
//      matches.map (m => m.map (set => set.iterator.next))
        matches.map (m => m.map (set => if (set.iterator.hasNext) set.iterator.next else 0))
    } // simplify

} // DualIsoCAR class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoCARTest` object is used to test the `DualIsoCAR` class.
 *  runMain scalation.graphalytics.DualIsoCARTest
 */
object DualIsoCARTest extends App
{
    val q = new Graph (Array (SET (1, 2),                    // ch(0)
                              SET (),                        // ch(1)
                              SET (1)),                      // ch(2)
                       stringArray ("xyz", "abc", "def"),
                       true, "q1")

    val g1 = new Graph (Array (SET (),                        // ch(0)
                               SET (0, 2, 3, 4),              // ch(1)
                               SET (0),                       // ch(2)
                               SET (4),                       // ch(3)
                               SET ()),                       // ch(4)
                       stringArray ("abc", "xyz" , "def","abc", "zzz"),          // vertex labels
                       true, "g1")                            // inverse, name

    val matcher = new DualIsoCAR (g1, q)                      // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections () }                  // time the matcher
    println ("Number of Matches: " + matcher.numMatches)
    for (p <- psi) println (s"psi = ${p.deep}")

} // DualIsoCARTest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoCARTest2` object is used to test the `DualIsoCAR` class.
 *  runMain scalation.graphalytics.DualIsoCARTest2
 */
object DualIsoCARTest2 extends App
{
    val g = Graph.g2
    val q = Graph.q2

    println (s"g.checkEdges = ${g.checkEdges}")
    g.printG ()
    println (s"q.checkEdges = ${q.checkEdges}")
    q.printG ()

    val matcher = new DualIsoCAR (g, q)                       // Dual Subgraph Isomorphism Pattern Matcher
    val psi = time { matcher.bijections() }                   // time the matcher
    println ("Number of Matches: " + matcher.numMatches)
    for (p <- psi) println (s"psi = ${p.deep}")

} // DualIsoCARTest2 object

