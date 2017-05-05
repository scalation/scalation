
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.3
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Pattern Matching Using Mutable Sets
 */

package scalation.graphalytics
package mutable

import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}

import scalation.util.{banner, time, Wildcard}
import scalation.util.Wildcard.hasWildcards

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphMatcher` abstract class serves as a template for implementing
 *  specific algorithms for graph pattern matching.
 *  @param g  the data graph  G(V, E, l) with vertices v in V
 *  @param q  the query graph Q(U, D, k) with vertices u in U
 */
abstract class GraphMatcher [TLabel] (g: Graph [TLabel], q: Graph [TLabel])
{
    protected val qRange     = 0 until q.size     // range for query graph vertices
    protected val gRange     = 0 until g.size     // range for data graph vertices
    protected val CHECK      = 1024               // check progress after this many matches
    protected val LIMIT      = 1E7                // quit after too many matches
    protected val SELF_LOOPS = false              // whether the directed graph has self-loops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find the mappings from the
     *  query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]] = prune (feasibleMates ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an initial array of feasible mappings 'phi' from each query
     *  vertex 'u' to the corresponding set of data graph vertices '{v}' whose
     *  label matches 'u's.
     */
    def feasibleMates (): Array [SET [Int]] =
    {
        val phi = Array.ofDim [SET [Int]] (q.size)
        for (u <- qRange) phi(u) = g.labelMap (q.label(u)).clone
        phi
//      q.label.map (u_label => g.getVerticesWithLabel (u_label).clone)
    } // feasibleMates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an initial array of feasible mappings 'phi' from each query
     *  vertex 'u' to the corresponding set of data graph vertices '{v}' whose
     *  label matches 'u's.
     *  This version handles query graph labels that have wildcards.
     */
    def feasibleMatesW (): Array [SET [Int]] =
    {
        val phi = Array.ofDim [SET [Int]] (q.size)
        for (u <- qRange) {                                                // iterate thru query graph
            if (hasWildcards (q.label(u))) {                                  // iterate thru data graph, FIX - need faster approach
                val qLabelW = new Wildcard (q.label(u).asInstanceOf [String])
                phi(u) = SET [Int] ()
                for (v <- gRange if qLabelW =~ g.label(v).asInstanceOf [String]) phi(u) += v
            } else {                                                       // use index
                phi(u) = g.labelMap (q.label(u)).clone
            } // if
        } // for
        phi
    } // feasibleMatesW

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings 'phi' produced by the 'feasibleMates' method,
     *  prune mappings 'u -> v' where v's children fail to match u's.
     *  @param phi  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (phi: Array [SET [Int]]): Array [SET [Int]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether two sets overlap, i.e., have a non-empty intersection.
     *  @param set1  the first set
     *  @param set2  the second set
     */
    def overlaps (set1: SET [Int], set2: SET [Int]): Boolean =
    {
        for (s <- set1 if set2 contains s) return true
        false
    } // overlaps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show all mappings between query graph vertices 'u_i' and their sets of
     *  data graph vertices {v}.
     *  @param phi  the set-valued mapping function
     */
    def showMappings (phi: Array [SET [Int]])
    {
        println ("query u \t--> graph {v}")
        for (i <- phi.indices) println ("u_" + i + " \t--> " + phi(i))
    } // showMappings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of mappings between query graph vertices 'u_i' and their sets
     *  of data graph vertices {v}, giving the number of distinct vertices and edges.
     *  @param phi  the set-valued mapping function
     */
    def countMappings (phi: Array [SET [Int]]): Pair =
    {
        val distVertices = SET [Int] ()
        val distEdges    = SET [Pair] ()
        for (i <- phi.indices) distVertices ++= phi(i)
        for (i <- phi.indices) {
            for (v <- phi(i); v_c <- g.ch(v) if distVertices contains v_c) distEdges += ((v, v_c))
        } // for
        (distVertices.size, distEdges.size)
    } // countMappings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the Graph Pattern Matcher.
     *  @param mName  the name of graph pattern matcher
     *  @param ans    the correct answer
     */
    def test (name: String, ans: Array [SET [Int]] = null)
    {
        val phi = time { mappings () }                                 // time the matcher
        banner (s"query ${q.name} (${q.size}) via $name on data ${g.name} (${g.size})")
        showMappings (phi)                                             // display results
        println (s"(#distVertices, #distEdges) = ${countMappings (phi)}")
        if (ans != null) for (i <- phi.indices) assert (phi(i) == ans(i))
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find subgraphs of data graph
     *  'g' that isomorphically match query graph 'q'.  These are represented
     *  by a set of single-valued bijections {'psi'} where each 'psi' function
     *  maps each query graph vertex 'u' to a data graph vertices 'v'.
     */
    def bijections (): SET [Array [Int]] = throw new UnsupportedOperationException ()

} // GraphMatcher abstract class

