
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.3
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}
//import scala.collection.mutable.{Set => SET}
//import scala.collection.mutable.{HashSet => SET}

import scalation.util.time

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphMatcher` abstract class serves as a template for implementing
 *  specific algorithms for graph pattern matching.
 *  @param g  the data graph  G(V, E, l) with vertices v in V
 *  @param q  the query graph Q(U, D, k) with vertices u in U
 */
abstract class GraphMatcher (g: Graph, q: Graph)
{
    protected val qRange     = 0 until q.size     // range for query vertices
    protected val CHECK      = 1024               // check progress after this many matches
    protected val LIMIT      = 1E7                // quit after too many matches
    protected val SELF_LOOPS = false              // whether the directed graph has self-loops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find the mappings from the
     *  query graph 'q' to the data graph 'g'.  These are represented by a
     *  multi-valued function 'phi' that maps each query graph vertex 'u' to a
     *  set of data graph vertices '{v}'.
     */
    def mappings (): Array [SET [Int]]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply a graph pattern matching algorithm to find subgraphs of data graph
     *  'g' that isomorphically match query graph 'q'.  These are represented
     *  by a set of single-valued bijections {'psi'} where each 'psi' function
     *  maps each query graph vertex 'u' to a data graph vertices 'v'.
     */
    def bijections (): SET [Array [Int]] =
    {
        throw new UnsupportedOperationException ()
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an initial array of feasible mappings 'phi' from each query
     *  vertex 'u' to the corresponding set of data graph vertices '{v}' whose
     *  label matches 'u's.
     */
    def feasibleMates (): Array [SET [Int]] =
    {
        q.label.map (u_label => g.getVerticesWithLabel (u_label))
    } // feasibleMates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the mappings between a query graph vertex u and a set of data
     *  graph vertices {v}.
     *  @param phi  the set-valued mapping function
     */
    def showMappings (phi: Array [SET [Int]])
    {
        println ("query u \t--> graph {v}")
        for (i <- phi.indices) println ("u_" + i + " \t--> " + phi(i))
    } // showMappings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the graph pattern matcher.
     *  @param  name  the name of graph pattern matcher
     *  @param  ans   the correct answer
     */
    def test (name: String, ans: Array [SET [Int]] = null)
    {
        val phi = time { mappings () }                     // time the matcher
        println (s"$name ---------------------------------------------------")
        showMappings (phi)                                 // display results
        if (ans != null) for (i <- phi.indices) assert (phi(i) == ans(i))
    } // test

} // GraphMatcher abstract class

