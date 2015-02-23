
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Sameer Gaherwar and John Miller
 *  @version 1.1
 *  @date    Sun May 18 11:23:45 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://en.wikipedia.org/wiki/Graph_isomorphism_problem
 */

package scalation.graphalytics

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIso` class determines whether two labelled directed graphs are
 *  Graph Isomorphic.  
 *
 *  @param g  the data graph G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphIso (g: Graph, q: Graph)
{
    /** Algorithm to solve the related Subgraph Isomorphism Problem
     */
    private val dualIso = new DualIso (g, q)      // create subgraph isomorphism matcher

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether graphs 'g' anf 'q 'are graph isomorphic.  This is done by
     *  (1) checking that 'g' and 'q' have the same number of vertices and
     *  (2) 'q' has at least one subgraph isomorphic match in 'g'.
     */
    def graphIsomorphic (): Boolean =
    {
        if (g.size == q.size) {                // g and q must have the same # vertices
            dualIso.setLimit (1)               // need only one match
            dualIso.bijections ()              // run the matcher
            if (dualIso.numMatches () > 0) return true 
        } // if
        false 
    } // graphIsomorphic

} // GraphIso class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIsoTest` object is used to test the `GraphIso` class.
 */
object GraphIsoTest extends App
{
        val adj_g = Array (Set (1, 2),
                           Set (2, 3),
                           Set (3),
                           Set (1))
        val label_g = Array (2, 1, 2, 1)
        val g = new Graph (adj_g, label_g, false)
        g.print
        
//      val adj_q = Array (Set (1, 2),
//                         Set (2, 3),
//                         Set (3),
//                         Set (1))
//      val label_q = Array (2, 1, 2, 1)
//      val q = new Graph (adj_q, label_q, false)
        
        val adj_q = Array (Set (1, 2),
                           Set (2),
                           Set [Int] ())
        val label_q = Array (2, 1, 2)
        val q = new Graph (adj_q, label_q, false)
        q.print

        val matcher = new GraphIso (g, q)
        println ("Are g and q graph isomorphic: " + matcher.graphIsomorphic ())

} // GraphIsoTest object

