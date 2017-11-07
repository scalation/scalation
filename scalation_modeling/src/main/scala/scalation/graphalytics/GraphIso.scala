
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Sameer Gaherwar and John Miller
 *  @version 1.4
 *  @date    Sun May 18 11:23:45 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://en.wikipedia.org/wiki/Graph_isomorphism_problem
 */

package scalation.graphalytics

import scala.collection.immutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIso` class determines whether two labelled directed graphs are
 *  Graph Isomorphic.  
 *  @param g  the data graph G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphIso (g: Graph, q: Graph)
{
    /** Algorithm to solve the related Subgraph Isomorphism Problem
     */
    private val dualIso = new DualIso (g, q)      // create subgraph isomorphism matcher

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether graphs 'g' and 'q' are graph isomorphic.  This is done by
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
        val g = new Graph (Array (SET (1, 2),
                                  SET (2, 3),
                                  SET (3),
                                  SET (1)),
                           Array (2, 1, 2, 1),
                           false, "g")
        g.printG ()
        
//      val adj_q = Array (SET (1, 2),
//                         SET (2, 3),
//                         SET (3),
//                         SET (1))
//      val label_q = Array (2, 1, 2, 1)
//      val q = new Graph (adj_q, label_q, false)
        
        val q = new Graph (Array (SET (1, 2),
                                  SET (2),
                                  SET [Int] ()),
                           Array (2, 1, 2),
                           false, "q")
        q.printG ()

        val matcher = new GraphIso (g, q)
        println ("Are g and q graph isomorphic: " + matcher.graphIsomorphic ())

} // GraphIsoTest object

