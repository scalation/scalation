
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Sameer Gaherwar and John Miller
 *  @version 1.1
 *  @date    Sun May 18 11:23:45 EDT 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://en.wikipedia.org/wiki/Graph_isomorphism_problem
 */

package jalation.graphalytics;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static java.lang.System.out;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIso` class determines whether two labelled directed graphs are
 *  Graph Isomorphic.  
 */
public class GraphIso
{
    /** Algorithm to solve the related Subgraph Isomorphism Problem
     */
    private static DualIso dualIso = null;

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether graphs 'g' anf 'q 'are graph isomorphic.  This is done by
     *  (1) checking that 'g' and 'q' have the same number of vertices and
     *  (2) 'q' has at least one subgraph isomorphic match in 'g'.
     * 
     *  @param g  the data graph G(V, E, l)
     *  @param q  the query graph Q(U, D, k)
     */
    public static boolean graphIsomorphic (Graph g, Graph q)
    {
        if (g.size () == q.size ()) {          // g and q must have the same # vertices
            dualIso = new DualIso (g, q);      // create subgraph isomorphism matcher
            dualIso.setLimit (1);              // need only one match
            dualIso.bijections ();             // run the matcher
            if (dualIso.numMatches () > 0) return true;
        } // if
        return false;
    } // graphIsomorphic

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The main method is used to test the `GraphIso` class.
     *
     *  @param args  the unused command-line arguments
     */
    @SuppressWarnings("unchecked")
    public static void main (String args [])
    {
        Set <Integer> [] adj_g = (Set <Integer> []) new Set <?> [4];
        adj_g [0] = new HashSet <> (Arrays.asList (1, 2));
        adj_g [1] = new HashSet <> (Arrays.asList (2, 3));
        adj_g [2] = new HashSet <> (Arrays.asList (3));
        adj_g [3] = new HashSet <> (Arrays.asList (1));
        Integer [] label_g = {2, 1, 2, 1};
        Graph g = new Graph (adj_g, label_g, false);
        g.print ();
        
//      Set <Integer> [] adj_q = (Set <Integer> []) new Set <?> [4];
//      adj_q [0] = new HashSet <> (Arrays.asList (1, 2));
//      adj_q [1] = new HashSet <> (Arrays.asList (2, 3));
//      adj_q [2] = new HashSet <> (Arrays.asList (3));
//      adj_q [3] = new HashSet <> (Arrays.asList (1));
//      Integer [] label_q = {2, 1, 2, 1};
//      Graph q = new Graph (adj_q, label_q, false);
        
        Set <Integer> [] adj_q = (Set <Integer> []) new Set <?> [3];
        adj_q [0] = new HashSet <> (Arrays.asList (1, 2));
        adj_q [1] = new HashSet <> (Arrays.asList (2));
        adj_q [2] = new HashSet <> ();
        Integer [] label_q = {2, 1, 2};
        Graph q = new Graph (adj_q, label_q, false);
        q.print (); 

        out.println ("Are g and q graph isomorphic: " + GraphIso.graphIsomorphic (g, q));
    } // main

} // GraphIso class

