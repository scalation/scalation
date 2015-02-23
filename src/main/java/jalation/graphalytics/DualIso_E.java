
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller, Ayushi Jain
 *  @version 1.1
 *  @date    Sat Dec 20 19:13:40 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.graphalytics;

import java.io.File;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static java.lang.System.out;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIso_E` class provides an implementation for Subgraph Isomorphism that
 *  uses Dual Graph Simulation for pruning.  It works on vertex and edge labeled
 *  graphs.
 *  @param g  the data graph G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
public class DualIso_E
{
    /** check progress after this many matches
     */
    private static final int CHECK = 1024;

    /** the vertex and edge labeled data graph G(V, E, l)
     */
    private final Graph_E g;

    /** the vertex and edge labeled query graph Q(U, D, k)
     */
    private final Graph_E q;

    /** the Dual Graph Simulation pattern matcher for vertex and edge labeled graphs
     */
    private final DualSim_E duals;

    /** hold matches between vertices in q and g
     */
    private Set <Set <Integer> []> matches = null;

    /** indication of whether bijection have been created
     */
    private boolean noBijections = true;

    /** quit after too many matches (use setLimit () to adjust)
     */
    private int limit = 1000000;

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a `DualIso_E` matcher for Subgraph Isomorphism.
     *  @param _g  the vertex and edge labeled data graph
     *  @param _q  the vertex and edge labeled query graph
     */
    public DualIso_E (Graph_E _g, Graph_E _q)
    {
        g     = _g;
        q     = _q;
        duals = new DualSim_E (g, q);
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the limit on the number of matches to find before quiting.
     *  @param _limit  the limit on the number of matches to find
     */
    public void setLimit (int _limit)
    {
        limit = _limit;
    } // setLimit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Subgraph Isomorphism algorithm to find subgraphs of data
     *  graph 'g' that isomorphically match query graph 'q'.  These are
     *  represented by a set of single-valued bijective functions {'psi'} where
     *  each 'psi' function maps each query graph vertex 'u' to a data graph
     *  vertices 'v'.
     */
    public Set <Integer []> bijections ()
    {
        matches = new HashSet <> ();                       // initialize matches to empty
        Set <Integer> [] phi = duals.feasibleMates ();     // initial mappings from label match
        saltzDualIso (duals.saltzDualSim (phi), 0);        // recursively find all bijections
        Set <Integer []> psi = simplify (matches);         // pull bijections out matches
        noBijections = false;                              // results now available
        return psi;                                        // return the set of bijections
    } // bijections

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply the Dual Subgraph Isomorphism pattern matching algorithm to find
     *  the mappings from the query graph 'q' to the data graph 'g'.  These are
     *  represented by a multi-valued function 'phi' that maps each query graph
     *  vertex 'u' to a set of data graph vertices '{v}'.
     */
    public Set <Integer> [] mappings ()
    {
        Set <Integer []> psi = null;                   // mappings from Dual Simulation
        if (noBijections) psi = bijections ();         // if no results, create them
        return merge (psi);                            // merge bijections to create mappings
    } // mappings

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the count of the number of matches.
     */
    public int numMatches ()
    {
        return matches.size ();
    } // numMatches

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Refine the mappings 'phi' using the Dual Subgraph Isomorphism algorithm.
     *  Enumerate bijections by using an Ullmann-like recursion that uses Dual
     *  Graph Simulation for pruning.
     *  @param phi    array of mappings from a query vertex u_q to { graph vertices v_g }
     *  @param depth  the depth of recursion
     */
    private void saltzDualIso (Set <Integer> [] phi, int depth)
    {
        if (depth == q.size ()) {
            if (phi.length > 0) {
                matches.add (phi);
                if (matches.size () % CHECK == 0) {
                    out.println ("dualIso: matches so far = " + matches.size ());
                } // if
            } // if

        } else if (phi.length > 0) {
            for (int i: phi [depth]) {
                if (! contains (phi, depth, i)) {
                    Set <Integer> [] phiCopy = copy (phi);                     // make a copy of phi
                    phiCopy [depth] = new HashSet <> (Arrays.asList (i));      // isolate vertex i
                    if (matches.size () >= limit) return;                      // quit if at LIMIT
                    saltzDualIso (duals.saltzDualSim (phiCopy), depth + 1);    // solve recursively for the next depth
                } // if
            } // for
        } // if
    } // saltzDualIso

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Copy the array of sets 'phi'.
     *  @param phi  the array of sets to be copied
     */
    @SuppressWarnings("unchecked")
    private Set <Integer> [] copy (Set <Integer> [] phi)
    {
        Set <Integer> [] c = (Set <Integer> []) new Set <?> [phi.length];
        for (int i = 0; i < phi.length; i++) c[i] = new HashSet <> (phi [i]);
        return c;
    } // copy

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Is vertex j contained in any phi(i) for the previous depths?
     *  @param phi    array of mappings from a query vertex u_q to { graph vertices v_g }
     *  @param depth  the current depth of recursion
     *  @param j      the vertex j to check
     */
    private boolean contains (Set <Integer> [] phi, int depth, int j)
    {
        for (int i = 0; i < depth; i++) {
            if (phi [i].contains (j)) return true;
        } // for
        return false;
    } // contains

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an array to hold matches for each vertex 'u' in the query graph
     *  'q' and initialize it to contain all empty sets.  Then for each bijection,
     *  add each element of the bijection to its corresponding match set.
     *  @param psi  the set of bijections
     */
    @SuppressWarnings("unchecked")
    private Set <Integer> [] merge (Set <Integer []> psi)
    {
        Set <Integer> [] match = (Set <Integer> []) new Set <?> [q.size ()];
        for (Integer [] b: bijections ()) {
            for (int i = 0; i < b.length; i++) match [i].add (b[i]);
        } // for
        return match;
    } // merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pull the bijections out of the complete match set.
     *  @param matches  the complete match set embedding all bijections
     */
    private Set <Integer []> simplify (Set <Set <Integer> []> matches)
    {
        Set <Integer []> rset = new HashSet <> ();
        for (Set <Integer> [] m: matches) {
            Integer [] result = new Integer [q.size ()];
            for (int j = 0; j < m.length; j++) result [j] = m[j].iterator ().next ();
            rset.add (result);
        } // for
//      out.println ("rset = " + Arrays.deepToString (rset.toArray ()));
        return rset;
    } // simplify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the `DualIso_E` class.
     *  @param args  unused command-line arguments
     */
    @SuppressWarnings("unchecked")
    public static void main (String [] args)
    {
    	Graph_E g = Graph_E.apply (new File("/home/airavind/newdatasets/dataM/data6M.csv"));            // FIX
        Graph_E q = Graph_E.apply (new File("/home/airavind/newdatasets/queryM/50V/query50_6M.csv"));

        DualIso_E dualIsoMatcher = new DualIso_E (g, q);             // Dual Subgraph Isomorphism Pattern Matcher
        
        long start_time = System.currentTimeMillis();      		
        	
        Set <Integer []> psi = dualIsoMatcher.bijections (); 
    
        double difference = System.currentTimeMillis () - start_time;
        out.println ("Time Taken: " + difference + " ms");
    	
    } // main

} // DualIso_E class

