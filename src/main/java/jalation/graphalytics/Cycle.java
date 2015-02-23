
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Tue Aug 19 15:49:27 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.graphalytics;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static java.lang.System.out;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cycle` class provides a means for building a precedence/directed graph
 *  and checking it for cycles.  For cycle detection, vertices are marked with
 *  traffic-light colors:
 *    - Green means go/unexplored,
 *    - Yellow means caution/been there before,
 *    - Red mean stop/already fully explored.
 */
public class Cycle
{
    /** Graph in which to check for cycles
     */
    private final Graph g;

    /** vertices are marked with traffic-light colors ('G'reen, 'Y'ellow, 'R'ed)
     */
    private final char [] color;

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a graph, contruct a cycle detector.
     *  @param gg  the graph to be checked for cycles
     */
    public Cycle (Graph gg)
    {
        g     = gg;
        color = new char [g.size ()];
       for (int i = 0; i < color.length; i++) color[i] = 'G';    // initialize colors to Green
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the graph contains a cycle.
     */
    public boolean hasCycle ()
    {
       for (int i = 0; i < color.length; i++) {
           if (color[i] == 'G' && loopback (i)) return true;
       } // for
       return false;
    } // hasCycle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Search the decendents of vertex 'i' to see if there is a loopback.
     *  @param i  the vertex where the search starts
     */
    private boolean loopback (int i)
    {
        if (color[i] == 'Y') return true;
        color[i] = 'Y';
        for (int j : g.adj[i]) {
            if (color[j] != 'R' && loopback (j)) return true;
        } // if
        color[i] = 'R';
        return false;
    } // loopback

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'main' method tests the `Cycle` class using a label-free precedence
     *  graph.  Graphs are created by passing in an array of adjacency sets (one for
     *  each vertex).
     */
    @SuppressWarnings("unchecked")
    public static void main (String [] args)
    {
        /** Test precedence graph 1 (does not have a cycle)
         */
        Set <Integer> [] adj1 = (Set <Integer> []) new Set <?> [3];
        adj1[0] = new HashSet <> (Arrays.asList (1, 2));        // edges from 0:  0 -> 1, 0 -> 2
        adj1[1] = new HashSet <> (Arrays.asList (2));           // edges from 1:  1 -> 2
        adj1[2] = new HashSet <> ();                            // edges from 2:  no such edges
        Graph pg1 = new Graph (adj1);

        out.println ("Precedence Graph pg1: ----------------------------------------------");
        pg1.print ();
        out.println ("pg1 has cycle? = " + (new Cycle (pg1).hasCycle ()));
    
        /** Test precedence graph 2 (has a cycle)
         */
        Set <Integer> [] adj2 = (Set <Integer> []) new Set <?> [3];
        adj2[0] = new HashSet <> (Arrays.asList (1, 2));        // edges from 0:  0 -> 1, 0 -> 2
        adj2[1] = new HashSet <> (Arrays.asList (2));           // edges from 1:  1 -> 2
        adj2[2] = new HashSet <> (Arrays.asList (0));           // edges from 1:  2 -> 0
        Graph pg2 = new Graph (adj2);

        out.println ("Precedence Graph pg2: ----------------------------------------------");
        pg2.print ();
        out.println ("pg2 has cycle? = " + (new Cycle (pg2).hasCycle ()));
    } // main
    
} // Cycle class

