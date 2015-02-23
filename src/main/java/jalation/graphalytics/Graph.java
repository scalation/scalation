
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.1
 *  @date    Sun May 18 11:23:45 EDT 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.graphalytics;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static java.lang.System.out;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` class stores vertex-labeled directed graphs using an adjacency
 *  set ('adj') representation, e.g., adj = { {1, 2}, {0}, {1} } means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'par' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 */
public class Graph implements Cloneable
{
    /** the array of vertex (child) adjacency sets (outgoing edges)
     */
    final Set <Integer> [] adj;

    /** the array of verter labels
     */
    final Integer [] label;

    /** whether to store inverse adjacency sets (parents)
     */
    private final boolean inverse;

    /** the map from label to the set of vertices with the label
     */
    private Map <Integer, Set <Integer>> labelMap;

    /** the optional array of vertex inverse (parent) adjacency sets (incoming edges)
     */
    private final Set <Integer> [] par;                // by default, don't use 'par'

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a `Graph`.
     *  @param adj      the array of vertex (child) adjacency sets (outgoing edges)
     *  @param label    the array of verter labels
     *  @param inverse  whether to store inverse adjacency sets (parents)
     */
    @SuppressWarnings("unchecked")
    public Graph (Set <Integer> [] adj,
                  Integer []       label,
                  boolean          inverse)
    {
        this.adj     = adj;
        this.label   = label;
        this.inverse = inverse;
        buildLabelMap (label);
        par          = (Set <Integer> []) new Set <?> [adj.length];
        if (inverse) addPar ();
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a label-free `Graph` with no parent references.
     *  @param adj      the array of vertex (child) adjacency sets (outgoing edges)
     *  @param label    the array of verter labels
     *  @param inverse  whether to store inverse adjacency sets (parents)
     */
    public Graph (Set <Integer> [] adj)
    {
        this.adj = adj;
        label    = null;
        inverse  = false;
        par      = null;
    } // constructor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of this graph.
     */
    @Override
    public Graph clone ()
    {
        return new Graph (adj.clone (), label.clone (), inverse);
    } // clone
   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the inverse adjacency sets for rapid accesses to parent vertices.
     */
    public void addPar ()
    {
        for (int j = 0; j < par.length; j++) par [j] = new HashSet <Integer> ();
        for (int i = 0; i < adj.length; i++) {
            for (int j = 0; j < adj [i].size (); j++) par [j].add (i);
        } // for
    } // addPar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of vertices in the graph.
     */
    public int size ()
    {
        return adj.length;
    } // size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an array of labels, return an index from labels to the sets of
     *  vertices containing those labels
     *  @param label  the array of vertex labels of type TLabel
     */
    public void buildLabelMap (Integer [] label)
    {
        labelMap = new HashMap <> ();
        for (int i = 0; i < adj.length; i++) {
             Set <Integer> vertices = labelMap.get (label [i]);      // get known vertices with given label
             if (vertices == null) vertices = new HashSet <> ();     // if none, make an empty set
             vertices.add (i);                                       // add the new vertex i
             labelMap.put (label [i], vertices);                     // add back into labelMap
        } // for
    } // buildLabelMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the set of vertices in the graph with label l.
     */
    public Set <Integer> getVerticesWithLabel (Integer l)
    {
        return labelMap.get (l);
    } // getVerticesWithLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate basic information about this graph.  Due to its potential size,
     *  use print to show graph details.
     */
    @Override
    public String toString ()
    {
        return "Graph with " + size () + " vertices";
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the adjacency set, labels, and labelMap of the graph.
     */
    public void print ()
    {
        out.println ("adj: ");
        for (int i = 0; i < adj.length; i++) out.println (i + " -> " + adj [i]);
        if (label != null) {
            out.println ("labels: ");
            for (int i = 0; i < label.length; i++) out.println (i + " -> " + label [i]);
            out.println ("labelMap: ");
            for (Map.Entry <Integer, Set <Integer>> entry: labelMap.entrySet ()) {
                    out.println ("Key: " + entry.getKey() + " Value: "
                                + Arrays.deepToString (entry.getValue ().toArray ()));
        } // if
        } // for
    } // print

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the `Graph` class.
     *  @param args  unused command-line arguments
     */
    @SuppressWarnings("unchecked")
    public static void main (String [] args)
    {
        
        Set <Integer> [] adj_ = (Set <Integer> []) new Set <?> [3];
        adj_[0] = new HashSet <> (Arrays.asList (1, 2));
        adj_[1] = new HashSet <> (Arrays.asList (2));
        adj_[2] = new HashSet <> ();
        Integer [] label_ = { 2, 1, 2 };
        Graph g = new Graph (adj_, label_, false);
        g.print ();
    } // main

} // Graph class

