
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller, Sumana Venkatesh
 *  @version 1.1
 *  @date    Sat Dec 20 19:13:40 EST 2014
 *  @see     LICENSE (MIT style license file).
 */

package jalation.graphalytics;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import static java.lang.System.out;

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Tuple_2` class simulates Scala's Tuple2 built in class.
 */
class Tuple_2
{
    int _1;
    int _2;
    Tuple_2 (int a, int b) { _1 = a; _2 = b; }

} // Tuple_2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph_E` class stores vertex-labeled directed graphs using an adjacency
 *  set ('adj') representation, e.g., adj = { {1, 2}, {0}, {1} } means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'par' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 */
public class Graph_E implements Cloneable
{
    /** the array of vertex (child) adjacency sets (outgoing edges)
     */
    final Set <Integer> [] adj;

    /** the array of verter labels
     */
    final String [] label;
    
    /** the map from an edge (pair of vertex ids) to its edge label
     */
    final Map <Tuple_2, String> elabel;

    /** whether to store inverse adjacency sets (parents)
     */
    private final boolean inverse;

    /** the map from label to the set of vertices with the label
     */
    private Map <String, Set <Integer>> labelMap;

    /** the optional array of vertex inverse (parent) adjacency sets (incoming edges)
     */
    private final Set <Integer> [] par;                // by default, don't use 'par'

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a `Graph_E`.
     *  @param adj      the array of vertex (child) adjacency sets (outgoing edges)
     *  @param label    the array of verter labels
     *  @param elabel   the map from pair of vertex ids to edge label
     *  @param inverse  whether to store inverse adjacency sets (parents)
     */
    @SuppressWarnings("unchecked")
    public Graph_E (Set <Integer> []         adj,
                    String []                label,
                    HashMap <Tuple_2,String> elabel,
                    boolean                  inverse)
    {
        this.adj     = adj;
        this.label   = label;
        this.elabel  = elabel;
        this.inverse = inverse;
        buildLabelMap (label);
        par          = (Set <Integer> []) new Set <?> [adj.length];
        if (inverse) addPar ();
    } // constructor 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a label-free `Graph_E` with no parent references.
     *  @param adj      the array of vertex (child) adjacency sets (outgoing edges)
     *  @param label    the array of verter labels
     *  @param inverse  whether to store inverse adjacency sets (parents)
     */
    public Graph_E (Set <Integer> [] adj)
    {
        this.adj = adj;
        label    = null;               // no vertex labels
        inverse  = false;
        elabel   = null;               // no edge labels
        par      = null;
    } // constructor 2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of this graph.
     * 
    @Override
    public Graph_E clone ()
    {
       // return new Graph_E (adj.clone (), label.clone (), elabel.clone (), inverse);
    } // clone
     */

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
    public void buildLabelMap (String [] label)
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
    public Set <Integer> getVerticesWithLabel (String l)
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
        return "Graph_E with " + size () + " vertices";
    } // toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the adjacency set, labels, and labelMap of the graph.
     */
    public void print ()
    {
        out.println ("adj: ");
        for (int i = 0; i < adj.length; i++) out.println (i + " -> " + adj [i]);
        if (label != null) {
            out.println ("Node labels: ");
            for (int i = 0; i < label.length; i++) out.println (i + " -> " + label [i]);
/***
            out.println ("labelMap: ");
            for (Map.Entry <String, Set <Integer>> entry: labelMap.entrySet ()) {
                    out.println ("Key: " + entry.getKey() + " Value: "
                                + Arrays.deepToString (entry.getValue ().toArray ()));                              
            } // for
***/
            out.println ("Edge labels: ");
            Iterator <Entry <Tuple_2, String>> i = elabel.entrySet ().iterator (); 
            while (i.hasNext ()){
                Tuple_2 key = i.next ().getKey ();
                out.println (key._1 + ", " + key._2 + ", " + elabel.get (key));
            } // while
        } // if
    } // print

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a graph from file input.
     *  @param gFile  the input graph file
     */
    @SuppressWarnings("unchecked")
    public static Graph_E apply (File gFile) 
    {
        Set <Integer> [] adj_new = null;
        String [] label_new      = null;
        boolean[] regex          = null;
        HashMap <Tuple_2, String> elabel = null;
        BufferedReader br;
        String strLine;

        try {           
             br = new BufferedReader (new FileReader (gFile));

             // first pass: get the vertex with the maximum value
             int max = -1;
             strLine = br.readLine();   // first line

             while ((strLine = br.readLine ()) != null) {
                 int val = Integer.parseInt (strLine.split (",") [0]);
                 if (val > max) max = val;
             } // while

             int numVertices = ++max;
             label_new = new String [numVertices];
             regex     = new boolean [numVertices];
             adj_new   = (Set <Integer> []) new Set <?> [numVertices];
             elabel    = new HashMap <> ();
             
             br.reset();    // second pass

             while ((strLine = br.readLine ()) != null) {               
                 String [] splits = strLine.split (",");     
                 int index = Integer.parseInt (splits [0]);
                 
                 label_new [index] = splits[1];
                 adj_new [index]   = null;
                 
                 for (int i = 2; i < splits.length; i++) {
                     int ver = Integer.parseInt(splits[i]);
                     if (adj_new[index] == null){
                         adj_new[index] = new HashSet <> (Arrays.asList (ver));
                     } else {
                         adj_new[index].addAll (Arrays.asList (ver));
                     } // if                          
                     i += 1;
                     String edgeLabel = splits [i];
                       
                     elabel.put(new Tuple_2 (index, ver), edgeLabel); 
                 } // for

                 if (adj_new [index] == null) adj_new [index] = new HashSet <> ();
             } // while
        } catch (Exception e) {
            e.printStackTrace();
        } // try
        
        return new Graph_E (adj_new, label_new, elabel, false);
    } // apply
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the `Graph_E` class.
     *  @param args  unused command-line arguments
     */
    @SuppressWarnings("unchecked")
    public static void main (String [] args)
    {
/***
        Set <Integer> [] adj_ = (Set <Integer> []) new Set <?> [3];
        adj_[0] = new HashSet <> (Arrays.asList (1, 2));
        adj_[1] = new HashSet <> (Arrays.asList (2));
        adj_[2] = new HashSet <> ();
        Integer [] label_ = { 2, 1, 2 };
        
        Graph_E g = new Graph_E (adj_, label_, false);
***/
        File file = new File ("/Users/sumanav/Desktop/input.csv");   // FIX
        Graph_E g = apply (file);
        g.print ();
    } // main

} // Graph_E class

