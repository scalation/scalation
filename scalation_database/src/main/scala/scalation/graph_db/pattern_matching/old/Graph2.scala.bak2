
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 1.1
 *  @date    Mon Nov 11 19:03:45 EST 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph2Types` specifies type for vertices and vertex labels.
 */
object Graph2Types
{
    /** List of Integers (adjacency list for a vertex)
     */
    type AList = ArrayBuffer [Int]

    /** Type for label, e.g., Int, String, etc. (not made generic for speed)
     */
    type TLabel = Int         // change and recompile (FIX: use Scala Macros)

} // Graph2Types

import Graph2Types._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph2` class stores vertex-labeled directed graphs using an adjacency
 *  list 'adj' representation, e.g., adj = { {1, 2}, {0}, {1} } means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, invserse adjacency via the 'par' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *  @param adj       the array of vertex (child) adjacency list (outgoing edges)
 *  @param label     the array of vertex labels
 *  @param inverse   whether to store inverse adjacency list (parents)
 */
class Graph2 (val adj:     Array [AList],                 // child vertices
              val label:   Array [Int] = Array.ofDim (0),   // vertex labels
                  inverse: Boolean = false)                 // parent vertices flag
{
    /** the map from label to the list of vertices with the label
     */
    val labelMap =  buildLabelMapFromLabels (label)

    /** the optional array of vertex inverse (parent) adjacency list (incoming edges)
     */
    var par: Array [AList] = null     // by default, don't use 'par'

    if (inverse) addPar ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the inverse adjacency list for rapid accesses to parent vertices.
     */
    def addPar ()
    {
        par = Array.ofDim [AList] (adj.size)
        for (j <- par.indices) par(j) = ArrayBuffer [Int] ()
        for (i <- adj.indices; j <- adj(i)) par(j) += i
    } // addPar

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the number of vertices in the graph.
     */
    def size = adj.size

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the number of edges in the graph.
     */
    def nEdges = adj.foldLeft (0) { (n, i) => n + i.size }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an array of labels, returns an index from labels to the lists of
     *  vertices containing those labels
     *  @param label  the array of vertex labels of type TLabel
     */
    def buildLabelMapFromLabels (label: Array [Int]): Map [TLabel, AList] =
    {
        var labelMap = Map [TLabel, AList] ()
        label.foldLeft (0) ( (i, label) => {
            labelMap = labelMap + (label -> (labelMap.getOrElse (label, ArrayBuffer [Int] ()) += i))
            i + 1
        })  // foldLeft
        labelMap
    } // buildLabelMapFromLabels

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the maximum label value.
     */ 
    def nLabels = labelMap.keys.max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determines the number of vertices in the graph that have outgoing edges
     *  to themselves.
     */ 
    def nSelfLoops: Int =
    {
        adj.indices.foldLeft (0) { (sum, i) => if (adj(i) contains i) sum + 1 else sum }
    } // nSelfLoops

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determines whether or not the graph is connected.
     */
    def isConnected: Boolean =
    {
        var connectedNodes = ArrayBuffer [Int] ()
        adj.foldLeft(0) { (i, list) =>
            if (! list.isEmpty) connectedNodes += i
            connectedNodes ++= list
            i + 1
        } // foldLeft
        connectedNodes.size == size
    } // isConnected

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the list of vertices in the graph with label l.
     */
    def getVerticesWithLabel (l: Int) = labelMap.getOrElse (l, ArrayBuffer [Int] ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate basic information about this graph.  Due to its potential size,
     *  use print to show graph details.
     */
    override def toString: String = "Graph with " + size + " vertices"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prints the adjacency list, labels, and labelMap of the graph.
     */
    def print
    {
        println ("adj: ");      adj.foldLeft (0)   { (i, u) => { println (i + " -> " + u); i+1 } }
        println ("labels: ");   label.foldLeft (0) { (i, l) => { println (i + " -- " + l); i+1 } }
        println ("labelMap: "); labelMap.foreach   { case (k, v) => println (k + " -> " + v) }
    } // print

} // Graph2 class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph2Test` object is used to test the `Graph2` class.
 */
object Graph2Test extends App
{
    val g = new Graph2 (Array (ArrayBuffer (4, 5),                // 0
                               ArrayBuffer (5),                   // 1
                               ArrayBuffer (6, 7),                // 2
                               ArrayBuffer (7, 8),                // 3
                               ArrayBuffer (0, 5, 9),             // 4
                               ArrayBuffer (0, 1, 4, 6, 10),      // 5
                               ArrayBuffer (2, 5, 7, 10, 11),     // 6
                               ArrayBuffer (2, 3, 6, 8),          // 7
                               ArrayBuffer (3, 7, 12),            // 8
                               ArrayBuffer (4),                   // 9
                               ArrayBuffer (5, 6),                // 10
                               ArrayBuffer (6),                   // 11
                               ArrayBuffer (8)))                  // 12

    g.print

} // Graph2Test

