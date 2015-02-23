
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz
 *  @version 1.1
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.io._
import java.io._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTypes` specifies type for vertices and vertex labels.
 */
object GraphTypes
{
    /** Set of Integers (adjacency set for a vertex)
     */
    type ISet = Set [Int]

    /** Type for label, e.g., Int, String, etc. (not made generic for speed)
     */
    type TLabel = Int         // change and recompile (FIX: use Scala Macros)

} // GraphTypes

import GraphTypes._


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` class stores vertex-labeled directed graphs using an adjacency
 *  set ('adj') representation, e.g., adj = { {1, 2}, {0}, {1} } means that the
 *  graph has the following edges { (0, 1), (0, 2), (1, 0), (2, 1) }.
 *  Optionally, inverse adjacency via the 'par' array can be stored at the cost
 *  of nearly doubling the storage requirements.
 *  @param adj      the array of vertex (child) adjacency sets (outgoing edges)
 *  @param label    the array of verter labels
 *  @param inverse  whether to store inverse adjacency sets (parents)
 */
case class Graph (adj:     Array [ISet],
                  label:   Array [Int] = Array.ofDim (0),
                  inverse: Boolean = false)
      extends Cloneable 
{
    /** the map from label to the set of vertices with the label
     */
    val labelMap =  buildLabelMap (label)

    /** The optional array of vertex inverse (parent) adjacency sets (incoming edges)
     */
    val par = Array.ofDim [ISet] (adj.size)     // by default, don't use 'par'

    if (inverse) addPar ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clone (make a deep copy) of this graph.
     */
    override def clone (): Graph = Graph (adj.clone (), label.clone (), inverse)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the inverse adjacency sets for rapid accesses to parent vertices.
     */
    def addPar ()
    {
        for (j <- par.indices) par(j) = Set [Int] ()
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
    /** Given an array of labels, returns an index from labels to the sets of
     *  vertices containing those labels
     *  @param label  the array of vertex labels of type TLabel
     */
    def buildLabelMap (label: Array [Int]): Map [TLabel, ISet] =
    {
        var labelMap = Map [TLabel, ISet] ()
        label.foldLeft(0) ( (i, label) => {
            labelMap = labelMap + (label -> (labelMap.getOrElse (label, Set [Int] ()) + i))
            i + 1
        })  // foldLeft
        labelMap
    } // buildLabelMap

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
        var connectedNodes = Set [Int]()
        adj.foldLeft(0) { (i, set) =>
            if (! set.isEmpty) connectedNodes += i
            connectedNodes ++= set
            i + 1
        } // foldLeft
        connectedNodes.size == size
    } // isConnected:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the set of vertices in the graph with label l.
     */
    def getVerticesWithLabel (l: Int) = labelMap.getOrElse (l, Set [Int] ())

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indicate basic information about this graph.  Due to its potential size,
     *  use print to show graph details.
     */
    override def toString: String = "Graph with " + size + " vertices"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Prints the adjacency set, labels, and labelMap of the graph.
     */
    def print
    {
        println ("adj: ");      adj.foldLeft (0)   { (i, u) => { println (i + " -> " + u); i+1 } }
        println ("labels: ");   label.foldLeft (0) { (i, l) => { println (i + " -- " + l); i+1 } }
        println ("labelMap: "); labelMap.foreach { case (k, v) => println (k + " -> " + v) }
    } // print

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Writes the graph to ONE file in the following format:
     *  <vertexId> <labelId> <adjVertex1> <adjVertex2> ...
     *  @param gFile  the file in which to write the graph's label and edge information
     */
    def write2File (gFile: String)
    {
        val out = new PrintWriter (gFile)
        for (i <- adj.indices) {
            out.println (i + " " + label(i) + " " + adj(i).foldLeft ("")((str, n) => n + " " + str))
        } // for
        out.close
    } // writeToFile

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Writes the graph to TWO igraph compatible files.
     *  @see igraph.sourceforge.net
     */
    def write2IgraphFiles (prefix: String): (String, String) =
    {
        val lFile = prefix + "igl.txt"
        val eFile  = prefix + "ige.txt"
        val lOut   = new PrintWriter (lFile)
        label.foreach (lOut.println (_))
        lOut.close
        val eOut   = new PrintWriter (eFile)
        for (i <- adj.indices) adj(i).foreach (x => eOut.println (i + " " + x))
        eOut.close
        (lFile, eFile)
    } // write2IgraphFiles

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Writes the graph to TWO Neo4J compatible files: lFile and eFile so that
     *  they may be fed into Neo4j with one of its utilities.
     *  FIX:  need to handle multiple edge types.
     *  @param lFile  the file containing the graph labels (line: vertex-id TAB label)
     *  @param eFile  the file the edges (line: start-id TAB end-id TAB type)
     */
    def write2Neo4JFiles (lFile: String, eFile: String)
    {
        val vertexLine = new PrintWriter (lFile)       // write the vertex ids and their labels
        vertexLine.println ("id\tlabel")
        label.foldLeft (1) { (i, l) => vertexLine.println (i + "\t" + l); i + 1 }
        vertexLine.close
        val edgeLine = new PrintWriter (eFile)        // write the edges and their types.
        edgeLine.println ("start\tend\ttype")
        adj.foldLeft (1) { (i, v) =>
            v.foreach { c => edgeLine.println (i + "\t" + (c+1) + "\tEDGE") }
            i + 1
        } // foldLeft
        edgeLine.close
    } // write2Neo4JFiles

} // Graph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Graph` object is the companion object to the `Graph` class and is used
 *  for reading graphs from files or graph databases.
 */
object Graph
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reads a graph from ONE file based on the format used by 'write2File':
     *  <vertexId> <label> <adjVertex> <adjVertex2> ...
     *  @param gFile     the file containing the graph's label and edge information
     *  @param inverse   whether to store inverse adjacency sets (parents)
     */
    def apply (gFile: String, inverse: Boolean): Graph =
    {
        val lines = Source.fromFile (gFile).getLines         // get the lines from gFile
        val label = lines.map (_.trim.split (" ")(1).trim.toInt).toArray
        val adj   = lines.map (line =>
            if (line.trim != "") line.split (" ").slice (2, line.length).map(_.trim.toInt).toSet
            else Set [Int] ()
        ).toArray
        Graph (adj, label, inverse)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reads a graph from TWO files:
     *  'lFile' is a file with one label per line, where each line represents
     *    the vertex with id <lineNumber>.
     *  'eFile' is a file with each line representing the vertex with id
     *    <lineNumber>, and each line contains a space-separated list of vertices
     *    to which the current vertex is adjacent.
     *  @param lFile  the file containing the graph labels
     *  @param eFile  the file the edges (to create adjacency sets)
     *  @param inverse   whether to store inverse adjacency sets (parents)
     */
    def read2Files (lFile: String, eFile: String, inverse: Boolean = false): Graph =
    {
        val lLines = Source.fromFile (lFile).getLines        // get the lines from lFile
        val label  = lLines.map (_.trim.toInt).toArray       // make the label array
        val eLines = Source.fromFile (eFile).getLines        // get the lines from eFile
        val adj    = eLines.map ( line =>                    // make the adj array
            if (line.trim != "") line.split (" ").map (x => x.trim.toInt).toSet
            else Set [Int] ()
        ).toArray
        Graph (adj, label)
    } // read2Files

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reads a graph from TWO specially formatted Pajek files.
     *  @param lFile     the file containing the graph labels
     *  @param eFile     the file the edges (to create adjacency sets)
     *  @param inverse   whether to store inverse adjacency sets (parents)
     */
    def read2PajekFile (lFile: String, eFile: String, inverse: Boolean = false): Graph =
    {
        val lLines = Source.fromFile (lFile).getLines        // get the lines from lFile
        val label  = lLines.map (_.trim.toInt).toArray
        val adj = Array.ofDim [ISet] (label.size)
        for (i <- adj.indices) adj(i) = Set [Int] ()
        val eLines = Source.fromFile (eFile).getLines        // get the lines from eFile
        for (line <- eLines) {
            val splitL = line.split (" ").map (_.trim)
            val adjs   = splitL.slice (1, splitL.length).map(_.trim.toInt).toSet
            adj(splitL (0).toInt-1) ++= adjs
        } // for
        Graph (adj, label)
    } // read2PajekFile

} //  Graph object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTest` object is used to test the `Graph` class and object.
 */
object GraphTest extends App
{
    val gFile = "graph_test.txt"
    val g     = GraphGenerator.genRandomGraph (10, 5, 2)
    g.print
    println ("start writing graph to " + gFile)
    g.write2File (gFile)
    println ("end writing graph to " + gFile)

} // GraphTest

