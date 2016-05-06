
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.2
 *  @date    Fri Jul 10 12:39:33 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics
package mutable

import java.io.PrintWriter

import scala.collection.mutable.{Set => SET}
import scala.io.Source.fromFile

import GraphIO.EXT
import LabelType.{TLabel, toTLabel}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIO` class is used to write digraphs to a file.
 *  @param g  the digraph to write
 */
class GraphIO (g: Graph)
{
    private val DEBUG = true                                     // debug flag

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write digraph 'g' to a file in the following format:
     *  <p>
     *      Graph (<name>, <inverse>, <nVertices>
     *      <vertexId> <label> <chVertex0> <chVertex1> ...
     *      ...
     *      )
     *  <p>
     *  @param name  the file-name containing the graph's vertex, edge and label information
     *  @param base  the base sub-directory for storing graphs
     *  @param ext   the standard file extension for graph
     */
    def write (name: String = g.name, base: String = BASE_DIR, ext: String = EXT)
    {
        val gFile = base + name + ext                             // relative path-name for file
        val pw    = new PrintWriter (gFile)
        if (DEBUG) println (s"write: gFile = $gFile")
        pw.println (s"Graph (${g.name}, ${g.inverse}, ${g.size}")
        for (i <- g.ch.indices) pw.println (g.toLine (i))
        pw.println (")")
        pw.close ()
    } // write

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write the graph to TWO 'igraph' compatible files.
     *  @see igraph.sourceforge.net
     */
    def write2IgraphFiles (prefix: String): (String, String) =
    {
        val lFile = prefix + "igl.txt"
        val eFile  = prefix + "ige.txt"
        val lOut   = new PrintWriter (lFile)
        g.label.foreach (lOut.println (_))
        lOut.close
        val eOut   = new PrintWriter (eFile)
        for (i <- g.ch.indices) g.ch(i).foreach (x => eOut.println (i + " " + x))
        eOut.close
        (lFile, eFile)
    } // write2IgraphFiles

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write the graph to TWO 'Neo4J' compatible files: 'lFile' and 'eFile' so that
     *  they may be fed into 'Neo4j' with one of its utilities.
     *  FIX:  need to handle multiple edge types.
     *  @param lFile  the file containing the graph labels (line: vertex-id TAB label)
     *  @param eFile  the file the edges (line: start-id TAB end-id TAB type)
     */
    def write2Neo4JFiles (lFile: String, eFile: String)
    {
        val vertexLine = new PrintWriter (lFile)       // write the vertex ids and their labels
        vertexLine.println ("id\tlabel")
        g.label.foldLeft (1) { (i, l) => vertexLine.println (i + "\t" + l); i + 1 }
        vertexLine.close
        val edgeLine = new PrintWriter (eFile)        // write the edges and their types.
        edgeLine.println ("start\tend\ttype")
        g.ch.foldLeft (1) { (i, v) =>
            v.foreach { c => edgeLine.println (i + "\t" + (c+1) + "\tEDGE") }
            i + 1
        } // foldLeft
        edgeLine.close
    } // write2Neo4JFiles

} // GraphIO class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIO` object is the companion object to the `GraphIO` class and
 *  is used for reading digraphs from files.
 */
object GraphIO
{
    /** The standard file extension for digraphs
     */
    val EXT = ".dig"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an array of integers as strings, make the corresponding set.
     *  @param line  the element string array
     */
    def makeSet (eStrArr: Array [String]): SET [Int] =
    {
        if (eStrArr(0) == "") SET [Int] () else SET (eStrArr.map (_.toInt): _*)
    } // makeSet

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read a digraph from a file based on the format used by 'print' and 'write':
     *  <p>
     *      Graph (<name>, <inverse>, <nVertices>
     *      <vertexId>, <label>, <chVertex0>, <chVertex1> ...
     *      ...
     *      )
     *  <p>
     *  @param name  the file-name containing the graph's vertex, edge and label information
     *  @param base  the base sub-directory for storing graphs
     *  @param ext   the standard file extension for graph
     *  @param sep   the character separating the values (e.g., ',', ' ', '\t')
     */
    def apply (name: String, base: String = BASE_DIR, ext: String = EXT, sep: Char = ','): Graph =
    {
        val gFile = base + name + ext                             // relative path-name for file
        val l     = fromFile (gFile).getLines.toArray             // get the lines from gFile
        var l0    = l(0).split ('(')(1).split (sep).map (_.trim)  // array for line 0
        val n     = l0(2).toInt                                   // number of lines in file
        val ch    = Array.ofDim [SET [Int]] (n)                   // adjacency: array of children (ch)
        val label = Array.ofDim [TLabel] (n)                      // array of vertex labels
        println (s"apply: read $n vertices from $gFile")

        for (i <- ch.indices) {
            val li   = l(i+1).split (sep).map (_.trim)            // line i (>0) splits into i, label, ch
            label(i) = toTLabel (li(1))                           // make vertex label
            ch(i)    = makeSet (li.slice (2, li.length))          // make ch set
        } // for
        new Graph (ch, label, l0(1) == "true", l0(0))
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read a graph from TWO files:
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
        val lLines = fromFile (lFile).getLines                    // get the lines from lFile
        val label  = lLines.map (x => toTLabel (x.trim)).toArray         // make the label array
        val eLines = fromFile (eFile).getLines                    // get the lines from eFile
        val ch     = eLines.map ( line =>                         // make the adj array
            if (line.trim != "") line.split (" ").map (x => x.trim.toInt).toSet.asInstanceOf [SET [Int]]
            else SET [Int] ()
        ).toArray
        new Graph (ch, label)
    } // read2Files

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Read a graph from TWO specially formatted Pajek files.
     *  @param lFile     the file containing the graph labels
     *  @param eFile     the file the edges (to create adjacency sets)
     *  @param inverse   whether to store inverse adjacency sets (parents)
     */
    def read2PajekFile (lFile: String, eFile: String, inverse: Boolean = false): Graph =
    {
        val lLines = fromFile (lFile).getLines               // get the lines from lFile
        val label  = lLines.map (x => toTLabel (x.trim)).toArray
        val ch     = Array.ofDim [SET [Int]] (label.size)
        for (i <- ch.indices) ch(i) = SET [Int] ()
        val eLines = fromFile (eFile).getLines               // get the lines from eFile
        for (line <- eLines) {
            val splitL = line.split (" ").map (_.trim)
            val adjs   = splitL.slice (1, splitL.length).map(_.trim.toInt).toSet
            ch(splitL(0).toInt-1) ++= adjs
        } // for
        new Graph (ch, label)
    } // read2PajekFile

} // GraphIO object

import GraphGen._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphIOTest` object is used to test the `GraphIO` class and object.
 *  > run-main scalation.graphalytics.mutable.GraphIOTest
 */
object GraphIOTest extends App
{
    val name     = "ran_graph"    // the name of the graph
    val size     = 50             // size of the graph
    val nLabels  = 10             // number of distinct vertex labels
    val avDegree =  5             // average vertex out degree for the graph
    val inverse  = false

    // Create a random graph and print it out

    val ran_graph = genRandomGraph (size, nLabels, avDegree, inverse, "ran_graph")
    println (s"ran_graph = $ran_graph")
    ran_graph.printG (false)
    ran_graph.printG ()

    // Write the graph to a file

    println ("start writing graph to " + name)
    (new GraphIO (ran_graph)).write ()
    println ("end writing graph to " + name)

    // Read the file to create a new identical graph

    val g = GraphIO (name)
    println (s"g = $g")
    g.printG ()

} // GraphIOTest object

