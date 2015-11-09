
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   John Miller, Casey Bowman
 *  @version  1.0
 *  @date     Sat Apr  12 12:50:34 EST 2013
 *  @see      LICENSE (MIT style license file).
 */

package scalation.graphalytics

import collection.mutable.ListBuffer

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType._
import scalation.random.Randi
import scalation.scala2d.Colors._
import scalation.scala2d.{Ellipse, QCurve, Rectangle}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorDAG` class provides a data structure Directed Acyclic Graphs (DAGs) with colored
 *  nodes.  The ColorDAG consists of source nodes (in-degree is 0), sink nodes (out-degree is 0)
 *  and internal nodes.  The edges connecting nodes must have color compatibility.
 *  The ColorDAG is divided into 'k' stages:  e.g., sources (stage 0), internals (stages 1..k-2)
 *  and sinks (stage k-1).
 *  @param dimensions  the dimension (number of nodes) for each stage
 *  @param maxIn       the maximum number of input colors for an internal node
 *  @param maxOut      the maximum number of output colors for an internal node
 *  @param minColors   the minimum number of child colors allowed in the color hierarchy
 *  @param maxColors   the maximum number of child colors allowed in the color hierarchy
 *  @param colorDepth  the maximum depth of the color hierarchy tree
 *  @param seed        the seed for the various random number generators
 */
class ColorDAG (dimensions: Array [Int], maxIn: Int, maxOut: Int,
                minColors: Int, maxColors: Int, colorDepth: Int, seed: Int = 0)
{
    val stages = dimensions.length                        // the number of stages in the DAG (k stages)
    val nodes  = Array.ofDim [Array [DagNode]] (stages)   // a ragged array of DagNode arrays to hold the nodes in the DAG
    val edges  = ListBuffer [Edge] ()                     // a list to hold the edges of the DAG

    val colors = ColorTree (colorDepth, minColors, maxColors)     // get a new ColorTree object with specified limits on child nodes
    val nColors = colors.size                             // get the number of colors in the ColorTree
 
    val randIn  = new Randi (1, maxIn, seed)              // random number generator for number of incoming edges for a node
    val randOut = new Randi (1, maxOut, seed)             // random number generator for number of outgoing edges for a node
    val randCol = new Randi (0, nColors-1, seed)          // random number generator for assigning colors to nodes and edges

    for (i <- 0 until stages) nodes(i) = Array.ofDim [DagNode] (dimensions(i))    // allocate an array for each stage of DAG

    println ("ColorDAG (dimensions = " + dimensions.deep +
                      " maxIn = " + maxIn +
                      " maxOut = " + maxOut +
                      " minColors = " + minColors +
                      " maxColors = " + maxColors +
                      " colorDepth = " + colorDepth +
                      " seed = " + seed)


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This innner class holds information about a node in the DAG.
     *  @param id         the id number for this node
     *  @param inDegree   the number of incoming edges/inputs for this node
     *  @param outDegree  the number of outgoing edges/outputs for this node
     */
    class DagNode (val id: Int, val inDegree: Int, val outDegree: Int) 
    {
        val inColors  = Array.ofDim [TreeNode] (inDegree)      // array to hold the node's input colors
        val outColors = Array.ofDim [TreeNode] (outDegree)     // array to hold the node's output colors
        val in        = ListBuffer [DagNode] ()                // list to hold all the nodes providing inputs to this node
        val out       = ListBuffer [DagNode] ()                // list to hold all the nodes this node provides outputs for
        var used      = false                                  // flag to mark that a node is definitlely included in DAG
        var unused    = false                                  // flag to mark that a node is ruled out of inclusion in DAG

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** This method determines whether or not the node is a source node.
         */
        def isSource: Boolean = inDegree == 0

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** This method determines whether or not the node is a sink node.
         */
        def isSink: Boolean = outDegree == 0

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert the DAG node to a string.
         */
        override def toString: String = "DagNode( " + id + ": " + inColors + ", " + outColors + " )"

    } // DagNode class


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This class is used to connect two nodes.
     *  @param fromNode  the node where this edge originates
     *  @param toNode    the node where this edge terminates
     *  @param color     the color for this edge
     */
    class Edge (val fromNode: DagNode, val toNode: DagNode, val color: Color)


    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate all the unique nodes that will exist in the DAG and assign
     *  colors/types to their inputs and outputs.
     */
    def genNodes ()
    {	       
        var numNodes = 0
        for (i <- 0 until stages; j <- 0 until dimensions(i)) {
            val n = new DagNode (numNodes,
                                 if (i == 0) 0 else if (i == stages-1) 1 else randIn.igen, 
                                 if (i == stages-1) 0 else if (i == 0) 1 else randOut.igen)
            numNodes += 1
            nodes(i)(j) = n 
            for (k <- 0 until n.inDegree)  n.inColors(k)  = genUniqueColor (n.inColors)
            for (k <- 0 until n.outDegree) n.outColors(k) = genUniqueColor (n.outColors)
        } // for	
    } // genNodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate and return a unique color for either a node's inputs or outputs.
     *  @param ioColors  the array of existing colors
     */
    def genUniqueColor (ioColors: Array [TreeNode]): TreeNode =
    {
        for (i <- 0 until 2*ioColors.length) {
            val colr = colors(randCol.igen)
            if (! (ioColors contains colr)) return colr
        } // for
        println ("genUniqueColor: unable to generate a unique color")
        null
    } // genUniqueColor

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For each node in the DAG, check that (1) all of its "inputs" are color/type compatible
     *  with an output from a node from the previous stage, and (2) at least one of its
     *  "outputs" is color/type compatible with an input to a node from the next stage.
     *  Nodes that fails to have all of their inputs satified (check number 1) are thrown
     *  out before check if they have at least one useful output.  Nodes failing check
     *  number 2 will also be thrown out.
     */
    def checkIO (): Int =
    {
        for (i <- 1 until stages; n <- nodes(i) if ! n.unused) {           // unused node n
            for (j <- 0 until n.inDegree) {                                // check inputs
                var found = false
                for (np <- nodes(i-1) if ! np.unused && ! found) {         // predecessor node np
                    for (k <- 0 until np.outDegree if ! found) {
                        if (n.inColors(j) isAncestor np.outColors(k)) found = true
                    } // for
                } // for
                if (! found) n.unused = true
            } // for
        } // for

        for (i <- stages-2 to 0 by -1; n <- nodes(i) if ! n.unused) {      // unused node n
            var found = false
            for (j <- 0 until n.outDegree if ! found) {                    // check outputs
                for (ns <- nodes(i+1) if ! ns.unused && ! found) {         // successor node ns
                    for (k <- 0 until ns.inDegree if ! found) {
                        if (ns.inColors(k) isAncestor n.outColors(j)) found = true
                    } // for
                } // for
            } // for
            if (! found) n.unused = true
        } // for

        var numNodes = 0
        for (i <- 0 until stages; n <- nodes(i) if ! n.unused) {
            n.used = true
            numNodes += 1
        } // for
        numNodes                       // return the number of active nodes
    } // checkIO

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Connect all the nodes that are going to be "used" in the DAG by creating an edge between
     *  two nodes in adjacent stages that are color/type compatible.  Also add these edges to
     *  the DAG's list of edges.
     */
    def connect ()
    {
        for (i <- 0 until stages-1; n <- nodes(i) if n.used) {             // used node n
            for (j <- 0 until n.outDegree) {
                for (ns <- nodes(i+1) if ns.used) {                        // successor node ns
                    for (k <- 0 until ns.inDegree) {
                        if (ns.inColors(k) isAncestor n.outColors(j)) {
                            edges += (if (n.isSource) new Edge (n, ns, n.outColors(0).colr)
                                 else if (ns.isSink)  new Edge (n, ns, ns.inColors(0).colr)
                                 else                 new Edge (n, ns, n.outColors(j).colr))
                        } // if
                    } // for
                } // for
            } // for
        } // for
    } // connect

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Populate the in and out ListBuffers for each node based on the DAG's list of edges.
     */
    def popInOut ()
    {
        for (e <- edges) {
            val from = e.fromNode
            val to   = e.toNode
            if (! (from.out contains to) && ! (to.in contains from)) {
                from.out += to
                to.in    += from
            } // if
        } // for
    } // popInOut

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the nodes and edges in the color DAG.
     */
    def printDAG
    {
        println ("-------------------------------------------------------------------------")
        println ("print ColorDAG")
        println ("Nodes -------------------------------------------------------------------")
        for (i <- 0 until stages) {
            println ("stage: " + i)
            for (n <- nodes(i) if n.used) {
                if (n.isSource) {
                    println ("node(" + n.id + "):\t" + n.outColors(0).colr)
                } else if (n.isSink) {
                    println ("node(" + n.id + "):\t" + n.inColors(0).colr)
                } else {
                    println ("node(" + n.id + "):\t" + n.inColors(0).colr)
                    println ("node(" + 1000 + n.id + "):\t" + n.outColors(0).colr)
                } // if
            } // for
        } // for
        println ("Edges -------------------------------------------------------------------")
        var count = 0
        for (e <- edges) {
            println ("edge(" + count + "):\t" + e.fromNode.id + " -> " + e.toNode.id + ", " + e.color)
            count += 1
        } // for
        println ("-------------------------------------------------------------------------")
    } // printDAG

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the in (for inputs) and out (for outputs) colors for each node in the DAG.
     */
    def printColors
    {
        println ("nodes(i): [in colors], [out colors]")
        for (s <- nodes; n <- s) {                                  // node n in stage s
            print ("node(" + n.id + "):\t[")
            for (n_in <- n.inColors) print (n_in.id + " ")
            print ("],\t[")
            for (n_out <- n.outColors) print (n_out.id + " ")
            println ("]")
        } // for
    } // printColors

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the in (for inputs) and out (for outputs) node list buffers for each used node
     *  in the DAG.
     */
    def printInOut
    {
        for (s <- nodes; n <- s if n.used) {                        // used node n in stage s
            print ("nodes(" + n.id + "): [")
            for (n_in <- n.in) print (n_in.id + " ")
            print ("], [")
            for (n_out <- n.out) print (n_out.id + " ")
            println ("]")
        } // for
    } // printInOut

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the ColorTree used by the DAG for node and edge colors/type and animate its
     *  construction.
     */ 
    def printColorTree
    {
        colors.printTree                     // print the ColorTree
        colors.showAnimation                 // show the animation of the ColorTree hierarchy
    } // printColorTree

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Animate the color DAG.
     */
    def animateDAG
    {
        println ("Run DAGAnimation")
        val nSize = 20
        val width = 1200 / (stages + 1)
        val dga   = new DgAnimator ("DAGAnimator", black, white)
        val cq    = dga.getCommandQueue

        for (i <- 0 until stages) {                               // display nodes
            var nodeCount = dimensions(i)                         // the number of nodes being animated for current stage
            var height = 800 / (nodeCount+1)                      // the height for each level in the current stage
            var count = 0
            for (n <- nodes(i)) {
                var y = -nSize/2 + height + height * count        // the y coordinate for the current node being animated
                var x = -nSize/2 + width + width * i              // the x coordinate for the current node being animated
                if (i == 0) {
                    cq.add (AnimateCommand (CreateNode, n.id, Ellipse (), "", false, n.outColors(0).colr,
                                            Array (x, y, nSize, nSize), 0))
                } else if (i == stages-1) {
                    cq.add (AnimateCommand (CreateNode, n.id, Ellipse (), "", false, n.inColors(0).colr,
                                            Array (x, y, nSize, nSize), 0))
                } else {
                    cq.add (AnimateCommand (CreateNode, n.id, Rectangle (), "", false, n.inColors(0).colr,
                                            Array (x, y, nSize, nSize), 0))
                    cq.add (AnimateCommand (CreateNode, 1000 + n.id, Rectangle (), "", false, n.outColors(0).colr,
                                            Array (x, y+nSize, nSize, nSize), 0))
                } // if
                count += 1
            } // for
        } // for

        var eCount = 0
        for (e <- edges) {                                        // display edges
            eCount += 1
            cq.add (AnimateCommand (CreateEdge, eCount, QCurve (), "", true, e.color,
                                    null, 2000, e.fromNode.id, e.toNode.id))
        } // for
        dga.animate (0, 100000)
    } // animateDAG

} // ColorDAG class


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ColorDAGTest` object used to test the `ColorDAG` class.
 */
object ColorDAGTest extends App
{
    val seed = if (args.length == 1) args(0).toInt else 0

    val stg = Array (5, 5, 5, 5)                       // define the number of nodes for each stage of color DAG
    val dag = new ColorDAG (stg, 2, 2, 2, 2, 2, seed)  // create a new color DAG
    dag.printColorTree                                 // print the color type representing the type hierarchy

    dag.genNodes ()                                    // generate the nodes in the color DAG
    dag.checkIO ()                                     // throw out nodes that are color/type incompatible
    dag.connect ()                                     // connect the remaining node via edges
    dag.popInOut ()                                    // populated the input/ouput lists from edges

    dag.printDAG                                       // print the nodes and edges
    dag.printColors                                    // print inputs and output colors for each node
//  dag.printInOut                                     // print the input and ouput lists for each node
    dag.animateDAG                                     // animate the color DAG

} // ColorDAGTest

