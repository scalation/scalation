
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Sep 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 */

package scalation.animation

import math.abs
import collection.mutable.{HashSet, ListBuffer}

import scalation.animation.Counter.{nextE, nextN, nextT}
import scalation.scala2d.{CurvilinearShape, Ellipse, QCurve, Rectangle, R2}
import scalation.scala2d.Colors._
import scalation.scala2d.Shapes.{Dimension, Graphics2D, RectangularShape}
import scalation.util.Error

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class is for defining graph structures suitable for animation.  Graphs
 *  consist of nodes, edges and tokens.  Tokens can be positioned within nodes or
 *  on edges.  A graph animation class that uses this class would typically move
 *  the tokens by changing there location over time.  This class supports both
 *  directed graphs and bipartite graphs.  Directed graphs contain only primary
 *  nodes, while bipartite graphs have both primary and secondary nodes along with
 *  the rule that edges must go from primaries to secondaries or secondaries to
 *  primaries.  Bipartite graphs can be used to represent Petri Nets by letting
 *  Transitions be primary nodes and Places be secondary nodes.  Everything can be
 *  labeled (nodes, edges and tokens as well as the graph itself).  Nodes and edges
 *  may be added to/removed from graphs, while tokens may be added to/removed from
 *  either nodes or edges.  Tokens may also be free (not bound to nodes or edges).
 */
class Dgraph (name: String = "Dgraph", bipartite: Boolean = false)
              extends Error
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This class is used to represent nodes in the graph.
     *  @param shape    the shape of the node
     *  @param label    the label for the created node
     *  @param primary  whether it is a primary/transition/true or secondary/place node/false
     *  @param color    the color of the node
     *  @param x        the x-coordinate (top left)
     *  @param y        the y-ccordinate (top left)
     *  @param w        the width
     *  @param h        the height
     */
    case class Node (shape: RectangularShape, label: String, primary: Boolean, var color: Color,
                     x: Double, y: Double, w: Double, h: Double)
    {
        {
            shape.setFrame (x, y, w, h)
        } // primary constructor

        /** Node identifier
         */
        val id = nextN ()

        /** List of outgoing edges
         */
        val outEdges = ListBuffer [Edge] ()

        /** List of tokens current in this node
         */
        val tokens = ListBuffer [Token] ()

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Set (or reset) the color.
         *  @param color  the new color
         */
        def setColor (color2: Color) { color = color2 }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Add an outgoing edge to this node.
         *  @param edge  the edge to add
         */
        def addEdge (edge: Edge): Boolean =
        {
            if (bipartite && edge.from.primary == edge.to.primary) {
                flaw ("addEdge", "node types for edge endpoints may not be the same")
                return false
            } // if
            outEdges += edge
            true
        } // addEdge

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Remove an outgoing edge from this node.
         *  @param edge  the edge to remove
         */
        def removeEdge (edge: Edge) { outEdges -= edge }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Add a token from this node.
         *  @param token  the token to add
         */
        def addToken (token: Token) { tokens += token }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Remove a token from this node.
         *  @param token  the token to remove
         */
        def removeToken (token: Token) { tokens -= token }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert this node to a string.
         */
        override def toString = "Node " + label + " [ " + id + " ]"

    } // Node class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This class is used to represent edges in the graph.  If bend = 0, a
     *  straight line is created, otherwise a quadratic curve is created.
     *  @param shape    the shape (line/curve) of the edge
     *  @param label    the label for the created edge
     *  @param primary  whether it is a primary/transition/true or secondary/place node/false
     *  @param color    the color of the edge
     *  @param from     the origination node
     *  @param to       the destination node
     *  @param bend     the amount of bend in the curve
     */
    case class Edge (shape: CurvilinearShape, label: String, primary: Boolean, var color: Color,
                     from: Node, to: Node, bend: Double)
    {
        /** A very small real number
         */
        private val EPSILON = .000001

        {
            from.addEdge (this)              // add this edge to outgoing edges of from node

            var x1 = from.shape.getCenterX ()
            val y1 = from.shape.getCenterY ()
            var x2 = to.shape.getCenterX ()
            val y2 = to.shape.getCenterY ()
            if (x1 < x2) {
                x1 += from.shape.getWidth () / 2.
                x2 -= to.shape.getWidth () / 2.
            } else {
                x1 -= from.shape.getWidth () / 2.
                x2 += to.shape.getWidth () / 2.
            } // if

            if (abs (bend) < EPSILON) {
                shape.setLine (R2 (x1, y1), R2 (x2, y2))
            } else {
                shape.setLine (R2 (x1, y1), R2 (x2, y2), bend)
            } // if

        } // primary constructor

        /** Edge identifier
         */
        private val id = nextE ()

        /** List of tokens current on this edge.
         */
        val tokens = ListBuffer [Token] ()

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Construct an edge with no bend.
         * @param shape    the shape (line/curve) of the edge
         * @param label    the label for the created edge
         * @param primary  whether it is a primary/transition/true or secondary/place node/false
         * @param color    the color of the edge
         * @param from     the origination node
         * @param to       the destination node
         */
        def this (shape: CurvilinearShape, label: String, primary: Boolean, color: Color, from: Node, to: Node)
        {
            this (shape, label, primary, color, from, to, 0.)
        } // Edge constructor

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Construct an edge as a line with explicit coordinates.
         *  @param shape    the shape (line) of the edge
         *  @param label    the label for the created edge
         *  @param primary  whether it is a primary/transition/true or secondary/place node/false
         *  @param color    the color of the edge
         *  @param from     the origination node
         *  @param to       the destination node
         *  @param x1       the x-coordinate of the edge's start
         *  @param y1       the y-coordinate of the edge's start
         *  @param x2       the x-coordinate of the edge's end
         *  @param y2       the y-coordinate of the edge's end
         */
        def this (shape: CurvilinearShape, label: String, primary: Boolean, color: Color, from: Node, to: Node,
                  x1: Double, y1: Double, x2: Double, y2: Double)
        {
            this (shape, label, primary, color, from, to, 0.)
            from.addEdge (this)                      // add this edge to outgoing edges of from node
            shape.setLine (R2 (x1, y1), R2 (x2, y2))
        } // Edge constructor

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Construct an edge as a curve with explicit coordinates.
         *  @param shape    the shape (curve) of the edge
         *  @param label    the label for the created edge
         *  @param primary  whether it is a primary/transition/true or secondary/place node/false
         *  @param color    the color of the edge
         *  @param from     the origination node
         *  @param to       the destination node
         *  @param x1       the x-coordinate of the edge's start
         *  @param y1       the y-coordinate of the edge's start
         *  @param xc       the x-coordinate of the edge's control point
         *  @param yc       the y-coordinate of the edge's control point
         *  @param x2       the x-coordinate of the edge's end
         *  @param y2       the y-coordinate of the edge's end
         */
        def this (shape: CurvilinearShape, label: String, primary: Boolean, color: Color, from: Node, to: Node,
                  x1: Double, y1: Double, xc: Double, yc: Double, x2: Double, y2: Double)
        {
            this (shape, label, primary, color, from, to, 0.)
            from.addEdge (this)                      // add this edge to outgoing edges of from node
            shape.setLine (R2 (x1, y1), R2 (xc, yc), R2 (x2, y2))
        } // Edge constructor

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Set (or reset) the color.
         *  @param color  the new color
         */
        def setColor (color2: Color) { color = color2 }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Add a token from this node.
         *  @param token  the token to add
         */
        def addToken (token: Token) { tokens += token }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Convert this edge to a string.
         */
        override def toString = "Edge " + label + " [ " + id + " ]"

    } // Edge class

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This class is used to represent tokens in the graph.
     *  @param shape    the shape of the token
     *  @param label    the label for the created token
     *  @param primary  whether the token is primary/free/true to secondary/bound/false
     *  @param color    the color of the token
     *  @param onNode   the node the token is on
     *  @param w        the width of the token
     *  @param h        the height of the token
     */
    case class Token (shape: RectangularShape, label: String, primary: Boolean, var color: Color,
                      var onNode: Node, val w: Double, val h: Double)
    {
        {
            if (onNode != null) {
                onNode.addToken (this)
                val x = onNode.shape.getCenterX () - w / 2.0
                val y = onNode.shape.getCenterY () - h / 2.0
                shape.setFrame (x, y, w, h)
            } // if
        } // primary constructor

        /** Token identifier
         */
        private val id = nextT ()

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Construct a primary/free token with explicit coordinates.
         *  Such tokens are free to move anywhere in the drawing panel.
         *  @param shape  the shape of the token
         *  @param label  the label for the created token
         *  @param color  the color of the token
         *  @param x      the x-coordinate of the token's location
         *  @param y      the y-coordinate of the token's location
         *  @param w      the width of the token
         *  @param h      the height of the token
         */
        def this (shape: RectangularShape, label: String, primary: Boolean, color: Color,
                  x: Double, y: Double, w: Double, h: Double)
        {
            this (shape, label, true, color, null, w, h)
            shape.setFrame (x, y, w, h)
        } // Token constructor
 
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Set (or reset) the color.
         *  @param color  the new color
         */
        def setColor (color2: Color) { color = color2 }

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Set the node the token is on.
         *  @param onNode2  the node the token is on
         */
        def setOnNode (onNode2: Node) { onNode = onNode2 }

    } // Token class

    /** List of nodes in the graph
     */
    val nodes = ListBuffer [Node] ()

    /** List of edges in the graph
     */
    val edges = ListBuffer [Edge] ()

    /** List of free tokens in the graph (bound tokens must be in a nodes or edges list)
     */
    val freeTokens = ListBuffer [Token] ()

    /** Whether the nodes have been visited (internal use only)
     */
    private val visited = new HashSet [Node] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a node to the graph.
     *  @param n  the node to add
     */
    def addNode (n: Node) { nodes += n }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a node from the graph.
     *  @param n  the node to remove
     */
    def removeNode (n: Node) { nodes -= n }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add an edge to the graph.
     *  @param e  the edge to add
     */
    def addEdge (e: Edge) { edges += e }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove an edge from the graph.
     *  @param e  the edge to remove
     */
    def removeEdge (e: Edge) { edges -= e }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add a free token to the graph.
     *  @param t  the free token to add
     */
    def addFreeToken (t: Token) { freeTokens += t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove a free token from the graph.
     *  @param t  the free token to remove
     */
    def removeFreeToken (t: Token) { freeTokens -= t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get all the root nodes (those with no incoming edges).
     */
    def getRoots =
    {
        val roots = new ListBuffer [Node] ()
        for (n <- nodes) {
            var keep = true
            for (e <- edges) {
                if (n == e.to) keep = false
            } // for
            if (keep) roots += n
        } // for
        roots
    } // getRoots

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Mark all nodes as unvisited by clearing them from the hash set.
     */
    private def clearVisited () { visited.clear () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursively visit all nodes in the graph.
     *  @param n      the current node
     *  @param level  the recursion level
     */
    def traverse (n: Node, level: Int)
    {
        for (i <- 0 until level) print ("\t")
        println (n)              // print visited node
        //visited.add (n)
        val outgoing = n.outEdges
        if (outgoing != null) {
            for (oEdge <- outgoing) {
                val next = oEdge.to
                traverse (next, level + 1)
                //if ( ! visited. contains (next)) traverse (next, level + 1)
            } // for
        } // if
    } // traverse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Traverse the graph printing out its nodes and showing connectivity by indentation.
     */
    def traverseNodes ()
    {
        clearVisited ()
        //traverse (nodes.get (0), 0)          // only from node 0
        for (r <- getRoots) traverse (r, 0)    // from all roots
     } // traverseNodes

} // Dgraph class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to maintain counters.
 */
object Counter
{
    private var nCounter = 0
    private var eCounter = 0
    private var tCounter = 0

    def nextN () = { nCounter += 1; nCounter }
    def nextE () = { eCounter += 1; eCounter }
    def nextT () = { tCounter += 1; tCounter }

} // Counter object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Object to test the Dgraph class.
 */
object DgraphTest extends App
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build and test a directed graph.
     */
    private def testDirectedGraph (g: Dgraph)
    {
        // Create nodes
        val n1 = g.Node (Ellipse (), "node1", true, red, 100, 200, 20, 20)
        val n2 = g.Node (Ellipse (), "node2", true, blue, 300, 100, 20, 20)
        val n3 = g.Node (Ellipse (), "node3", true, green, 300, 300, 20, 20)
        val n4 = g.Node (Ellipse (), "node4", true, purple, 500, 200, 20, 20)

        // Create edges
        val e1 = new g.Edge (QCurve (), "edge1", true, black, n1, n2) // 120, 210, 300, 110)
        n1.addEdge (e1)
        val e2 = new g.Edge (QCurve (), "edge1", true, black, n1, n3) // 120, 210, 300, 310)
        n1.addEdge (e2)
        val e3 = new g.Edge (QCurve (), "edge1", true, black, n2, n4) // 320, 110, 500, 210)
        n2.addEdge (e3)
        val e4 = new g.Edge (QCurve (), "edge1", true, black, n3, n4) // 320, 310, 500, 210)
        n3.addEdge (e4)

        // Add the nodes and edges to the directed graph
        g.addNode (n1)
        g.addNode (n2)
        g.addNode (n3)
        g.addNode (n4)
        g.addEdge (e1)
        g.addEdge (e2)
        g.addEdge (e3)
        g.addEdge (e4)

        // Traverse the directed graph printing out its nodes
        g.traverseNodes ()
    } // testDirectedGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build and test a bipartite graph.
     */
    private def testBipartiteGraph (g: Dgraph)
    {
        // Create nodes
        val n1 = g.Node (Ellipse (), "node1", false, orange, 100, 100, 30, 30)
        val n2 = g.Node (Ellipse (), "node2", false, orange, 100, 300, 30, 30)
        val n3 = g.Node (Rectangle (), "node2", true, lightgreen, 300, 185, 30, 60)
        val n4 = g.Node (Ellipse (), "node4", false, red, 500, 100, 30, 30)
        val n5 = g.Node (Ellipse (), "node5", false, red, 500, 300, 30, 30)

        // Create edges
        val e1 = new g.Edge (QCurve (), "edge1", true, black, n1, n3) // 130, 115, 300, 215)
        n1.addEdge (e1)
        val e2 = new g.Edge (QCurve (), "edge2", true, black, n2, n3) // 130, 315, 300, 215)
        n2.addEdge (e2)
        val e3 = new g.Edge (QCurve (), "edge3", true, black, n3, n4) // 330, 215, 500, 115)
        n3.addEdge (e3)
        val e4 = new g.Edge (QCurve (), "edge4", true, black, n3, n5) // 330, 215, 500, 315)
        n3.addEdge (e4)

        // Add the nodes and edges to the directed graph
        g.addNode (n1)
        g.addNode (n2)
        g.addNode (n3)
        g.addNode (n4)
        g.addNode (n5)
        g.addEdge (e1)
        g.addEdge (e2)
        g.addEdge (e3)
        g.addEdge (e4)

        // Traverse the directed graph printing out its nodes
        g.traverseNodes ()
    } // testBipartiteGraph

    println ("Run DgraphTest - Bipartite Graph Test\n")
    val bg = new Dgraph ("Bipartite_Graph", true)
    testBipartiteGraph (bg)

    println ("Run DgraphTest - Directed Graph Test\n")
    val dg = new Dgraph ("Directed_Graph", false)
    testDirectedGraph (dg)

} // DgraphTest object

