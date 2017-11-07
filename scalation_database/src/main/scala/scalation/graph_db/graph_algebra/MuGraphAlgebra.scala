
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author   Supriya Ramireddy, John Miller
 *  @version 1.4
 *  @date    Tue Jun 23 18:10:12 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  'MuGraphALgebra' contains all the algebra operations on graphs
 */

package scalation.graph_db
package graph_algebra

import scala.collection.mutable.{ArrayBuffer, Map, Set => SET}
import scala.reflect.ClassTag
import util.control.Breaks.{break, breakable}

import scalation.util.{ReArray, time, timed, Wildcard}

import MuGraph._
import PathType._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphAlgebra` class provide graph algebra operation on `MuGraph`s.
 *----------------------------------------------------------------------------
 *  @param ch       the array of child (adjacency) vertex sets (outgoing edges)
 *  @param label    the array of vertex labels: v -> vertex label
 *  @param elabel   the map of edge labels: (u, v) -> edge label
 *  @param inverse  whether to store inverse adjacency sets (parents)
 *  @param name     the name of the multi-digraph
 *  @param id       the array of vertex id's
 *  @param schema   optional schema: map from label to label type
 */
class MuGraphAlgebra [TLabel: ClassTag] (ch:      Array [SET [Int]],
                                         label:   Array [TLabel],
                                         elabel:  Map [Pair, SET [TLabel]],
                                         inverse: Boolean = false,
                                         name:    String = "g",
                                         id:      Array [Int] = Array (),
                                         schema:  Array [String] = Array ())
      extends MuGraph (ch, label, elabel, inverse, "g", id, schema)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the indices of all vertices in the graph.
     */
    def vertexSet: SET [Int] = SET (Array.range (0, size):_*)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return all the vertices in the graph.
     */
    def getVertices: Rows =
    {
        val rows = new Rows
        for (v <- ch.indices) {
            val path = new Path
            path += v
            rows += path
        } // for
        rows
    } // getVertices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select by vertex label the vertices in the graph satifying the theta predicate
     *  'label(v) θ c'.
     *  @param θ   comparison/theta operator
     *  @param c   the vertex label sought
     *  @param vs  the set of vertices to be considered for the input graph
     */
    def selectByVLabel (θ: (TLabel, TLabel) => Boolean, c: TLabel)(vs: SET[Int] = null): Rows =
    {
        val rows = new Rows
        for (v <- vs; if θ (label(v), c)) {
            val path = new Path
            path += v
            rows += path
        } // for
        rows
    } // selectByVLabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select by schema type the vertices in the graph satifying the theta predicate
     *  'schema(v) θ c'.
     *  @param θ   comparison/theta operator
     *  @param c   the vertex label sought
     *  @param vs  the set of vertices to be considered for the input graph
     */
    def selectBySchema (θ: (String, String) => Boolean, c: String)(vs: SET[Int] = null): Rows =
    {
        val rows = new Rows
        for (v <- vs; if θ (schema(v), c)) {
            val path = new Path
            path += v
            rows += path
        } // for
        rows
    } // selectBySchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select by vertex, the graph with the vertices satisfying the theta predicate, and
     *  the edges connecting the selected vertices
     *  @param θ  comparison/theta operator
     *  @param c  label of the vertex
     */
    def selectByVertexGraph (θ: (TLabel, TLabel) => Boolean, c: TLabel): MuGraph[TLabel] =
    {
        var n    = 0
        val vset = new Path
        for (v <- label.indices if θ(label(v), c)) vset += v   // to collect all the vertices matching the condition
        val lab  = new ReArray [TLabel]()                      // resizable array of vertex labels
        val new_id    = new ReArray [Int]()                    // resizable array of old id's of vertices
        val new_index = new ReArray [Int]()                    // restore old indices for the creation of edges
        for (v <- vset) {
            new_index(n) =  v                                  // assigning old index
            new_id(n)  =  id(v)
            lab(n) =  label(v)
            n         += 1
        } // for
        val le = induceEdges (vset, new_index)                 // induce the edges of the vertices selected
        MuGraph (lab.toArray, le, new_id.toArray, inverse, schema)
    } // selectByVertex_Graph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Induce edges, the map of edges with the vertices only from vset
     *  @param vset      the set of vertices, to be considered for selecting the edges
     *  @param new_index the old index of the vertices to be used for accessing edges of graph
     */
    def induceEdges (vset: Path, new_index: ReArray[Int]): Map [Pair, SET [TLabel]]=
    {
        val le = Map [Pair, SET [TLabel]] ()
        for (edge <- elabel.keys) {
            if ((vset contains edge._1) && (vset contains edge._2))
                le += ((new_index.indexOf(edge._1), new_index.indexOf(edge._2)) -> elabel(edge))
        } // for
        le
    } // induceEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select by edge label the edges in the graph having both the vertices 'vs' and
     *  satifying the theta predicate 'exists elabel(e) θ c'.
     *  @param θ   comparison/theta operator
     *  @param c   the vertex label sought
     *  @param vs  the set of vertices to be considered for the input graph
     */
    def selectByELabel (θ: (TLabel, TLabel) => Boolean, c: TLabel, schema: Array [String]) (vs: SET [Int]):
                      Array [(Pair, SET [TLabel])] =
    {
        val le = Map [Pair, SET [TLabel]] ()
        for (e <- elabel.keys if elabel(e).exists (θ(_, c))) {
            if ((vs contains e._1) && (vs contains e._2)) le += e -> elabel(e)   
        } // for
        le.toArray
    } // selectByELabel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Select by edge, the Graph with the edges satisfying the theta predicate
     *  @param θ  comparison operator
     *  @param c  label of the edge
     */
    def selectByEdgeGraph (θ: (TLabel, TLabel) => Boolean, c: TLabel): MuGraph [TLabel] =
    {
        val le        = Map[Pair, SET[TLabel]]()
        val old_index = ArrayBuffer[Int]()

        for (edge <- elabel.keys) {
            if (elabel(edge).exists(θ(_, c))) le += (edge -> elabel(edge))
        } // for
        MuGraph (label, le, id, inverse, schema)
    } // selectByEdgeGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand all, expands the given paths by adding one forward edge to each path.
     *  @param paths  the paths to be expanded
     *  @param keep   whether the given path has to be included along with the expanded vertices
     */
    def expandAll (paths: Rows, keep: Boolean = false): Rows =
    {
        val rows = new Rows
        for (i <- paths.indices) {
            val path = paths(i)
            val v    = path(path.length - 1)
            for (u <- ch(v)) {
                val path2 = if (keep) path.clone else new Path
                path2 += u
                rows  += path2
            } // for
        } // for
        rows
    } // expandAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand, expands the given paths by adding one forward edge of given edge
     *  label to each path.
     *  @param paths  the paths to be expanded
     *  @param elab   the label of the edges to be expanded
     *  @param keep   whether the given path has to be inluded along with the expanded vertices
     */
    def expand (paths: Rows, elab:TLabel, keep: Boolean = false): Rows =
    {
        val rows = new Rows
        for (i <- paths.indices) {
            val path = paths(i)
            val v    = path(path.length - 1)
            for (u <- ch(v)) {
                if (elabel((v, u)) contains elab) {
                    val path2 = if (keep) path.clone else new Path
                    path2 += u
                    rows  += path2
                } // if
            } // for
        } // for
        rows
    } // expand

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand by schema, expands the given paths by adding one forward edge (of given type lab)
     *  to each path.
     *  @param paths  the paths to be expanded
     *  @param keep   whether the given path has to be inluded along with the expanded vertices
     */
    def expandBySchema (paths: Rows, lab: String, keep: Boolean  = false): Rows =
    {
        val rows = new Rows
        for(i <- paths.indices) {
            val path = paths(i)
            val v    = path(path.length - 1)
            for(u <- ch(v)) {
                if (schema(u) == lab) {
                    val path2 = if (keep) path.clone() else new Path
                    path2 += u
                    rows  += path2
                } // if
            } // for
        } // for
        rows
    } // expandBySchema

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand edges, expands the paths and return the name of edges.
     *  @param paths  the paths to be expanded
     */
    def expandEdges (paths: Rows): Rows_lab =
    {
        val edges = ArrayBuffer [ArrayBuffer [String]] ()
        for (i <- paths.indices) {
            val path = paths(i)
            val v    = path(path.length - 1)
            for (u <- ch(v)) {
                for (e <- elabel(v, u)) { val p = ArrayBuffer [String] (); p += e.toString; edges += p }
            } // for
        } // for
        edges
    } // expandEdges

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exapand back all, expands the edges of given paths in backward direction, one at a time.
     *  @param paths  the paths(set of edges)
     *  @param keep   whether the given path has to be inluded along with the expanded vertices
     */
    def expandBackAll (paths: Rows, keep: Boolean = false): Rows =
    {
        val rows = new Rows
        for (i <- paths.indices) {
            val path = paths(i)
            val v    = path(0)
            for (u <- pa(v)) {
                val path2 = if (keep) path.clone() else new Path
                u    +=: path2
                rows +=  path2
            } // for
        } // for
        rows
    } // expandBackAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exapand back, expands(of given edge label) the edges in backward direction, one at a time.
     *  @param paths  the paths(set of edges) that are already explored
     *  @param elab   the edge label of the edges to be expanded
     *  @param keep   whether the given path has to be inluded along with the expanded vertices
     */
    def expandBack (paths: Rows, elab: TLabel, keep: Boolean = false): Rows =
    {
        val rows = new Rows
        for (i <- paths.indices) {
            val path = paths(i)
            val v    = path(0)
            for (u <- pa(v)) {
                if (elabel((u, v)) contains elab) {
                    val path2 = if (keep) path.clone() else new Path
                    u    +=: path2
                    rows +=  path2
                } // if
            } // for
        } // for
        rows
    } // expandBack

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand back by type, expands the given paths by adding one backward edge
     *  (of given type lab) to each path.
     *  @param paths  the paths to be expanded
     *  @param keep   whether the given path has to be inluded along with the expanded vertices
     */
    def expandBackBySchema (paths: Rows, lab: String, keep: Boolean  = false): Rows =
    {
        val rows = new Rows
        for (i <- paths.indices) {
            val path = paths(i)
            val v    = path(0)
            for(u <- pa(v)) {
                if(schema(u) == lab) {
                    val path2 = if (keep) path.clone() else new Path
                    u    +=: path2
                    rows += path2
                } // if
            } // for
        } // for
        rows
    } // expandBackBySchema

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expand into, checks if there is an edge from the last node in the
     *  given path to the first node in the same path (leading to a triangle).
     *  @param paths  the vertices connected by a path
     *  @param elab   the edge label to be checked from the end node in the path to the start node
     */
    def expandInto (paths: Rows, elab: TLabel): Rows =
    {
        val newvsn = new Rows
        for (path <- paths) {
            if (elabel contains (path(path.length - 1), path(0))) {
                if (elabel(path(path.length - 1), path(0)) contains elab) newvsn += path
            } // if
        } // for
        newvsn
    } // expandInto

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union, performs the union of two graphs including both vertices and edges
     *  @param g2  the graph that has to be unioned with 'this' graph
     */
    def union (g2: MuGraph [TLabel]): MuGraph [TLabel] =
    {
        val lab    = new ReArray [TLabel] ()
        val renumb = Map [Int, Int] ()          // map from old index to new index of vertex
        val new_id = new ReArray [Int] ()
        for (i <- label.indices) {
            lab(i)     = label(i)               // clone the vertex labels from first graph
            new_id(i) = id(i)                   // clone the id's from first graph
        } //for

        var n = lab.size
        for (j <- g2.id.indices) {              // to check the vertices of g2 that are not contained in g1
        val id_j  = g2.id(j)
            val i = id.indexOf(id_j)
            if (i < 0 || label(i) != g2.label(j)) {
                lab(n)     = g2.label(j)
                renumb   += j -> n
                new_id(n) = if(! new_id.contains(g2.id(j))) id(j)
                else new_id.max + 1
                n +=1
            } //if
        } //for
        val le = induceEdges (renumb, g2)       // induce edges for the vertices selected
        MuGraph (lab.toArray, le, new_id.toArray, inverse, schema)
    } //union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Induce edges, edges corresponding to 'this' graph and 'g2' graph are induced.
     *  @param renumb  the map from new index -> old index
     *  @param g2      the graph g2
     */
    def induceEdges (renumb: Map [Int, Int], g2: MuGraph [TLabel]): Map [Pair, SET [TLabel]] =
    {
        val le = elabel.clone()
        for ((x, y) <- g2.elabel) {           //To check the edges of g2 that are not contained in g1
            val z = renumber (x, renumb)
            le   += z -> (le.getOrElse (z, SET [TLabel]()) ++ g2.elabel(x))
        } //for
        le
    } // induceEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Union, the result rows are combined.
     *  @param x  rows produced as result from the first operation
     *  @param y  rows produced as result from the second operation
     */
    def union (x: Rows_lab, y: Rows_lab): Rows_lab =
    {
        var z = x
        for (i <- y.indices if ! (z contains(y(i)))) z += y(i)    // eliminates duplicates
        z
    } // union

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Intersect, performs the interscetion of two graphs including vertices and edges.
     *  @param g2  the graph that has to be intersect with the current graph
     */
    def intersect (g2: MuGraph [TLabel]): MuGraph [TLabel] =
    {
        val lab       = new ReArray [TLabel]()
        val vset      = new Path
        val new_id    = new ReArray[Int]()
        val new_index = new ReArray[Int]()
        var n      = 0
        for(i <- id.indices)
        {
            if(same(i, this, g2))
            {
                new_index(n) = i
                new_id(n)    = this.id(i)
                vset        += i
                lab(n)       = label(i)
                n           +=1
            } // if
        } // for
        val le = induceEdges (vset, new_index, g2)             //induce edges for the vertices selected
        MuGraph (lab.toArray, le, new_id.toArray, inverse, schema)
    } // intersect

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Induce edges, of the vertices 'vset' that are conatined in 'this' graph and graph g2.
     *  @param vset       the vertex set
     *  @param new_index  contains the reference to old indexes
     *  @param g2         graph g2
     */
    def induceEdges (vset: Path, new_index: ReArray [Int], g2: MuGraph [TLabel]): Map [Pair, SET [TLabel]] =
    {
        val le = Map [Pair, SET [TLabel]] ()
        for ((x,l) <- g2.elabel) {
            if ((vset contains x._1) && (vset contains x._2)) {
                if (elabel.contains (x._1, x._2)) {
                    if (elabel(x._1,x._2) == g2.elabel(x._1,x._2))
                        le += ((new_index.indexOf (x._1),new_index.indexOf (x._2)) -> elabel(x._1,x._2))
                } // if
            } // if
        } // for
        le
    } // induceEdges

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform the minus operation of two graphs.
     *  @param g2  the subgraph to be excluded from this graph
     */
    def minus (g2: MuGraph [TLabel]): MuGraph [TLabel] =
    {
        var n         = 0
        val vset      = new Path
        val lab       = new ReArray [TLabel]()
        val new_id    = new ReArray [Int]()
        val new_schema= new ReArray [String]()
        val old_index = new ReArray[Int]()
        for(i <- id.indices) {
            if (! same (i, this, g2)) {                    // check if the vertex i is same in two graphs
                old_index(n)  = i
                new_id(n)     = this.id(i)
                vset         += i
                lab(n)        = label(i)
                new_schema(n) = schema(i)
                n            += 1
            } // if
        } // for
        val le = induceEdges (vset, old_index)             // induce the edges for the vertices selected
        MuGraph (lab.toArray, le, new_id.toArray, inverse, new_schema.toArray)
    } // minus

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Same, checks if a vertex is same in two graphs.
     *  @param i   index of the vertex
     *  @param g1  first graph
     *  @param g2  second graph
     */
    def same (i: Int, g1: MuGraph [TLabel], g2:MuGraph [TLabel]): Boolean =
    {
        val id_i = g1.id(i)
        val j    = g2.id.indexOf(id_i)
        j >= 0 && g1.label(i) == g2.label(i)
    } // same

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Renumber, check if the vertices are renumbered.
      * @param x       pair of vertices
      * @param renumb  a map containing the map from old to new vertex
      */
    def renumber(x: Pair, renumb: Map [Int, Int]): Pair =
    {
        val start = if (renumb contains x._1) renumb(x._1) else x._1
        val end   = if (renumb contains x._2) renumb(x._2) else x._2
        (start, end)
    } // renumber

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The getLabels returns the labels of the given vertices (indices).
      * @param rows  the indices of vertices in the form of rows
      */
    def getLabels (rows: Rows): Rows_lab =
    {
        rows.map (_.map (label (_).toString))
    } // getLabels

} // MuGraphAlgebra


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MuGraphAlgebra` companion object provides builder methods and
  * the setup of Neo4j for performing queries through Neo4j API calls.
  */
object MuGraphAlgebra
{
    val user_name  = "neo4j"
    val password   = "Thesis@2017"
    val connection = "bolt://localhost:7687"
    var neo: Neo4j = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build 'MuGraphAlgebra' from 'MuGraph'.
     *  @param g MuGraph to build 'MuGraphAlgebra'
     */
    def apply [TLabel: ClassTag] (g: MuGraph [TLabel]): MuGraphAlgebra [TLabel] =
    {
        new MuGraphAlgebra(g.ch,g.label,g.elabel,g.inverse,g.name,g.id,g.schema)
    } // apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Setup the connection with Neo4j server.
     *  @param createStmt create statement for creating the graph in Neo4j
     *  @param username   the username to connect with the Neo4j
     *  @param pwd        the password to connect with the Neo4j
     *  @param uri        the uri to connect with the Neo4j
     */
    def testSetup (createStmt: String, username: String = user_name, pwd: String = password, uri: String = connection) =
    {
        neo = new Neo4j (connection, username, pwd)             // establish connection to the Neo4j server
        neo.deleteAll            
        neo.createGraph (createStmt)
    } // testSetup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'test' method tests the correctness of scalation result to the queries,
     *  against the Neo4j results.
     *  @param query       the scalation query
     *  @param queryNeo4j  the Neo4j query
     *  @param print       whether to print the result or not
     */
    def test (query: => Rows_lab, queryNeo4j: String, print: Boolean = false): Unit =
    {
        val result = query
        time { for(i <- 0 until 5) query }

        val neoResult = neo.runQuery (queryNeo4j)
        time { for (j <- 0 until 5) neo.runQuery (queryNeo4j)}
        if (print) {
            println (s"result = $result")
            println (s"neoResult = $neoResult")
        } // if
        assert (same (result, neoResult))
    } // test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The same method check if the query results are the same.
     *  @param x  the first parameter
     *  @param y  the second parameter
     */
    def same (x: ArrayBuffer [ArrayBuffer [String]], y: ArrayBuffer [ArrayBuffer [String]]): Boolean =
    {
        for(i <- x.indices if ! (y contains x(i))) return false
        for(i <- y.indices if ! (x contains y(i))) return false
        true
    } // same

    val lv = Array ("Oliver Stone", "Michael Douglas", "Charlie Sheen", "Martin Sheen", "Rob Reiner", "Wall Street", "The American President")
    val le = Map ((0, 5) -> ν("directed"),
                  (1, 5) -> ν("acted"),
                  (1, 6) -> ν("acted"),
                  (2, 5) -> ν("acted"),
                  (3, 5) -> ν("acted"),
                  (3, 6) -> ν("acted"),
                  (4, 6) -> ν("directed"))
    val schema = Array ("Person", "Person", "Person", "Person", "Person", "Movie", "Movie")
    val dg     = MuGraph [String] (lv, le, null, true, schema)
    val ga     = MuGraphAlgebra (dg)

} // MuGraphAlgebraa companion object

