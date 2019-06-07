
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Supriya Ramireddy
 *  @version 1.6
 *  @date    Sun Oct 15 19:12:16 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db.graph_algebra

import scalation.random.Randi0

import MuGraphAlgebra._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TestOperators` object tests the correctness of query results of scalation
 *  against Neo4j on randomly generated graphs.
 *  > runMain scalation.graph_db.graph_algebra.TestOperators
 */
object TestOperator extends App
{
    val randGraph   = new RandomGraph (100, 50, 99)                // create randomgraph
    val rLabel      = new Randi0 (99)                              // the first parameter should be one less than the no of vertices
    val vertexLabel = randGraph.lv (rLabel.igen)                   // randomly pick one of the vertex label
    val labelType   = randGraph.schema (randGraph.lv.indexOf (vertexLabel))  // get the type of the randomly picked vertex
    val edgeLabel   = randGraph.pickELabels (randGraph.genElabels)  // randomly pick one of the edge label

    val (g1, ng)    = randGraph.gen
    g1.printG ()
    println (ng)

    testSetup (ng)                                                  // create the graph in Neo4j

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'θ' is the comparison operator, that checks if two given labels are same
     *  @param x the first parameter
     *  @param y the second parameter
     */
    def θ [TLabel] (x: TLabel, y: TLabel): Boolean = y.toString == x.toString

    val neo4jQuery1 = "MATCH (n) RETURN n.name"
    test (g1.getLabels (g1.getVertices), neo4jQuery1)
    println ("=============================================")

    val neo4jQuery2 = "MATCH (movie:Movie) RETURN movie.name"
    test (g1.getLabels (g1.selectBySchema (θ, "Movie")(g1.vertexSet)), neo4jQuery2)
    println ("=============================================")

    val neo4jQuery3 = s"MATCH (Person { name: '${vertexLabel}' })--(movie) RETURN movie.name"
    MuGraphAlgebra.test(g1.union (g1.getLabels (g1.expandAll (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet))),
                                  g1.getLabels (g1.expandBackAll (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet)))),
        neo4jQuery3)
    println ("=============================================")

    val neo4jQuery4 = s"MATCH (:Person { name: '${vertexLabel}' })-->(movie:Movie) RETURN movie.name"
    MuGraphAlgebra.test (g1.getLabels (g1.expandBySchema (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet), "Movie")), neo4jQuery4)
    println ("=============================================")

    val neo4jQuery5 = s"MATCH (:Person { name: '${vertexLabel}' })-->(movie) RETURN movie.name"
    MuGraphAlgebra.test (g1.getLabels (g1.expandAll (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet))), neo4jQuery5)
    println ("=================================")

    val neo4jQuery6 = s"MATCH (:Person { name: '${vertexLabel}' })-[r]->(movie) RETURN type(r)"
    MuGraphAlgebra.test (g1.expandEdges (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet)), neo4jQuery6)
    println("=================================")

//  val neo4jQuery7 = s"MATCH (:Person { name: '$vertexLabel' })<-[:bgqu]-(actor) RETURN actor.name"
//  MuGraphAlgebra.test (g1.getLabels (g1.expandBack (g1.selectByVLabel (θ, "nzxwl")(g1.vertexSet), "bgqu")), neo4jQuery7)
//  println ("=================================")
//
//  val neo4jQuery8 = s"MATCH (wallstreet { name: '${vertexLabel}' })<-[:bgqu|:tpcqjc]-(person) RETURN person.name"
//  MuGraphAlgebra.test (g1.union(g1.getLabels (g1.expandBack (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet), "tpcqjc")),
//                         g1.getLabels (g1.expandBack (g1.selectByVLabel (θ, vertexLabel)(g1.vertexSet), "bgqu"))),
//                         neo4jQuery8)

} // TestOperator object

