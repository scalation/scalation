//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Supriya Ramireddy
 *  @version 1.4
 *  @date    Sun Oct 1 12:12:06 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db.graph_algebra

import MuGraphAlgebra.{test, testSetup}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RelationshipQuery` object used to test the correctness of the results for
 *  relationship queries on a small graph, against the results produced by Neo4j
 *  > run-main scalation.graph_db.graph_algebra.RelationshipQuery.
 */
object RelationshipQuery extends App
{
    val g1 = MuGraphAlgebra.ga
    g1.printG ()

    testSetup ("""CREATE (a:Person {name: "Oliver Stone"}),
                         (b:Person {name: "Michael Douglas"}),
                         (c:Person {name: "Charlie Sheen"}),
                         (d:Person {name: "Martin Sheen"}),
                         (e:Person {name: "Rob Reiner"}),
                         (f:Movie {name: "Wall Street"}),
                         (g:Movie {name: "The American President"}),
                         (a) - [:directed] -> (f),
                         (b) - [:acted]    -> (f),
                         (b) - [:acted]    -> (g),
                         (c) - [:acted]    -> (f),
                         (d) - [:acted]    -> (f),
                         (d) - [:acted]    -> (g),
                         (e) - [:directed] -> (g)""")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'θ' is the comparison operator, that checks if two given labels are same
     *  @param x the first parameter
     *  @param y the second parameter
     */
    def θ [TLabel] (x: TLabel, y: TLabel): Boolean = y.toString == x.toString

    val neo4jQuery5 = "MATCH (:Person { name: 'Oliver Stone' })-->(movie) RETURN movie.name"
    test (g1.getLabels (g1.expandAll (g1.selectByVLabel (θ, "Oliver Stone")(g1.vertexSet))), neo4jQuery5)
    println ("=================================")

    val neo4jQuery6 = "MATCH (:Person { name: 'Oliver Stone' })-[r]->(movie) RETURN type(r)"
    test (g1.expandEdges (g1.selectByVLabel (θ, "Oliver Stone")(g1.vertexSet)), neo4jQuery6)
    println ("=================================")

    val neo4jQuery7 = "MATCH (:Movie { name: 'Wall Street' })<-[:acted]-(actor) RETURN actor.name"
    test  (g1.getLabels(g1.expandBack (g1.selectByVLabel (θ, "Wall Street")(g1.vertexSet), "acted")), neo4jQuery7)
    println ("=================================")

    val neo4jQuery8 = "MATCH (wallstreet { name: 'Wall Street' })<-[:acted|:directed]-(person) RETURN person.name"
    test (g1.union (g1.getLabels (g1.expandBack (g1.selectByVLabel (θ, "Wall Street")(g1.vertexSet), "acted")),
                    g1.getLabels (g1.expandBack (g1.selectByVLabel (θ, "Wall Street")(g1.vertexSet), "directed"))),
        neo4jQuery8)

} // RelationshipQuery object

