
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Supriya Ramireddy
  * @version 1.6
  * @date    Sun Oct 1 12:12:06 EDT 2017
  * @see     LICENSE (MIT style license file).
  */

package scalation.graph_db.graph_algebra

import MuGraphAlgebra._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BasicQueryTest` object is used to test the correctness of the results
 *  for basic queries on a small graph, against the results produced by Neo4j.
 *  > scalation.graph_db.graph_algebra.BasicQueryTest
 */
object BasicQueryTest extends App
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
                         (e) - [:directed] -> (g)""")            // create statement for creating the graph in Neo4j

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The 'θ' is the comparison operator, that checks if two given labels are same
     *  @param x the first parameter
     *  @param y the second parameter
     */
    def θ [TLabel] (x: TLabel, y: TLabel): Boolean = y.toString == x.toString

    val neo4jQuery1 = "MATCH (n) RETURN n.name"
    test (g1.getLabels (g1.getVertices), neo4jQuery1)
    println ("=================================")

    val neo4jQuery2 =  "MATCH (movie:Movie) RETURN movie.name"
    test (g1.getLabels (g1.selectBySchema (θ, "Movie")(g1.vertexSet)), neo4jQuery2)
    println ("=================================")

    val neo4jQuery3 = "MATCH (Person { name: 'Oliver Stone' })--(movie) RETURN movie.name"
    test (g1.union (g1.getLabels (g1.expandAll (g1.selectByVLabel (θ, "Oliver Stone")(g1.vertexSet))),
                    g1.getLabels (g1.expandBackAll (g1.selectByVLabel (θ, "Oliver Stone")(g1.vertexSet)))),
          neo4jQuery3)
    println ("=================================")

    val neo4jQuery4 = "MATCH (:Person { name: 'Oliver Stone' })-->(movie:Movie) RETURN movie.name"
    test (g1.getLabels (g1.expandBySchema (g1.selectByVLabel (θ, "Oliver Stone")(g1.vertexSet), "Movie")), neo4jQuery4)
    println ("=================================")

} // BasicQueryTest object

