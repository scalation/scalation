//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Vinay Kumar Bingi, Supriya Ramireddy
  * @version 1.6
  * @date    Fri Oct  10 16:41:16 EDT 2017
  * @see     LICENSE (MIT style license file).
  */

package scalation.graph_db.graph_algebra

import org.neo4j.driver.v1._

import scala.collection.mutable.ArrayBuffer

import PathType.Rows_lab

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'Neo4j' class creates a connection to the Neo4j Database server.
 *  @param uri       the URI to connect with the Neo4j server
 *  @param user      the username to connect with the server
 *  @param password  the password to connect with the server
 */
class Neo4j (uri: String, user: String, password: String)
      extends AutoCloseable
{
    private val driver: Driver = GraphDatabase.driver (uri, AuthTokens.basic (user, password))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the connection with the Neo4j server.
     */
    override def close () { driver.close () }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The graph will be created in the Neo4j server.
     *  @param createStmt the cypher query to be excuted for creating the graph in Neo4j
     */
    def createGraph (createStmt: String)
    {
        val session = driver.session ()
        val greeting: String = session.writeTransaction (new TransactionWork [String] () {
            override def execute (tx: Transaction): String =
            {
                var result: StatementResult = tx.run (createStmt)
                "Graph created"
            } // execute
        }) // writeTransaction
    } // createGraph

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The query is run on the graph in Neo4j and the result is returned.
     *  @param query The query to be run on the graph in Neo4j database
     */
    def runQuery (query: String): Rows_lab =
    {
        val finalres = new Rows_lab
        val session  = driver.session ()
        val greeting: String = session.writeTransaction (new TransactionWork [String] () {
            override def execute(tx: Transaction): String =
            {
                val result: StatementResult = tx.run (query)
                while (result.hasNext) {
                    val res = ArrayBuffer [String] ()
                    val record: Record = result.next ()
                    val it = record.values().iterator ()
                    while (it.hasNext) res += it.next ().toString.replace ("\"","")
                    finalres += res
                } // while
                "exiting match"
            } // execute
        }) // writeTransaction
        finalres
    } // runQuery

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Delelte any existing graphs in Neo4j.
     */
    def deleteAll
    {
        val session = driver.session ()
        val greeting: String = session.writeTransaction (new TransactionWork [String] () {
            override def execute (tx: Transaction): String =
            {
                var result: StatementResult = tx.run ("match (n) DETACH delete n")
                "Nodes deleted"
            } // execute
        }) // writeTransaction
    } // deleteAll

} // Neo4j class

