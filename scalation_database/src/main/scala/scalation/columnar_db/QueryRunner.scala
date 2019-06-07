
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Santosh Uttam Bobade, John Miller
 *  @version 1.6
 *  @date    Sun Aug 23 15:42:06 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  This provides a SQL-like BASH prompt for querying database.
 */
package scalation.columnar_db

import java.util.Scanner

import scalation.util.gauge

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QueryRunner` object provides a command prompt for querying a database.
 *  Commands and their usage:
 *  1.  'prefix package'     - set the package prefix for convenience, e.g., scalation.columnar.db
 *  2.  'create createTable' - execute the create table(s) statement, e.g., CreateTable1
 *  3.  'load table'         - load the table
 *  4.  'save table'         - save the table
 *  5.  'query queryStat'    - execute the compiled query statement, e.g., Query1
 *  6.  'test queryStat'     - perfoamnce test the complier query statement
 *  7.  'update updateStmt'  - execute the compiled update statement, e.g., Update1
 *  8.  'show table'         - show the contents of the table
 *  9.  'help'               - list the commands
 *  10. 'quit'               - quit the current QueryRunner session
 *
 *  > runMain scalation.columnar_db.QueryRunner
 */
object QueryRunner extends App
{
    var scanner = new Scanner (System.in)
    var running = true
    var prefix  = ""

    while (running) {
        println ("\n Enter Command : ")
        var input = scanner.nextLine ()
        val token = input.split (" ")

        token(0) match {

        case "prefix" => prefix = token(1) + "."

        case "create" => try getClass.getClassLoader.loadClass (prefix + token(1) + "$")
                                     .getField ("MODULE$").get (null)
                         catch {
                             case ex: Exception => println (s"Object ${prefix + token(1)} does not exist.")
                         } // catch

        case "load"   => val rel = Catalog.getRelation (token(1))
                         if (rel == null) {
                             val rel = RelationSQL (token(1))
                             if (rel != null) { val repr = rel.repr; Catalog.add (repr.name, repr.colName, repr.key, repr.domain, rel) }
                             else println (s" Relation ${token(1)} does not exist.")
                         } // if

        case "save"   => Catalog.getRelation (token(1)).asInstanceOf [RelationSQL].save ()

        case "query"  => var query: Query = null
                         val t1 = gauge { query = loadStatement (prefix + token(1)) }
                         val t2 = gauge { query.doQuery () }
                         println (s"\n class load time = $t1 ms, query time $t2 ms")

        case "test"   => var query: Query =  null
                         val t1 = gauge { query = loadStatement (prefix + token(1)) }
                         var t2 = 0.0
                         val iter = 10
                         query.doQuery ()
                         for (i <- 0 until iter) t2 += gauge { query.doQuery () }
                         println (s"\n class load time = $t1 ms, avg query time ${t2 / iter} ms")

        case "update" => var update: Query = null
                         val t1 = gauge { update = loadStatement (prefix + token(1)) }
                         val t2 = gauge { update.doUpdate () }
                         println (s"\n class load time = $t1 ms, update time $t2 ms")

        case "show"   => if (token.length > 1) {
                             if (token(1) equalsIgnoreCase "tables") println (Catalog.names)
                             else Catalog.getRelation (token(1)).asInstanceOf [RelationSQL].show ()
                                  // can have exception handling similar to case load
                         } // if

        case "help"   => help

        case "quit"   => running = false

        case _        => println ("unrecognized command: try again or type 'quit' to quit")
        } // match
    } // while

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load the compiled statement (query or update).
     *  @param name  the name of the compile query/update
     */
    def loadStatement (name: String): Query =
    {
        getClass.getClassLoader.loadClass (name + "$").getField ("MODULE$").get (null).asInstanceOf [Query]
    } // loadStatement

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the `QueryRunner` commands.
     */
    def help ()
    { 
        println ("""
Commands and their usage:
1.  'prefix package'     - set the package prefix for convenience, e.g., scalation.columnar.db
2.  'create createTable' - execute the create table(s) statement, e.g., CreateTable1
3.  'load table'         - load the table
4.  'save table'         - save the table
5.  'query queryStat'    - execute the compiled query statement, e.g., Query1
6.  'test queryStat'     - perfoamnce test the complier query statement
7.  'update updateStmt'  - execute the compiled update statement, e.g., Update1
8.  'show table'         - show the contents of the table
9.  'help'               - list the commands
10. 'quit'               - quit the current QueryRunner session
        """)
    } // help

} // QueryRunner object

