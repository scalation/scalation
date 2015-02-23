
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Matthew Saltz, John Miller
 *  @version 1.1
 *  @date    Thu Jul 25 11:28:31 EDT 2013
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

// import collection.mutable.Set

import scalation.util.Timer.time

import GraphTypes._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PatternMatcherTest` object creates data graph and query graph that
 *  can be used to test the results produced by the pattern matching algorithms.
 *  @see MS Thesis, "A Comparison of Techniques for Graph Analytics on Big Data"
 */
object TestGraphContainer
{
    // Data Graph ------------------------------------------------------------

    /** The data graph adjacency sets, one for each vertex
     */
    val testGraphAdj = Array (Set (1, 2),
                              Set (2),
                              Set (7),
                              Set (7),
                              Set (7),
                              Set (6, 7),
                              Set (5),
                              Set [Int] (),
                              Set (11),
                              Set (7, 8),
                              Set (9),
                              Set (7, 10, 12),
                              Set [Int] (),
                              Set (10),
                              Set (7, 15),
                              Set (16),
                              Set (17, 20),
                              Set (18),
                              Set (19, 22),
                              Set (14),
                              Set [Int] (),
                              Set (20),
                              Set [Int] (),
                              Set (22))

    /** The vertex labels, one for each vertex in the data graph
     */
    val testGraphLabels = Array (0, 1, 2, 0, 0, 3, 4, 2, 4, 3, 4, 3, 0, 2, 3, 4, 3, 4, 3, 4, 2, 0, 2, 0)

    /** The test data graph
     */
    val testGraph = Graph (testGraphAdj, testGraphLabels)

    //------------------------------------------------------------------------
    // Query Graph
    //------------------------------------------------------------------------

    /** The query graph adjacency sets, one for each vertex
     */
    val testQueryAdj = Array (Set (3),           // PM
                              Set (3, 2),        // DB
                              Set (1),           // AI
                              Set [Int] ())      // SA

    /** The vertex labels, one for each vertex in the query graph
     */
    val testQueryLabels = Array (0, 3, 4, 2)

    /** The test query graph
     */
    val testQuery = Graph (testQueryAdj, testQueryLabels)

    //------------------------------------------------------------------------
    // Specified correct reults for Simple Graph Simulation, Dual Graph Simulation
    // and Subgraph Isomorphism
    //------------------------------------------------------------------------

    val correctSimpleResult = Array (Set (0, 3, 4, 21, 23), 
                                     Set (5, 9, 11, 14, 16, 18), 
                                     Set (6, 8, 10, 15, 17, 19),
                                     Set (2, 7, 13, 20, 22))
    val correctDualResult   = Array (Set (3, 4, 21, 23), 
                                     Set (5, 9, 11, 14, 16, 18), 
                                     Set (6, 8, 10, 15, 17, 19),
                                     Set (7, 20, 22))
    val correctIsoResult    = Set (Array (3, 5, 6, 7),
                                   Array (4, 5, 6, 7))
  
} // TestGraphContainer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The 'PatternMatcherTest' object runs six pattern matching algorithms on
 *  the above test graph.  The algorithms tested are the following:
 *  GraphSim   - Simple Graph Simulation
 *  GraphSim2  - Simple Graph Simulation (with early termination)
 *  DualSim    - Dual Graph Simulation
 *  DualSim2   - Dual Graph Simulation (with reduced memory footprint) 
 *  UllmannIso - Adjacency List Version of Ullmann's Subgraph Isomorphism Algorithm
 *  DualIso    - Subgraph Isomorphism using Dual Simulation for Pruning
 */
object PatternMatcherTest extends App 
{
    import TestGraphContainer._

    val g = testGraph          // test query graph
    val q = testQuery          // test data graph

    def testBijections (matcher: PatternMatcher, name: String, answer: Set [Array [Int]])
    {
        println ("-------------------------------------------------------------------")
        println (name + " Bijections: ")
        val psi = time { matcher.bijections () }
        psi.foreach { b => println (b.mkString (", " )) }
        print (if (psi.map (_.deep) == answer.map (_.deep)) "Success: " else "Failure: ")
        println (name)
    } // test

    testBijections (new GraphSimIso (g, q), "GraphSimIso", correctIsoResult)
    testBijections (new DualIso (g, q),     "DualIso",     correctIsoResult)

    def testMappings (matcher: PatternMatcher, name: String, answer: Array [ISet])
    {
        println ("-------------------------------------------------------------------")
        println (name + " Mappings: ")
        val phi = time { matcher.mappings () }
        for (i <- phi.indices) println (i + " ->  " + phi(i))
        print (if (phi.deep == answer.deep) "Success: " else "Failure: ")
        println (name)
    } // test

    testMappings (new GraphSim2 (g, q), "GraphSim2", correctSimpleResult)
    testMappings (new GraphSim (g, q),  "GraphSim",  correctSimpleResult)
    testMappings (new DualSim2 (g, q),  "DualSim2",  correctDualResult)

    // DualSim requires inverse adjacency ('par') for access to parent vertices
    g.addPar (); q.addPar ()
    testMappings (new DualSim (g, q),   "DualSim",   correctDualResult)

} // PatternMatcherTest

