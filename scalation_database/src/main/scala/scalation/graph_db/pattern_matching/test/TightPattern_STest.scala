
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graphalytics.{TightSim => ImTightSim, TightSim2 => ImTightSim2}
import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightPattern_STest` object is used to test all the Tight Simulation pattern
 *  matchers for labels of type `String`.
 *  > runMain scalation.graph_db.pattern_matching.test.TightPattern_STest
 */
object TightPattern_STest extends App
{
    val g2  = EX_GRAPH.g2p
    val q2  = EX_GRAPH.q2p

    val g3  = EX_GRAPH.g3p
    val q3  = EX_GRAPH.q3p

    val g4  = EX_GRAPH.g4p                                              // query graph
    val q4w = EX_GRAPH.q4wp                                             // query graph with wildcards
    val q4x = EX_GRAPH.q4xp                                             // query graph with regex

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q4w.printG ()
    q4x.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g4.printG ()

    // Tight Graph Simulation Pattern Matcher

    (new TightSim (g2, q2, new DualSim (g2, q2))).test ("TightSim", Answers_g2.phi13)                  // Tight Simulation 
    (new TightSim (g2, q2, new DualSim2 (g2, q2))).test ("TightSim2", Answers_g2.phi13)                // Tight Simulation 2 
    (new TightSim (g2, q2, new DualSimCAR (g2, q2))).test ("TightSimCAR", Answers_g2.phi14)            // Tight Simulation CAR

    // Tight MGraph Simulation Pattern Matcher

    (new MTightSim (g2, q2, new MDualSim (g2, q2))).test ("MTightSim", Answers_g2.phi13)               // MTight Simulation 
    (new MTightSim (g2, q2, new MDualSimW (g2, q2))).test ("MTightSimW", Answers_g2.phi13)             // MTight Simulation W on q2
    (new MTightSim (g4, q4w, new MDualSimW (g4, q4w))).test ("MTightSimW", Answers_g2.phi13)           // MTight Simulation W on q4
    (new MTightSim (g4, q4x, new MDualSimX (g4, q4x))).test ("MTightSimX", Answers_g2.phi13)           // MTight Simulation X on q5
    (new MTightSim (g2, q2, new MDualSim2 (g2, q2))).test ("MTightSim2", Answers_g2.phi13)             // MTight Simulation 2
    (new MTightSim (g2, q2, new MDualSim2W (g2, q2))).test ("MTightSim2W", Answers_g2.phi13)           // MTight Simulation 2 W on q2
    (new MTightSim (g4, q4w, new MDualSim2W (g4, q4w))).test ("MTightSim2W", Answers_g2.phi13)         // MTight Simulation 2 W on q4
    (new MTightSim (g4, q4x, new MDualSim2X (g4, q4x))).test ("MTightSim2X", Answers_g2.phi13)         // MTight Simulation 2 X on q5
    (new MTightSim (g2, q2, new MDualSimCAR (g2, q2))).test ("MTightSimCAR", Answers_g2.phi14)         // MTight Simulation CAR

    // MTight Simulation Pattern Matcher- checks mismatched edge labels

    (new MTightSim (g3, q3, new MDualSim (g3, q3))).test ("MTightSim", Answers_g3.phi15)               // MTight Simulation         
    (new MTightSim (g3, q3, new MDualSimW (g3, q3))).test ("MTightSimW", Answers_g3.phi15)             // MTight Simulation W on q4  
    (new MTightSim (g3, q3, new MDualSimX (g3, q3))).test ("MTightSimX", Answers_g3.phi15)             // MTight Simulation X on q5  
    (new MTightSim (g3, q3, new MDualSim2 (g3, q3))).test ("MTightSim2", Answers_g3.phi15)             // MTight Simulation 2       
    (new MTightSim (g3, q3, new MDualSim2W (g3, q3))).test ("MTightSim2W", Answers_g3.phi15)           // MTight Simulation 2 W on q3
    (new MTightSim (g3, q3, new MDualSim2W (g3, q3))).test ("MTightSim2W", Answers_g3.phi15)           // MTight Simulation 2 W on q4
    (new MTightSim (g3, q3, new MDualSim2X (g3, q3))).test ("MTightSim2X", Answers_g3.phi15)           // MTight Simulation 2 X on q5
    (new MTightSim (g3, q3, new MDualSimCAR (g3, q3))).test ("MTightSimCAR", Answers_g3.phi16)         // MTight Simulation CAR
 
} // TightPattern_STest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightPattern_STest2` object is used to test all the Tight Simulation pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.TightPattern_STest2
 */
object TightPattern_STest2 extends App
{
    val UNIFORM = true                                                      // uniform vs. power law
    val gLabels = 10                                                        // number of labels in g
    val gDegree = 20                                                        // average out-degree for g
    val qSize   = 10                                                        // number of vertices in g
    val qDegree = 2                                                         // average out-degree for q
    val inverse = true                                                      // include links to parents

    for (gSize <- 1000 to 5000 by 1000) {                                   // number of vertices in g

//      val stream  = 11                                                    // random number stream 0 to 999
        for (stream <- 0 until 5) {
            val rg = new GraphGen ("0", stream)                             // random graph generator
            val g  = if (UNIFORM) rg.genRandomConnectedGraph (gSize, gLabels, gDegree)
                     else rg.genPowerLawGraph (gSize, gLabels, gDegree)     // data graph g
            banner ("data graph")
//          g.printG ()
            println (GraphMetrics.stats (g))

            val q  = rg.genBFSQuery (qSize, qDegree, g, inverse, "q")       // query graph q
            banner ("query graph")
            q.printG ()
            println (GraphMetrics.stats (q))

            // Tight Simulation Pattern Matcher

            (new TightSim (g, q, new DualSim (g, q))).test ("TightSim")          // Tight Simulation
            (new TightSim (g, q, new DualSim2 (g, q))).test ("TightSim2")        // Tight Simulation 2
            (new TightSim (g, q, new DualSimCAR (g, q))).test ("TightSimCAR")    // Tight Simulation CAR
        } // for
    } // for

} // TightPattern_STest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TightPattern_STest3` object is used to test all the Tight Simulation pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.TightPattern_STest3
 */
object TightPattern_STest3 extends App
{
    val UNIFORM = true                                                      // uniform vs. power law
    val gLabels = 10                                                        // number of labels in g
    val gDegree = 20                                                        // average out-degree for g
    val qSize   = 10                                                        // number of vertices in g
    val qDegree = 2                                                         // average out-degree for q
    val inverse = true                                                      // include links to parents

    for (gSize <- 1000 to 5000 by 1000) {                                   // number of vertices in g

//      val stream  = 11                                                    // random number stream 0 to 999
        for (stream <- 0 until 5) {
            val rg = new GraphGen ("0", stream)                             // random graph generator
            val g  = if (UNIFORM) rg.genRandomConnectedGraph (gSize, gLabels, gDegree)
                     else rg.genPowerLawGraph (gSize, gLabels, gDegree)     // data graph g
            banner ("data graph")
//          g.printG ()
            println (GraphMetrics.stats (g))

            val q  = rg.genBFSQuery (qSize, qDegree, g, inverse, "q")       // query graph q
            banner ("query graph")
            q.printG ()
            println (GraphMetrics.stats (q))

            val gSIM = g.toGraphIm ("gSIM")
            val qSIM = q.toGraphIm ("gSIM")

            // Immutable Tight Simulation Pattern Matcher

            (new ImTightSim (gSIM, qSIM)).test ("TightSim")                   // Tight Simulation
            (new ImTightSim2 (gSIM, qSIM)).test ("TightSim2")                 // Tight Simulation 2
        } // for
    } // for

} // TightPattern_STest3 object

