
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graphalytics.{StrictSim => ImStrictSim, StrictSim2 => ImStrictSim2}
import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrictPattern_STest` object is used to test all the Strict Simulation pattern
 *  matchers for labels of type `String`.
 *  > runMain scalation.graph_db.pattern_matching.test.StrictPattern_STest
 */
object StrictPattern_STest extends App
{
    val g2  = EX_GRAPH.g2p
    val q2  = EX_GRAPH.q2p

    val g3  = EX_GRAPH.g3p
    val q3  = EX_GRAPH.q3p

    val g4  = EX_GRAPH.g4p                                             // query graph
    val q4w = EX_GRAPH.q4wp                                            // query graph with wildcards
    val q4x = EX_GRAPH.q4xp                                            // query graph with regex

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q4w.printG ()
    q4x.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g4.printG ()

    // Strict Graph Simulation Pattern Matcher

    (new StrictSim (g2, q2, new DualSim (g2, q2))).test ("StrictSim", Answers_g2.phi9)                  // Strict Simulation
    (new StrictSim (g2, q2, new DualSim2 (g2, q2))).test ("StrictSim2", Answers_g2.phi9)                // Strict Simulation 2
    (new StrictSim (g2, q2, new DualSimCAR (g2, q2))).test ("StrictSimCAR", Answers_g2.phi10)           // Strict Simulation CAR

    // Strict MGraph Simulation Pattern Matcher

    (new MStrictSim (g2, q2, new MDualSim (g2, q2))).test ("MStrictSim", Answers_g2.phi9)               // MStrict Simulation
    (new MStrictSim (g2, q2, new MDualSimW (g2, q2))).test ("MStrictSimW", Answers_g2.phi9)             // MStrict Simulation W on q2
    (new MStrictSim (g4, q4w, new MDualSimW (g4, q4w))).test ("MStrictSimW", Answers_g2.phi9)           // MStrict Simulation W on q4
    (new MStrictSim (g4, q4x, new MDualSimX (g4, q4x))).test ("MStrictSimX", Answers_g2.phi9)           // MStrict Simulation X on q5
    (new MStrictSim (g2, q2, new MDualSim2 (g2, q2))).test ("MStrictSim2", Answers_g2.phi9)             // MStrict Simulation 2
    (new MStrictSim (g2, q2, new MDualSim2W (g2, q2))).test ("MStrictSim2W", Answers_g2.phi9)           // MStrict Simulation 2 W on q2
    (new MStrictSim (g4, q4w, new MDualSim2W (g4, q4w))).test ("MStrictSim2W", Answers_g2.phi9)         // MStrict Simulation 2 W on q4
    (new MStrictSim (g4, q4x, new MDualSim2X (g4, q4x))).test ("MStrictSim2X", Answers_g2.phi9)         // MStrict Simulation 2 X on q5
    (new MStrictSim (g2, q2, new MDualSimCAR (g2, q2))).test ("MStrictSimCAR", Answers_g2.phi10)        // MStrict Simulation CAR

    // MStrict Simulation Pattern Matcher- checks mismatched edge labels

    (new MStrictSim (g3, q3, new MDualSim (g3, q3))).test ("MStrictSim", Answers_g3.phi11)              // MStrict Simulation
    (new MStrictSim (g3, q3, new MDualSimW (g3, q3))).test ("MStrictSimW", Answers_g3.phi11)            // MStrict Simulation W on q3
    (new MStrictSim (g3, q3, new MDualSimX (g3, q3))).test ("MStrictSimX", Answers_g3.phi11)            // MStrict Simulation X on q5
    (new MStrictSim (g3, q3, new MDualSim2 (g3, q3))).test ("MStrictSim2", Answers_g3.phi11)            // MStrict Simulation 2
    (new MStrictSim (g3, q3, new MDualSim2W (g3, q3))).test ("MStrictSim2W", Answers_g3.phi11)          // MStrict Simulation 2 W on q3
    (new MStrictSim (g3, q3, new MDualSim2X (g3, q3))).test ("MStrictSim2X", Answers_g3.phi11)          // MStrict Simulation 2 X on q5
    (new MStrictSim (g3, q3, new MDualSimCAR (g3, q3))).test ("MStrictSimCAR", Answers_g3.phi12)        // MStrict Simulation CAR
 
} // StrictPattern_STest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrictPattern_STest2` object is used to test all the Strict Simulation pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.StrictPattern_STest2
 */
object StrictPattern_STest2 extends App
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

            // Strict Simulation Pattern Matcher

            (new StrictSim (g, q, new DualSim (g, q))).test ("StrictSim")          // Strict Simulation
            (new StrictSim (g, q, new DualSim2 (g, q))).test ("StrictSim2")        // Strict Simulation 2
            (new StrictSim (g, q, new DualSimCAR (g, q))).test ("StrictSimCAR")    // Strict Simulation CAR
        } // for
    } // for

} // StrictPattern_STest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrictPattern_STest3` object is used to test all the Strict Simulation pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.StrictPattern_STest3
 */
object StrictPattern_STest3 extends App
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

            // Immutable Strict Simulation Pattern Matcher

            (new ImStrictSim (gSIM, qSIM)).test ("StrictSim")                   // Strict Simulation
            (new ImStrictSim2 (gSIM, qSIM)).test ("StrictSim2")                 // Strict Simulation 2
        } // for
    } // for

} // StrictPattern_STest3 object

