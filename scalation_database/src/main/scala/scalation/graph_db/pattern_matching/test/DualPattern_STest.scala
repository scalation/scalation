
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graphalytics.{DualSim => ImDualSim, DualSim2 => ImDualSim2}
import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualPattern_STest` object is used to test all the Dual Simulation pattern
 *  matchers for labels of type `String`.
 *  > runMain scalation.graph_db.pattern_matching.test.DualPattern_STest
 */
object DualPattern_STest extends App
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

    // Dual Simulation Pattern Matcher

    (new DualSim (g2, q2)).test ("DualSim", Answers_g2.phi5)            // Dual Simulation
    (new DualSim2 (g2, q2)).test ("DualSim2", Answers_g2.phi5)          // Dual Simulation 2
    (new DualSimCAR (g2, q2)).test ("DualSimCAR", Answers_g2.phi6)      // Dual Simulation CAR

    // MDual Simulation Pattern Matcher

    (new MDualSim (g2, q2)).test ("MDualSim", Answers_g2.phi5)          // MDual Simulation
    (new MDualSimW (g2, q2)).test ("MDualSimW", Answers_g2.phi5)        // MDual Simulation W on q2
    (new MDualSimW (g4, q4w)).test ("MDualSimW", Answers_g2.phi5)       // MDual Simulation W on q4 Wildcards on EdgeLabels
    (new MDualSimX (g4, q4x)).test ("MDualSimX", Answers_g2.phi5)       // MDual Simulation X on q5 Regex on EdgeLabels
    (new MDualSim2 (g2, q2)).test ("MDualSim2", Answers_g2.phi5)        // MDual Simulation 2
    (new MDualSim2W (g2, q2)).test ("MDualSim2W", Answers_g2.phi5)      // MDual Simulation 2 W on q2
    (new MDualSim2W (g4, q4w)).test ("MDualSim2W", Answers_g2.phi5)     // MDual Simulation 2 W on q4 Wildcards on EdgeLabels
    (new MDualSim2X (g4, q4x)).test ("MDualSim2X", Answers_g2.phi5)     // MDual Simulation 2 X on q5 Regex on EdgeLabels
    (new MDualSimCAR (g2, q2)).test ("MDualSimCAR", Answers_g2.phi6)    // MDual Simulation CAR

    // MDual Simulation Pattern Matcher - checks mismatched edge labels

    (new MDualSim (g3, q3)).test ("MDualSim", Answers_g3.phi7)          // MDual Simulation
    (new MDualSimW (g3, q3)).test ("MDualSimW", Answers_g3.phi7)        // MDual Simulation W on q3
    (new MDualSimX (g3, q3)).test ("MDualSimX", Answers_g3.phi7)        // MDual Simulation Rx on q5 Regex
    (new MDualSim2 (g3, q3)).test ("MDualSim2", Answers_g3.phi7)        // MDual Simulation 2
    (new MDualSim2W (g3, q3)).test ("MDualSim2W", Answers_g3.phi7)      // MDual Simulation 2 W on q3
    (new MDualSim2X (g3, q3)).test ("MDualSim2X", Answers_g3.phi7)      // MDual Simulation 2 Rx on q5 Regex
    (new MDualSimCAR (g3, q3)).test ("MDualSimCAR", Answers_g3.phi8)    // MDual Simulation CAR

} // DualPattern_STest object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualPattern_STest2` object is used to test all the Dual Simulation pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.DualPattern_STest2
 */
object DualPattern_STest2 extends App
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

            // Dual Simulation Pattern Matcher

            (new DualSim (g, q)).test ("DualSim")                           // Dual Simulation
            (new DualSim2 (g, q)).test ("DualSim2")                         // Dual Simulation 2
            (new DualSimCAR (g, q)).test ("DualSimCAR")                     // Dual Simulation CAR
        } // for
    } // for

} // DualPattern_STest2 object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualPattern_STest3` object is used to test all the Dual Simulation pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.DualPattern_STest3
 */
object DualPattern_STest3 extends App
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

            // Immutable Dual Simulation Pattern Matcher

            (new ImDualSim (gSIM, qSIM)).test ("DualSim")                   // Dual Simulation
            (new ImDualSim2 (gSIM, qSIM)).test ("DualSim2")                 // Dual Simulation 2
        } // for
    } // for

} // DualPattern_STest3 object

