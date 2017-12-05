
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.4
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graph_db.{ExampleMGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `StrictPattern_DTest` object is used to test all the Strict Simulation pattern
 *  matchers for labels of type `Double`.
 *  > run-main scalation.graph_db.pattern_matching.StrictPattern_DTest
 */
object StrictPattern_DTest extends App
{
    val g2 = EX_GRAPH.g2p
    val q2 = EX_GRAPH.q2p

    val g3 = EX_GRAPH.g3p
    val q3 = EX_GRAPH.q3p

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q2.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g2.printG ()

    // Strict Graph Simulation Pattern Matcher

    (new StrictSim (g2, q2, new DualSim (g2, q2))).test ("StrictSim", Answers_g2.phi9)                  // Strict Simulation 
    (new StrictSim (g2, q2, new DualSim2 (g2, q2))).test ("StrictSim2", Answers_g2.phi9)                // Strict Simulation 2 
    (new StrictSim (g2, q2, new DualSimCAR (g2, q2))).test ("StrictSimCAR", Answers_g2.phi10)           // Strict Simulation CAR

    // Strict MGraph Simulation Pattern Matcher

    (new MStrictSim (g2, q2, new MDualSim (g2, q2))).test ("MStrictSim", Answers_g2.phi9)               // MStrict Simulation 
    (new MStrictSim (g2, q2, new MDualSim2 (g2, q2))).test ("MStrictSim2", Answers_g2.phi9)             // MStrict Simulation 2
    (new MStrictSim (g2, q2, new MDualSimCAR (g2, q2))).test ("MStrictSimCAR", Answers_g2.phi10)        // MStrict Simulation CAR

    // MStrict Simulation Pattern Matcher- checks mismatched edge labels

    (new MStrictSim (g3, q3, new MDualSim (g3, q3))).test ("MStrictSim", Answers_g3.phi11)              // MStrict Simulation         
    (new MStrictSim (g3, q3, new MDualSim2 (g3, q3))).test ("MStrictSim2", Answers_g3.phi11)            // MStrict Simulation 2       
    (new MStrictSim (g3, q3, new MDualSimCAR (g3, q3))).test ("MStrictSimCAR", Answers_g3.phi12)        // MStrict Simulation CAR
 
} // StrictPattern_DTest object

