
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
/** The `TightPattern_DTest` object is used to test all the Tight Simulation pattern
 *  matchers for labels of type `Double`.
 *  > run-main scalation.graph_db.pattern_matching.test.TightPattern_DTest
 */
object TightPattern_DTest extends App
{
    val g2 = EX_GRAPH.g2p
    val q2 = EX_GRAPH.q2p

    val g3 = EX_GRAPH.g3p
    val q3 = EX_GRAPH.q3p

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q2.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g2.printG ()

    // Tight Graph Simulation Pattern Matcher

    (new TightSim (g2, q2, new DualSim (g2, q2))).test ("TightSim", Answers_g2.phi13)                 // Tight Simulation 
    (new TightSim (g2, q2, new DualSim2 (g2, q2))).test ("TightSim2", Answers_g2.phi13)               // Tight Simulation 2 
    (new TightSim (g2, q2, new DualSimCAR (g2, q2))).test ("TightSimCAR", Answers_g2.phi14)           // Tight Simulation CAR

    // Tight MGraph Simulation Pattern Matcher

    (new MTightSim (g2, q2, new MDualSim (g2, q2))).test ("MTightSim", Answers_g2.phi13)              // MTight Simulation 
    (new MTightSim (g2, q2, new MDualSim2 (g2, q2))).test ("MTightSim2", Answers_g2.phi13)            // MTight Simulation 2
    (new MTightSim (g2, q2, new MDualSimCAR (g2, q2))).test ("MTightSimCAR", Answers_g2.phi14)        // MTight Simulation CAR

    // MTight Simulation Pattern Matcher- checks mismatched edge labels

    (new MTightSim (g3, q3, new MDualSim (g3, q3))).test ("MTightSim", Answers_g3.phi15)              // MTight Simulation         
    (new MTightSim (g3, q3, new MDualSim2 (g3, q3))).test ("MTightSim2", Answers_g3.phi15)            // MTight Simulation 2       
    (new MTightSim (g3, q3, new MDualSimCAR (g3, q3))).test ("MTightSimCAR", Answers_g3.phi16)        // MTight Simulation CAR
 
} // TightPattern_DTest object

