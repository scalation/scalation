
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.6
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graph_db.{ExampleMGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualPatternTest_D` object is used to test all the Dual Simulation pattern
 *  matchers for labels of type `Double`.
 *  > runMain scalation.graph_db.pattern_matching.test.DualPatternTest_D
 */
object DualPatternTest_D extends App
{
    val g2 = EX_GRAPH.g2p
    val q2 = EX_GRAPH.q2p

    val g3 = EX_GRAPH.g3p
    val q3 = EX_GRAPH.q3p

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q2.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g2.printG ()

    // Dual Graph Simulation Pattern Matcher

    (new DualSim (g2, q2)).test ("DualSim", Answers_g2.phi5)               // Dual Simulation 
    (new DualSim2 (g2, q2)).test ("DualSim2", Answers_g2.phi5)             // Dual Simulation 2 
    (new DualSimCAR (g2, q2)).test ("DualSimCAR", Answers_g2.phi6)         // Dual Simulation CAR

    // Dual MGraph Simulation Pattern Matcher

    (new MDualSim (g2, q2)).test ("MDualSim", Answers_g2.phi5)             // MDual Simulation 
    (new MDualSim2 (g2, q2)).test ("MDualSim2", Answers_g2.phi5)           // MDual Simulation 2
    (new MDualSimCAR (g2, q2)).test ("MDualSimCAR", Answers_g2.phi6)       // MDual Simulation CAR

    // MDual Simulation Pattern Matcher- checks mismatched edge labels

    (new MDualSim (g3, q3)).test ("MDualSim", Answers_g3.phi7)             // MDual Simulation         
    (new MDualSim2 (g3, q3)).test ("MDualSim2", Answers_g3.phi7)           // MDual Simulation 2       
    (new MDualSimCAR (g3, q3)).test ("MDualSimCAR", Answers_g3.phi8)       // MDual Simulation CAR
 
} // DualPatternTest_D object

