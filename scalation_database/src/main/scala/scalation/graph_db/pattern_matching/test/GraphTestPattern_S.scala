
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.4
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graph_db.{ExampleMGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphTestPattern_S` object is used to test all the Graph Simulation pattern
 *  matchers for labels of type `String`.
 *  > run-main scalation.graph_db.pattern_matching.test.GraphTestPattern_S
 */
object GraphTestPattern_S extends App
{
    val g2  = EX_GRAPH.g2p
    val q2  = EX_GRAPH.q2p

    val g3  = EX_GRAPH.g3p
    val q3  = EX_GRAPH.q3p

    val g4  = EX_GRAPH.g4p                                      // query graph
    val q4w = EX_GRAPH.q4wp                                     // query graph with wildcards
    val q4x = EX_GRAPH.q4xp                                     // query graph with regex


    println (s"g2.checkEdges = ${g2.checkEdges}")
    q4w.printG ()
    q4x.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g4.printG ()

    //Answers.convert (new MGraphSim (g4, q4).test ("MGraphSim", Answers_g2.phi5W))           // MDual Simulation W on q4W Wildcards

    // Graph Simulation Pattern Matcher

    (new GraphSim (g2, q2)).test ("GraphSim", Answers_g2.phi1)            // Graph Simulation
    (new GraphSim2 (g2, q2)).test ("GraphSim2", Answers_g2.phi1)          // Graph Simulation 2
    (new GraphSimCAR (g2, q2)).test ("GraphSimCAR", Answers_g2.phi2)      // Graph Simulation CAR

    // MGraph Simulation Pattern Matcher

    (new MGraphSim (g2, q2)).test ("MGraphSim", Answers_g2.phi1)          // MGraph Simulation
    (new MGraphSimW (g2, q2)).test ("MGraphSimW", Answers_g2.phi1)        // MGraph Simulation W on q2
    (new MGraphSimW (g4, q4w)).test ("MGraphSimW", Answers_g2.phi1)       // MGraph Simulation W on q4 Wildcards on EdgeLabels
    (new MGraphSimX (g4, q4x)).test ("MGraphSimX", Answers_g2.phi1)       // MGraph Simulation X on q5 Regex on EdgeLabels
    (new MGraphSim2 (g2, q2)).test ("MGraphSim2", Answers_g2.phi1)        // MGraph Simulation 2
    (new MGraphSim2W (g2, q2)).test ("MGraphSim2W", Answers_g2.phi1)      // MGraph Simulation 2 W on q2
    (new MGraphSim2W (g4, q4w)).test ("MGraphSim2W", Answers_g2.phi1)     // MGraph Simulation 2 W on q4 Wildcards on EdgeLabels
    (new MGraphSim2X (g4, q4x)).test ("MGraphSim2X", Answers_g2.phi1)     // MGraph Simulation 2 X on q5 Regex on EdgeLabels
    (new MGraphSimCAR (g2, q2)).test ("MGraphSimCAR", Answers_g2.phi2)    // MGraph Simulation CAR

    // MGraph Simulation Pattern Matcher- checks mismatched edge labels

    (new MGraphSim (g3, q3)).test ("MGraphSim", Answers_g3.phi3)          // MGraph Simulation
    (new MGraphSimW (g3, q3)).test ("MGraphSimW", Answers_g3.phi3)        // MGraph Simulation W on q3
    (new MGraphSimX (g3, q3)).test ("MGraphSimX", Answers_g3.phi3)        // MGraph Simulation Rx on q5 Regex
    (new MGraphSim2 (g3, q3)).test ("MGraphSim2", Answers_g3.phi3)        // MGraph Simulation 2
    (new MGraphSim2W (g3, q3)).test ("MGraphSim2W", Answers_g3.phi3)      // MGraph Simulation 2 W on q3
    (new MGraphSim2X (g3, q3)).test ("MGraphSim2X", Answers_g3.phi3)      // MGraph Simulation 2 Rx on q5 Regex
    (new MGraphSimCAR (g3, q3)).test ("MGraphSimCAR", Answers_g3.phi4)    // MGraph Simulation CAR

} // GraphTestPattern_S object

