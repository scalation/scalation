
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Tue Jan 24 09:13:15 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graph_db.{ExampleMGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphPattern_DTest` object is used to test all the Graph Simulation pattern
 *  matchers for labels of type `Double`.
 *  > runMain scalation.graph_db.pattern_matching.test.GraphPattern_DTest
 */
object GraphPattern_DTest extends App
{
    val g2 = EX_GRAPH.g2p
    val q2 = EX_GRAPH.q2p

    val g3 = EX_GRAPH.g3p
    val q3 = EX_GRAPH.q3p

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q2.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g2.printG ()

    // Graph Simulation Pattern Matcher

    (new GraphSim (g2, q2)).test ("GraphSim", Answers_g2.phi1)            // Graph Simulation 
    (new GraphSim2 (g2, q2)).test ("GraphSim2", Answers_g2.phi1)          // Graph Simulation 2 
    (new GraphSimCAR (g2, q2)).test ("GraphSimCAR", Answers_g2.phi2)      // Graph Simulation CAR 

    // MGraph Simulation Pattern Matcher

    (new MGraphSim (g2, q2)).test ("MGraphSim", Answers_g2.phi1)          // MGraph Simulation 
    (new MGraphSim2 (g2, q2)).test ("MGraphSim2", Answers_g2.phi1)        // MGraph Simulation 2 
    (new MGraphSimCAR (g2, q2)).test ("MGraphSimCAR", Answers_g2.phi2)    // MGraph Simulation CAR 

    // MGraph Simulation Pattern Matcher- checks mismatched edge labels

    (new MGraphSim (g3, q3)).test ("MGraphSim", Answers_g3.phi3)          // MGraph Simulation            
    (new MGraphSim2 (g3, q3)).test ("MGraphSim2", Answers_g3.phi3)        // MGraph Simulation 2          
    (new MGraphSimCAR (g3, q3)).test ("MGraphSimCAR", Answers_g3.phi4)    // MGraph Simulation CAR 

} // GraphPattern_DTest object

