
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Tue Jul 25 10:50:31 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scalation.graphalytics.{ExampleGraphS => EX_GRAPH}
import scala.collection.{Set => SET}

import scalation.util.sline
import Answers._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphPatternTest` object is used to test all the Graph pattern matchers.
 *  > runMain scalation.graphalytics.GraphPatternTest
 */
object GraphPatternTest extends App
{
    val g1 = EX_GRAPH.g1p
    val q1 = EX_GRAPH.q1p

    val g2 = EX_GRAPH.g2p
    val q2 = EX_GRAPH.q2p

    val g3 = EX_GRAPH.g3p
    val q3 = EX_GRAPH.q3p

    println (s"g2.checkEdges = ${g2.checkEdges}")
    q2.printG ()
    println (s"q2.checkEdges = ${q2.checkEdges}")
    g2.printG ()

    // Graph Simulation Pattern Matcher

    (new GraphSim (g2, q2)).test ("GraphSim", phi1)             // Graph Simulation 
    (new GraphSim2 (g2, q2)).test ("GraphSim2", phi1)           // Graph Simulation 2 

    println (sline ())

    // Dual Simulation Pattern Matcher

    (new DualSim (g2, q2)).test ("DualSim", phi2)               // Dual Simulation 
    (new DualSim2 (g2, q2)).test ("DualSim2", phi2)             // Dual Simulation 2 

    println (sline ())

    // Strict Simulation Pattern Matcher

    (new StrictSim (g2, q2)).test ("StrictSim", phi3)           // Strict Simulation 
    (new StrictSim2 (g2, q2)).test ("StrictSim2", phi3)         // Strict Simulation 2 


    println (sline ())

    // Tight Simulation Pattern Matcher

    (new TightSim (g2, q2)).test ("TightSim", phi4)             // Tight Simulation 
    (new TightSim2 (g2, q2)).test ("TightSim2", phi4)           // Tight Simulation 2 


} // GraphPatternTest object

