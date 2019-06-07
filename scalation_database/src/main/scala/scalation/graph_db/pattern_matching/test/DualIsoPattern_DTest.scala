
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Aravind Kalimurthy
 *  @version 1.6
 *  @date    Sun Dec  3 17:25:32 EST 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching
package test

import scalation.graph_db.{ExampleMGraphD => EX_GRAPH}
import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoPattern_DTest` object is used to test all the Dual Iso pattern
 *  matchers for labels of type `Double`.
 *  FIX - finish the various implementations
 *  > runMain scalation.graph_db.pattern_matching.test.DualIsoPattern_DTest
 *
object DualIsoPattern_DTest extends App
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

    // Dual Iso Pattern Matcher

    (new DualIso (g2, q2)).test ("DualSim", Answers_g2.phi5)            // Dual Iso
    (new DualIso2 (g2, q2)).test ("DualSim2", Answers_g2.phi5)          // Dual Iso 2
    (new DualIsoCAR (g2, q2)).test ("DualSimCAR", Answers_g2.phi6)      // Dual Iso CAR

    // MDual Iso Pattern Matcher

    (new MDualIso (g2, q2)).test ("MDualSim", Answers_g2.phi5)          // MDual Iso
    (new MDualIsoW (g2, q2)).test ("MDualSimW", Answers_g2.phi5)        // MDual Iso W on q2
    (new MDualIsoW (g4, q4w)).test ("MDualSimW", Answers_g2.phi5)       // MDual Iso W on q4 Wildcards on EdgeLabels
    (new MDualIsoX (g4, q4x)).test ("MDualSimX", Answers_g2.phi5)       // MDual Iso X on q5 Regex on EdgeLabels
    (new MDualIso2 (g2, q2)).test ("MDualSim2", Answers_g2.phi5)        // MDual Iso 2
    (new MDualIso2W (g2, q2)).test ("MDualSim2W", Answers_g2.phi5)      // MDual Iso 2 W on q2
    (new MDualIso2W (g4, q4w)).test ("MDualSim2W", Answers_g2.phi5)     // MDual Iso 2 W on q4 Wildcards on EdgeLabels
    (new MDualIso2X (g4, q4x)).test ("MDualSim2X", Answers_g2.phi5)     // MDual Iso 2 X on q5 Regex on EdgeLabels
    (new MDualIsoCAR (g2, q2)).test ("MDualSimCAR", Answers_g2.phi6)    // MDual Iso CAR

    // MDual Iso Pattern Matcher - checks mismatched edge labels

    (new MDualIso (g3, q3)).test ("MDualSim", Answers_g3.phi7)          // MDual Iso
    (new MDualIsoW (g3, q3)).test ("MDualSimW", Answers_g3.phi7)        // MDual Iso W on q3
    (new MDualIsoX (g3, q3)).test ("MDualSimX", Answers_g3.phi7)        // MDual Iso Rx on q5 Regex
    (new MDualIso2 (g3, q3)).test ("MDualSim2", Answers_g3.phi7)        // MDual Iso 2
    (new MDualIso2W (g3, q3)).test ("MDualSim2W", Answers_g3.phi7)      // MDual Iso 2 W on q3
    (new MDualIso2X (g3, q3)).test ("MDualSim2X", Answers_g3.phi7)      // MDual Iso 2 Rx on q5 Regex
    (new MDualIsoCAR (g3, q3)).test ("MDualSimCAR", Answers_g3.phi8)    // MDual Iso CAR

} // DualIsoPattern_DTest object
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DualIsoPattern_DTest2` object is used to test all the Dual Iso pattern
 *  matchers for labels of type `String` for randomly generated graphs.
 *  > runMain scalation.graph_db.pattern_matching.test.DualIsoPattern_DTest2
 */
object DualIsoPattern_DTest2 extends App
{
    val gSize   = 200                                                   // number of vertices in g
    val gLabels = 10                                                    // number of labels in g
    val gDegree = 20                                                    // average out-degree for g
    val qSize   = 10                                                    // number of vertices in g
    val qDegree = 2                                                     // average out-degree for q
    val inverse = true                                                  // include links to parents

//  val stream  = 11                                                    // random number stream 0 to 999
    for (stream <- 0 until 5) {
        val rg = new GraphGen ("0", stream)                             // random graph generator
        val g  = rg.genRandomConnectedGraph (gSize, gLabels, gDegree)   // data graph g 
        banner ("data graph")
//      g.printG ()
        println (GraphMetrics.stats (g))

        val q  = rg.genBFSQuery (qSize, qDegree, g, inverse, "q")       // query graph q
        banner ("query graph")
//      q.printG ()
        println (GraphMetrics.stats (q))

        // Dual Iso Pattern Matcher

        banner ("DualIso Test")
//      (new DualIso (g, q, new DualSim (g, q))).test ("DualIso", null)         // Dual Iso - may fail

        banner ("DualIso2 Test")
        (new DualIso (g, q, new DualSim2 (g, q))).test ("DualIso2", null)       // Dual Iso

        banner ("DualIsoCAR Test")
//      (new DualIso (g, q, new DualSimCAR (g, q))).test ("DualIsoCAR", null)   // Dual Iso CAR - may fail
    } // for

} // DualIsoPattern_DTest2 object

