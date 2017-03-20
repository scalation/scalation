
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Mon Nov  7 16:13:44 EST 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Mutable Sets
 */

package scalation.graphalytics.multi

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graphalytics.mutable.Graph
import scalation.graphalytics.mutable.{ExampleGraphI => EX_GRAPH}

import MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMuGraphI` object contains example query and data multi-digraphs
 *  in which  the vertex label type `TLabel` is `Int`.
 */
object ExampleMuGraphI
{
    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    val schema1 = Map (10 -> "user",
                       11 -> "user",
                       -1 -> "knows",
                       -2 -> "employes")

    // data multi-digraph g1 -------------------------------------------------

    val g1 = new MuGraph (Array (SET (),                          // ch(0)
                                 SET (0, 2, 3, 4),                // ch(1)
                                 SET (0),                         // ch(2)
                                 SET (4),                         // ch(3)
                                 SET ()),                         // ch(4)
                          Array (11, 10, 11, 11, 11),             // vertex labels
                          Map ((1, 0) -> ν(-1),                   // edge labels
                               (1, 2) -> ν(-1),
                               (1, 3) -> ν(-1),
                               (1, 4) -> ν(-1),
                               (2, 0) -> ν(-1),
                               (3, 4) -> ν(-2)),                  // change from -1 to -2 filter out vertices
                          false, "g1", schema1)                   // inverse, name, schema

    // query multi-digraph q1 ------------------------------------------------

    val q1 = new MuGraph (Array (SET (1, 2),                      // ch(0)
                                 SET (),                          // ch(1)
                                 SET (1)),                        // ch(2)
                          Array (10, 11, 11),                     // vertex labels
                          Map ((0, 1) -> ν(-1),                   // edge labels
                               (0, 2) -> ν(-1),
                               (2, 1) -> ν(-1)),
                          false, "q1", schema1)                   // inverse, name, schema

    val g1p = new MuGraph (g1.ch, g1.label, g1.elabel, true, g1.name, schema1)    // with parents
    val q1p = new MuGraph (q1.ch, q1.label, q1.elabel, true, q1.name, schema1)    // with parents

    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data multi-digraph g2 -------------------------------------------------

    val g2 = MuGraph (EX_GRAPH.g2,
                      Map ((0, 1)   -> ν(1),
                           (1, 0)   -> ν(1),
                           (1, 2)   -> ν(1),
                           (1, 3)   -> ν(1),
                           (1, 4)   -> ν(1),                  // 2
                           (1, 5)   -> ν(1),
                           (5, 6)   -> ν(1),
                           (5, 10)  -> ν(1),
                           (6, 7)   -> ν(1),
                           (6, 4)   -> ν(1),                  // 2
                           (6, 8)   -> ν(1),
                           (6, 9)   -> ν(1),
                           (7, 1)   -> ν(1),
                           (10, 11) -> ν(1),
                           (11, 12) -> ν(1),
                           (12, 11) -> ν(1),
                           (12, 13) -> ν(1),
                           (14, 13) -> ν(1),
                           (14, 15) -> ν(1),
                           (15, 16) -> ν(1),
                           (16, 17) -> ν(1),
                           (16, 18) -> ν(1),
                           (17, 14) -> ν(1),
                           (17, 19) -> ν(1),
                           (18, 20) -> ν(1),
                           (19, 14) -> ν(1),
                           (20, 19) -> ν(1),
                           (20, 21) -> ν(1),
                           (22, 21) -> ν(1),
                           (22, 23) -> ν(1),
                           (23, 25) -> ν(1),
                           (25, 24) -> ν(1),
                           (25, 26) -> ν(1),
                           (26, 28) -> ν(1),
                           (28, 27) -> ν(1),
                           (28, 29) -> ν(1),
                           (29, 22) -> ν(1)),
                      "g2")

    // query multi-digraph q2 ------------------------------------------------

    val q2 = MuGraph (EX_GRAPH.q2,
                      Map ((0, 1) -> ν(1),
                           (1, 0) -> ν(1),
                           (1, 2) -> ν(1),
                           (1, 3) -> ν(1)),
                      "q2")

    val g2p = new MuGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MuGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // ExampleMuGraphI object

