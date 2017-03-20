
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Tue Aug  9 16:39:41 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graphalytics.mutable.{ExampleGraphD => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMGraphD` object contains example query and data multi-digraphs
 *  in which  the vertex label type `TLabel` is `Double`.
 */
object ExampleMGraphD
{
    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    // data multi-digraph g1 -------------------------------------------------

    val g1 = MGraph (EX_GRAPH.g1,
                     Map ((1, 0) -> -1.0,
                          (1, 2) -> -1.0,
                          (1, 3) -> -1.0,
                          (1, 4) -> -1.0,
                          (2, 0) -> -1.0,
                          (3, 4) -> -2.0),                // change from -1 to -2 filter out vertices
                     "g1")

     // query multi-digraph q1 ------------------------------------------------

     val q1 = MGraph (EX_GRAPH.q1,
                      Map ((0, 1) -> -1.0,
                           (0, 2) -> -1.0,
                           (2, 1) -> -1.0),
                      "q1")

    val g1p = new MGraph (g1.ch, g1.label, g1.elabel, true, g1.name)    // with parents
    val q1p = new MGraph (q1.ch, q1.label, q1.elabel, true, q1.name)    // with parents

    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data multi-digraph g2 -------------------------------------------------

     val g2 = MGraph (EX_GRAPH.g2,
                      Map ((0, 1) -> 1.0,
                           (1, 0) -> 1.0,
                           (1, 2) -> 1.0,
                           (1, 3) -> 1.0,
                           (1, 4) -> 1.0,                  // 2
                           (1, 5) -> 1.0,
                           (5, 6) -> 1.0,
                           (5, 10) -> 1.0,
                           (6, 7) -> 1.0,
                           (6, 4) -> 1.0,                  // 2
                           (6, 8) -> 1.0,
                           (6, 9) -> 1.0,
                           (7, 1) -> 1.0,
                           (10, 11) -> 1.0,
                           (11, 12) -> 1.0,
                           (12, 11) -> 1.0,
                           (12, 13) -> 1.0,
                           (14, 13) -> 1.0,
                           (14, 15) -> 1.0,
                           (15, 16) -> 1.0,
                           (16, 17) -> 1.0,
                           (16, 18) -> 1.0,
                           (17, 14) -> 1.0,
                           (17, 19) -> 1.0,
                           (18, 20) -> 1.0,
                           (19, 14) -> 1.0,
                           (20, 19) -> 1.0,
                           (20, 21) -> 1.0,
                           (22, 21) -> 1.0,
                           (22, 23) -> 1.0,
                           (23, 25) -> 1.0,
                           (25, 24) -> 1.0,
                           (25, 26) -> 1.0,
                           (26, 28) -> 1.0,
                           (28, 27) -> 1.0,
                           (28, 29) -> 1.0,
                           (29, 22) -> 1.0),
                      "g2")

     // query multi-digraph q2 ------------------------------------------------

     val q2 = MGraph (EX_GRAPH.q2,
                      Map ((0, 1) -> 1.0,
                           (1, 0) -> 1.0,
                           (1, 2) -> 1.0,
                           (1, 3) -> 1.0),
                      "q2")

    val g2p = new MGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // ExampleMGraphD object

