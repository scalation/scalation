
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Aravind Kalimurthy
 *  @version 1.4
 *  @date    Tue Aug  9 16:39:41 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Mutable Sets
 */

package scalation.graph_db

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graph_db.{ExampleGraphI => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMGraphI` object contains example query and data multi-digraphs
 *  in which  the vertex label type `TLabel` is `Int`.
 */
object ExampleMGraphI
{
    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    // data multi-digraph g1 -------------------------------------------------

    val g1 = MGraph (EX_GRAPH.g1,
                     Map ((1, 0) -> -1,
                          (1, 2) -> -1,
                          (1, 3) -> -1,
                          (1, 4) -> -1,
                          (2, 0) -> -1,
                          (3, 4) -> -2),                // change from -1 to -2 filter out vertices
                     "g1")

     // query multi-digraph q1 ------------------------------------------------

     val q1 = MGraph (EX_GRAPH.q1,
                      Map ((0, 1) -> -1,
                           (0, 2) -> -1,
                           (2, 1) -> -1),
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
                      Map ((0, 1) -> 1,
                           (1, 0) -> 1,
                           (1, 2) -> 1,
                           (1, 3) -> 1,
                           (1, 4) -> 1,                  // 2
                           (1, 5) -> 1,
                           (5, 6) -> 1,
                           (5, 10) -> 1,
                           (6, 7) -> 1,
                           (6, 4) -> 1,                  // 2
                           (6, 8) -> 1,
                           (6, 9) -> 1,
                           (7, 1) -> 1,
                           (10, 11) -> 1,
                           (11, 12) -> 1,
                           (12, 11) -> 1,
                           (12, 13) -> 1,
                           (14, 13) -> 1,
                           (14, 15) -> 1,
                           (15, 16) -> 1,
                           (16, 17) -> 1,
                           (16, 18) -> 1,
                           (17, 14) -> 1,
                           (17, 19) -> 1,
                           (18, 20) -> 1,
                           (19, 14) -> 1,
                           (20, 19) -> 1,
                           (20, 21) -> 1,
                           (22, 21) -> 1,
                           (22, 23) -> 1,
                           (23, 25) -> 1,
                           (25, 24) -> 1,
                           (25, 26) -> 1,
                           (26, 28) -> 1,
                           (28, 27) -> 1,
                           (28, 29) -> 1,
                           (29, 22) -> 1),
                      "g2")

     // query multi-digraph q2 ------------------------------------------------

     val q2 = MGraph (EX_GRAPH.q2,
                      Map ((0, 1) -> 1,
                           (1, 0) -> 1,
                           (1, 2) -> 1,
                           (1, 3) -> 1),
                      "q2")

    val g2p = new MGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // ExampleMGraphI object

