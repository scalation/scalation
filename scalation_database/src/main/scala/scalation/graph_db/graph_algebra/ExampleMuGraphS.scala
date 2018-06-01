
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Aravind Kalimurthy
 *  @version 1.5
 *  @date    Tue Aug  9 16:39:41 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db.graph_algebra

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graph_db.{ExampleGraphS => EX_GRAPH}
import scalation.graph_db.graph_algebra.MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMuGraphS` object contains example query and data multi-digraphs
 *  in which  the vertex label type `TLabel` is `String`.
 */
object ExampleMuGraphS
{
    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    // data multi-digraph g1 -------------------------------------------------

    val g1 = MuGraph (EX_GRAPH.g1,
                     Map ((0, 3) -> ν("knows"),
                          (0, 4) -> ν("knows"),
                          (1, 0) -> ν("knows"),
                          (1, 2) -> ν("knows"),
                          (1, 4) -> ν("knows"),
                          (2, 0) -> ν("knows", "likes"),
                          (2, 4) -> ν("likes"),
                          (3, 2) -> ν("knows"),
                          (3, 4) -> ν("likes")),
                     "g1")

    // query multi-digraph q1 ------------------------------------------------

    val q1 = MuGraph (EX_GRAPH.q1,
                      Map ((0, 1) -> ν("knows"),
                           (0, 2) -> ν("knows"),
                           (2, 1) -> ν("knows")),
                      "q1")

    val g1p = new MuGraph (g1.ch, g1.label, g1.elabel, true, g1.name)    // with parents
    val q1p = new MuGraph (q1.ch, q1.label, q1.elabel, true, q1.name)    // with parents

    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data multi-digraph g2 -------------------------------------------------

    val g2 = MuGraph (EX_GRAPH.g2,
                      Map ((0, 1) -> ν("knows"),
                           (1, 0) -> ν("knows"),
                           (1, 2) -> ν("knows"),
                           (1, 3) -> ν("knows"),
                           (1, 4) -> ν("knows"),                  // likes
                           (1, 5) -> ν("knows"),
                           (5, 6) -> ν("knows"),
                           (5, 10) -> ν("knows"),
                           (6, 7) -> ν("knows"),
                           (6, 4) -> ν("knows"),                  // likes
                           (6, 8) -> ν("knows"),
                           (6, 9) -> ν("knows"),
                           (7, 1) -> ν("knows"),
                           (10, 11) -> ν("knows"),
                           (11, 12) -> ν("knows"),
                           (12, 11) -> ν("knows"),
                           (12, 13) -> ν("knows"),
                           (14, 13) -> ν("knows"),
                           (14, 15) -> ν("knows"),
                           (15, 16) -> ν("knows"),
                           (16, 17) -> ν("knows"),
                           (16, 18) -> ν("knows"),
                           (17, 14) -> ν("knows"),
                           (17, 19) -> ν("knows"),
                           (18, 20) -> ν("knows"),
                           (19, 14) -> ν("knows"),
                           (20, 19) -> ν("knows"),
                           (20, 21) -> ν("knows"),
                           (22, 21) -> ν("knows"),
                           (22, 23) -> ν("knows"),
                           (23, 25) -> ν("knows"),
                           (25, 24) -> ν("knows"),
                           (25, 26) -> ν("knows"),
                           (26, 28) -> ν("knows"),
                           (28, 27) -> ν("knows"),
                           (28, 29) -> ν("knows"),
                           (29, 22) -> ν("knows")),
                      "g2")

    // query multi-digraph q2 ------------------------------------------------

    val q2 = MuGraph (EX_GRAPH.q2,
                      Map ((0, 1) -> ν("knows"),
                           (1, 0) -> ν("knows"),
                           (1, 2) -> ν("knows"),
                           (1, 3) -> ν("knows")),
                      "q2")

    val g2p = new MuGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MuGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // ExampleMuGraphS object

