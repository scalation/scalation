
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Aravind Kalimurthy
 *  @version 1.3
 *  @date    Tue Aug  9 16:39:41 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Mutable Sets
 */

package scalation.graphalytics.mutable

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graphalytics.mutable.{ExampleGraphS => EX_GRAPH}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMGraphS` object contains example query and data multi-digraphs
 *  in which  the vertex label type `TLabel` is `String`.
 */
object ExampleMGraphS
{
    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------

    // data multi-digraph g1 -------------------------------------------------

    val g1 = MGraph (EX_GRAPH.g1,
                     Map ((1, 0) -> "knows",
                          (1, 2) -> "knows",
                          (1, 3) -> "knows",
                          (1, 4) -> "knows",
                          (2, 0) -> "knows",
                          (3, 4) -> "likes"),                // change from "knows" to "likes" filter out vertices
                     "g1")

     // query multi-digraph q1 ------------------------------------------------

     val q1 = MGraph (EX_GRAPH.q1,
                      Map ((0, 1) -> "knows",
                           (0, 2) -> "knows",
                           (2, 1) -> "knows"),
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
                      Map ((0, 1) -> "knows",
                           (1, 0) -> "knows",
                           (1, 2) -> "knows",
                           (1, 3) -> "knows",
                           (1, 4) -> "knows",                  // likes
                           (1, 5) -> "knows",
                           (5, 6) -> "knows",
                           (5, 10) -> "knows",
                           (6, 7) -> "knows",
                           (6, 4) -> "knows",                  // likes
                           (6, 8) -> "knows",
                           (6, 9) -> "knows",
                           (7, 1) -> "knows",
                           (10, 11) -> "knows",
                           (11, 12) -> "knows",
                           (12, 11) -> "knows",
                           (12, 13) -> "knows",
                           (14, 13) -> "knows",
                           (14, 15) -> "knows",
                           (15, 16) -> "knows",
                           (16, 17) -> "knows",
                           (16, 18) -> "knows",
                           (17, 14) -> "knows",
                           (17, 19) -> "knows",
                           (18, 20) -> "knows",
                           (19, 14) -> "knows",
                           (20, 19) -> "knows",
                           (20, 21) -> "knows",
                           (22, 21) -> "knows",
                           (22, 23) -> "knows",
                           (23, 25) -> "knows",
                           (25, 24) -> "knows",
                           (25, 26) -> "knows",
                           (26, 28) -> "knows",
                           (28, 27) -> "knows",
                           (28, 29) -> "knows",
                           (29, 22) -> "knows"),
                      "g2")

     // query multi-digraph q2 ------------------------------------------------

     val q2 = MGraph (EX_GRAPH.q2,
                      Map ((0, 1) -> "knows",
                           (1, 0) -> "knows",
                           (1, 2) -> "knows",
                           (1, 3) -> "knows"),
                      "q2")

    val g2p = new MGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // ExampleMGraphS object

