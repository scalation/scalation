
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Tue Nov  1 19:12:16 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db.graph_algebra

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

import scalation.graph_db.{ExampleGraphD => EX_GRAPH}

import MuGraph.ν

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleMuGraphD` object contains example query and data multi-digraphs
 *  in which  the vertex label type `TLabel` is `Double`.
 */
object ExampleMuGraphD
{
    // -----------------------------------------------------------------------
    // Simple data and query multi-digraphs.
    // -----------------------------------------------------------------------
    val schema  = Array ("user", "user", "user", "user","user")

    val schema1 = Array ("user", "user", "user")
    // data multi-digraph g1 -------------------------------------------------

    val g1 = new MuGraph (Array (SET (),                          // ch(0)
                                 SET (0, 2, 3, 4),                // ch(1)
                                 SET (0),                         // ch(2)
                                 SET (4),                         // ch(3)
                                 SET ()),                         // ch(4)
                          Array (11.0, 10.0, 11.0, 11.0, 11.0),   // vertex labels
                          Map ((1, 0) -> ν(-1.0),                 // edge labels
                               (1, 2) -> ν(-1.0),
                               (1, 3) -> ν(-1.0),
                               (1, 4) -> ν(-1.0),
                               (2, 0) -> ν(-1.0),
                               (3, 4) -> ν(-2.0)),                // change from -1 to -2 filter out vertices
                          false, "g1")                            // inverse, name, schema

    // query multi-digraph q1 ------------------------------------------------

    val q1 = new MuGraph (Array (SET (1, 2),                      // ch(0)
                                 SET (),                          // ch(1)
                                 SET (1)),                        // ch(2)
                           Array (10.0, 11.0, 11.0),              // vertex labels
                          Map ((0, 1) -> ν(-1.0),                 // edge labels
                               (0, 2) -> ν(-1.0),
                               (2, 1) -> ν(-1.0)),
                          false, "q1")                            // inverse, name

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
                      Map ((0, 1)   -> ν(1.0),
                           (1, 0)   -> ν(1.0),
                           (1, 2)   -> ν(1.0),
                           (1, 3)   -> ν(1.0),
                           (1, 4)   -> ν(1.0),                  // 2
                           (1, 5)   -> ν(1.0),
                           (5, 6)   -> ν(1.0),
                           (5, 10)  -> ν(1.0),
                           (6, 7)   -> ν(1.0),
                           (6, 4)   -> ν(1.0),                  // 2
                           (6, 8)   -> ν(1.0),
                           (6, 9)   -> ν(1.0),
                           (7, 1)   -> ν(1.0),
                           (10, 11) -> ν(1.0),
                           (11, 12) -> ν(1.0),
                           (12, 11) -> ν(1.0),
                           (12, 13) -> ν(1.0),
                           (14, 13) -> ν(1.0),
                           (14, 15) -> ν(1.0),
                           (15, 16) -> ν(1.0),
                           (16, 17) -> ν(1.0),
                           (16, 18) -> ν(1.0),
                           (17, 14) -> ν(1.0),
                           (17, 19) -> ν(1.0),
                           (18, 20) -> ν(1.0),
                           (19, 14) -> ν(1.0),
                           (20, 19) -> ν(1.0),
                           (20, 21) -> ν(1.0),
                           (22, 21) -> ν(1.0),
                           (22, 23) -> ν(1.0),
                           (23, 25) -> ν(1.0),
                           (25, 24) -> ν(1.0),
                           (25, 26) -> ν(1.0),
                           (26, 28) -> ν(1.0),
                           (28, 27) -> ν(1.0),
                           (28, 29) -> ν(1.0),
                           (29, 22) -> ν(1.0)),
                      "g2")

    // query multi-digraph q2 ------------------------------------------------

    val q2 = MuGraph (EX_GRAPH.q2,
                      Map ((0, 1) -> ν(1.0),
                           (1, 0) -> ν(1.0),
                           (1, 2) -> ν(1.0),
                           (1, 3) -> ν(1.0)),
                      "q2")

    val g2p = new MuGraph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new MuGraph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

} // ExampleMuGraphD object

