
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.5
 *  @date    Tue Aug  9 16:39:41 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  Graph Data Structure Using Mutable Sets
 */

package scalation.graph_db

import scala.collection.mutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleGraphD` object contains example query and data digraphs in which
 *  the vertex label type `TLabel` is `Double`.
 */
object ExampleGraphD
{
    // -----------------------------------------------------------------------
    // Simple data and query digraphs.
    // -----------------------------------------------------------------------

    // data digraph g1 -------------------------------------------------------

    val g1 = new Graph (Array (SET (),                          // ch(0)
                               SET (0, 2, 3, 4),                // ch(1)
                               SET (0),                         // ch(2)
                               SET (4),                         // ch(3)
                               SET ()),                         // ch(4)
                        Array (11.0, 10.0, 11.0, 11.0, 11.0),   // vertex labels
                        false, "g1")                            // inverse, name

    // query digraph q1 ------------------------------------------------------

    val q1 = new Graph (Array (SET (1, 2),                      // ch(0)
                               SET (),                          // ch(1)
                               SET (1)),                        // ch(2)
                        Array (10.0, 11.0, 11.0),
                        false, "q1")

    val g1p = new Graph (g1.ch, g1.label, true, g1.name)        // with parents
    val q1p = new Graph (q1.ch, q1.label, true, q1.name)        // with parents

    // -----------------------------------------------------------------------
    // Data and query digraphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data digraph g2 -------------------------------------------------------

    val g2 = new Graph (Array (SET (1),                       // ch(0)
                               SET (0, 2, 3, 4, 5),           // ch(1)
                               SET (),                        // ch(2)
                               SET (),                        // ch(3)
                               SET (),                        // ch(4)
                               SET (6, 10),                   // ch(5)
                               SET (7, 4, 8, 9),              // ch(6)
                               SET (1),                       // ch(7)
                               SET (),                        // ch(8)
                               SET (),                        // ch(9)
                               SET (11),                      // ch(10)
                               SET (12),                      // ch(11)
                               SET (11, 13),                  // ch(12)
                               SET (),                        // ch(13)
                               SET (13, 15),                  // ch(14)
                               SET (16),                      // ch(15)
                               SET (17, 18),                  // ch(16)
                               SET (14, 19),                  // ch(17)
                               SET (20),                      // ch(18)
                               SET (14),                      // ch(19)
                               SET (19, 21),                  // ch(20)
                               SET (),                        // ch(21)
                               SET (21, 23),                  // ch(22)
                               SET (25),                      // ch(23)
                               SET (),                        // ch(24)
                               SET (24, 26),                  // ch(25)
                               SET (28),                      // ch(26)
                               SET (),                        // ch(27)
                               SET (27, 29),                  // ch(28)
                               SET (22)),                     // ch(29)
                        Array (10.0, 11.0, 12.0, 12.0, 12.0, 10.0, 11.0, 10.0, 12.0, 15.0, 12.0, 10.0, 11.0, 12.0, 11.0,
                               10.0, 11.0, 12.0, 10.0, 10.0, 11.0, 12.0, 11.0, 10.0, 12.0, 11.0, 10.0, 12.0, 11.0, 10.0),
                        false, "g2")


    // query digraph q2 ------------------------------------------------------

    val q2 = new Graph (Array (SET (1),                       // ch(0)
                               SET (0, 2, 3),                 // ch(1)
                               SET (),                        // ch(2)
                               SET ()),                       // ch(3)
                        Array (10.0, 11.0, 12.0, 12.0),
                        false, "q2")

    val g2p = new Graph (g2.ch, g2.label, true, g2.name)      // with parents
    val q2p = new Graph (q2.ch, q2.label, true, q2.name)      // with parents



    // -----------------------------------------------------------------------
    // Data and query digraphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data digraph g3 -------------------------------------------------------

    val g3 = new Graph (Array (SET (1),                       // ch(0)
                               SET (0, 2, 3, 4, 5),           // ch(1)
                               SET (),                        // ch(2)
                               SET (),                        // ch(3)
                               SET (),                        // ch(4)
                               SET (6, 10),                   // ch(5)
                               SET (7, 4, 8, 9),              // ch(6)
                               SET (1),                       // ch(7)
                               SET (),                        // ch(8)
                               SET (),                        // ch(9)
                               SET (11),                      // ch(10)
                               SET (12),                      // ch(11)
                               SET (11, 13),                  // ch(12)
                               SET (),                        // ch(13)
                               SET (13, 15),                  // ch(14)
                               SET (16),                      // ch(15)
                               SET (17, 18),                  // ch(16)
                               SET (14, 19),                  // ch(17)
                               SET (20),                      // ch(18)
                               SET (14),                      // ch(19)
                               SET (19, 21),                  // ch(20)
                               SET (),                        // ch(21)
                               SET (21, 23),                  // ch(22)
                               SET (25),                      // ch(23)
                               SET (),                        // ch(24)
                               SET (24, 26),                  // ch(25)
                               SET (28),                      // ch(26)
                               SET (),                        // ch(27)
                               SET (27, 29),                  // ch(28)
                               SET (22)),                     // ch(29)
                        Array (10.0, 11.0, 12.0, 12.0, 12.0, 10.0, 11.0, 10.0, 12.0, 15.0, 12.0, 10.0, 11.0, 12.0, 11.0,
                               10.0, 11.0, 12.0, 10.0, 10.0, 11.0, 12.0, 11.0, 10.0, 12.0, 11.0, 10.0, 12.0, 11.0, 10.0),
                        false, "g3")


    // query digraph q3 ------------------------------------------------------

    val q3 = new Graph (Array (SET (1),                       // ch(0)
                               SET (0, 2, 3),                 // ch(1)
                               SET (),                        // ch(2)
                               SET ()),                       // ch(3)
                        Array (10.0, 11.0, 12.0, 12.0),
                        false, "q3")

    val g3p = new Graph (g3.ch, g3.label, true, g3.name)      // with parents
    val q3p = new Graph (q3.ch, q3.label, true, q3.name)      // with parents


} // ExampleGraphD object

