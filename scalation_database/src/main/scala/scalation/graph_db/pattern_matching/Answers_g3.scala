
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Sat May  6 13:40:02 EDT 2017 
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Answers_g3` object is used to test all the Graph pattern matchers on g3.
 *  1 -> Graph Simulation
 *  2 -> CAR Graph Simulation
 *  3 -> MGraph Simulation
 *  4 -> CAR MGraph Simulation
 *  5 -> Dual Simulation
 *  6 -> CAR Dual Simulation
 *  7 -> MDual Simulation
 *  8 -> CAR MDual Simulation
 *  9 -> Strict Simulation
 *  10 -> CAR Strict Simulation
 *  11 -> MStrict Simulation
 *  12 -> CAR Strict Simulation
 *  13 -> Tight Simulation
 *  14 -> CAR Tight Simulation
 *  15 -> MTight Simulation
 *  16 -> CAR MTight Simulation
 */
object Answers_g3 
{
    // MGraph Simulation

    val phi3 = Array (Set (0, 15, 19, 18, 7, 11, 26, 23, 29),
                      Set (12, 1, 16, 20, 28, 25, 22, 14),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8))
    
    // CAR MGraph Simulation

    val phi4 = Array (Set (0, 7),
                      Set (1),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8))
    
    // MDual Simulation 

    val phi7 = Array (Set (0, 15, 19, 18, 11, 26, 23, 29),
                      Set (12, 1, 16, 20, 28, 25, 22, 14),
                      Set (27, 13, 2, 17, 24, 3, 21),
                      Set (27, 13, 2, 17, 24, 3, 21))

    // CAR MDual Simulation

    val phi8 = Array (Set (0),
                      Set (1),
                      Set (2, 3),
                      Set (2, 3))

    // MStrict Simulation 

    val phi11 = Array (Set (0, 11),
                       Set (12, 1),
                       Set (13, 2, 3),
                       Set (13, 2, 3))

    // CAR MStrict Simulation

    val phi12 = Array (Set (0),
                       Set (1),
                       Set (2, 3),
                       Set (2, 3))

    // MTight Simulation

    val phi15 = Array (Set (0, 11),
                       Set (12, 1),
                       Set (13, 2, 3),
                       Set (13, 2, 3))

    // CAR MTight Simulation

    val phi16 = Array (Set (0),
                       Set (1),
                       Set (2, 3),
                       Set (2, 3)) 

} // Answers_g3 object

