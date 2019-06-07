
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.6
 *  @date    Sat May  6 13:40:02 EDT 2017 
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Answers_g2` object is used to test all the Graph pattern matchers on 'g2'.
 *   1 -> Graph Simulation
 *   2 -> CAR Graph Simulation
 *   3 -> MGraph Simulation
 *   4 -> CAR MGraph Simulation
 *   5 -> Dual Simulation
 *   6 -> CAR Dual Simulation
 *   7 -> MDual Simulation
 *   8 -> CAR MDual Simulation
 *   9 -> Strict Simulation
 *  10 -> CAR Strict Simulation
 *  11 -> MStrict Simulation
 *  12 -> CAR Strict Simulation
 *  13 -> Tight Simulation
 *  14 -> CAR Tight Simulation
 *  15 -> MTight Simulation
 *  16 -> CAR MTight Simulation
 */
object Answers_g2
{
    // Graph Simulation, same for MGraph Simulation 

    val phi1 = Array (Set (0, 15, 19, 5, 18, 7, 11, 26, 23, 29),
                      Set (12, 1, 16, 20, 6, 28, 25, 22, 14),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8))

    // CAR Graph Simulation, same for CAR MGraph Simulation  

    val phi2 = Array (Set (0, 5, 7),
                      Set (1, 6),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8))
    
    // Dual Simulation, same for MDual Simulation 

    val phi5 = Array (Set (0, 15, 19, 5, 18, 7, 11, 26, 23, 29),
                      Set (12, 1, 16, 20, 6, 28, 25, 22, 14),
                      Set (27, 13, 2, 17, 24, 3, 21, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 4, 8))

    val phi5W = Array (Set (0, 15, 19, 5, 18, 7, 11, 26, 23, 29),
                       Set (12, 1, 16, 20, 6, 28, 25, 22, 14),
                       Set (27, 13, 2, 17, 24, 3, 21, 4, 8, 10),           // added 10 to phi5
                       Set (27, 13, 2, 17, 24, 3, 21, 4, 8, 10))           // added 10 to phi5

    // CAR Dual Simulation, same for CAR MDual Simulation 

    val phi6 = Array (Set (0, 5, 7),
                      Set (1, 6),
                      Set (2, 3, 4, 8),
                      Set (2, 3, 4, 8))

    // Strict Simulation, same for MStrict Simulation 

    val phi9 = Array (Set (0, 5, 7, 11),
                      Set (12, 1, 6),
                      Set (13, 2, 3, 4, 8),
                      Set (13, 2, 3, 4, 8))
 
    // CAR Strict Simulation, same for CAR MStrict Simulation 

    val phi10 = Array (Set (0, 5, 7),
                       Set (1, 6),
                       Set (2, 3, 4, 8),
                       Set( 2, 3, 4, 8))
 
    // Tight Simulation, same for MTight Simulation 

    val phi13 = Array (Set (0, 11),
                       Set (12, 1),
                       Set (13, 2, 3, 4),
                       Set (13, 2, 3, 4))
 
    // CAR Tight Simulation, same for CAR MTight Simulation 

    val phi14 = Array (Set (0),
                       Set (1),
                       Set (2, 3, 4),
                       Set (2, 3, 4))

} // Answers_g2 object

import Answers_g2._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The Answers_g2Test` show the answers for 'g2'.
 */
object Answers_g2Test extends App with Answers
{
    println ("Graph Sim");      convert (phi1)
    println ("CAR Graph Sim");  convert (phi2)
    println ("Dual Sim");       convert (phi5)
    println ("CAR Dual Sim");   convert (phi6)
    println ("Strict Sim");     convert (phi9)
    println ("CAR Strict Sim"); convert (phi10)
    println ("Tight Sim");      convert (phi13)
    println ("CAR Tight Sim");  convert (phi14)

} // Answers_g2Test object

