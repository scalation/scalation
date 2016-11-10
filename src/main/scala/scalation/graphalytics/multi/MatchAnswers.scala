
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Supriya Ramireddy, John Miller
 *  @version 1.2
 *  @date    Wed Nov  9 14:58:54 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics.multi

import scala.collection.mutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatchAnswers` object contains answers for the graph matching problem
 *  given in the following paper:
 *  John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
 *  "Research Directions in Big Data Graph Analytics,"
 *  Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
 *  New York, New York (June-July 2015) pp. 785-794.
 */
object MatchAnswers
{
    /** Answer for Graph Simulation
     */
    val graphSim = Array (Set (1, 6, 8, 12, 16, 19, 20, 24, 27, 30), 
                          Set (2, 7, 13, 15, 17, 21, 23, 26, 29), 
                          Set (3, 4, 5, 9, 11, 14, 18, 22, 25, 28), 
                          Set (3, 4, 5, 9, 11, 14, 18, 22, 25, 28))
                        
    /** Answer for Dual Graph Simulation
     */
    val dualSim = Array (Set (1, 6, 8, 12, 16, 19, 20, 24, 27, 30), 
                         Set (2, 7, 13, 15, 17, 21, 23, 26, 29), 
                         Set (3, 4, 5, 9, 14, 18, 22, 25, 28), 
                         Set (3, 4, 5, 9, 14, 18, 22, 25, 28))

    /** Answer for Strict Graph Simulation
     */
    val strictSim = Array (Set (1, 6, 8, 12), 
                           Set (2, 7, 13), 
                           Set (3, 4, 5, 9, 14), 
                           Set (3, 4, 5, 9, 14))

    /** Answer for Tight Graph Simulation
     */
    val tightSim = Array (Set (1, 12), 
                          Set (2, 13),  
                          Set (3, 4, 5, 14), 
                          Set (3, 4, 5, 14))

    /** Answer for Subgraph Isomorphism
     */
    val dualIso = Array (Set (1), 
                         Set (2), 
                         Set (3, 4, 5), 
                         Set (3, 4, 5))
                         
/*
    val dualIso = Set (Array (1, 2, 3, 4), 
                       Array (1, 2, 3, 5), 
                       Array (1, 2, 4, 5), 
                       Array (1, 2, 4, 3), 
                       Array (1, 2, 5, 3), 
                       Array (1, 2, 5, 4))
*/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the values in the result 'sim' by the offset (e.g., 1 -> 0).
     *  'sim' is an array of sets (e.g., set-values functions).
     *  @param sim     the unshifted answer
     *  @param offset  how far to shift the answers
     */
    def shift (sim: Array [Set [Int]], offset: Int = -1): Array [Set [Int]] =
    {
        sim.map (_.map ((i: Int) => i + offset))
    } // shift

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Shift the values in the result 'fun' by the offset (e.g., 1 -> 0).
     *  'fun' is an set of arrays (.e.g., set of bijections).
     *  @param fun     the unshifted answer
     *  @param offset  how far to shift the answers
     */
    def shiftf (fun: Set [Array [Int]], offset: Int = -1): Set [Array [Int]] =
    {
        val offset = -1
        fun.map (_.map ((i: Int) => i + offset))
    } // shiftf

} // MatchAnswers object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MatchAnswersTest` object is used to test the `MatchAnswers` object.
 *  > run-main scalation.graphalytics.multi.MatchAnswersTest
 */
object MatchAnswersTest extends App
{
    import MatchAnswers._

    println ("graphSim:   " + shift (graphSim).deep)
    println ("dualSim:    " + shift (dualSim).deep)
    println ("strictSim:  " + shift (strictSim).deep)
    println ("tightSim:   " + shift (tightSim).deep)
    println ("dualIsoSim: " + shift (dualIso).deep)

} // MatchAnswersTest object

