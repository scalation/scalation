
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.5
 *  @date    Sat May  6 13:40:02 EDT 2017 
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scala.collection.immutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Answers` object is used to test all the Graph pattern matchers.
 */
object Answers
{
    val phi1 = Array (Set (0, 15, 19, 5, 18, 7, 11, 26, 23, 29),
                      Set (12, 1, 16, 20, 6, 28, 25, 22, 14),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 10, 4, 8))

    val phi2 = Array (Set (0, 15, 19, 5, 18, 7, 11, 26, 23, 29),
                      Set (12, 1, 16, 20, 6, 28, 25, 22, 14),
                      Set (27, 13, 2, 17, 24, 3, 21, 4, 8),
                      Set (27, 13, 2, 17, 24, 3, 21, 4, 8))

    val phi3 = Array (Set(0, 5, 7, 11),
                      Set(12, 1, 6),
                      Set(13, 2, 3, 4, 8),
                      Set(13, 2, 3, 4, 8))
 
    val phi4 = Array (Set(0, 11),
                      Set(12, 1),
                      Set(13, 2, 3, 4),
                      Set(13, 2, 3, 4))

} // Answers object

