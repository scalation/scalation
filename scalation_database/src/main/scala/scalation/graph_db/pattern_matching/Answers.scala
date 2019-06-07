
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Aravind Kalimurthy, John Miller
 *  @version 1.6
 *  @date    Sat May  6 13:40:02 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graph_db
package pattern_matching

import scala.collection.mutable.{Set, TreeSet}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Answers` trait provides a utility for other answer objects to use.
 */
trait Answers
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the answers (the 'phi' mapping) to ordered and starting at one,
     *  matching the style given in papers.
     *  @param phi  the mapping from query vertices for matching data graph vertices
     */
    def convert (phi: Array [Set [Int]])
    {
        for (i <- phi.indices) {
            val s = new TreeSet [Int] ()
            s ++= phi(i).map (_ + 1)
            println (s"$i: $s")
         } // for
    } // convert

} // Answers trait

