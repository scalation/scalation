
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Zhe Jin
 *  @version 1.5
 *  @date    Sat Jul 2 01:27:00 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics.classifier

import scala.collection.mutable

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TabuFeatures` keeps track of pairs of features, so they are not
 *  repeatedly tried.
 */
class TabuFeatures
{
    private val tabu = new mutable.Queue [Tuple2 [Int, Int]] ()   // the tabu list used in feature swapping

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the 'j'th and 'j+1'th element of 'featureOrder' into the tabu list.
     *  If the tabu list is too large, remove the oldest element.
     *  @param feature1  'j'th element of 'featureOrder'
     *  @param feature2  'j+1'th element of 'featureOrder'
     *  @param cutOff    tabu list size
     */
    def addTaboo(feature1: Int, feature2: Int, cutOff: Int)
    {
        tabu.enqueue ((feature1, feature2))
        if (tabu.size > cutOff) tabu.dequeue
    } // addTaboo

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check if the 'j'th and 'j+1'th element of 'featureOrder' are in the tabu list.
     *  @param feature1  'j'th element of 'featureOrder'
     *  @param feature2  'j+1'th element of 'featureOrder'
     */
    def notInTaboo(feature1: Int, feature2: Int): Boolean =
    {
        ! (tabu contains (feature1, feature2)) && ! (tabu contains (feature2, feature1))
    } // notInTaboo

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear all features.
     */
    def clear () { tabu.clear () }

} // TabuFeatures class

