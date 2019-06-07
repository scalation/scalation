
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miler
 *  @version 1.6
 *  @date    Wed Nov 4 12:27:00 EDT 2017
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scalation.linalgebra.VectoD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `clusterer` package contains classes, traits and objects for clustering
 *  algorithms.
 */
package object clusterer
{
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a distance metric (e.g., distance squared) between vectors/points
     *  'u' and 'v'.  Override this methods to use a different metric, e.g.,
     *     'norm'   - the Euclidean distance, 2-norm
     *     'norm1'  - the Manhattan distance, 1-norm
     *  @param u  the first vector/point
     *  @param v  the second vector/point
     */
    def dist (u: VectoD, v: VectoD): Double =
    {
        (u - v).normSq     // squared Euclidean norm used for efficiency, may use other norms
    } // dist

} // clusterer package object

