
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat May 18 14:57:50 EDT 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics
package classifier

import scalation.linalgebra.{VectoI, VectorI}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Feature` class is used to store branch values for feature 'f'.
 *  When 'f' is categorical or low order ordinal, these will be all its distinct values.
 *  When 'f' is continuous or high order ordinal, these will be 0 (up to threshold)
 *                                                           or 1 (above threshold).
 *  @see `Node` for 'threshold' 
 *  @param x       the feature/column data vector
 *  @param f       the selected feature/column
 *  @param isCont  whether the feature is treated as continuous
 */
case class Feature (f: Int, x: VectoI = null, isCont: Boolean = false)
{
    val values = if (isCont) VectorI (0, 1)                // 0 => below, 1 => above threshold
                 else x.distinct                           // the distinct values
    if (isCont) values.sort ()                             // in increasing order

} // Feature class

