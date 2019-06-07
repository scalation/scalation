
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 */

package scalation

import scala.math.sqrt

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `minima` package contains classes, traits and objects for
 *  optimization to find minima.
 */
package object minima
{
    /** the golden ratio (1.618033988749895)
     */
    val G_RATIO = (1.0 + sqrt (5.0)) / 2.0

    /** the golden section number (0.6180339887498949)
     */
    val G_SECTION = G_RATIO / (1.0 + G_RATIO)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `FunctionSelector` provides an enumeration of function types.
     */
    object FunctionSelector extends Enumeration
    {
        type FunctionSelector = Value
        val Function, Derivative, FiniteDifference = Value
    } // FunctionSelector object

} // minima package object 

