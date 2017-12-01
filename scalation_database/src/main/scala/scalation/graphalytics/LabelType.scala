
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Wed May 13 14:58:25 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.graphalytics

import scalation.linalgebra.VectorD
import scalation.math.ExtremeD.MAX_VALUE

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LabelType` specifies (data) type for vertex and edge labels.
 *  `TLabel` must be a subtype of Ordering [Any].
 *  @see www.scala-lang.org/api/current/#scala.math.Ordering
 */
object LabelType
{
    /** Type for label, e.g., `Int`, `Double`, `String`, `Vector`D, etc. (not made generic for speed)
     */
//  type TLabel = Int                      // change and recompile
//  type TLabel = Double
    type TLabel = String
//  type TLabel = VectorD

    /** String indicator, e.g., `Int`, `Double`, `String`, `Vector`D, etc. (not made generic for speed)
     */
//  val sTLabel = "Int"                    // change and recompile
//  val sTLabel = "Double"
    val sTLabel = "String"
//  val sTLabel = "VectorD"

    /** The default value for the `TLabel` type
     */
//  val TLabel_DEFAULT = 0                 // change and recompile
//  val TLabel_DEFAULT = 0.0
    val TLabel_DEFAULT = null
//  val TLabel_DEFAULT = new VectorD (0)

    /** The maximum value for the `TLabel` type
     */
//  val TLabel_MAX = Int.MaxValue          // change and recompile
//  val TLabel_MAX = MAX_VALUE
    val TLabel_MAX = null
//  val TLabel_MAX = new VectorD (MAX_VALUE)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert to the string to a `TLabel` type.
     *  @param s  the string to convert
     */
//  def toTLabel (s: String): TLabel = s.toInt                   // for Int
//  def toTLabel (s: String): TLabel = s.toDouble                // for Double
    def toTLabel (s: String): TLabel = s                         // for String
//  def toTLabel (s: String): TLabel = VectorD (s)               // for VectorD

} // LabelType object

