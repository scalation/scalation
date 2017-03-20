
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
 *  @version 1.3
 *  @date    Mon Oct 12 17:26:57 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra.mem_mapped

import scala.collection.mutable.Map
import scalation.math.StrO.StrNum

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Converter` object converts string number vectors to integer vectors.
 */
object Converter
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a `VectorS` into a `VectorI` by mapping each distinct value in
     *  `VectorS` into a distinct numeric integer value, returning the new vector
     *  and the mapping.
     *  e.g.. VectorS ("A", "B", "C", "A", "D") will be mapped to VectorI (0, 1, 2, 0, 3)
     *  @param s  the vector of string numbers to convert
     */
    def mapToInt (s: VectorS): Tuple2 [VectorI, Map [StrNum, Int]] =
    {
      val map = Map [StrNum, Int] ()
      var counter = 0
      for (i <- 0 until s.dim if ! (map contains s(i))) {
          map     += s(i) -> counter
          counter += 1
      } // for
      val c = new VectorI (s.dim)
      for (i <- 0 until s.dim) c(i) = map(s(i))
      (c, map)
    } // mapToInt

} // Converter

