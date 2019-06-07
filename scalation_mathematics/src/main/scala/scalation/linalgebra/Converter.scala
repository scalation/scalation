
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
 *  @version 1.6
 *  @date    Mon Oct 12 17:26:57 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.linalgebra

import scalation.math.StrO.StrNum
import scalation.util.BiMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Converter` object converts string number vectors to regular numeric vectors.
 */
object Converter
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a `VectorS` into a `VectorI` by mapping each distinct value in
     *  `VectorS` into a distinct numeric integer value, returning the new vector
     *  and the bidirectional mapping.  Use the 'from' method in `BiMap` to recover
     *  the original string.
     *  e.g., VectorS ("A", "B", "C", "A", "D") will be mapped to VectorI (0, 1, 2, 0, 3)
     *  @param s  the vector of string numbers to convert
     */
    def map2Int (s: VectoS): (VectoI, BiMap [StrNum, Int]) =
    {
        val map = new BiMap [StrNum, Int] ()
        var counter = 0
        for (i <- 0 until s.dim if ! (map contains s(i))) {
            map     += s(i) -> counter
            counter += 1
        } // for
        val c = new VectorI (s.dim)
        for (i <- 0 until s.dim) c(i) = map(s(i))
        (c, map)
    } // map2Int

} // Converter


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConverterTest` object is used to test the `Converter` class.
 *  > runMain scalation.linalgebra.ConverterTest
 */
object ConverterTest extends App
{
    val months = VectorS ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    val (c, map) = Converter.map2Int (months)

    println (s"c   = $c") 
    println (s"map = $map") 
    for (ci <- c) println (s"from ($ci) = ${map.from (ci)}")

} // ConverterTest object

