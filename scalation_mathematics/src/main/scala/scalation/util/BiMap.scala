
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Fri Aug 24 15:46:03 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.util

import scala.collection.mutable.HashMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BiMap` class maps keys to values and values to keys.
 *  @tparam K  the key type
 *  @tparam V  the value type
 */
class BiMap [K, V] extends HashMap [K, V]
{
    /** The back map from values to keys
     */
    private val back = HashMap [V, K] ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the (key, value) to the map.
     *  @param kv  the key-value pair
     */
    override def += (kv: (K, V)): BiMap.this.type =
    {
        super.+= (kv)
        back  += kv._2 -> kv._1
        this
    } // +=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the a value 'v', return the most recent key it was entered with.
     *  If values are unqiue, keys and values will be one-to-one correspondence.
     *  Note, if all keys are needed should use a `MultiMap`.
     *  @param v  the value whose key is sought
     */
    def from (v: V) = back (v)

} // BiMap class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BiMapTest` class is used the test the `BiMap` class.
 * runMain scalation.util.BiMapTest
 */
object BiMapTest extends App
{
    val m = new BiMap [String, Int] ()
    m += "Jan" ->  1
    m += "Feb" ->  2
    m += "Mar" ->  3
    m += "Apr" ->  4
    m += "May" ->  5
    m += "Jun" ->  6
    m += "Jul" ->  7
    m += "Aug" ->  8
    m += "Sep" ->  9
    m += "Oct" -> 10
    m += "Nov" -> 11
    m += "Dec" -> 12

    for (i <- 1 to 12) println (s"month $i is ${m.from (i)}")

} // BiMapTest object

