
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed Jan  2 15:20:24 EST 2019
 *  @see     LICENSE (MIT style license file).
 */

package scalation.analytics

import scala.collection.mutable.HashMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HyperParameter` class provides a simple and flexible means for handling
 *  model hyper-parameters.  A model may have one or more hyper-parameters that
 *  are organized into a map 'name -> (value, defaultV)'.
 */
class HyperParameter extends Cloneable
{
   private val hparam = HashMap [String, PairD] ()       // hyper-parameter map

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Given the 'name', return the hyper-parameter value.
    *  @param name  the name of the hyper-parameter
    */
   def apply (name: String): Double = hparam (name)._1

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Given the 'name', return the hyper-parameter default value.
    *  @param name  the name of the hyper-parameter
    */
   def default (name: String): Double = hparam (name)._2

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Given the 'name', update the hyper-parameter value.
    *  @param name   the name of the hyper-parameter
    *  @param value  the value of the hyper-parameter
    */
   def update (name: String, value: Double)
   {
       val (v, d) = hparam (name)
       hparam += name -> (value, d)
   } // update

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Create and return a new set of hyper-parameters, updating the one with the
    *  given 'name'.
    *  @param name   the name of the hyper-parameter
    *  @param value  the value of the hyper-parameter
    */
   def updateReturn (name: String, value: Double): HyperParameter =
   {
       val hp2 = clone ().asInstanceOf [HyperParameter]
       val (v, d) = hp2.hparam (name)
       hp2.hparam += name -> (value, d)
       hp2
   } // updateReturn

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Create and return a new set of hyper-parameters, updating the one with the
    *  given 'name'.
    *  @param nvs  the name-value pair for the hyper-parameter
    */
   def updateReturn (nvs: (String, Double)*): HyperParameter =
   {
       val hp2 = clone ().asInstanceOf [HyperParameter]
       for (nv <- nvs) {
           val (v, d) = hp2.hparam (nv._1)
           hp2.hparam += nv._1 -> (nv._2, d)
       } // for
       hp2
   } // updateReturn

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Given the 'name', update the hyper-parameter default value.
    *  @param name      the name of the hyper-parameter
    *  @param defaultV  the defualt value of the hyper-parameter
    */
   def updateDefault (name: String, defaultV: Double)
   {
       val (v, d) = hparam (name)
       hparam -= name
       hparam += name -> (v, defaultV)
   } // updateDefault

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Add a new hyper-parameter to the map.
    *  @param name      the name of the hyper-parameter
    *  @param value     the value of the hyper-parameter
    *  @param defaultV  the defualt value of the hyper-parameter
    */
   def += (name: String, value: Double, defaultV: Double)
   {
       hparam += name -> (value, defaultV)
   } // +=

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Remove the hyper-parameter with the given name from the map.
    *  @param name  the name of the hyper-parameter
    */
   def -= (name: String)
   {
       hparam -= name
   } // -=

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Convert the hyper-parameter map to a string.
    */
   override def toString: String = "HyperParameter (" + hparam.toString + ")"

} // HyperParameter class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `HyperParameterTest` object is used to test the `HyperParameter` class.
 *  runMain scalation.analytics.HyperParameterTest
 */
object HyperParameterTest extends App
{
    val hp = new HyperParameter
    hp += ("eta", 0.1, 0.1)
    hp += ("bSize", 10, 10)
    hp += ("maxEpochs", 10000, 10000)

    println (s"hp = $hp")

    //hp("eta") = 0.2

    println (s"hp = $hp")
    println (s"""hp ("eta") = ${hp ("eta")}""")
    println (s"""hp.default ("eta") = ${hp.default ("eta")}""")

} // HyperParameterTest object

