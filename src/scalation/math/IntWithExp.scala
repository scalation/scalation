
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Mon Nov 14 2:34:38 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.scala-lang.org/node/724
 */

package scalation.math

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This class defines an expontiation operator '~^' for Ints.
 *  @param x  the base
 */
case class IntWithExp (x: Int)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exponentiation operator for scala Ints (x ~^ y).
     *  @param y  the exponent
     */
    def ~^ (y: Int) = math.pow (x, y).toInt

} // IntWithExp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Implicit conversion from Int to IntWithExp allowing '~^' to be applied
 *  to Ints.
 */
object IntWithExp
{
    implicit def intWithExp (d: Int) = IntWithExp (d)

} // IntWithExp object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** This object is used to test the IntWithExp class.
 */
object IntWithExpTest extends App
{
    import IntWithExp._
    println (2 ~^ 3)

} // IntWithExpTest object

