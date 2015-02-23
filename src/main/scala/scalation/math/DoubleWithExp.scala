
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.1
 *  @date    Mon Nov 14 2:34:38 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.scala-lang.org/node/724
 */

package scalation.math

import scala.language.implicitConversions

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoubleWithExp` class defines an expontiation operator '~^' for Doubles.
 *  @param x  the base
 */
case class DoubleWithExp (x: Double)
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Exponentiation operator for scala Doubles (x ~^ y).
     *  @param y  the exponent
     */
    def ~^ (y: Double) = math.pow (x, y)

} // DoubleWithExp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoubleWithExp` companion object provides implicit conversion from Double
 *  to DoubleWithExp allowing '~^' to be applied to Doubles.
 *  It also provide a negative exponential function.
 *
 */
object DoubleWithExp
{
    implicit def doubleWithExp (d: Double) = DoubleWithExp (d)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Negative exponential funtion (e to the minus x).
     *  @param x  the argument of the function
     */
    def nexp (x: Double) = math.exp (-x)

} // DoubleWithExp object


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DoubleWithExpTest` object is used to test the `DoubleWithExp` class.
 */
object DoubleWithExpTest extends App
{
    import DoubleWithExp._
    println (2.0 ~^ 3.0)

} // DoubleWithExpTest object

