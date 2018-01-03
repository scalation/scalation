
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.4
 *  @date    Mon Nov 14 2:34:38 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.scala-lang.org/node/724
 *
 *  The `Int_Exp`, `Long_Exp` and `Double_Exp` classes provide extension methods:
 *  exponentiation, approximate equals and Unicode comparison operators.
 *  For efficiency, they are value classes that enrich `Int`, `Long` and 'Double`,
 *  respectively.  The corresponding implicit conversions are in the `math` package object.
 *  @see stackoverflow.com/questions/14861862/how-do-you-enrich-value-classes-without-overhead
 */

package scalation.math

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Int_Exp` value class adds an exponentiation operator 'x ~^ y' and a
 *  'near_eq' operator 'x =~ y' to `Int`.
 *  The '~^' has higher precedence than '*' or '/'.
 *  @param self  the underlying object to be accessed via the self accessor
 */
class Int_Exp (val self: Int) extends AnyVal
{ 
    def ~^ (y: Int)     = scala.math.pow (self, y).toInt 
    def =~ (y: Double)  = near_eq (self, y)
    def !=~ (y: Double) = ! near_eq (self, y)
    def ≠ (y: Int)      = self != y
    def ≤ (y: Int)      = self <= y
    def ≥ (y: Int)      = self >= y

} // Int_Exp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Long_Exp` value class adds an exponentiation operator 'x ~^ y' and a
 *  'near_eq' operator 'x =~ y' to `Long`.
 *  The '~^' has higher precedence than '*' or '/'.
 *  @param self  the underlying object to be accessed via the self accessor
 */
class Long_Exp (val self: Long) extends AnyVal
{
    def ~^ (y: Long)    = pow (self, y) 
    def =~ (y: Double)  = near_eq (self, y)
    def !=~ (y: Double) = ! near_eq (self, y)
    def ≠ (y: Long)     = self != y
    def ≤ (y: Long)     = self <= y
    def ≥ (y: Long)     = self >= y

} // Long_Exp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Double_Exp` value class adds an exponentiation operator 'x ~^ y' and
 *  a 'near_eq' operator 'x =~ y' to `Double`.
 *  The '~^' has higher precedence than '*' or '/'.
 *  @param self  the underlying object to be accessed via the self accessor
 */
class Double_Exp (val self: Double) extends AnyVal
{
    def ~^ (y: Double)  = scala.math.pow (self, y)
    def =~ (y: Double)  = near_eq (self, y)
    def !=~ (y: Double) = ! near_eq (self, y)
    def ≠ (y: Double)   = self != y
    def ≤ (y: Double)   = self <= y
    def ≥ (y: Double)   = self >= y

} // Double_Exp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExtensionTest` object is used to test the `Int_Exp`, `Long_Exp` and `Double_Exp`
 *  classes.
 *  > runMain scalation.math.ExtensionTest
 */
object ExtensionTest extends App
{
    println (2 ~^ 3)
    println (2l ~^ 3l)
    println (2.0 ~^ 3.0)

} // ExtensionTest object

