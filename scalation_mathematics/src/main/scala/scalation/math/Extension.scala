
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Mon Nov 14 2:34:38 EST 2011
 *  @see     LICENSE (MIT style license file).
 *  @see     http://www.scala-lang.org/node/724
 *
 *  The `Int_Exp`, `Long_Exp` and `Double_Exp` classes provide extension methods:
 *      exponentiation (~^, ↑)
 *      approximately equal to (=~, ≈)
 *      not approximately equal to (!=~, ≈)
 *      not equal to (≠)
 *      less than or equal to (≤)
 *      greater than or equal to (≥)
 *      within bounds (in, ∈)
 *      element of set (in, ∈)
 *      not within bounds (not_in, ∉)
 *      not element of set (not_in, ∉)
 *  Note, the precedence of operators is determined by their first character.
 *  Unicode characters (that are not ASCII) and ~ have the highest precedence (e.g., ↑),
 *  while letters have the lowest precedence (e.g., in).
 *  For efficiency, they are value classes that enrich `Int`, `Long` and 'Double`,
 *  respectively.  The corresponding implicit conversions are in the `math` package object.
 *  @see stackoverflow.com/questions/14861862/how-do-you-enrich-value-classes-without-overhead
 */

package scalation.math

import scalation.util.banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Int_Exp` value class adds an exponentiation operator 'x ~^ y' and a
 *  'near_eq' operator 'x =~ y' to `Int`.
 *  The '~^' has higher precedence than '*' or '/'.
 *  @param self  the underlying object to be accessed via the self accessor
 */
class Int_Exp (val self: Int) extends AnyVal
{ 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' to the 'y'-th power.
     *  @param y  the given exponent
     */
    def ~^ (y: Int): Int = scala.math.pow (self, y).toInt 
    def ↑ (y: Int): Int  = scala.math.pow (self, y).toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is approximately equal to 'y'.
     *  @param y  the given value
     */
    def =~ (y: Double): Boolean = near_eq (self, y)
    def ≈ (y: Double): Boolean  = near_eq (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not approximately equal to 'y'.
     *  @param y  the given value
     */
    def !=~ (y: Double): Boolean = ! near_eq (self, y)
    def ≉ (y: Double): Boolean   = ! near_eq (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not equal to 'y'.
     *  @param y  the given value
     */
    def ≠ (y: Int): Boolean = self != y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is less than or equal to 'y'.
     *  @param y  the given value
     */
    def ≤ (y: Int): Boolean = self <= y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is greater than or equal to 'y'.
     *  @param y  the given value
     */
    def ≥ (y: Int): Boolean = self >= y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is within the given bounds
     *  @param lim  the given (lower, upper) bounds
     */
    def in (lim: (Int, Int)): Boolean = lim._1 <= self && self <= lim._2
    def ∈ (lim: (Int, Int)): Boolean  = lim._1 <= self && self <= lim._2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is in the given set.
     *  @param lim  the given set of values
     */
    def in (set: Set [Int]): Boolean = set contains self
    def ∈ (set: Set [Int]): Boolean  = set contains self

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not within the given bounds
     *  @param lim  the given (lower, upper) bounds
     */
    def not_in (lim: (Int, Int)): Boolean = ! (lim._1 <= self && self <= lim._2)
    def ∉ (lim: (Int, Int)): Boolean      = ! (lim._1 <= self && self <= lim._2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not in the given set.
     *  @param lim  the given set of values
     */
    def not_in (set: Set [Int]): Boolean = ! (set contains self)
    def ∉ (set: Set [Int]): Boolean      = ! (set contains self)

} // Int_Exp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Long_Exp` value class adds an exponentiation operator 'x ~^ y' and a
 *  'near_eq' operator 'x =~ y' to `Long`.
 *  The '~^' has higher precedence than '*' or '/'.
 *  @param self  the underlying object to be accessed via the self accessor
 */
class Long_Exp (val self: Long) extends AnyVal
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' to the 'y'-th power.
     *  @param y  the given exponent
     */
    def ~^ (y: Long): Long = scala.math.pow (self, y).toLong 
    def ↑ (y: Long): Long  = scala.math.pow (self, y).toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is approximately equal to 'y'.
     *  @param y  the given value
     */
    def =~ (y: Double): Boolean = near_eq (self, y)
    def ≈ (y: Double): Boolean  = near_eq (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not approximately equal to 'y'.
     *  @param y  the given value
     */
    def !=~ (y: Double): Boolean = ! near_eq (self, y)
    def ≉ (y: Double): Boolean   = ! near_eq (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not equal to 'y'.
     *  @param y  the given value
     */
    def ≠ (y: Long): Boolean = self != y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is less than or equal to 'y'.
     *  @param y  the given value
     */
    def ≤ (y: Long): Boolean = self <= y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is greater than or equal to 'y'.
     *  @param y  the given value
     */
    def ≥ (y: Long): Boolean = self >= y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is within the given bounds
     *  @param lim  the given (lower, upper) bounds
     */
    def in (lim: (Long, Long)): Boolean = lim._1 <= self && self <= lim._2
    def ∈ (lim: (Long, Long)): Boolean  = lim._1 <= self && self <= lim._2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is in the given set.
     *  @param lim  the given set of values
     */
    def in (set: Set [Long]): Boolean = set contains self
    def ∈ (set: Set [Long]): Boolean  = set contains self

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not within the given bounds
     *  @param lim  the given (lower, upper) bounds
     */
    def not_in (lim: (Long, Long)): Boolean = ! (lim._1 <= self && self <= lim._2)
    def ∉ (lim: (Long, Long)): Boolean      = ! (lim._1 <= self && self <= lim._2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not in the given set.
     *  @param lim  the given set of values
     */
    def not_in (set: Set [Long]): Boolean = ! (set contains self)
    def ∉ (set: Set [Long]): Boolean      = ! (set contains self)

} // Long_Exp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Double_Exp` value class adds an exponentiation operator 'x ~^ y' and
 *  a 'near_eq' operator 'x =~ y' to `Double`.
 *  The '~^' has higher precedence than '*' or '/'.
 *  @param self  the underlying object to be accessed via the self accessor
 */
class Double_Exp (val self: Double) extends AnyVal
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Raise 'this' to the 'y'-th power.
     *  @param y  the given exponent
     */
    def ~^ (y: Double): Double = scala.math.pow (self, y)
    def ↑ (y: Double): Double  = scala.math.pow (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is approximately equal to 'y'.
     *  @param y  the given value
     */
    def =~ (y: Double): Boolean = near_eq (self, y)
    def ≈ (y: Double): Boolean  = near_eq (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not approximately equal to 'y'.
     *  @param y  the given value
     */
    def !=~ (y: Double): Boolean = ! near_eq (self, y)
    def ≉ (y: Double): Boolean   = ! near_eq (self, y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not equal to 'y'.
     *  @param y  the given value
     */
    def ≠ (y: Double): Boolean = self != y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is less than or equal to 'y'.
     *  @param y  the given value
     */
    def ≤ (y: Double): Boolean = self <= y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is greater than or equal to 'y'.
     *  @param y  the given value
     */
    def ≥ (y: Double): Boolean = self >= y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is within the given bounds
     *  @param lim  the given (lower, upper) bounds
     */
    def in (lim: (Double, Double)): Boolean = lim._1 <= self && self <= lim._2
    def ∈ (lim: (Double, Double)): Boolean  = lim._1 <= self && self <= lim._2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is in the given set.
     *  @param lim  the given set of values
     */
    def in (set: Set [Double]): Boolean = set contains self
    def ∈ (set: Set [Double]): Boolean  = set contains self

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not within the given bounds
     *  @param lim  the given (lower, upper) bounds
     */
    def not_in (lim: (Double, Double)): Boolean = ! (lim._1 <= self && self <= lim._2)
    def ∉ (lim: (Double, Double)): Boolean      = ! (lim._1 <= self && self <= lim._2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' is not in the given set.
     *  @param lim  the given set of values
     */
    def not_in (set: Set [Double]): Boolean = ! (set contains self)
    def ∉ (set: Set [Double]): Boolean      = ! (set contains self)

} // Double_Exp class


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExtensionTest` object is used to test the `Int_Exp`, `Long_Exp` and `Double_Exp`
 *  classes.
 *  > runMain scalation.math.ExtensionTest
 */
object ExtensionTest extends App
{
    banner ("Test exponentiation operator: ~^, ↑")
    println ("2   ~^ 3   = " + 2   ~^ 3)
    println ("2l  ~^ 3l  = " + 2l  ~^ 3l)
    println ("2.0 ~^ 3.0 = " + 2.0 ~^ 3.0)

    println ("2   ↑ 3   = " + 2   ↑ 3)
    println ("2l  ↑ 3l  = " + 2l  ↑ 3l)
    println ("2.0 ↑ 3.0 = " + 2.0 ↑ 3.0)

    banner ("Test approximately equal to operator: =~, ≈")
    println ("2   =~ 3   = " + (2   =~ 3))
    println ("2l  =~ 3l  = " + (2l  =~ 3l))
    println ("2.0 =~ 3.0 = " + (2.0 =~ 3.0))

    println ("2   =~ 2   = " + (2   =~ 2))
    println ("2l  =~ 2l  = " + (2l  =~ 2l))
    println ("2.0 =~ 2.0000000000001 = " + (2.0 =~ 2.0000000000001))
    println ("2.0 =~ 2.000000000001  = " + (2.0 =~ 2.000000000001))

    println ("2   ≈ 3   = " + 2   ≈ 3)
    println ("2l  ≈ 3l  = " + 2l  ≈ 3l)
    println ("2.0 ≈ 3.0 = " + 2.0 ≈ 3.0)

    println ("2   ≈ 2   = " + 2   ≈ 2)
    println ("2l  ≈ 2l  = " + 2l  ≈ 2l)
    println ("2.0 ≈ 2.0000000000001 = " + 2.0 ≈ 2.0000000000001)
    println ("2.0 ≈ 2.000000000001  = " + 2.0 ≈ 2.000000000001)

    banner ("Test not approximately equal to operator: !=~, ≉")
    println ("2   !=~ 3   = " + (2   !=~ 3))
    println ("2l  !=~ 3l  = " + (2l  !=~ 3l))
    println ("2.0 !=~ 3.0 = " + (2.0 !=~ 3.0))

    println ("2   !=~ 2   = " + (2   !=~ 2))
    println ("2l  !=~ 2l  = " + (2l  !=~ 2l))
    println ("2.0 !=~ 2.0 = " + (2.0 !=~ 2.0))

    println ("2   ≉ 3   = " + 2   ≉ 3)
    println ("2l  ≉ 3l  = " + 2l  ≉ 3l)
    println ("2.0 ≉ 3.0 = " + 2.0 ≉ 3.0)

    println ("2   ≉ 2   = " + 2   ≉ 2)
    println ("2l  ≉ 2l  = " + 2l  ≉ 2l)
    println ("2.0 ≉ 2.0 = " + 2.0 ≉ 2.0)

    banner ("Test not equal to operator: ≠")
    println ("2   ≠ 3   = " + 2   ≠ 3)
    println ("2l  ≠ 3l  = " + 2l  ≠ 3l)
    println ("2.0 ≠ 3.0 = " + 2.0 ≠ 3.0)

    println ("2   ≠ 2   = " + 2   ≠ 2)
    println ("2l  ≠ 2l  = " + 2l  ≠ 2l)
    println ("2.0 ≠ 2.0 = " + 2.0 ≠ 2.0)

    banner ("Test less than to equal to operator: ≤") 
    println ("2   ≤ 3   = " + 2   ≤ 3)
    println ("2l  ≤ 3l  = " + 2l  ≤ 3l)
    println ("2.0 ≤ 3.0 = " + 2.0 ≤ 3.0)

    println ("2   ≤ 1   = " + 2   ≤ 1)
    println ("2l  ≤ 1l  = " + 2l  ≤ 1l)
    println ("2.0 ≤ 1.0 = " + 2.0 ≤ 1.0)

    banner ("Test greater than to equal to operator: ≥")
    println ("2   ≥ 3   = " + 2   ≥ 3)
    println ("2l  ≥ 3l  = " + 2l  ≥ 3l)
    println ("2.0 ≥ 3.0 = " + 2.0 ≥ 3.0)

    println ("2   ≥ 1   = " + 2   ≥ 1)
    println ("2l  ≥ 1l  = " + 2l  ≥ 1l)
    println ("2.0 ≥ 1.0 = " + 2.0 ≥ 1.0)

    banner ("Test within bounds: in, ∈")
    println ("3   in (2, 4)     = " + (3   in (2, 4)))
    println ("3l  in (2l, 4l)   = " + (3l  in (2l, 4l)))
    println ("3.0 in (2.0, 4.0) = " + (3.0 in (2.0, 4.0)))

    println ("5   in (2, 4)     = " + (5   in (2, 4)))
    println ("5l  in (2l, 4l)   = " + (5l  in (2l, 4l)))
    println ("5.0 in (2.0, 4.0) = " + (5.0 in (2.0, 4.0)))

    println ("3   ∈ (2, 4)     = " + 3   ∈ (2, 4))
    println ("3l  ∈ (2l, 4l)   = " + 3l  ∈ (2l, 4l))
    println ("3.0 ∈ (2.0, 4.0) = " + 3.0 ∈ (2.0, 4.0))

    println ("5   ∈ (2, 4)     = " + 5   ∈ (2, 4))
    println ("5l  ∈ (2l, 4l)   = " + 5l  ∈ (2l, 4l))
    println ("5.0 ∈ (2.0, 4.0) = " + 5.0 ∈ (2.0, 4.0))

    banner ("Test element of set: in, ∈")
    println ("3   in Set (2, 3)     = " + (3   in Set (2, 3)))
    println ("3l  in Set (2l, 3l)   = " + (3l  in Set (2l, 3l)))
    println ("3.0 in Set (2.0, 3.0) = " + (3.0 in Set (2.0, 3.0)))

    println ("3   in Set (2, 4)     = " + (3   in Set (2, 4)))
    println ("3l  in Set (2l, 4l)   = " + (3l  in Set (2l, 4l)))
    println ("3.0 in Set (2.0, 4.0) = " + (3.0 in Set (2.0, 4.0)))

    println ("3   ∈ Set (2, 3)     = " + (3   ∈ Set (2, 3)))
    println ("3l  ∈ Set (2l, 3l)   = " + (3l  ∈ Set (2l, 3l)))
    println ("3.0 ∈ Set (2.0, 3.0) = " + (3.0 ∈ Set (2.0, 3.0)))

    println ("3   ∈ Set (2, 4)     = " + (3   ∈ Set (2, 4)))
    println ("3l  ∈ Set (2l, 4l)   = " + (3l  ∈ Set (2l, 4l)))
    println ("3.0 ∈ Set (2.0, 4.0) = " + (3.0 ∈ Set (2.0, 4.0)))

    banner ("Test within bounds: not_in, ∉")
    println ("3   not_in (2, 4)     = " + (3   not_in (2, 4)))
    println ("3l  not_in (2l, 4l)   = " + (3l  not_in (2l, 4l)))
    println ("3.0 not_in (2.0, 4.0) = " + (3.0 not_in (2.0, 4.0)))

    println ("5   not_in (2, 4)     = " + (5   not_in (2, 4)))
    println ("5l  not_in (2l, 4l)   = " + (5l  not_in (2l, 4l)))
    println ("5.0 not_in (2.0, 4.0) = " + (5.0 not_in (2.0, 4.0)))

    println ("3   ∉ (2, 4)     = " + 3   ∉ (2, 4))
    println ("3l  ∉ (2l, 4l)   = " + 3l  ∉ (2l, 4l))
    println ("3.0 ∉ (2.0, 4.0) = " + 3.0 ∉ (2.0, 4.0))

    println ("5   ∉ (2, 4)     = " + 5   ∉ (2, 4))
    println ("5l  ∉ (2l, 4l)   = " + 5l  ∉ (2l, 4l))
    println ("5.0 ∉ (2.0, 4.0) = " + 5.0 ∉ (2.0, 4.0))

    banner ("Test element of set: not_in, ∉")
    println ("3   not_in Set (2, 3)     = " + (3   not_in Set (2, 3)))
    println ("3l  not_in Set (2l, 3l)   = " + (3l  not_in Set (2l, 3l)))
    println ("3.0 not_in Set (2.0, 3.0) = " + (3.0 not_in Set (2.0, 3.0)))

    println ("3   not_in Set (2, 4)     = " + (3   not_in Set (2, 4)))
    println ("3l  not_in Set (2l, 4l)   = " + (3l  not_in Set (2l, 4l)))
    println ("3.0 not_in Set (2.0, 4.0) = " + (3.0 not_in Set (2.0, 4.0)))

    println ("3   ∉ Set (2, 3)     = " + (3   ∉ Set (2, 3)))
    println ("3l  ∉ Set (2l, 3l)   = " + (3l  ∉ Set (2l, 3l)))
    println ("3.0 ∉ Set (2.0, 3.0) = " + (3.0 ∉ Set (2.0, 3.0)))

    println ("3   ∉ Set (2, 4)     = " + (3   ∉ Set (2, 4)))
    println ("3l  ∉ Set (2l, 4l)   = " + (3l  ∉ Set (2l, 4l)))
    println ("3.0 ∉ Set (2.0, 4.0) = " + (3.0 ∉ Set (2.0, 4.0)))

} // ExtensionTest object

