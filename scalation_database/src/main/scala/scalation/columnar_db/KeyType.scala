
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yang Fan
 *  @version 1.5
 *  @date    Tue Apr 24 17:50:11 EDT 2018
 *  @see     LICENSE (MIT style license file).
 */

package scalation.columnar_db

import scalation.columnar_db.TableObj.Row
import scalation.math.{Complex, Rational, Real}
import scalation.math.StrO.StrNum
import scalation.util.BpTreeMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KeyType` class is used as key type for relation key and is needed for
 *  `SortedMap` such as `BpTreeMap`.
 *  @param valu  the value for the key (any of several types)
 */
case class KeyType (valu: Any) extends Ordered [KeyType]
{
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether 'this' key equals 'that' key.
     *  @param that  the other key
     */
    def equals (that: KeyType): Boolean = compare (that) == 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare 'this' key with 'that' key.
     *  @param that  the other key
     */
    def compare (that: KeyType): Int =
    {
        valu match {
        case _: Complex  => valu.asInstanceOf [Complex]  compare (that.valu.asInstanceOf [Complex])
        case _: Double   => valu.asInstanceOf [Double]   compare (that.valu.asInstanceOf [Double])
        case _: Int      => valu.asInstanceOf [Int]      compare (that.valu.asInstanceOf [Int])
        case _: Long     => valu.asInstanceOf [Long]     compare (that.valu.asInstanceOf [Long])
        case _: StrNum   => valu.asInstanceOf [StrNum]   compare (that.valu.asInstanceOf [StrNum])
        case _: Rational => valu.asInstanceOf [Rational] compare (that.valu.asInstanceOf [Rational])
        case _: Real     => valu.asInstanceOf [Real]     compare (that.valu.asInstanceOf [Real])
        case _           => println ("KeyType: type not supported type is " + valu.getClass); 0
        } // match
    } // compare

} // KeyType class

