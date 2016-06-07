
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.2
 *  @date    Wed May 27 15:41:54 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scala.math.log10

import ExtremeD.{EPSILON, MIN_NORMAL}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BasicTest` object is used to test the exponential/logarithmic functions
 *  defined in the 'scalation.math' package object.
 *  > run-main scalation.math.BasicTest
 */
object BasicTest extends App
{
    println ("2 ~^ 3        = " + 2 ~^ 3)
    println ("2l ~^ 3l      = " + 2l ~^ 3l)
    println ("2.0 ~^ 3.0    = " + 2.0 ~^ 3.0)
    println ("pow (2l, 3l)  = " + pow (2l, 3l))
    println ("root (8l, 3l) = " + root (8l, 3l))
    println ("nexp (2.0)    = " + nexp (2.0))
    println ("log2 (2)      = " + log2 (2.0))
    println ("log2 (4)      = " + log2 (4.0))
    println ("log10 (10)    = " + log10 (10.0))
    println ("log10 (100)   = " + log10 (100.0))
    println ("logb (4, 4)   = " + logb (4.0, 4.0))
    println ("logb (4, 16)  = " + logb (4.0, 16.0))
    println ("sign (4, -2)  = " + sign (4, -2))
    println ("oneIf (2 > 1) = " + oneIf (2 > 1))
    println ("oneIf (1 > 2) = " + oneIf (1 > 2))

    println (s"EPSILON    = $EPSILON")
    println (s"MIN_NORMAL = $MIN_NORMAL")

    var (x, delta) = (1.0, 1E-15)
    println ("Test Near Equality - middle numbers")
    println (s"$x =~ $x + $delta     \t= ${x =~ x + delta}")
    println (s"$x =~ $x + 3x EPSILON \t= ${x =~ x + 3*x * EPSILON}")
    println (s"$x =~ $x + 2x EPSILON \t= ${x =~ x + 2*x * EPSILON}")
    println (s"$x =~ $x + x EPSILON  \t= ${x =~ x + x * EPSILON}")

    x = 1E15; delta = 1.0
    println ("Test Near Equality - large numbers")
    println (s"$x =~ $x + $delta     \t= ${x =~ x + delta}")
    println (s"$x =~ $x + 3x EPSILON \t= ${x =~ x + 3*x * EPSILON}")
    println (s"$x =~ $x + 2x EPSILON \t= ${x =~ x + 2*x * EPSILON}")
    println (s"$x =~ $x + x EPSILON  \t= ${x =~ x + x * EPSILON}")

    x = 1E-15; delta = 1E-30
    println ("Test Near Equality - small numbers")
    println (s"$x =~ $x + $delta     \t= ${x =~ x + delta}")
    println (s"$x =~ $x + 3x EPSILON \t= ${x =~ x + 3*x * EPSILON}")
    println (s"$x =~ $x + 2x EPSILON \t= ${x =~ x + 2*x * EPSILON}")
    println (s"$x =~ $x + x EPSILON  \t= ${x =~ x + x * EPSILON}")

    x = 1E-15; delta = 1E-30
    println ("Test Near Equality - x == 0")
    println (s"$x           =~ 0.0 \t= ${x              =~ 0.0}")
    println (s"EPSILON      =~ 0.0 \t= ${EPSILON        =~ 0.0}")
    println (s"3 MIN_NORMAL =~ 0.0 \t= ${5 * MIN_NORMAL =~ 0.0}")
    println (s"2 MIN_NORMAL =~ 0.0 \t= ${4 * MIN_NORMAL =~ 0.0}")
    println (s"MIN_NORMAL   =~ 0.0 \t= ${MIN_NORMAL     =~ 0.0}")

} // BasicTest object

