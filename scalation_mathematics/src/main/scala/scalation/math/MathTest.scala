
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Wed May 27 15:41:54 EDT 2015
 *  @see     LICENSE (MIT style license file).
 */

package scalation.math

import scala.math.log10

import scalation.util.banner

import ExtremeD.{EPSILON, MIN_NORMAL}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MathTest` object is used to test the supplemental
 *  power, root, exponential, sign, indicator, logarithmic and trigonmetric functions
 *  defined in the 'scalation.math' package object.
 *  > runMain scalation.math.MathTest
 */
object MathTest extends App
{
    banner ("Test Power and Root Functions")
    println ("2 ~^ 3        = " + 2 ~^ 3)
    println ("2l ~^ 3l      = " + 2l ~^ 3l)
    println ("2.0 ~^ 3.0    = " + 2.0 ~^ 3.0)
    println ("pow (2l, 3l)  = " + pow (2l, 3l))
    println ("root (8l, 3l) = " + root (8l, 3l))
    println ("nexp (2.0)    = " + nexp (2.0))

    banner ("Test Logarithmic Functions")
    println ("log2 (2)      = " + log2 (2.0))
    println ("log2 (4)      = " + log2 (4.0))
    println ("log10 (10)    = " + log10 (10.0))
    println ("log10 (100)   = " + log10 (100.0))
    println ("logb (4, 4)   = " + logb (4.0, 4.0))
    println ("logb (4, 16)  = " + logb (4.0, 16.0))

    banner ("Test Trignometric Functions")
    println ("cot (0.7854)  = " + cot (0.7854))
    println ("csc (0.7854)  = " + csc (0.7854))
    println ("sec (0.7854)  = " + sec (0.7854))

    banner ("Test Sign and Indicator Functions")
    println ("sign (4, -2)  = " + sign (4, -2))
    println ("oneIf (2 > 1) = " + oneIf (2 > 1))
    println ("oneIf (1 > 2) = " + oneIf (1 > 2))

    banner ("Test/Show machine epsilon")
    println (s"EPSILON    = $EPSILON")
    println (s"MIN_NORMAL = $MIN_NORMAL")

    var (x, delta) = (1.0, 1E-15)
    banner ("Test Near Equality - middle numbers")
    println (s"$x =~ $x + $delta     \t= ${x =~ x + delta}")
    println (s"$x =~ $x + 3x EPSILON \t= ${x =~ x + 3*x * EPSILON}")
    println (s"$x =~ $x + 2x EPSILON \t= ${x =~ x + 2*x * EPSILON}")
    println (s"$x =~ $x + x EPSILON  \t= ${x =~ x + x * EPSILON}")

    x = 1E15; delta = 1.0
    banner ("Test Near Equality - large numbers")
    println (s"$x =~ $x + $delta     \t= ${x =~ x + delta}")
    println (s"$x =~ $x + 3x EPSILON \t= ${x =~ x + 3*x * EPSILON}")
    println (s"$x =~ $x + 2x EPSILON \t= ${x =~ x + 2*x * EPSILON}")
    println (s"$x =~ $x + x EPSILON  \t= ${x =~ x + x * EPSILON}")

    x = 1E-15; delta = 1E-30
    banner ("Test Near Equality - small numbers")
    println (s"$x =~ $x + $delta     \t= ${x =~ x + delta}")
    println (s"$x =~ $x + 3x EPSILON \t= ${x =~ x + 3*x * EPSILON}")
    println (s"$x =~ $x + 2x EPSILON \t= ${x =~ x + 2*x * EPSILON}")
    println (s"$x =~ $x + x EPSILON  \t= ${x =~ x + x * EPSILON}")

    x = 1E-15; delta = 1E-30
    banner ("Test Near Equality - x == 0")
    println (s"$x           =~ 0.0 \t= ${x              =~ 0.0}")
    println (s"EPSILON      =~ 0.0 \t= ${EPSILON        =~ 0.0}")
    println (s"3 MIN_NORMAL =~ 0.0 \t= ${5 * MIN_NORMAL =~ 0.0}")
    println (s"2 MIN_NORMAL =~ 0.0 \t= ${4 * MIN_NORMAL =~ 0.0}")
    println (s"MIN_NORMAL   =~ 0.0 \t= ${MIN_NORMAL     =~ 0.0}")

    banner ("Test/Show Missing Value Representations") 
    println (s"noComplex  = $noComplex")
    println (s"noDouble   = $noDouble")
    println (s"noInt      = $noInt")
    println (s"noLong     = $noLong")
    println (s"noRational = $noRational")
    println (s"noReal     = $noReal")
    println (s"noStrNum   = $noStrNum")
    println (s"noTimeNum  = $noTimeNum")

} // MathTest object

